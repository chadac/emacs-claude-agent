"""Claude Emacs Agent - Wrapper for Claude CLI with structured output for Emacs.

Outputs structured markers that Emacs can parse to format the conversation nicely:
- [USER] ... [/USER] for echoing user input
- [ASSISTANT] ... [/ASSISTANT] for Claude's responses
- [TOOL name] ... [/TOOL] for tool calls
- [STATUS model cost] for status updates
- [READY] to signal ready for input
- [ERROR] ... [/ERROR] for errors
"""

import asyncio
import io
import json
import os
import sys
import signal
import traceback
from dataclasses import dataclass, field
from typing import Optional
import argparse


# Markers for Emacs to parse
MARKER_USER_START = "[USER]"
MARKER_USER_END = "[/USER]"
MARKER_ASSISTANT_START = "[ASSISTANT]"
MARKER_ASSISTANT_END = "[/ASSISTANT]"
MARKER_TOOL_START = "[TOOL "  # followed by tool name and ]
MARKER_TOOL_END = "[/TOOL]"
MARKER_READY = "[READY]"
MARKER_ERROR_START = "[ERROR]"
MARKER_ERROR_END = "[/ERROR]"
MARKER_SESSION_START = "[SESSION]"
MARKER_SESSION_END = "[/SESSION]"
MARKER_THINKING = "[THINKING]"  # Signals Claude is processing
MARKER_PROGRESS = "[PROGRESS "  # followed by JSON and ]
MARKER_RESULT = "[RESULT "  # followed by JSON and ]
MARKER_PERMISSION_REQUEST = "[PERMISSION_REQUEST "  # followed by JSON and ]
MARKER_PERMISSION_RESPONSE = "[PERMISSION_RESPONSE "  # followed by JSON and ]


def _format_traceback() -> str:
    """Format the current exception traceback as a string."""
    return traceback.format_exc()


def _make_stdout_blocking() -> None:
    """Ensure stdout is in blocking mode to avoid write errors."""
    try:
        import fcntl
        flags = fcntl.fcntl(sys.stdout.fileno(), fcntl.F_GETFL)
        fcntl.fcntl(sys.stdout.fileno(), fcntl.F_SETFL, flags & ~os.O_NONBLOCK)
    except (io.UnsupportedOperation, AttributeError, OSError):
        pass


@dataclass
class AgentState:
    """Current state of the agent session."""

    session_id: Optional[str] = None
    model: Optional[str] = None
    status: str = "initializing"
    pending_tool_calls: dict = field(default_factory=dict)
    # Permission tracking
    session_permissions: set = field(default_factory=set)  # Patterns allowed for session
    pending_permission: Optional[dict] = None  # Current permission request awaiting response
    last_user_message: Optional[str] = None  # For retry after permission grant


class ClaudeEmacsAgent:
    """Wrapper that bridges Emacs comint and Claude CLI.

    This agent outputs in a format suitable for comint-mode:
    - Regular output for messages
    - A prompt (claude> ) when ready for input
    """

    def __init__(
        self,
        work_dir: str,
        mcp_config: Optional[str] = None,
        allowed_tools: Optional[list[str]] = None,
        log_file: Optional[str] = None,
    ):
        self.work_dir = work_dir
        self.mcp_config = mcp_config
        self.allowed_tools = allowed_tools or []
        self.state = AgentState()
        self.claude_process: Optional[asyncio.subprocess.Process] = None
        self._output_buffer = ""
        self._running = True
        self._log_file = log_file
        self._log_handle = None
        if log_file:
            self._log_handle = open(log_file, "w")

    def _log_json(self, direction: str, msg: dict) -> None:
        """Log JSON message to file if logging is enabled."""
        if self._log_handle:
            import datetime
            timestamp = datetime.datetime.now().isoformat()
            self._log_handle.write(f"[{timestamp}] {direction}: {json.dumps(msg)}\n")
            self._log_handle.flush()

    def _emit(self, message: str) -> None:
        """Send message to Emacs (stdout)."""
        try:
            _make_stdout_blocking()
            print(message, flush=True)
        except (BlockingIOError, BrokenPipeError, OSError) as e:
            sys.stderr.write(f"Failed to emit: {e}\n")

    def _emit_ready(self) -> None:
        """Emit the ready marker to signal ready for input."""
        self._emit(MARKER_READY)

    async def start(
        self,
        resume_session: Optional[str] = None,
        continue_session: bool = False,
    ) -> None:
        """Start Claude CLI in JSON streaming mode."""
        cmd = [
            "claude",
            "-p",
            "--input-format",
            "stream-json",
            "--output-format",
            "stream-json",
            "--verbose",
        ]

        if resume_session:
            cmd.extend(["--resume", resume_session])
        elif continue_session:
            cmd.append("--continue")

        if self.mcp_config:
            cmd.extend(["--mcp-config", self.mcp_config])

        if self.allowed_tools:
            cmd.extend(["--allowedTools", ",".join(self.allowed_tools)])

        self.claude_process = await asyncio.create_subprocess_exec(
            *cmd,
            stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            cwd=self.work_dir,
        )

        # Start reading Claude's output
        asyncio.create_task(self._read_claude_output())
        asyncio.create_task(self._read_claude_stderr())

        # Show initial session info
        self._emit(f"{MARKER_SESSION_START}")
        self._emit(f"Session started in {self.work_dir}")
        self._emit(f"{MARKER_SESSION_END}")
        self._emit_ready()

    async def send_user_message(self, message: str, echo: bool = True) -> None:
        """Send a user message to Claude."""
        if not self.claude_process or not self.claude_process.stdin:
            self._emit(f"{MARKER_ERROR_START}")
            self._emit("Claude process not running")
            self._emit(f"{MARKER_ERROR_END}")
            self._emit_ready()
            return

        # Store for potential retry after permission grant
        self.state.last_user_message = message

        # Echo the user message back with markers (skip on retry)
        if echo:
            self._emit(f"{MARKER_USER_START}")
            self._emit(message)
            self._emit(f"{MARKER_USER_END}")

        # Add org-mode reminder to message
        full_message = f"{message}\n\n(Reminder: format your response in org-mode.)"

        msg = {
            "type": "user",
            "message": {"role": "user", "content": [{"type": "text", "text": full_message}]},
        }
        await self._send_to_claude(msg)
        self.state.status = "thinking"
        self._emit(MARKER_THINKING)

    async def handle_permission_response(self, response: dict) -> None:
        """Handle permission response from Emacs."""
        action = response.get("action")  # "allow_once", "allow_session", "allow_always", "deny"
        pattern = response.get("pattern")  # Permission pattern for allow_always

        if action == "deny":
            self.state.pending_permission = None
            self._emit(f"{MARKER_SESSION_START}")
            self._emit("Permission denied by user")
            self._emit(f"{MARKER_SESSION_END}")
            self._emit_ready()
            return

        # Add to session permissions if requested
        if action == "allow_session" and pattern:
            self.state.session_permissions.add(pattern)

        # For allow_always, we need to update settings file
        if action == "allow_always" and pattern:
            self._add_permanent_permission(pattern)

        # Clear pending and retry the last message
        self.state.pending_permission = None
        if self.state.last_user_message:
            self._emit(f"{MARKER_SESSION_START}")
            self._emit(f"Retrying with permission: {pattern or 'once'}")
            self._emit(f"{MARKER_SESSION_END}")
            await self.send_user_message(self.state.last_user_message, echo=False)

    def _add_permanent_permission(self, pattern: str) -> None:
        """Add a permission pattern to .claude/settings.local.json."""
        import os
        settings_path = os.path.join(self.work_dir, ".claude", "settings.local.json")
        try:
            settings = {}
            if os.path.exists(settings_path):
                with open(settings_path, "r") as f:
                    settings = json.load(f)

            if "permissions" not in settings:
                settings["permissions"] = {}
            if "allow" not in settings["permissions"]:
                settings["permissions"]["allow"] = []

            if pattern not in settings["permissions"]["allow"]:
                settings["permissions"]["allow"].append(pattern)

                # Ensure directory exists
                os.makedirs(os.path.dirname(settings_path), exist_ok=True)

                with open(settings_path, "w") as f:
                    json.dump(settings, f, indent=2)

                self._emit(f"{MARKER_SESSION_START}")
                self._emit(f"Added permanent permission: {pattern}")
                self._emit(f"{MARKER_SESSION_END}")
        except Exception as e:
            self._emit(f"{MARKER_ERROR_START}")
            self._emit(f"Failed to save permission: {e}")
            self._emit(f"{MARKER_ERROR_END}")

    async def _send_to_claude(self, msg: dict) -> None:
        """Send JSON message to Claude's stdin."""
        if not self.claude_process or not self.claude_process.stdin:
            return
        self._log_json("SEND", msg)
        line = json.dumps(msg) + "\n"
        try:
            self.claude_process.stdin.write(line.encode())
            await self.claude_process.stdin.drain()
        except (BrokenPipeError, ConnectionResetError, BlockingIOError) as e:
            self._emit(f"Error: Failed to send to Claude: {e}")

    async def _read_claude_output(self) -> None:
        """Read and process JSON lines from Claude's stdout."""
        if not self.claude_process or not self.claude_process.stdout:
            return

        while self._running:
            try:
                line = await self.claude_process.stdout.readline()
                if not line:
                    break
                await self._process_claude_line(line.decode())
            except Exception as e:
                self._emit(f"Error reading Claude output: {e}")
                break

        self._emit("\n[Session ended]")
        self.state.status = "dead"

    async def _read_claude_stderr(self) -> None:
        """Read Claude's stderr for error messages."""
        if not self.claude_process or not self.claude_process.stderr:
            return

        while self._running:
            try:
                line = await self.claude_process.stderr.readline()
                if not line:
                    break
                text = line.decode().strip()
                if text and not text.startswith("Debugger"):
                    self._emit(f"[stderr: {text}]")
            except Exception:
                break

    async def _process_claude_line(self, line: str) -> None:
        """Process a single line from Claude (may be partial JSON)."""
        self._output_buffer += line

        while "\n" in self._output_buffer:
            json_line, self._output_buffer = self._output_buffer.split("\n", 1)
            json_line = json_line.strip()
            if not json_line:
                continue

            try:
                msg = json.loads(json_line)
                await self._handle_claude_message(msg)
            except json.JSONDecodeError as e:
                self._emit(f"Error: Invalid JSON from Claude: {e}")

    async def _handle_claude_message(self, msg: dict) -> None:
        """Process a message from Claude CLI."""
        self._log_json("RECV", msg)
        msg_type = msg.get("type")

        if msg_type == "system" and msg.get("subtype") == "init":
            self.state.session_id = msg.get("session_id")
            self.state.model = msg.get("model")
            self.state.status = "ready"

        elif msg_type == "assistant":
            # Emit progress with token usage
            usage = msg.get("message", {}).get("usage", {})
            if usage:
                progress = {
                    "output_tokens": usage.get("output_tokens", 0),
                    "input_tokens": usage.get("input_tokens", 0),
                    "cache_read": usage.get("cache_read_input_tokens", 0),
                }
                self._emit(f"{MARKER_PROGRESS}{json.dumps(progress)}]")

            content = msg.get("message", {}).get("content", [])
            for item in content:
                item_type = item.get("type")
                if item_type == "text":
                    text = item.get("text", "")
                    if text:
                        # Wrap assistant text in markers
                        self._emit(f"{MARKER_ASSISTANT_START}")
                        self._emit(text)
                        self._emit(f"{MARKER_ASSISTANT_END}")
                elif item_type == "tool_use":
                    tool_id = item.get("id")
                    tool_name = item.get("name")
                    self.state.pending_tool_calls[tool_id] = item
                    self._emit(f"{MARKER_TOOL_START}{tool_name}]")

        elif msg_type == "tool_result":
            tool_id = msg.get("tool_use_id")
            if tool_id in self.state.pending_tool_calls:
                del self.state.pending_tool_calls[tool_id]
            self._emit(f"{MARKER_TOOL_END}")

        elif msg_type == "result":
            self.state.status = "ready"

            # Check for permission denials
            permission_denials = msg.get("permission_denials", [])
            if permission_denials:
                # Emit permission request for user approval
                denial = permission_denials[0]  # Handle first denial
                self.state.pending_permission = denial
                perm_request = {
                    "tool_name": denial.get("tool_name"),
                    "tool_input": denial.get("tool_input"),
                    "tool_use_id": denial.get("tool_use_id"),
                }
                self._emit(f"{MARKER_PERMISSION_REQUEST}{json.dumps(perm_request)}]")
                # Don't emit ready - wait for permission response
                return

            # Emit final result stats
            result_info = {
                "cost_usd": msg.get("total_cost_usd", 0),
                "duration_ms": msg.get("duration_ms", 0),
                "num_turns": msg.get("num_turns", 0),
            }
            usage = msg.get("usage", {})
            if usage:
                result_info["total_output"] = usage.get("output_tokens", 0)
                result_info["total_input"] = usage.get("input_tokens", 0)
            self._emit(f"{MARKER_RESULT}{json.dumps(result_info)}]")
            self._emit_ready()

        elif msg_type == "error":
            self._emit(f"{MARKER_ERROR_START}")
            self._emit(msg.get("error", "Unknown error"))
            self._emit(f"{MARKER_ERROR_END}")
            self._emit_ready()

    async def interrupt(self) -> None:
        """Send interrupt signal to Claude process."""
        if self.claude_process:
            self.claude_process.send_signal(signal.SIGINT)
            self._emit(f"{MARKER_SESSION_START}")
            self._emit("Interrupted")
            self._emit(f"{MARKER_SESSION_END}")
            self._emit_ready()

    async def quit(self) -> None:
        """Gracefully shutdown the agent."""
        self._running = False
        if self.claude_process:
            self.claude_process.terminate()
            try:
                await asyncio.wait_for(self.claude_process.wait(), timeout=5.0)
            except asyncio.TimeoutError:
                self.claude_process.kill()


async def run_agent(
    work_dir: str,
    mcp_config: Optional[str] = None,
    resume_session: Optional[str] = None,
    continue_session: bool = False,
    allowed_tools: Optional[list[str]] = None,
    log_file: Optional[str] = None,
) -> None:
    """Run the agent, reading commands from stdin."""
    agent = ClaudeEmacsAgent(work_dir, mcp_config, allowed_tools, log_file)
    await agent.start(resume_session, continue_session)

    # Read input from stdin (comint sends lines)
    loop = asyncio.get_event_loop()
    reader = asyncio.StreamReader()
    protocol = asyncio.StreamReaderProtocol(reader)
    await loop.connect_read_pipe(lambda: protocol, sys.stdin)

    while agent._running:
        try:
            line = await reader.readline()
            if not line:
                break

            text = line.decode().strip()
            if not text:
                continue

            # Handle special commands
            if text == "/quit":
                await agent.quit()
                break
            elif text == "/interrupt":
                await agent.interrupt()
            elif text.startswith("/permit "):
                # Permission response from Emacs: /permit {"action": "allow_once", "pattern": "..."}
                try:
                    json_str = text[8:]  # Strip "/permit "
                    response = json.loads(json_str)
                    await agent.handle_permission_response(response)
                except json.JSONDecodeError as e:
                    agent._emit(f"{MARKER_ERROR_START}")
                    agent._emit(f"Invalid permission response: {e}")
                    agent._emit(f"{MARKER_ERROR_END}")
                    agent._emit_ready()
            else:
                # Regular message to Claude
                await agent.send_user_message(text)

        except Exception as e:
            agent._emit(f"Error: {e}")
            agent._emit_ready()

    await agent.quit()


def main() -> None:
    """Entry point for the agent."""
    _make_stdout_blocking()

    parser = argparse.ArgumentParser(
        description="Claude Emacs Agent - Wrapper for Claude CLI"
    )
    parser.add_argument(
        "--work-dir",
        required=True,
        help="Working directory for Claude session",
    )
    parser.add_argument(
        "--mcp-config",
        default=None,
        help="Path to MCP configuration file",
    )
    parser.add_argument(
        "--resume",
        default=None,
        help="Resume session by ID",
    )
    parser.add_argument(
        "--continue",
        dest="continue_session",
        action="store_true",
        help="Continue most recent session",
    )
    parser.add_argument(
        "--allowed-tools",
        default=None,
        help="Comma-separated list of allowed tools",
    )
    parser.add_argument(
        "--log-file",
        default=None,
        help="Path to write JSON message log (for debugging)",
    )
    args = parser.parse_args()

    allowed_tools = None
    if args.allowed_tools:
        allowed_tools = [t.strip() for t in args.allowed_tools.split(",")]

    asyncio.run(
        run_agent(
            work_dir=args.work_dir,
            mcp_config=args.mcp_config,
            resume_session=args.resume,
            continue_session=args.continue_session,
            allowed_tools=allowed_tools,
            log_file=args.log_file,
        )
    )


if __name__ == "__main__":
    main()
