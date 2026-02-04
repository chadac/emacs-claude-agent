"""Claude Agent - Wrapper using Claude Agent SDK with dynamic permissions.

Outputs newline-delimited JSON messages for Emacs to parse.
See protocol.py for message type definitions.
"""

import asyncio
import glob
import io
import json
import os
import random
import re
import sys
import traceback
from dataclasses import dataclass, field
from typing import Optional, Any
import argparse

from claude_agent_sdk import (
    ClaudeSDKClient,
    ClaudeAgentOptions,
    ToolPermissionContext,
    PermissionResultAllow,
    PermissionResultDeny,
    HookMatcher,
    PostToolUseHookInput,
    HookContext,
    HookJSONOutput,
)


def _format_traceback() -> str:
    """Format the current exception traceback as a string."""
    return traceback.format_exc()


def validate_tool_result(tool_response, tool_name: str = "unknown") -> dict:
    """Centralized validation and fixing of tool results.

    Ensures that tool results with is_error=True always have non-empty content,
    which is required by the Anthropic API.

    Args:
        tool_response: The tool response (dict with 'content' and 'is_error' fields,
                       or sometimes a list which we wrap)
        tool_name: Name of the tool for error message context

    Returns:
        The validated/fixed tool response
    """
    # Handle case where tool_response is not a dict (e.g., a list)
    if not isinstance(tool_response, dict):
        return {"content": tool_response, "is_error": False}

    is_error = tool_response.get("is_error", False)
    content = tool_response.get("content")

    # If not an error, no validation needed
    if not is_error:
        return tool_response

    # Check if content is empty/missing
    needs_placeholder = False
    if content is None:
        needs_placeholder = True
    elif isinstance(content, str) and not content.strip():
        needs_placeholder = True
    elif isinstance(content, list) and len(content) == 0:
        needs_placeholder = True
    elif isinstance(content, list):
        # Check if list has only empty text blocks
        has_content = False
        for item in content:
            if isinstance(item, dict):
                text = item.get("text", "")
                if isinstance(text, str) and text.strip():
                    has_content = True
                    break
            elif isinstance(item, str) and item.strip():
                has_content = True
                break
        if not has_content:
            needs_placeholder = True

    # Insert placeholder if needed
    if needs_placeholder:
        tool_response["content"] = [
            {
                "type": "text",
                "text": f"Tool '{tool_name}' failed without providing error details"
            }
        ]

    return tool_response


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
    always_permissions: set = field(default_factory=set)  # Patterns always allowed



def _build_system_reminder_block(messages: list[str]) -> str:
    """Wrap messages in <system-reminder> tags for injection.

    Each message gets its own <system-reminder> block, which is consistent
    with the existing filtering in _filter_system_reminders().
    """
    parts = []
    for msg in messages:
        parts.append(f"<system-reminder>\n{msg}\n</system-reminder>")
    return "\n\n".join(parts)



class ClaudeAgent:
    """Wrapper that bridges Emacs and Claude using the SDK.

    Uses claude-agent-sdk for programmatic access with dynamic permission
    handling via the can_use_tool callback.
    """

    # Tools that are blocked when running in Emacs integration mode.
    # These bypass Emacs and break pair programming - use MCP tools instead.
    BLOCKED_TOOLS = {"Edit", "Write"}

    def __init__(
        self,
        work_dir: str,
        mcp_config: Optional[str] = None,
        allowed_tools: Optional[list[str]] = None,
        disallowed_tools: Optional[list[str]] = None,
        log_file: Optional[str] = None,
        resume_session: Optional[str] = None,
        continue_session: bool = False,
        model: Optional[str] = None,
        system_prompt: Optional[str] = None,
        block_direct_edit: bool = True,
        auto_reject_rules: Optional[list[dict]] = None,
        max_retries: int = 3,
    ):
        self.work_dir = work_dir
        self.mcp_config = mcp_config
        self.allowed_tools = allowed_tools or []
        self.disallowed_tools = disallowed_tools or []
        self.state = AgentState()
        self._running = True
        self._log_file = log_file
        self._log_handle = None
        self._resume_session = resume_session
        self._continue_session = continue_session
        self._model = model
        self._system_prompt = system_prompt
        self._block_direct_edit = block_direct_edit  # Block Edit/Write tools for Emacs integration
        self._auto_reject_rules = auto_reject_rules or []  # Auto-reject rules for worktree confinement
        self._first_message = True  # Track if this is the first message
        self._max_retries = max_retries  # Max retries for transient API errors
        if log_file:
            os.makedirs(os.path.dirname(log_file), exist_ok=True)
            self._log_handle = open(log_file, "w")

        # Pending system messages from Emacs (injected via stdin)
        self._pending_system_messages: list[str] = []

        # SDK client - persistent across conversation turns
        self._client: Optional[ClaudeSDKClient] = None

        # Stderr capture for better error messages
        self._stderr_lines: list[str] = []

        # Permission handling - async event for waiting on user response
        # Per-request permission tracking, keyed by tool_use_id
        self._permission_events: dict[str, asyncio.Event] = {}
        self._permission_responses: dict[str, Optional[dict]] = {}
        self._pending_permission_requests: dict[str, dict] = {}

        # Load existing permissions from settings
        self._load_permissions()

    def _handle_stderr(self, line: str) -> None:
        """Capture stderr output for better error messages."""
        self._stderr_lines.append(line)
        # Keep only last 50 lines to avoid unbounded memory growth
        if len(self._stderr_lines) > 50:
            self._stderr_lines = self._stderr_lines[-50:]

    def _get_stderr_context(self) -> str:
        """Get recent stderr output for error context."""
        if not self._stderr_lines:
            return ""
        return "\n".join(self._stderr_lines[-20:])  # Last 20 lines

    def _load_permissions(self) -> None:
        """Load allowed permissions from .claude/settings.local.json."""
        settings_path = os.path.join(self.work_dir, ".claude", "settings.local.json")
        try:
            if os.path.exists(settings_path):
                with open(settings_path, "r") as f:
                    settings = json.load(f)
                    allow_list = settings.get("permissions", {}).get("allow", [])
                    self.state.always_permissions = set(allow_list)
        except Exception:
            pass

    def _log_json(self, direction: str, msg: dict) -> None:
        """Log JSON message to file if logging is enabled."""
        if self._log_handle:
            import datetime
            timestamp = datetime.datetime.now().isoformat()
            self._log_handle.write(f"[{timestamp}] {direction}: {json.dumps(msg)}\n")
            self._log_handle.flush()

    def _emit(self, msg: dict) -> None:
        """Send JSON message to Emacs (stdout as NDJSON)."""
        try:
            _make_stdout_blocking()
            json_str = json.dumps(msg, ensure_ascii=False)
            print(json_str, flush=True)
            sys.stdout.flush()  # Extra flush to ensure delivery
            self._log_json("EMIT", msg)
        except (BlockingIOError, BrokenPipeError, OSError) as e:
            sys.stderr.write(f"Failed to emit: {e}\n")

    def _emit_ready(self) -> None:
        """Emit the ready message to signal ready for input."""
        self._emit({"type": "ready"})

    def _emit_session_info(self, model: Optional[str] = None, session_id: Optional[str] = None) -> None:
        """Emit session info message with model and session_id."""
        if model:
            self.state.model = model
        if session_id:
            self.state.session_id = session_id
        if model or session_id:
            self._emit({
                "type": "session_info",
                "model": model,
                "session_id": session_id,
            })

    def _emit_error(self, message: str, tb: Optional[str] = None) -> None:
        """Emit error message."""
        self._emit({
            "type": "error",
            "message": message,
            "traceback": tb,
        })

    def _emit_session_message(self, text: str) -> None:
        """Emit a session info message (for system notifications)."""
        self._emit({"type": "session_message_start"})
        self._emit({"type": "session_message_text", "text": text})
        self._emit({"type": "session_message_end"})

    def _filter_system_reminders(self, text: str) -> str:
        """Remove <system-reminder>...</system-reminder> blocks from text."""
        import re
        # Remove system-reminder blocks (including newlines around them)
        filtered = re.sub(
            r'\n?<system-reminder>.*?</system-reminder>\n?',
            '',
            text,
            flags=re.DOTALL
        )
        return filtered

    def _format_tool_args(self, tool_name: str, tool_input: dict) -> str:
        """Format tool input for display as function-style args."""
        if tool_name in ("Read", "Write", "Edit"):
            return tool_input.get("file_path", "")
        elif tool_name == "Bash":
            return tool_input.get("command", "")
        elif tool_name == "Glob":
            return tool_input.get("pattern", "")
        elif tool_name == "Grep":
            pattern = tool_input.get("pattern", "")
            path = tool_input.get("path", "")
            if path:
                return f"{pattern}, {path}"
            return pattern
        elif tool_name == "WebFetch":
            return tool_input.get("url", "")
        elif tool_name == "Task":
            return tool_input.get("description", "")
        else:
            # For unknown tools, show first key=value
            for k, v in tool_input.items():
                return f"{k}={v}"
            return ""

    def _matches_permission(self, tool_name: str, tool_input: dict) -> bool:
        """Check if this tool use matches any allowed permission pattern."""
        all_permissions = self.state.always_permissions | self.state.session_permissions

        for pattern in all_permissions:
            if self._pattern_matches(pattern, tool_name, tool_input):
                return True
        return False

    def _pattern_matches(self, pattern: str, tool_name: str, tool_input: dict) -> bool:
        """Check if a permission pattern matches the tool use."""
        # Simple pattern matching
        # Patterns can be:
        #   "Read" - match all Read calls
        #   "Read(/path/to/file)" - match exact file
        #   "Read(/path/*)" - match files in directory
        #   "Bash(echo:*)" - match bash commands starting with echo

        if pattern == tool_name:
            return True

        if not pattern.startswith(tool_name + "("):
            return False

        # Extract the pattern content
        pattern_content = pattern[len(tool_name) + 1:-1]  # Remove "ToolName(" and ")"

        # Get the relevant input value based on tool type
        if tool_name in ("Read", "Write", "Edit"):
            input_value = tool_input.get("file_path", "")
        elif tool_name == "Bash":
            input_value = tool_input.get("command", "")
        elif tool_name == "Glob":
            input_value = tool_input.get("pattern", "")
        elif tool_name == "Grep":
            input_value = tool_input.get("pattern", "")
        elif tool_name == "WebFetch":
            # Check domain pattern
            url = tool_input.get("url", "")
            if pattern_content.startswith("domain:"):
                domain = pattern_content[7:]  # Remove "domain:"
                return domain in url
            input_value = url
        elif tool_name.startswith("mcp__emacs__"):
            # MCP emacs tools - match against file_path parameter
            input_value = tool_input.get("file_path", "")
        else:
            # For unknown tools, try to match any input value
            input_value = str(tool_input)

        # Handle wildcards
        if pattern_content.endswith("*"):
            prefix = pattern_content[:-1]
            # For Bash commands, strip the colon separator from the prefix
            # and ensure word boundary (command ends or has space/tab after)
            # So "Bash(ls:*)" matches "ls" or "ls -la" but NOT "lsof"
            if tool_name == "Bash" and prefix.endswith(":"):
                prefix = prefix[:-1]  # Remove colon
                # Check for word boundary: exact match or followed by whitespace
                if input_value == prefix:
                    return True
                elif input_value.startswith(prefix) and len(input_value) > len(prefix):
                    next_char = input_value[len(prefix)]
                    return next_char in (' ', '\t', '\n')
                return False
            return input_value.startswith(prefix)

        # Handle exact match
        return pattern_content == input_value

    def _extract_file_path(self, tool_name: str, tool_input: dict) -> str | None:
        """Extract file path from tool input for path-based matching."""
        if tool_name in ("Read", "Write", "Edit"):
            return tool_input.get("file_path")
        elif tool_name.startswith("mcp__emacs__"):
            # MCP emacs tools use file_path parameter
            return tool_input.get("file_path")
        elif tool_name == "Glob":
            return tool_input.get("path")
        elif tool_name == "Grep":
            return tool_input.get("path")
        return None

    def _matches_auto_reject(self, rule: dict, tool_name: str, tool_input: dict) -> bool:
        """Check if a tool call matches an auto-reject rule.

        Rules have:
          pattern: Tool pattern like "Edit(/path/*)" or "mcp__emacs__lock"
          path_prefix: Optional path prefix - if the tool operates on a file
                       within this prefix, it matches.
          message: Rejection message to show the agent.
        """
        pattern = rule.get("pattern")
        path_prefix = rule.get("path_prefix")

        if pattern:
            return self._pattern_matches(pattern, tool_name, tool_input)

        if path_prefix:
            # Match any tool that operates on files within this path prefix
            file_path = self._extract_file_path(tool_name, tool_input)
            if file_path and file_path.startswith(path_prefix):
                return True

        return False

    async def _fix_empty_error_content(
        self,
        hook_input: PostToolUseHookInput,
        tool_use_id: Optional[str],
        context: HookContext,
    ) -> HookJSONOutput:
        """Hook to ensure tool results with errors always have content.

        The Anthropic API requires that when is_error=True, content cannot be empty.
        This hook fixes any tool responses that violate this requirement.
        """
        self._log_json("HOOK_INPUT", {"hook_input": str(hook_input)[:500], "tool_use_id": tool_use_id})
        tool_response = hook_input.get("tool_response", {})
        tool_name = hook_input.get("tool_name", "unknown")

        # Get original content for comparison (handle non-dict tool_response)
        original_content = tool_response.get("content") if isinstance(tool_response, dict) else tool_response

        # Use centralized validation
        tool_response = validate_tool_result(tool_response, tool_name)

        # Log if we made a fix
        if original_content != tool_response.get("content"):
            self._log_json("HOOK_FIX", {
                "action": "fixed_empty_error_content",
                "tool": tool_name,
                "original_content": str(original_content)[:200],
            })

        return {"tool_response": tool_response}

    async def _clear_plan_mode_on_exit(
        self,
        hook_input: PostToolUseHookInput,
        tool_use_id: Optional[str],
        context: HookContext,
    ) -> HookJSONOutput:
        """Hook to clear plan mode state when ExitPlanMode is called.

        The Claude CLI tracks plan mode via plan files in ~/.claude/plans/.
        When ExitPlanMode is called, it marks the plan as approved but doesn't
        remove the plan file. This causes the plan mode system prompt to persist
        on subsequent turns, giving the agent conflicting instructions.

        This hook deletes the plan file after ExitPlanMode succeeds, so the
        CLI won't re-inject the plan mode system prompt.
        """
        tool_name = hook_input.get("tool_name", "")
        if tool_name != "ExitPlanMode":
            return {}

        tool_response = hook_input.get("tool_response", {})
        # Only clear if ExitPlanMode succeeded (not an error)
        is_error = False
        if isinstance(tool_response, dict):
            is_error = tool_response.get("is_error", False)
        if is_error:
            self._log_json("PLAN_MODE", {
                "action": "exit_plan_mode_failed",
                "response": str(tool_response)[:200],
            })
            return {}

        # Find and delete plan files in ~/.claude/plans/
        plans_dir = os.path.join(os.path.expanduser("~"), ".claude", "plans")
        if not os.path.isdir(plans_dir):
            self._log_json("PLAN_MODE", {
                "action": "no_plans_dir",
                "path": plans_dir,
            })
            return {}

        # Find the most recently modified plan file (the active one)
        plan_files = glob.glob(os.path.join(plans_dir, "*.md"))
        if not plan_files:
            self._log_json("PLAN_MODE", {
                "action": "no_plan_files",
                "path": plans_dir,
            })
            return {}

        # Sort by modification time, most recent first
        plan_files.sort(key=lambda f: os.path.getmtime(f), reverse=True)
        most_recent = plan_files[0]

        try:
            os.remove(most_recent)
            self._log_json("PLAN_MODE", {
                "action": "deleted_plan_file",
                "path": most_recent,
            })
        except OSError as e:
            self._log_json("PLAN_MODE", {
                "action": "failed_to_delete_plan_file",
                "path": most_recent,
                "error": str(e),
            })

        return {}

    # Workflow tools that should always be allowed without permission prompts
    # These have no side effects on the filesystem and are required for plan mode
    ALWAYS_SAFE_TOOLS = {
        "ExitPlanMode",
        "EnterPlanMode",
        "TodoWrite",
    }

    async def _can_use_tool(
        self,
        tool_name: str,
        tool_input: dict[str, Any],
        context: ToolPermissionContext,
    ) -> PermissionResultAllow | PermissionResultDeny:
        """Callback for permission checks. Asks user if not pre-approved."""
        # Try to get tool_use_id from context, or generate a unique one
        tool_use_id = getattr(context, "tool_use_id", None) or f"perm_{id(tool_input)}"
        self._log_json("PERMISSION_CHECK", {"tool": tool_name, "input": tool_input, "tool_use_id": tool_use_id})

        # Block Edit/Write tools when in Emacs integration mode
        # These bypass Emacs and break pair programming - must use MCP tools instead
        if self._block_direct_edit and tool_name in self.BLOCKED_TOOLS:
            self._log_json("PERMISSION_BLOCKED", {"tool": tool_name, "reason": "use_emacs_mcp"})
            reason = (f"Tool '{tool_name}' is blocked in Emacs integration. "
                      f"Use mcp__emacs__lock_region + mcp__emacs__write_region instead for pair programming support. "
                      f"See claude-agent-prompt.md for details.")
            self._emit({
                "type": "permission_denied",
                "tool_use_id": tool_use_id,
                "tool_name": tool_name,
                "reason": reason,
                "denial_type": "blocked",
            })
            return PermissionResultDeny(message=reason)

        # Check auto-reject rules (worktree confinement, etc.)
        for rule in self._auto_reject_rules:
            if self._matches_auto_reject(rule, tool_name, tool_input):
                reason = rule.get("message", "Auto-rejected by configuration")
                self._log_json("PERMISSION_AUTO_REJECT", {
                    "tool": tool_name, "reason": reason,
                    "pattern": rule.get("pattern", ""),
                    "path_prefix": rule.get("path_prefix", ""),
                })
                self._emit({
                    "type": "permission_denied",
                    "tool_use_id": tool_use_id,
                    "tool_name": tool_name,
                    "reason": reason,
                    "denial_type": "auto_reject",
                })
                return PermissionResultDeny(message=reason)

        # Always allow workflow/planning tools without prompting
        if tool_name in self.ALWAYS_SAFE_TOOLS:
            self._log_json("PERMISSION_AUTO_ALLOW", {"tool": tool_name, "reason": "workflow_tool"})
            return PermissionResultAllow()

        # Check if already permitted
        if self._matches_permission(tool_name, tool_input):
            self._log_json("PERMISSION_AUTO_ALLOW", {"tool": tool_name})
            return PermissionResultAllow()

        # Need to ask user - emit permission request
        self._pending_permission_requests[tool_use_id] = {
            "tool_use_id": tool_use_id,
            "tool_name": tool_name,
            "tool_input": tool_input,
        }
        self._emit({
            "type": "permission_request",
            "tool_use_id": tool_use_id,
            "tool_name": tool_name,
            "tool_input": tool_input,
        })

        # Wait for user response via stdin (per-request event)
        event = asyncio.Event()
        self._permission_events[tool_use_id] = event
        self._permission_responses[tool_use_id] = None

        try:
            # Wait for permission response (with timeout - 1 hour to allow user to step away)
            await asyncio.wait_for(event.wait(), timeout=3600.0)
        except asyncio.TimeoutError:
            self._permission_events.pop(tool_use_id, None)
            self._permission_responses.pop(tool_use_id, None)
            self._pending_permission_requests.pop(tool_use_id, None)
            self._emit_session_message("Permission request timed out")
            return PermissionResultDeny(message="Permission request timed out after 1 hour")

        response = self._permission_responses.pop(tool_use_id, None)
        self._permission_events.pop(tool_use_id, None)
        self._pending_permission_requests.pop(tool_use_id, None)

        if not response:
            return PermissionResultDeny(message="Permission request failed: no response received")

        action = response.get("action")
        pattern = response.get("pattern")

        if action == "deny":
            return PermissionResultDeny(message=f"Permission denied by user for tool '{tool_name}'")

        # Add to appropriate permission set
        if pattern:
            if action == "allow_always":
                self.state.always_permissions.add(pattern)
                self._save_permission(pattern)
                self._emit({
                    "type": "permission_granted",
                    "pattern": pattern,
                    "scope": "always",
                })
            elif action == "allow_session":
                self.state.session_permissions.add(pattern)
                self._emit({
                    "type": "permission_granted",
                    "pattern": pattern,
                    "scope": "session",
                })
            elif action == "allow_once":
                self._emit({
                    "type": "permission_granted",
                    "pattern": pattern,
                    "scope": "once",
                })

        return PermissionResultAllow()

    def _save_permission(self, pattern: str) -> None:
        """Save a permission pattern to .claude/settings.local.json."""
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

                os.makedirs(os.path.dirname(settings_path), exist_ok=True)

                with open(settings_path, "w") as f:
                    json.dump(settings, f, indent=2)
        except Exception as e:
            self._emit_error(f"Failed to save permission: {e}")

    def handle_permission_response(self, response: dict) -> None:
        """Handle permission response from Emacs (called from stdin reader).
        Routes response to the correct waiting coroutine by tool_use_id."""
        tool_use_id = response.get("tool_use_id")
        if tool_use_id and tool_use_id in self._permission_events:
            self._permission_responses[tool_use_id] = response
            self._permission_events[tool_use_id].set()
        else:
            self._log_json("PERMISSION_RESPONSE_ORPHAN", {
                "tool_use_id": tool_use_id,
                "reason": "no matching pending request",
            })

    async def connect(self) -> None:
        """Initialize and connect the SDK client."""
        # Create hook to fix empty error content
        # This prevents API errors when tools fail without providing error messages
        hooks = {
            "PostToolUse": [
                HookMatcher(hooks=[self._fix_empty_error_content]),
                HookMatcher(
                    matcher="ExitPlanMode",
                    hooks=[self._clear_plan_mode_on_exit],
                ),
            ]
        }

        options_kwargs = {
            "cwd": self.work_dir,
            "can_use_tool": self._can_use_tool,
            "mcp_servers": self.mcp_config or {},  # Pass path string or empty dict
            "allowed_tools": self.allowed_tools if self.allowed_tools else [],
            "disallowed_tools": self.disallowed_tools if self.disallowed_tools else [],
            "resume": self._resume_session,
            "continue_conversation": self._continue_session,
            "hooks": hooks,
            "model": self._model,
            "stderr": self._handle_stderr,  # Capture stderr for better error messages
        }
        # Add system prompt if provided (for oneshot agents)
        if self._system_prompt:
            options_kwargs["system_prompt"] = self._system_prompt

        options = ClaudeAgentOptions(**options_kwargs)

        self._client = ClaudeSDKClient(options=options)
        await self._client.connect()

    # ── Retry helpers ──────────────────────────────────────────────────

    # Patterns that indicate transient / retryable errors
    _TRANSIENT_PATTERNS: list[re.Pattern] = [
        re.compile(r"\b(500|502|503)\b"),          # HTTP 5xx
        re.compile(r"\b529\b"),                     # Anthropic overloaded
        re.compile(r"overloaded", re.IGNORECASE),
        re.compile(r"server.error", re.IGNORECASE),
        re.compile(r"rate.limit", re.IGNORECASE),
        re.compile(r"\b429\b"),                     # HTTP 429 rate-limit
        re.compile(r"too many requests", re.IGNORECASE),
        re.compile(r"internal server error", re.IGNORECASE),
        re.compile(r"service unavailable", re.IGNORECASE),
        re.compile(r"bad gateway", re.IGNORECASE),
        re.compile(r"gateway timeout", re.IGNORECASE),
    ]

    # Patterns that indicate permanent / non-retryable errors
    _PERMANENT_PATTERNS: list[re.Pattern] = [
        re.compile(r"\b(400|401|403|404)\b"),       # HTTP client errors
        re.compile(r"authentication.failed", re.IGNORECASE),
        re.compile(r"billing.error", re.IGNORECASE),
        re.compile(r"invalid.request", re.IGNORECASE),
        re.compile(r"permission denied", re.IGNORECASE),
        re.compile(r"content cannot be empty", re.IGNORECASE),  # SDK bug
    ]

    def _is_transient_error(self, error_text: str, subtype: str = "") -> bool:
        """Determine whether an error is transient (retryable).

        Checks both the error text and the SDK error subtype.
        Permanent errors are checked first — if an error matches a permanent
        pattern it is never retried, even if it also matches a transient one.
        """
        combined = f"{error_text} {subtype}"

        # Permanent errors are never retried
        for pat in self._PERMANENT_PATTERNS:
            if pat.search(combined):
                return False

        # Check for transient patterns
        for pat in self._TRANSIENT_PATTERNS:
            if pat.search(combined):
                return True

        # Also treat the SDK "server_error" subtype as transient
        if subtype in ("server_error", "rate_limit"):
            return True

        return False

    def _extract_retry_after(self, error_text: str) -> Optional[float]:
        """Try to extract a Retry-After value (seconds) from error/stderr text.

        Looks for patterns like 'Retry-After: 30' or 'retry after 30s' in
        the error message and recent stderr output.
        """
        sources = [error_text] + self._stderr_lines[-10:]
        for text in sources:
            # Standard header: "Retry-After: <seconds>"
            m = re.search(r"retry[- ]after\s*[:=]\s*(\d+)", text, re.IGNORECASE)
            if m:
                return float(m.group(1))
            # Prose: "retry after 30s" / "retry in 30 seconds"
            m = re.search(r"retry (?:after|in)\s+(\d+)\s*s", text, re.IGNORECASE)
            if m:
                return float(m.group(1))
        return None

    def _calculate_retry_delay(self, attempt: int, retry_after: Optional[float] = None) -> float:
        """Calculate delay before the next retry.

        Uses exponential back-off with jitter: base * 2^attempt + random jitter.
        If a Retry-After value is available (e.g. from a 429), it is used as
        the minimum delay.
        """
        base_delay = 1.0  # 1 second base
        backoff = base_delay * (2 ** attempt)
        jitter = random.uniform(0, backoff * 0.25)
        delay = backoff + jitter

        if retry_after is not None:
            delay = max(delay, retry_after)

        # Cap at 60 seconds
        return min(delay, 60.0)

    def _emit_retry_status(self, attempt: int, max_retries: int,
                           error_detail: str, delay: float) -> None:
        """Emit a retry_status message so Emacs can display retry progress."""
        self._emit({
            "type": "retry_status",
            "attempt": attempt,
            "max_retries": max_retries,
            "error": error_detail,
            "delay_seconds": round(delay, 1),
        })

    async def send_user_message(self, message: str) -> None:
        """Send a user message to Claude and stream the response.

        Wraps the query/stream cycle in a retry loop so that transient API
        errors (500, 529, 429, etc.) are retried with exponential back-off
        instead of killing the session.
        """
        # Clear stderr buffer for fresh error context
        self._stderr_lines.clear()
        # Ensure client is connected
        if not self._client:
            await self.connect()

        # Echo the user message back (only once, not on retries)
        self._emit({"type": "user_start"})
        self._emit({"type": "user_text", "text": message})
        self._emit({"type": "user_end"})

        # Slash commands (e.g. /compact, /clear) must be sent verbatim to the SDK
        # without appending system reminders, which would break command parsing.
        is_slash_command = message.startswith("/")

        # Consume any pending system messages (display already happened at receipt time)
        if self._pending_system_messages and not is_slash_command:
            reminder_block = _build_system_reminder_block(self._pending_system_messages)
            full_message = f"{message}\n\n{reminder_block}"
            self._pending_system_messages.clear()
        else:
            full_message = message

        self.state.status = "thinking"
        self._emit({"type": "thinking", "status": "Thinking..."})

        # ── Retry loop ─────────────────────────────────────────────────
        last_error_msg: Optional[str] = None
        for attempt in range(self._max_retries + 1):
            # On retry, reconnect the client (the previous process likely died)
            if attempt > 0:
                self._stderr_lines.clear()
                self._log_json("RETRY", {
                    "attempt": attempt,
                    "max_retries": self._max_retries,
                    "previous_error": last_error_msg,
                })
                # Reconnect — the old process is gone after a ProcessError
                if not self._client:
                    try:
                        await self.connect()
                    except Exception as conn_err:
                        self._emit_error(
                            f"Failed to reconnect on retry {attempt}: {conn_err}",
                            _format_traceback(),
                        )
                        break
                self._emit({"type": "thinking", "status": f"Retrying ({attempt}/{self._max_retries})..."})

            _should_retry = False
            try:
                # Send the query
                await self._client.query(full_message)

                # Stream the response
                total_output_tokens = 0
                total_input_tokens = 0
                in_assistant_block = False
                # Track pending tools by their tool_use_id for proper result association
                # Dict of tool_use_id -> tool_name
                pending_tools: dict[str, str] = {}

                self._log_json("DEBUG", {"action": "waiting for SDK messages..."})
                async for msg in self._client.receive_messages():
                    self._log_json("RECV", {"type": type(msg).__name__, "data": str(msg)[:200]})
                    msg_type = type(msg).__name__

                    if msg_type == "SystemMessage":
                        # Check subtype for special handling
                        subtype = getattr(msg, "subtype", None)
                        data = getattr(msg, "data", {})

                        # Handle compacting notification
                        if subtype and "compact" in subtype.lower():
                            self._emit({
                                "type": "compacting",
                                "status": "start",
                                "subtype": subtype,
                            })
                            self._emit({
                                "type": "thinking",
                                "status": "Compacting conversation...",
                            })
                        # Handle other system message subtypes
                        elif subtype:
                            self._log_json("SYSTEM_SUBTYPE", {"subtype": subtype, "data": data})

                        # Init message with model and session info
                        model = data.get("model")
                        session_id = data.get("session_id")
                        if model or session_id:
                            self._emit_session_info(model=model, session_id=session_id)

                        # Emit MCP server status if available
                        mcp_servers = data.get("mcp_servers", [])
                        if mcp_servers:
                            self._emit({
                                "type": "mcp_status",
                                "servers": mcp_servers,
                            })

                    elif msg_type == "AssistantMessage":
                        # Extract model if available
                        model = getattr(msg, "model", None)
                        if model and model != self.state.model:
                            self._emit_session_info(model=model)

                        # Handle content blocks in the assistant message
                        for block in getattr(msg, "content", []):
                            block_type = type(block).__name__

                            if block_type == "TextBlock":
                                text = getattr(block, "text", "")
                                if text:
                                    if not in_assistant_block:
                                        # Strip leading newlines from the first text block.
                                        # The SDK often returns text starting with "\n\n"
                                        # which creates unwanted blank lines above the message.
                                        text = text.lstrip("\n")
                                        self._emit({"type": "assistant_start"})
                                        in_assistant_block = True
                                    if text:
                                        self._emit({"type": "assistant_text", "text": text})

                            elif block_type == "ToolUseBlock":
                                if in_assistant_block:
                                    self._emit({"type": "assistant_end"})
                                    in_assistant_block = False
                                tool_name = getattr(block, "name", "unknown")
                                tool_input = getattr(block, "input", {})
                                tool_use_id = getattr(block, "id", None) or f"tool_{len(pending_tools)}"
                                # Track this tool for result association
                                pending_tools[tool_use_id] = tool_name

                                # Special handling for Edit tool - emit diff info
                                if tool_name == "Edit":
                                    self._emit({
                                        "type": "edit_tool",
                                        "tool_use_id": tool_use_id,
                                        "file_path": tool_input.get("file_path", ""),
                                        "old_string": tool_input.get("old_string", ""),
                                        "new_string": tool_input.get("new_string", ""),
                                    })
                                # Write tool - emit with content for diff-like popup display
                                elif tool_name == "Write":
                                    self._emit({
                                        "type": "write_tool",
                                        "tool_use_id": tool_use_id,
                                        "file_path": tool_input.get("file_path", ""),
                                        "content": tool_input.get("content", ""),
                                    })
                                # Special handling for TodoWrite - emit todo list
                                elif tool_name == "TodoWrite":
                                    todos = tool_input.get("todos", [])
                                    self._emit({
                                        "type": "todo_update",
                                        "todos": todos,
                                    })
                                else:
                                    # Emit tool call with unique ID
                                    self._emit({
                                        "type": "tool_call",
                                        "tool_use_id": tool_use_id,
                                        "name": tool_name,
                                        "input": tool_input,
                                    })
                                    # Also emit thinking status for tool execution
                                    tool_args = self._format_tool_args(tool_name, tool_input)
                                    self._emit({
                                        "type": "thinking",
                                        "status": f"Running: {tool_name}({tool_args})",
                                    })

                        # Update token counts if available
                        usage = getattr(msg, "usage", None)
                        if usage:
                            total_output_tokens = getattr(usage, "output_tokens", 0)
                            total_input_tokens = getattr(usage, "input_tokens", 0)
                            self._emit({
                                "type": "progress",
                                "input_tokens": total_input_tokens,
                                "output_tokens": total_output_tokens,
                            })

                    elif msg_type == "UserMessage":
                        # UserMessage contains tool results
                        for block in getattr(msg, "content", []):
                            block_type = type(block).__name__

                            if block_type == "ToolResultBlock":
                                # Get the tool_use_id to match with the original tool call
                                tool_use_id = getattr(block, "tool_use_id", None)
                                tool_name = pending_tools.get(tool_use_id, "unknown") if tool_use_id else "unknown"
                                # Emit tool result content
                                content = getattr(block, "content", None)
                                is_error = getattr(block, "is_error", False)
                                result_text = ""
                                if content:
                                    # Content can be a string or list of content blocks
                                    if isinstance(content, str):
                                        result_text = self._filter_system_reminders(content)
                                    elif isinstance(content, list):
                                        parts = []
                                        for item in content:
                                            if hasattr(item, "text"):
                                                parts.append(self._filter_system_reminders(item.text))
                                            elif isinstance(item, dict) and "text" in item:
                                                parts.append(self._filter_system_reminders(item["text"]))
                                            elif isinstance(item, str):
                                                parts.append(self._filter_system_reminders(item))
                                        result_text = "\n".join(parts)

                                # Safety net: Use centralized validation to ensure non-empty error content
                                # This should already be handled by the hook, but we validate here as well
                                # to catch any SDK bugs that bypass the hook
                                validated = validate_tool_result(
                                    {"content": content, "is_error": is_error},
                                    tool_name
                                )

                                # If validation added content, extract it
                                if not result_text and validated.get("content"):
                                    validated_content = validated["content"]
                                    if isinstance(validated_content, list):
                                        parts = []
                                        for item in validated_content:
                                            if isinstance(item, dict) and "text" in item:
                                                parts.append(item["text"])
                                            elif isinstance(item, str):
                                                parts.append(item)
                                        result_text = "\n".join(parts)
                                    elif isinstance(validated_content, str):
                                        result_text = validated_content

                                self._emit({
                                    "type": "tool_result",
                                    "tool_use_id": tool_use_id or "unknown",
                                    "content": result_text,
                                    "is_error": is_error,
                                })
                                # Close the tool and remove from pending
                                if tool_use_id:
                                    self._emit({"type": "tool_end", "tool_use_id": tool_use_id})
                                    pending_tools.pop(tool_use_id, None)
                                self._log_json("DEBUG", {"action": "tool_result processed", "tool_use_id": tool_use_id})

                    elif msg_type == "ResultMessage":
                        # Conversation turn complete
                        if in_assistant_block:
                            self._emit({"type": "assistant_end"})
                            in_assistant_block = False
                        # Close any remaining pending tools
                        for tool_id in list(pending_tools.keys()):
                            self._emit({"type": "tool_end", "tool_use_id": tool_id})
                        pending_tools.clear()

                        # Check for error results from the CLI
                        subtype = getattr(msg, "subtype", "success")
                        is_error = getattr(msg, "is_error", False)
                        result_text = getattr(msg, "result", None)

                        if is_error or subtype == "error_during_execution":
                            error_detail = result_text or subtype
                            self._log_json("RESULT_ERROR", {
                                "subtype": subtype,
                                "is_error": is_error,
                                "result": result_text,
                            })

                            # Check if this is a transient error we can retry
                            if (attempt < self._max_retries
                                    and self._is_transient_error(error_detail, subtype)):
                                retry_after = self._extract_retry_after(error_detail)
                                delay = self._calculate_retry_delay(attempt, retry_after)
                                self._emit_retry_status(
                                    attempt + 1, self._max_retries, error_detail, delay,
                                )
                                last_error_msg = error_detail
                                # Disconnect before retry — we need a fresh process
                                if self._client:
                                    try:
                                        await self._client.disconnect()
                                    except Exception:
                                        pass
                                self._client = None
                                await asyncio.sleep(delay)
                                # Use a flag to signal the outer retry loop
                                _should_retry = True
                                break  # break out of receive_messages loop → retry
                            else:
                                # Permanent error or retries exhausted
                                self._emit_error(
                                    f"Claude encountered an error: {error_detail}",
                                )

                        # Get cost and session info
                        cost = getattr(msg, "total_cost_usd", None) or getattr(msg, "cost_usd", 0) or 0
                        session_id = getattr(msg, "session_id", None)

                        # Emit final stats
                        self._emit({
                            "type": "result",
                            "cost_usd": cost,
                            "duration_ms": getattr(msg, "duration_ms", 0) or 0,
                            "num_turns": getattr(msg, "num_turns", 0) or 0,
                            "total_input": total_input_tokens,
                            "total_output": total_output_tokens,
                        })
                        # ResultMessage signals end of turn - break out of loop
                        self._log_json("DEBUG", {"action": "breaking out of receive_messages loop"})
                        break

                # After the receive_messages loop:
                # If _should_retry was set, continue the retry loop; otherwise we're done.
                if _should_retry:
                    continue  # next retry attempt
                break  # success or permanent error — exit retry loop
            except Exception as e:
                error_msg = str(e)
                # Check for the specific SDK bug with empty error content
                if "content cannot be empty if `is_error` is true" in error_msg:
                    self._emit_error(
                        "Session corrupted due to SDK bug (empty error content after permission timeout). "
                        "This is a known issue. Please restart the session.",
                        _format_traceback()
                    )
                    # Force disconnect to prevent further corruption
                    if self._client:
                        try:
                            await self._client.disconnect()
                        except Exception:
                            pass
                    self._client = None
                    self._emit_session_message(
                        "Session terminated. Please use claude-run to start a new session."
                    )
                    break  # permanent — no retry

                # Check if this exception represents a transient error
                stderr_context = self._get_stderr_context() if (
                    "Check stderr" in error_msg or "exit code" in error_msg
                ) else ""
                combined_error = f"{error_msg} {stderr_context}"

                if (attempt < self._max_retries
                        and self._is_transient_error(combined_error)):
                    retry_after = self._extract_retry_after(combined_error)
                    delay = self._calculate_retry_delay(attempt, retry_after)
                    self._emit_retry_status(
                        attempt + 1, self._max_retries, error_msg, delay,
                    )
                    last_error_msg = error_msg
                    # Ensure client is cleaned up for reconnection
                    if self._client:
                        try:
                            await self._client.disconnect()
                        except Exception:
                            pass
                    self._client = None
                    await asyncio.sleep(delay)
                    continue  # retry

                # Permanent error or retries exhausted — emit and stop
                if stderr_context:
                    error_msg = f"{error_msg}\n\nStderr output:\n{stderr_context}"
                self._emit_error(error_msg, _format_traceback())
                break

        self.state.status = "ready"
        self._emit_ready()
        # Small yield to ensure output is flushed before returning to event loop
        await asyncio.sleep(0.01)

    async def interrupt(self) -> None:
        """Interrupt the current Claude operation."""
        if self._client:
            try:
                await self._client.interrupt()
            except Exception:
                pass
        self._emit_session_message("Interrupted")
        self._emit_ready()

    async def quit(self) -> None:
        """Gracefully shutdown the agent."""
        self._running = False
        if self._client:
            try:
                await self._client.disconnect()
            except Exception:
                pass
        if self._log_handle:
            self._log_handle.close()


async def run_agent(
    work_dir: str,
    mcp_config: Optional[str] = None,
    resume_session: Optional[str] = None,
    continue_session: bool = False,
    allowed_tools: Optional[list[str]] = None,
    disallowed_tools: Optional[list[str]] = None,
    log_file: Optional[str] = None,
    model: Optional[str] = None,
    system_prompt: Optional[str] = None,
    block_direct_edit: bool = True,
    auto_reject_rules: Optional[list[dict]] = None,
    max_retries: int = 3,
) -> None:
    """Run the agent, reading commands from stdin."""
    agent = ClaudeAgent(
        work_dir=work_dir,
        mcp_config=mcp_config,
        allowed_tools=allowed_tools,
        disallowed_tools=disallowed_tools,
        log_file=log_file,
        resume_session=resume_session,
        continue_session=continue_session,
        model=model,
        system_prompt=system_prompt,
        block_direct_edit=block_direct_edit,
        auto_reject_rules=auto_reject_rules,
        max_retries=max_retries,
    )

    # Show initial session info
    agent._emit({"type": "session_start", "work_dir": work_dir})
    agent._emit_session_message(f"Session started in {work_dir}")
    agent._emit_session_message("Using Claude Agent SDK with dynamic permissions")
    agent._emit_ready()

    # Read input from stdin - expect NDJSON messages
    loop = asyncio.get_event_loop()
    reader = asyncio.StreamReader()
    protocol = asyncio.StreamReaderProtocol(reader)
    await loop.connect_read_pipe(lambda: protocol, sys.stdin)

    # Queue for user messages (so we can handle permissions while processing)
    message_queue: asyncio.Queue[str] = asyncio.Queue()

    # Task to read stdin and dispatch commands
    async def read_stdin():
        while agent._running:
            try:
                line = await reader.readline()
                if not line:
                    break

                text = line.decode().strip()
                if not text:
                    continue

                # Parse JSON message
                try:
                    msg = json.loads(text)
                except json.JSONDecodeError:
                    agent._emit_error(f"Invalid JSON: {text}")
                    continue

                msg_type = msg.get("type")

                if msg_type == "quit":
                    await agent.quit()
                    break
                elif msg_type == "interrupt":
                    await agent.interrupt()
                elif msg_type == "permission_response":
                    # Permission response - handle immediately (unblocks can_use_tool)
                    agent.handle_permission_response(msg)
                elif msg_type == "message":
                    # Queue user messages for processing
                    await message_queue.put(msg.get("text", ""))
                elif msg_type == "system_message":
                    # Display immediately in REPL and queue for injection
                    text = msg.get("text", "")
                    if text:
                        # Emit display events right away so REPL shows them
                        agent._emit({"type": "system_start"})
                        agent._emit({"type": "system_text", "text": text})
                        agent._emit({"type": "system_end"})
                        # Queue for injection into next user message
                        agent._pending_system_messages.append(text)
                else:
                    agent._emit_error(f"Unknown message type: {msg_type}")

            except Exception as e:
                agent._emit_error(f"stdin error: {e}")

    # Task to process user messages to Claude
    async def process_messages():
        while agent._running:
            try:
                text = await asyncio.wait_for(message_queue.get(), timeout=1.0)
                await agent.send_user_message(text)
            except asyncio.TimeoutError:
                continue
            except Exception as e:
                agent._emit_error(f"Error: {e}")
                agent._emit_ready()

    # Run both tasks concurrently
    stdin_task = asyncio.create_task(read_stdin())
    message_task = asyncio.create_task(process_messages())

    # Wait for stdin to close (quit command or EOF)
    await stdin_task
    message_task.cancel()
    try:
        await message_task
    except asyncio.CancelledError:
        pass

    await agent.quit()


def main() -> None:
    """Entry point for the agent."""
    _make_stdout_blocking()

    parser = argparse.ArgumentParser(
        description="Claude Agent - Using Claude Agent SDK"
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
        help="Resume session by ID (not used with SDK)",
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
        "--disallowed-tools",
        default=None,
        help="Comma-separated list of disallowed tools",
    )
    parser.add_argument(
        "--log-file",
        default=None,
        help="Path to write JSON message log (for debugging)",
    )
    parser.add_argument(
        "--model",
        default=None,
        help="Model to use (e.g., 'sonnet', 'opus', 'haiku')",
    )
    parser.add_argument(
        "--system-prompt",
        default=None,
        help="System prompt to use (overrides default Claude Code system prompt)",
    )
    parser.add_argument(
        "--system-prompt-file",
        default=None,
        help="Path to file containing system prompt (for multiline prompts)",
    )
    parser.add_argument(
        "--no-block-direct-edit",
        action="store_true",
        help="Disable blocking of Edit/Write tools (allow direct file editing)",
    )
    parser.add_argument(
        "--auto-reject-config",
        default=None,
        help="Path to JSON file with auto-reject rules",
    )
    parser.add_argument(
        "--max-retries",
        type=int,
        default=3,
        help="Max retries for transient API errors (default: 3, 0 to disable)",
    )
    args = parser.parse_args()

    allowed_tools = None
    if args.allowed_tools:
        allowed_tools = [t.strip() for t in args.allowed_tools.split(",")]

    disallowed_tools = None
    if args.disallowed_tools:
        disallowed_tools = [t.strip() for t in args.disallowed_tools.split(",")]

    # Load system prompt from file if specified
    system_prompt = args.system_prompt
    if args.system_prompt_file:
        with open(args.system_prompt_file, "r") as f:
            system_prompt = f.read()

    # Load auto-reject rules from JSON file if specified
    auto_reject_rules = None
    if args.auto_reject_config:
        with open(args.auto_reject_config, "r") as f:
            auto_reject_rules = json.load(f)

    asyncio.run(
        run_agent(
            work_dir=args.work_dir,
            mcp_config=args.mcp_config,
            resume_session=args.resume,
            continue_session=args.continue_session,
            allowed_tools=allowed_tools,
            disallowed_tools=disallowed_tools,
            log_file=args.log_file,
            model=args.model,
            system_prompt=system_prompt,
            block_direct_edit=not args.no_block_direct_edit,
            auto_reject_rules=auto_reject_rules,
            max_retries=args.max_retries,
        )
    )


if __name__ == "__main__":
    main()
