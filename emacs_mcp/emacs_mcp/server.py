#!/usr/bin/env python3
"""Claudemacs MCP Server - Expose Emacs operations to Claude via MCP.

Tools are registered in Emacs using claude-mcp-deftool and queried via emacsclient.
Native async tools (like watch functions) are defined in Python's NATIVE_TOOLS.
"""

import asyncio
import json
import os
import sys
import time
import uuid
from pathlib import Path

import yaml
from aiohttp import web
from mcp.server import Server
from mcp.server.stdio import stdio_server
from mcp.types import Tool, TextContent

from . import lib


# Create the MCP server
app = Server("emacs")

# Store the buffer identity for this MCP server instance
# This is set during server initialization from CLAUDE_AGENT_BUFFER_NAME env var
SESSION_BUFFER_NAME: str | None = None

# Auto-reject path prefixes for worktree confinement.
# When set, the MCP server will reject lock/edit operations on files
# whose paths start with any of these prefixes.
# Loaded from CLAUDE_AUTO_REJECT_PATHS env var (unit-separator delimited).
AUTO_REJECT_PATH_PREFIXES: list[str] = []

# Tools that modify files and need auto-reject path checking.
# lock/locks acquire write access; edit/edits perform writes.
AUTO_REJECT_TOOLS = {"lock", "locks", "edit", "edits"}

# Hint appended to error messages so agents know to file bug reports
BUG_REPORT_HINT = (
    "\n\nNote: If this looks like a bug in the Emacs MCP tools (not a usage mistake), "
    "please file a bug report using the `report_bug` tool with the tool name, "
    "arguments, error message, and expected behavior."
)

# Tool definitions loaded from Emacs registry
TOOL_DEFS: dict = {}

# Native Python tools (async, don't block Emacs)
NATIVE_TOOLS: dict = {
    "watch_buffer": {
        "description": "Watch a buffer until its content stabilizes (stops changing). Non-blocking async polling.",
        "safe": True,
        "args": {
            "buffer_name": {"type": "string", "description": "Name of the buffer to watch", "required": True},
            "timeout": {"type": "integer", "description": "Maximum seconds to wait (default: 30)"},
            "stable_time": {"type": "number", "description": "Seconds of no change before considered stable (default: 0.5)"},
        },
    },
    "watch_for_pattern": {
        "description": "Watch a buffer until a regex pattern appears. Non-blocking async polling. Returns match info or null on timeout.",
        "safe": True,
        "args": {
            "buffer_name": {"type": "string", "description": "Name of the buffer to watch", "required": True},
            "pattern": {"type": "string", "description": "Regex pattern to wait for", "required": True},
            "timeout": {"type": "integer", "description": "Maximum seconds to wait (default: 30)"},
        },
    },
    "watch_for_change": {
        "description": "Watch a buffer until any change occurs. Non-blocking async polling. Returns 'changed' or 'timeout'.",
        "safe": True,
        "args": {
            "buffer_name": {"type": "string", "description": "Name of the buffer to watch", "required": True},
            "timeout": {"type": "integer", "description": "Maximum seconds to wait (default: 30)"},
        },
    },
    "send_and_watch": {
        "description": "[EXECUTE] Send input to a buffer and wait for completion. Non-blocking async polling. Returns new content after input.",
        "safe": False,
        "args": {
            "buffer_name": {"type": "string", "description": "Name of the buffer", "required": True},
            "input": {"type": "string", "description": "Text/command to send", "required": True},
            "done_pattern": {"type": "string", "description": "Optional regex pattern that signals completion (otherwise waits for stability)"},
            "timeout": {"type": "integer", "description": "Maximum seconds to wait (default: 30)"},
        },
    },
    "reload_file": {
        "description": "[EXECUTE] Reload one or more elisp files in Emacs. IMPORTANT: Files are loaded in the order provided - this matters when files have dependencies. If loading fails due to syntax errors, automatically checks for unbalanced parentheses using indentation-aware analysis on the first file that fails.",
        "safe": False,
        "args": {
            "file_path": {"type": "string", "description": "Path to a single elisp file to reload", "required": False},
            "file_paths": {"type": "array", "description": "Array of file paths to reload in order (use when loading multiple files)", "required": False},
        },
    },
    # Agent messaging tools: spawn_agent, list_agents, send_message,
    # message_board_summary are registered via claude-mcp-deftool in
    # claude-mcp-messaging.el.  send_and_wait lives here because the
    # async polling loop (waiting for a reply) must run in Python.
    "send_and_wait": {
        "description": "Send a message to another agent and wait for their reply. The recipient will see a reminder that you are waiting. Blocks until the recipient sends a message back to you (via send_message or send_and_wait), or until the timeout expires. Returns the reply message content on success, or a timeout indicator. Use this when you need a response from another agent.",
        "safe": True,
        "args": {
            "buffer_name": {"type": "string", "description": "Buffer name of the recipient agent", "required": True},
            "message": {"type": "string", "description": "Message to send to the agent", "required": True},
            "timeout": {"type": "integer", "description": "Max seconds to wait for reply (default: 1800 = 30 minutes)"},
        },
    },
    "whoami": {
        "description": "Get the buffer name/identity of the current claudemacs session. Use this to identify yourself when sending messages or logging actions.",
        "safe": True,
        "args": {},
    },
}


async def load_tools_async() -> dict:
    """Load tool definitions from Emacs registry via emacsclient (async)."""
    global TOOL_DEFS

    # Query Emacs for registered tools
    result = await lib.call_emacs_async("(claude-mcp-export-tools)")

    # emacsclient returns elisp string representation, unescape it
    result = lib.unescape_elisp_string(result)

    try:
        TOOL_DEFS = json.loads(result)
    except json.JSONDecodeError as e:
        # Provide helpful error context for debugging
        context_start = max(0, e.pos - 50)
        context_end = min(len(result), e.pos + 50)
        context = result[context_start:context_end]
        pointer = " " * min(50, e.pos - context_start) + "^"
        raise RuntimeError(
            f"Failed to parse tools JSON from Emacs:\n"
            f"  Error: {e.msg} at position {e.pos}\n"
            f"  Context: {repr(context)}\n"
            f"           {pointer}\n"
            f"  Hint: Check for unescaped special characters in tool descriptions"
        ) from e

    print(f"Loaded {len(TOOL_DEFS)} tools from Emacs registry", file=sys.stderr, flush=True)

    return TOOL_DEFS


def load_tools_sync() -> dict:
    """Load tool definitions from Emacs registry via emacsclient (sync, for CLI use only)."""
    global TOOL_DEFS

    # Query Emacs for registered tools
    result = lib.call_emacs("(claude-mcp-export-tools)")

    # emacsclient returns elisp string representation, unescape it
    result = lib.unescape_elisp_string(result)

    try:
        TOOL_DEFS = json.loads(result)
    except json.JSONDecodeError as e:
        # Provide helpful error context for debugging
        context_start = max(0, e.pos - 50)
        context_end = min(len(result), e.pos + 50)
        context = result[context_start:context_end]
        pointer = " " * min(50, e.pos - context_start) + "^"
        raise RuntimeError(
            f"Failed to parse tools JSON from Emacs:\n"
            f"  Error: {e.msg} at position {e.pos}\n"
            f"  Context: {repr(context)}\n"
            f"           {pointer}\n"
            f"  Hint: Check for unescaped special characters in tool descriptions"
        ) from e

    print(f"Loaded {len(TOOL_DEFS)} tools from Emacs registry", file=sys.stderr, flush=True)

    return TOOL_DEFS


def build_input_schema(tool_def: dict) -> dict:
    """Build JSON schema from tool definition."""
    args = tool_def.get("args", {})
    if not args:
        return {"type": "object", "properties": {}}

    properties = {}
    required = []

    for arg_name, arg_def in args.items():
        properties[arg_name] = {
            "type": arg_def.get("type", "string"),
            "description": arg_def.get("description", ""),
        }
        if arg_def.get("required", False):
            required.append(arg_name)

    schema = {"type": "object", "properties": properties}
    if required:
        schema["required"] = required
    return schema


@app.list_tools()
async def list_tools() -> list[Tool]:
    """List available Emacs interaction tools from registry and native definitions."""
    # Reload tools on each list_tools call to pick up changes
    await load_tools_async()

    tools = []
    # Add Emacs-registered tools
    for name, tool_def in TOOL_DEFS.items():
        tools.append(Tool(
            name=name,
            description=tool_def.get("description", ""),
            inputSchema=build_input_schema(tool_def),
        ))
    # Add native Python tools
    for name, tool_def in NATIVE_TOOLS.items():
        tools.append(Tool(
            name=name,
            description=tool_def.get("description", ""),
            inputSchema=build_input_schema(tool_def),
        ))
    return tools


def escape_elisp_string(s: str) -> str:
    """Escape a string for use in elisp."""
    return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')


def format_elisp_value(value) -> str:
    """Format a Python value as an elisp expression.

    Handles strings, numbers, booleans, lists, and dicts (as alists).
    """
    if value is None:
        return "nil"
    elif isinstance(value, bool):
        return "t" if value else "nil"
    elif isinstance(value, (int, float)):
        return str(value)
    elif isinstance(value, str):
        return f'"{escape_elisp_string(value)}"'
    elif isinstance(value, list):
        return format_elisp_list(value)
    elif isinstance(value, dict):
        return format_elisp_alist(value)
    else:
        return f'"{escape_elisp_string(str(value))}"'


def format_elisp_list(items: list) -> str:
    """Format a Python list as a quoted elisp list.

    Handles nested lists and dicts (converted to alists).
    Examples:
        ["a", "b"] -> '("a" "b")
        [{"key": "val"}] -> '((("key" . "val")))
        [1, 2, 3] -> '(1 2 3)
    """
    if not items:
        return "'()"
    parts = [format_elisp_value(item) for item in items]
    return f"'({' '.join(parts)})"


def format_elisp_alist(obj: dict) -> str:
    """Format a Python dict as an elisp alist.

    Example:
        {"name": "foo", "count": 42} -> (("name" . "foo") ("count" . 42))
    """
    if not obj:
        return "nil"
    pairs = []
    for key, val in obj.items():
        formatted_key = f'"{escape_elisp_string(str(key))}"'
        formatted_val = format_elisp_value(val)
        pairs.append(f"({formatted_key} . {formatted_val})")
    return f"({' '.join(pairs)})"


def substitute_variables(template: str, args: dict, arg_defs: dict) -> str:
    """Substitute variables in template string using {{var}} or $var syntax."""
    result = template

    for arg_name, value in args.items():
        arg_def = arg_defs.get(arg_name, {})
        arg_type = arg_def.get("type", "string")

        # Format value based on type
        if arg_type == "string":
            formatted_value = f'"{escape_elisp_string(str(value))}"'
        elif arg_type == "integer":
            formatted_value = str(int(value))
        elif arg_type == "boolean":
            formatted_value = "t" if value else "nil"
        elif arg_type == "array":
            # Handle array as elisp list (supports nested dicts/lists)
            if isinstance(value, list):
                formatted_value = format_elisp_list(value)
            else:
                formatted_value = "'()"
        else:
            formatted_value = f'"{escape_elisp_string(str(value))}"'

        # Replace {{var}} and $var patterns
        result = result.replace(f"{{{{{arg_name}}}}}", formatted_value)
        result = result.replace(f"${arg_name}", formatted_value)

    return result


def build_elisp_from_spec(elisp_spec, args: dict, arg_defs: dict,
                          prepend_args: list[str] | None = None) -> str:
    """Build elisp expression from various spec formats.

    Args:
        elisp_spec: Function name, template string, list, or dict spec.
        args: Tool arguments from the MCP call.
        arg_defs: Tool argument definitions.
        prepend_args: Optional list of pre-formatted elisp strings to insert
            before normal arguments (used for async task-id injection).
    """
    if isinstance(elisp_spec, str):
        # Simple function name or template string
        if "{{" in elisp_spec or "$" in elisp_spec:
            # It's a template string with variables
            return substitute_variables(elisp_spec, args, arg_defs)
        else:
            # It's a simple function name, use old behavior
            return build_elisp_call(elisp_spec, args, arg_defs,
                                    prepend_args=prepend_args)

    elif isinstance(elisp_spec, list):
        # List format: ["progn", ["message", "{{msg}}"], ["sit-for", 1]]
        return build_elisp_from_list(elisp_spec, args, arg_defs)

    elif isinstance(elisp_spec, dict):
        # Dict format for more complex expressions
        if "template" in elisp_spec:
            return substitute_variables(elisp_spec["template"], args, arg_defs)
        elif "function" in elisp_spec:
            # Traditional function call
            return build_elisp_call(elisp_spec["function"], args, arg_defs,
                                    prepend_args=prepend_args)

    # Fallback to old behavior
    return build_elisp_call(str(elisp_spec), args, arg_defs,
                            prepend_args=prepend_args)


def build_elisp_from_list(spec_list: list, args: dict, arg_defs: dict) -> str:
    """Build elisp from list specification."""
    if not spec_list:
        return "nil"

    result_parts = []
    for item in spec_list:
        if isinstance(item, str):
            # Check if it's a pure template variable (e.g., "{{item}}" or "$item")
            # These should be substituted without adding quotes
            is_pure_template = False
            if item.strip().startswith("{{") and item.strip().endswith("}}"):
                var_name = item.strip()[2:-2].strip()
                if var_name in args:
                    is_pure_template = True
                    # Substitute just the variable, quotes are already added by substitute_variables
                    result_parts.append(substitute_variables(item, args, arg_defs))
            elif item.strip().startswith("$"):
                var_name = item.strip()[1:]
                if var_name in args:
                    is_pure_template = True
                    result_parts.append(substitute_variables(item, args, arg_defs))

            if not is_pure_template:
                # Could be a function name, literal string, or template with mixed content
                if "{{" in item or "$" in item:
                    # Mixed template - needs to be handled as string literal
                    substituted = substitute_variables(item, args, arg_defs)
                    # If it doesn't start with a quote, it needs to be quoted
                    if not substituted.startswith('"'):
                        result_parts.append(f'"{escape_elisp_string(substituted)}"')
                    else:
                        result_parts.append(substituted)
                else:
                    # Plain string - could be function name or literal
                    # Check if it's a valid elisp symbol (function/variable name)
                    import re
                    # Valid symbols: alphanumeric, -, +, *, /, <, >, =, !, ?, etc.
                    # but NOT if it contains spaces, %, :, etc which indicate it's a string
                    if re.match(r'^[a-zA-Z_+\-*/<>=!?][a-zA-Z0-9_+\-*/<>=!?]*$', item):
                        # Looks like a symbol/function name, don't quote
                        result_parts.append(item)
                    else:
                        # It's a literal string, needs quotes
                        result_parts.append(f'"{escape_elisp_string(item)}"')
        elif isinstance(item, list):
            # Nested list
            result_parts.append(build_elisp_from_list(item, args, arg_defs))
        elif isinstance(item, (int, float)):
            result_parts.append(str(item))
        elif isinstance(item, bool):
            result_parts.append("t" if item else "nil")
        else:
            result_parts.append(str(item))

    return f"({' '.join(result_parts)})"


def build_elisp_call(elisp_fn: str, args: dict, arg_defs: dict,
                     prepend_args: list[str] | None = None) -> str:
    """Build an elisp function call from tool arguments (legacy format).

    Args:
        elisp_fn: The elisp function name.
        args: Tool arguments from the MCP call.
        arg_defs: Tool argument definitions.
        prepend_args: Optional list of pre-formatted elisp strings to insert
            before normal arguments (used for async task-id injection).
    """
    # Start with any prepended args (e.g., task-id for async tools)
    elisp_args = list(prepend_args) if prepend_args else []

    if not arg_defs:
        if elisp_args:
            return f"({elisp_fn} {' '.join(elisp_args)})"
        return f"({elisp_fn})"

    # Build argument list in definition order
    # We need to handle optional args carefully - if a later arg is provided,
    # we need to pass nil for earlier optional args to maintain positional order
    arg_names = list(arg_defs.keys())

    # Find the last provided argument
    last_provided_idx = -1
    for i, arg_name in enumerate(arg_names):
        if arg_name in args:
            last_provided_idx = i

    # Build args up to the last provided one
    for i, arg_name in enumerate(arg_names):
        if i > last_provided_idx:
            break

        arg_def = arg_defs[arg_name]

        if arg_name in args:
            value = args[arg_name]
            arg_type = arg_def.get("type", "string")

            if arg_type == "string":
                elisp_args.append(f'"{escape_elisp_string(str(value))}"')
            elif arg_type == "integer":
                elisp_args.append(str(int(value)))
            elif arg_type == "boolean":
                elisp_args.append("t" if value else "nil")
            elif arg_type == "array":
                # Handle array as elisp list
                if isinstance(value, list):
                    elisp_args.append(format_elisp_list(value))
                else:
                    elisp_args.append("'()")
            else:
                elisp_args.append(f'"{escape_elisp_string(str(value))}"')
        else:
            # Optional arg not provided - pass nil to maintain positional order
            elisp_args.append("nil")

    if elisp_args:
        return f"({elisp_fn} {' '.join(elisp_args)})"
    return f"({elisp_fn})"


def needs_session_cwd(name: str, tool_def: dict | None = None) -> bool:
    """Check if a tool needs the session cwd binding.

    First checks the tool definition for needs_session_cwd attribute.
    Falls back to hardcoded list for backward compatibility.
    """
    # Check tool definition first (set via :needs-session-cwd t in elisp)
    if tool_def and tool_def.get("needs_session_cwd"):
        return True

    # Fallback: hardcoded list for backward compatibility
    # These can be removed once all tools use :needs-session-cwd attribute
    return (name.startswith("notes_") or
            name.startswith("todo_") or
            name.startswith("kb_") or
            name in {
                "get_notes", "set_notes", "append_notes", "clear_notes",
                "restart_and_resume"
            })


def wrap_with_context(elisp_expr: str, cwd: str | None = None, buffer_name: str | None = None, file_path: str | None = None) -> str:
    """Wrap an elisp expression with proper context (directory, buffer, or file).

    Args:
        elisp_expr: The elisp expression to evaluate
        cwd: Working directory to use (sets default-directory)
        buffer_name: Buffer to execute in (for buffer-local vars and modes)
        file_path: File to visit before execution (for file-specific modes)

    Returns:
        Wrapped elisp expression that executes in the proper context
    """
    # Get session cwd for claudemacs-session-cwd binding
    session_cwd = lib.get_session_cwd() if not cwd else cwd

    if file_path:
        # Execute in the context of a file buffer
        escaped_file = escape_elisp_string(file_path)
        return f'''(with-current-buffer (find-file-noselect "{escaped_file}")
                     {elisp_expr})'''
    elif buffer_name:
        # Execute in the context of a specific buffer
        # Also set claudemacs-session-cwd for tools that need it
        escaped_buffer = escape_elisp_string(buffer_name)
        if session_cwd:
            escaped_cwd = escape_elisp_string(session_cwd)
            return f'''(if (get-buffer "{escaped_buffer}")
                         (with-current-buffer "{escaped_buffer}"
                           (let ((claudemacs-session-cwd "{escaped_cwd}"))
                             {elisp_expr}))
                         (error "Buffer %s does not exist" "{escaped_buffer}"))'''
        else:
            # No session cwd available, use simple buffer context
            return f'''(if (get-buffer "{escaped_buffer}")
                         (with-current-buffer "{escaped_buffer}"
                           {elisp_expr})
                         (error "Buffer %s does not exist" "{escaped_buffer}"))'''
    elif cwd:
        # Execute with a specific default-directory
        escaped_cwd = escape_elisp_string(cwd)
        return f'''(let ((default-directory "{escaped_cwd}")
                         (claudemacs-session-cwd "{escaped_cwd}"))
                     {elisp_expr})'''
    else:
        # No context wrapping needed
        return elisp_expr


def wrap_with_cwd(elisp_expr: str, cwd: str) -> str:
    """Legacy wrapper - now calls wrap_with_context for backward compatibility."""
    return wrap_with_context(elisp_expr, cwd=cwd)


async def handle_native_tool(name: str, arguments: dict) -> str:
    """Handle native Python tools (async, non-blocking)."""
    if name == "watch_buffer":
        buffer_name = arguments["buffer_name"]
        timeout = float(arguments.get("timeout", 30))
        stable_time = float(arguments.get("stable_time", 0.5))
        result = await lib.watch_buffer_async(buffer_name, timeout, stable_time)
        return result

    elif name == "watch_for_pattern":
        buffer_name = arguments["buffer_name"]
        pattern = arguments["pattern"]
        timeout = float(arguments.get("timeout", 30))
        watch_result = await lib.watch_for_pattern_async(buffer_name, pattern, timeout)
        if watch_result:
            return json.dumps(watch_result)
        return "null (timeout - pattern not found)"

    elif name == "watch_for_change":
        buffer_name = arguments["buffer_name"]
        timeout = float(arguments.get("timeout", 30))
        result = await lib.watch_for_change_async(buffer_name, timeout)
        return result

    elif name == "send_and_watch":
        buffer_name = arguments["buffer_name"]
        input_text = arguments["input"]
        done_pattern = arguments.get("done_pattern")
        timeout = float(arguments.get("timeout", 30))
        result = await lib.send_and_watch_async(buffer_name, input_text, done_pattern, timeout)
        return result

    elif name == "reload_file":
        # Support both single file_path and multiple file_paths
        if "file_paths" in arguments:
            file_paths = arguments["file_paths"]
        elif "file_path" in arguments:
            file_paths = arguments["file_path"]
        else:
            raise ValueError("Either file_path or file_paths must be provided")
        result = await lib.reload_elisp_file(file_paths)
        return result

    elif name == "send_and_wait":
        return await handle_send_and_wait(arguments)

    elif name == "whoami":
        if not SESSION_BUFFER_NAME:
            raise ValueError("Buffer name not configured - CLAUDE_AGENT_BUFFER_NAME environment variable is not set")
        return SESSION_BUFFER_NAME

    else:
        raise ValueError(f"Unknown native tool: {name}")


async def handle_send_and_wait(arguments: dict) -> str:
    """Handle send_and_wait: send message then poll for reply.

    1. Call elisp claude-mcp-send-and-wait to deliver the message
       (with reply reminder for recipient)
    2. Poll the sender's MCP message queue for a reply from the recipient
    3. Return the reply content, or timeout indicator
    """
    recipient = arguments["buffer_name"]
    message = arguments["message"]
    timeout = float(arguments.get("timeout", 1800))

    # Determine sender (this agent's buffer name)
    sender = SESSION_BUFFER_NAME
    if not sender:
        raise ValueError(
            "Buffer name not configured - CLAUDE_AGENT_BUFFER_NAME "
            "environment variable is not set. Cannot use send_and_wait "
            "without a session identity."
        )

    escaped_recipient = escape_elisp_string(recipient)
    escaped_message = escape_elisp_string(message)
    escaped_sender = escape_elisp_string(sender)

    # Step 1: Deliver the message via elisp (with reply reminder)
    elisp_expr = (
        f'(claude-mcp-send-and-wait '
        f'"{escaped_recipient}" '
        f'"{escaped_message}" '
        f'"{escaped_sender}")'
    )
    delivery_result = await lib.call_emacs_async(elisp_expr, timeout=10)
    if delivery_result.startswith('"') and delivery_result.endswith('"'):
        delivery_result = lib.unescape_elisp_string(delivery_result)

    # Step 2: Poll the sender's queue for a reply from the recipient
    # Use the same async polling pattern as watch_for_pattern
    start_time = time.time()
    poll_interval = 1.0  # Check every second

    escaped_sender_for_peek = escape_elisp_string(sender)
    escaped_recipient_for_peek = escape_elisp_string(recipient)

    while (time.time() - start_time) < timeout:
        # Ask Emacs if there's a message from the recipient in the sender's queue
        peek_elisp = (
            f'(let ((msg (claude-mcp-message-queue-peek-from '
            f'"{escaped_sender_for_peek}" '
            f'"{escaped_recipient_for_peek}")))'
            f'  (if msg "found" "nil"))'
        )
        peek_result = await lib.call_emacs_async(peek_elisp, timeout=5)
        if peek_result.startswith('"') and peek_result.endswith('"'):
            peek_result = lib.unescape_elisp_string(peek_result)

        if peek_result == "found":
            # Pop the reply message from the queue and return it
            pop_elisp = (
                f'(let ((msg (claude-mcp-message-queue-pop-from '
                f'"{escaped_sender_for_peek}" '
                f'"{escaped_recipient_for_peek}")))'
                f'  (if msg (plist-get msg :message) ""))'
            )
            pop_result = await lib.call_emacs_async(pop_elisp, timeout=5)
            if pop_result.startswith('"') and pop_result.endswith('"'):
                pop_result = lib.unescape_elisp_string(pop_result)
            return f"[Reply from {recipient}]: {pop_result}"

        await asyncio.sleep(poll_interval)

    # Timeout
    return (
        f"TIMEOUT: No reply received from {recipient} within "
        f"{int(timeout)} seconds. The message was delivered successfully, "
        f"but the recipient did not respond in time. You can try again "
        f"with send_and_wait, or use send_message for fire-and-forget."
    )


async def check_auto_reject(name: str, arguments: dict) -> str | None:
    """Check if a tool call should be auto-rejected based on path prefixes.

    For worktree confinement: prevents agents from editing files in the
    main repository via MCP lock/edit tools.

    For lock/locks: checks file_path argument and buffer_name resolution.
    For edit/edits: resolves the buffer from lock_id/buffer_name/file_path
    and checks its file path.

    Returns an error message string if rejected, None if allowed.
    """

    if not AUTO_REJECT_PATH_PREFIXES:
        return None
    if name not in AUTO_REJECT_TOOLS:
        return None

    # Collect file paths to check (expand ~ to home directory)
    paths_to_check: list[str] = []

    # Direct file_path argument (used by lock, edit, locks, edits)
    if "file_path" in arguments and arguments["file_path"]:
        paths_to_check.append(os.path.expanduser(arguments["file_path"]))

    # For lock/edit with buffer_name but no file_path, resolve the buffer's file
    if not paths_to_check and "buffer_name" in arguments and arguments["buffer_name"]:
        try:
            escaped_buf = escape_elisp_string(arguments["buffer_name"])
            result = await lib.call_emacs_async(
                f'(buffer-file-name (get-buffer "{escaped_buf}"))',
                timeout=5,
            )
            if result and result != "nil":
                resolved = lib.unescape_elisp_string(result) if result.startswith('"') else result
                if resolved:
                    paths_to_check.append(resolved)
        except Exception:
            pass  # Best effort — if we can't resolve, allow the call through



    # Check all collected paths against auto-reject prefixes
    for path in paths_to_check:
        for prefix in AUTO_REJECT_PATH_PREFIXES:
            if path.startswith(prefix):
                return (
                    f"REJECTED: Cannot {name} files in '{prefix}'. "
                    f"Path '{path}' is in an auto-rejected directory. "
                    f"You must only edit files in your assigned worktree."
                )

    return None


@app.call_tool()
async def call_tool(name: str, arguments: dict) -> list[TextContent]:
    """Handle tool calls by invoking elisp or native Python functions."""
    try:
        # Enforce auto-reject path rules for file-modifying tools
        reject_msg = await check_auto_reject(name, arguments)
        if reject_msg:
            return [TextContent(type="text", text=f"Error: {reject_msg}")]

        # Check if it's a native Python tool
        if name in NATIVE_TOOLS:
            result = await handle_native_tool(name, arguments)
            return [TextContent(type="text", text=result)]

        # Otherwise, it's an Emacs-registered elisp tool
        if name not in TOOL_DEFS:
            raise ValueError(f"Unknown tool: {name}")

        tool_def = TOOL_DEFS[name]
        # Support both 'function' (new Emacs registry) and 'elisp' (legacy YAML)
        elisp_fn = tool_def.get("function") or tool_def.get("elisp")

        if not elisp_fn:
            raise ValueError(f"Tool {name} has no function defined")

        # Auto-inject agent_name for lock/lock_region/locks if not provided
        if name in ("lock", "lock_region", "locks") and "agent_name" not in arguments:
            if SESSION_BUFFER_NAME:
                arguments["agent_name"] = SESSION_BUFFER_NAME

        # Auto-inject from_buffer for send_message if not provided
        if name == "send_message" and not arguments.get("from_buffer"):
            if SESSION_BUFFER_NAME:
                arguments["from_buffer"] = SESSION_BUFFER_NAME

        # Auto-inject agent_buffer for show_proposal
        if name == "show_proposal" and "agent_buffer" not in arguments:
            if SESSION_BUFFER_NAME:
                arguments["agent_buffer"] = SESSION_BUFFER_NAME

        # Extract explicit context parameters first (these are special and not passed to elisp)
        context_buffer = arguments.pop("__buffer", None)
        context_file = arguments.pop("__file", None)
        context_dir = arguments.pop("__dir", None)

        # Infer context from tool arguments if not explicitly provided
        if not context_file and not context_buffer and not context_dir:
            # Check tool definition for context hints
            context_hint = tool_def.get("context", "auto")  # Default to auto

            # Tools that need session context should not infer from arguments
            # (e.g., file_path in notes_add_documentation is metadata, not execution context)
            if needs_session_cwd(name, tool_def):
                context_hint = "none"

            # Always try to auto-detect unless explicitly disabled
            if context_hint != "none":
                # Look for file-related arguments (highest priority)
                if context_hint in ["file", "auto"]:
                    for arg_name in ["file_path", "file", "path", "filename", "source", "target"]:
                        if arg_name in arguments and arguments[arg_name]:
                            arg_val = arguments[arg_name]
                            # Check if it looks like a file path
                            if isinstance(arg_val, str) and ("." in arg_val or "/" in arg_val):
                                # More comprehensive file extension check
                                if os.path.splitext(arg_val)[1] or not arg_val.endswith("/"):
                                    context_file = arg_val
                                    break

                # Look for buffer-related arguments (second priority)
                if not context_file and context_hint in ["buffer", "auto"]:
                    for arg_name in ["buffer_name", "buffer", "buf"]:
                        if arg_name in arguments and arguments[arg_name]:
                            context_buffer = arguments[arg_name]
                            break

                # Look for directory-related arguments (third priority)
                if not context_file and not context_buffer and context_hint in ["dir", "auto"]:
                    for arg_name in ["directory", "dir", "folder", "project_dir", "work_dir"]:
                        if arg_name in arguments and arguments[arg_name]:
                            context_dir = arguments[arg_name]
                            break
                    # Also check if 'path' looks like a directory
                    if not context_dir and "path" in arguments:
                        path_val = arguments["path"]
                        if isinstance(path_val, str) and (path_val.endswith("/") or not "." in os.path.basename(path_val)):
                            context_dir = path_val

        # Check if this is an async tool
        is_async = tool_def.get("async", False)
        task_id = str(uuid.uuid4()) if is_async else None

        # Special case for eval_elisp - pass expression directly
        if elisp_fn == "eval" and "expression" in arguments:
            elisp_expr = arguments["expression"]
            is_async = False  # eval_elisp is never async
        elif is_async:
            # For async tools, prepend task-id and server port as first two arguments
            assert task_id is not None
            elisp_expr = build_elisp_from_spec(
                elisp_fn,
                arguments,
                tool_def.get("args", {}),
                prepend_args=[
                    f'"{escape_elisp_string(task_id)}"',
                    str(lib.http_server_port),
                ],
            )
        else:
            elisp_expr = build_elisp_from_spec(
                elisp_fn,
                arguments,
                tool_def.get("args", {})
            )

        # Determine the context to use
        session_cwd = lib.get_session_cwd()
        session_buffer = os.environ.get("CLAUDE_AGENT_BUFFER_NAME")

        # Apply context wrapping based on priority: file > buffer > dir > session
        if context_file:
            elisp_expr = wrap_with_context(elisp_expr, file_path=context_file)
        elif context_buffer:
            elisp_expr = wrap_with_context(elisp_expr, buffer_name=context_buffer)
        elif context_dir:
            elisp_expr = wrap_with_context(elisp_expr, cwd=context_dir)
        elif needs_session_cwd(name, tool_def) and (session_buffer or session_cwd):
            # For tools that need session context, use session defaults
            # Prefer executing in the claudemacs buffer if available (uses buffer's default-directory)
            if session_buffer and name != "eval_elisp":
                elisp_expr = wrap_with_context(elisp_expr, buffer_name=session_buffer)
            elif session_cwd:
                elisp_expr = wrap_with_context(elisp_expr, cwd=session_cwd)

        # --- Async tool dispatch ---
        if is_async:
            assert task_id is not None  # guaranteed by is_async branch above
            tool_timeout = tool_def.get("timeout")
            if tool_timeout is None:
                raise RuntimeError(
                    f"Async tool '{name}' has no timeout configured. "
                    f"This is a bug — all async tools must declare :timeout.")

            # Check concurrent task limit
            max_tasks_result = await lib.call_emacs_async(
                '(claude-mcp-async-task-count)', timeout=5)
            try:
                active_count = int(max_tasks_result)
            except (ValueError, TypeError):
                active_count = 0
            max_tasks_result = await lib.call_emacs_async(
                '(symbol-value \'claude-mcp-async-max-tasks)', timeout=5)
            try:
                max_tasks = int(max_tasks_result)
            except (ValueError, TypeError):
                max_tasks = 5
            if active_count >= max_tasks:
                return [TextContent(
                    type="text",
                    text=f"Error: Too many concurrent async tasks "
                         f"({active_count}/{max_tasks}). "
                         f"Please wait for some to complete and retry.")]

            # Register before calling elisp so the event exists when callback arrives
            event = lib.async_tool_tracker.register(task_id)

            # Call elisp — the tool should return :async-started quickly
            startup_result = await lib.call_emacs_async(elisp_expr, timeout=10)

            # Unescape if needed
            if startup_result.startswith('"') and startup_result.endswith('"'):
                startup_result = lib.unescape_elisp_string(startup_result)

            # Fast path: tool completed synchronously despite being async-capable
            if startup_result != ":async-started":
                lib.async_tool_tracker.get_result(task_id)  # clean up unused registration
                return [TextContent(type="text", text=startup_result)]

            # Slow path: wait for HTTP callback from Emacs
            try:
                await asyncio.wait_for(event.wait(), timeout=tool_timeout)
            except asyncio.TimeoutError:
                lib.async_tool_tracker.get_result(task_id)  # clean up
                # Tell Emacs to cancel the task
                try:
                    await lib.call_emacs_async(
                        f'(claude-mcp-async-cancel "{escape_elisp_string(task_id)}")',
                        timeout=5)
                except Exception:
                    pass  # Best-effort cancel
                return [TextContent(
                    type="text",
                    text=f"Error: Async tool '{name}' timed out after {tool_timeout}s{BUG_REPORT_HINT}")]

            result_data = lib.async_tool_tracker.get_result(task_id)
            if result_data and result_data.get('is_error'):
                return [TextContent(
                    type="text",
                    text=f"Error: {result_data['result']}{BUG_REPORT_HINT}")]
            return [TextContent(
                type="text",
                text=result_data['result'] if result_data else "")]

        # --- Sync tool dispatch (existing path) ---
        result = await lib.call_emacs_async(elisp_expr)

        # Unescape string results
        if result.startswith('"') and result.endswith('"'):
            result = lib.unescape_elisp_string(result)

        return [TextContent(type="text", text=result)]

    except Exception as e:
        return [TextContent(type="text", text=f"Error: {str(e)}{BUG_REPORT_HINT}")]


async def bash_command_callback(request):
    """HTTP endpoint: POST /bash-command

    Receives completion notifications from shell hooks.
    Payload: {"shell_id": "xxx", "exit_code": 0}
    """
    try:
        data = await request.json()
        shell_id = data.get('shell_id')
        exit_code = data.get('exit_code', -1)

        if not shell_id:
            return web.json_response({'error': 'shell_id required'}, status=400)

        # Notify the waiting command via the global tracker
        lib.command_tracker.complete_command(shell_id, exit_code)

        return web.json_response({'status': 'ok'})

    except Exception as e:
        print(f"Error in bash_command_callback: {e}", file=sys.stderr)
        return web.json_response({'error': str(e)}, status=500)


async def async_tool_result_callback(request):
    """HTTP endpoint: POST /async-tool-result

    Receives completion from elisp async tools.
    Payload: {"task_id": "xxx", "result": "...", "status": "ok"}
         or: {"task_id": "xxx", "error": "...", "status": "error"}
    """
    try:
        data = await request.json()
        task_id = data.get('task_id')
        if not task_id:
            return web.json_response({'error': 'task_id required'}, status=400)

        status = data.get('status', 'ok')
        if status == 'error':
            lib.async_tool_tracker.complete(
                task_id, data.get('error', 'Unknown error'), is_error=True)
        else:
            lib.async_tool_tracker.complete(
                task_id, data.get('result', ''))

        return web.json_response({'status': 'ok'})

    except Exception as e:
        print(f"Error in async_tool_result_callback: {e}", file=sys.stderr)
        return web.json_response({'error': str(e)}, status=500)


async def start_http_server():
    """Start HTTP server on random available port.

    Returns (port, runner) tuple.
    """
    http_app = web.Application()
    http_app.router.add_post('/bash-command', bash_command_callback)
    http_app.router.add_post('/async-tool-result', async_tool_result_callback)

    runner = web.AppRunner(http_app)
    await runner.setup()

    # Bind to localhost on random available port
    site = web.TCPSite(runner, 'localhost', 0)
    await site.start()

    # Get actual port assigned
    port = site._server.sockets[0].getsockname()[1]

    # Print port to stderr for Emacs to capture
    print(f"CLAUDE_MCP_HTTP_PORT={port}", file=sys.stderr, flush=True)

    return port, runner


async def main():
    """Run the MCP server with HTTP endpoint for bash callbacks."""
    global SESSION_BUFFER_NAME, AUTO_REJECT_PATH_PREFIXES

    # Initialize session buffer name from environment
    SESSION_BUFFER_NAME = os.environ.get("CLAUDE_AGENT_BUFFER_NAME")

    # Load auto-reject path prefixes from environment (unit-separator delimited)
    reject_paths_env = os.environ.get("CLAUDE_AUTO_REJECT_PATHS", "")
    if reject_paths_env:
        AUTO_REJECT_PATH_PREFIXES = [p for p in reject_paths_env.split("\x1f") if p]
        print(f"Auto-reject path prefixes: {AUTO_REJECT_PATH_PREFIXES}", file=sys.stderr, flush=True)

    # Load tools on startup
    await load_tools_async()

    # Start HTTP server for bash command callbacks and async tool results
    http_port, http_runner = await start_http_server()
    print(f"HTTP server listening on localhost:{http_port}", file=sys.stderr, flush=True)

    # Store port in lib for use by bash_async
    lib.http_server_port = http_port

    # Ensure the lease timer is running in Emacs (safe to call multiple times)
    try:
        await lib.call_emacs_async(
            '(claude-mcp-async-start-lease-timer)', timeout=5)
        print(f"Async lease timer started in Emacs",
              file=sys.stderr, flush=True)
    except Exception as e:
        print(f"Warning: Could not start async lease timer: {e}",
              file=sys.stderr, flush=True)

    # Clean up any orphan async tasks from a previous MCP server session
    try:
        await lib.call_emacs_async('(claude-mcp-async-cancel-all)', timeout=5)
    except Exception as e:
        print(f"Warning: Could not clean up orphan async tasks: {e}",
              file=sys.stderr, flush=True)

    try:
        # Run stdio MCP server (this blocks until server exits)
        async with stdio_server() as (read_stream, write_stream):
            await app.run(
                read_stream,
                write_stream,
                app.create_initialization_options()
            )
    finally:
        # Clean up HTTP server
        await http_runner.cleanup()


def get_safe_tools() -> list[str]:
    """Return list of tool names marked as safe in the registry and native tools."""
    load_tools_sync()
    safe = [name for name, defn in TOOL_DEFS.items() if defn.get("safe", False)]
    safe.extend([name for name, defn in NATIVE_TOOLS.items() if defn.get("safe", False)])
    return safe


def print_safe_tools():
    """Print safe tool names, one per line. For use by elisp."""
    for tool in get_safe_tools():
        print(tool)


if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1 and sys.argv[1] == "--safe-tools":
        print_safe_tools()
    else:
        import asyncio
        asyncio.run(main())
