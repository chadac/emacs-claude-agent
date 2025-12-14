"""Protocol definitions for Claude Emacs Agent communication.

All messages are newline-delimited JSON objects.
Each message has a "type" field that determines its structure.
"""

from typing import TypedDict, Literal, Any, Union


# ============================================================================
# Messages from Python Agent → Emacs
# ============================================================================

class BaseMessage(TypedDict):
    """Base for all messages."""
    type: str


# Session lifecycle messages

class SessionStartMessage(TypedDict):
    """Session has started."""
    type: Literal["session_start"]
    work_dir: str


class SessionInfoMessage(TypedDict):
    """Session metadata update (model, session_id)."""
    type: Literal["session_info"]
    model: str | None
    session_id: str | None


class ReadyMessage(TypedDict):
    """Agent is ready for next input."""
    type: Literal["ready"]


# Conversation turn messages

class UserMessageStartMessage(TypedDict):
    """User message block started."""
    type: Literal["user_start"]


class UserMessageTextMessage(TypedDict):
    """User message text content."""
    type: Literal["user_text"]
    text: str


class UserMessageEndMessage(TypedDict):
    """User message block ended."""
    type: Literal["user_end"]


class AssistantMessageStartMessage(TypedDict):
    """Assistant message block started."""
    type: Literal["assistant_start"]


class AssistantMessageTextMessage(TypedDict):
    """Assistant message text content."""
    type: Literal["assistant_text"]
    text: str


class AssistantMessageEndMessage(TypedDict):
    """Assistant message block ended."""
    type: Literal["assistant_end"]


# Tool call messages

class ToolCallMessage(TypedDict):
    """Tool call initiated."""
    type: Literal["tool_call"]
    name: str
    input: dict[str, Any]


class ToolResultMessage(TypedDict):
    """Tool result content."""
    type: Literal["tool_result"]
    content: str
    is_error: bool


class ToolEndMessage(TypedDict):
    """Tool call completed."""
    type: Literal["tool_end"]


# Special tool display messages (for fancy UI rendering)

class EditToolMessage(TypedDict):
    """Edit tool with diff display data."""
    type: Literal["edit_tool"]
    file_path: str
    old_string: str
    new_string: str


class WriteToolMessage(TypedDict):
    """Write tool with content preview."""
    type: Literal["write_tool"]
    file_path: str
    content: str


# Status messages

class ThinkingMessage(TypedDict):
    """Agent is thinking/processing."""
    type: Literal["thinking"]
    status: str  # e.g. "Thinking...", "Running: Read", "Editing: foo.py"


class ProgressMessage(TypedDict):
    """Progress update (token counts during streaming)."""
    type: Literal["progress"]
    input_tokens: int
    output_tokens: int


class ResultMessage(TypedDict):
    """Turn completed with final stats."""
    type: Literal["result"]
    cost_usd: float
    duration_ms: int
    num_turns: int
    total_input: int
    total_output: int


# MCP messages

class MCPStatusMessage(TypedDict):
    """MCP server connection status."""
    type: Literal["mcp_status"]
    servers: list[dict[str, str]]  # Each: {"name": "...", "status": "..."}


# Permission messages

class PermissionRequestMessage(TypedDict):
    """Request permission for tool use."""
    type: Literal["permission_request"]
    tool_name: str
    tool_input: dict[str, Any]


class PermissionGrantedMessage(TypedDict):
    """Permission was granted (for logging)."""
    type: Literal["permission_granted"]
    pattern: str
    scope: Literal["once", "session", "always"]


# Error messages

class ErrorMessage(TypedDict):
    """Error occurred."""
    type: Literal["error"]
    message: str
    traceback: str | None


# Session info messages (legacy compatibility)

class SessionMessageStartMessage(TypedDict):
    """Session info message started."""
    type: Literal["session_message_start"]


class SessionMessageTextMessage(TypedDict):
    """Session info text."""
    type: Literal["session_message_text"]
    text: str


class SessionMessageEndMessage(TypedDict):
    """Session info message ended."""
    type: Literal["session_message_end"]


# Union type for all agent→emacs messages

AgentMessage = (
    SessionStartMessage
    | SessionInfoMessage
    | ReadyMessage
    | UserMessageStartMessage
    | UserMessageTextMessage
    | UserMessageEndMessage
    | AssistantMessageStartMessage
    | AssistantMessageTextMessage
    | AssistantMessageEndMessage
    | ToolCallMessage
    | ToolResultMessage
    | ToolEndMessage
    | EditToolMessage
    | WriteToolMessage
    | ThinkingMessage
    | ProgressMessage
    | ResultMessage
    | MCPStatusMessage
    | PermissionRequestMessage
    | PermissionGrantedMessage
    | ErrorMessage
    | SessionMessageStartMessage
    | SessionMessageTextMessage
    | SessionMessageEndMessage
)


# ============================================================================
# Messages from Emacs → Python Agent
# ============================================================================

class UserInputMessage(TypedDict):
    """User wants to send a message to Claude."""
    type: Literal["message"]
    text: str


class InterruptMessage(TypedDict):
    """Interrupt current operation."""
    type: Literal["interrupt"]


class QuitMessage(TypedDict):
    """Quit the session."""
    type: Literal["quit"]


class PermissionResponseMessage(TypedDict):
    """Response to permission request."""
    type: Literal["permission_response"]
    action: Literal["allow_once", "allow_session", "allow_always", "deny"]
    pattern: str | None  # None for deny


# Union type for all emacs→agent messages

EmacsMessage = (
    UserInputMessage
    | InterruptMessage
    | QuitMessage
    | PermissionResponseMessage
)
