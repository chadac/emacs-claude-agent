"""Tests for claude_emacs_agent."""

import json
import pytest
from claude_emacs_agent.agent import (
    ClaudeEmacsAgent,
    AgentState,
    MARKER_READY,
    MARKER_ASSISTANT_START,
    MARKER_ASSISTANT_END,
    MARKER_TOOL_START,
    MARKER_PERMISSION_REQUEST,
)


class TestAgentState:
    """Tests for AgentState dataclass."""

    def test_default_state(self):
        """Test default state values."""
        state = AgentState()
        assert state.session_id is None
        assert state.model is None
        assert state.status == "initializing"
        assert state.pending_tool_calls == {}


class TestClaudeEmacsAgent:
    """Tests for ClaudeEmacsAgent."""

    def test_init(self):
        """Test agent initialization."""
        agent = ClaudeEmacsAgent("/tmp/test", mcp_config="/tmp/config.json")
        assert agent.work_dir == "/tmp/test"
        assert agent.mcp_config == "/tmp/config.json"
        assert agent.state.status == "initializing"

    def test_emit(self, capsys):
        """Test that _emit writes to stdout."""
        agent = ClaudeEmacsAgent("/tmp/test")
        agent._emit("TEST message")
        captured = capsys.readouterr()
        assert captured.out == "TEST message\n"

    def test_emit_ready(self, capsys):
        """Test ready marker emission."""
        agent = ClaudeEmacsAgent("/tmp/test")
        agent._emit_ready()
        captured = capsys.readouterr()
        assert MARKER_READY in captured.out

class TestMessageParsing:
    """Tests for parsing Claude CLI JSON messages."""

    @pytest.mark.asyncio
    async def test_handle_init_message(self, capsys):
        """Test handling of init message."""
        agent = ClaudeEmacsAgent("/tmp/test")
        init_msg = {
            "type": "system",
            "subtype": "init",
            "session_id": "test-session-123",
            "model": "claude-opus-4-5",
            "tools": ["Bash", "Read", "Write"],
        }
        await agent._handle_claude_message(init_msg)

        assert agent.state.session_id == "test-session-123"
        assert agent.state.model == "claude-opus-4-5"
        assert agent.state.status == "ready"

    @pytest.mark.asyncio
    async def test_handle_assistant_text(self, capsys):
        """Test handling of assistant text message."""
        agent = ClaudeEmacsAgent("/tmp/test")
        assistant_msg = {
            "type": "assistant",
            "message": {
                "content": [{"type": "text", "text": "Hello, I can help!"}]
            },
        }
        await agent._handle_claude_message(assistant_msg)

        captured = capsys.readouterr()
        assert MARKER_ASSISTANT_START in captured.out
        assert "Hello, I can help!" in captured.out
        assert MARKER_ASSISTANT_END in captured.out

    @pytest.mark.asyncio
    async def test_handle_tool_call(self, capsys):
        """Test handling of tool call message."""
        agent = ClaudeEmacsAgent("/tmp/test")
        tool_msg = {
            "type": "assistant",
            "message": {
                "content": [
                    {
                        "type": "tool_use",
                        "id": "tool-123",
                        "name": "Bash",
                        "input": {"command": "ls -la"},
                    }
                ]
            },
        }
        await agent._handle_claude_message(tool_msg)

        assert "tool-123" in agent.state.pending_tool_calls
        captured = capsys.readouterr()
        assert f"{MARKER_TOOL_START}Bash]" in captured.out

    @pytest.mark.asyncio
    async def test_handle_result(self, capsys):
        """Test handling of result message."""
        agent = ClaudeEmacsAgent("/tmp/test")
        result_msg = {
            "type": "result",
            "usage": {"input_tokens": 100, "output_tokens": 50},
        }
        await agent._handle_claude_message(result_msg)

        assert agent.state.status == "ready"

        captured = capsys.readouterr()
        # Should show ready marker
        assert MARKER_READY in captured.out

    @pytest.mark.asyncio
    async def test_handle_result_with_permission_denial(self, capsys):
        """Test handling of result with permission denial."""
        agent = ClaudeEmacsAgent("/tmp/test")
        result_msg = {
            "type": "result",
            "permission_denials": [
                {
                    "tool_name": "Read",
                    "tool_use_id": "tool-456",
                    "tool_input": {"file_path": "/etc/passwd"},
                }
            ],
        }
        await agent._handle_claude_message(result_msg)

        # Should emit permission request, not ready
        captured = capsys.readouterr()
        assert MARKER_PERMISSION_REQUEST in captured.out
        assert MARKER_READY not in captured.out
        assert agent.state.pending_permission is not None

    @pytest.mark.asyncio
    async def test_permission_response_deny(self, capsys):
        """Test denying a permission request."""
        agent = ClaudeEmacsAgent("/tmp/test")
        agent.state.pending_permission = {
            "tool_name": "Read",
            "tool_input": {"file_path": "/etc/passwd"},
        }

        await agent.handle_permission_response({"action": "deny"})

        assert agent.state.pending_permission is None
        captured = capsys.readouterr()
        assert MARKER_READY in captured.out
