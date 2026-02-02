"""Tests for the system message injection framework.

Tests the Python-side behavior: pending system message queue,
reminder block building, and system reminder filtering.
Hook orchestration (triggers, intervals, etc.) is handled by Emacs.
"""

import inspect
import pytest
from claude_agent.agent import (
    _build_system_reminder_block,
    ClaudeAgent,
)


class TestBuildSystemReminderBlock:
    """Tests for the _build_system_reminder_block function."""

    def test_single_message(self):
        """Test wrapping a single message."""
        result = _build_system_reminder_block(["hello world"])
        assert result == "<system-reminder>\nhello world\n</system-reminder>"

    def test_multiple_messages(self):
        """Test wrapping multiple messages."""
        result = _build_system_reminder_block(["msg1", "msg2"])
        assert "<system-reminder>\nmsg1\n</system-reminder>" in result
        assert "<system-reminder>\nmsg2\n</system-reminder>" in result
        # They should be separated by double newlines
        assert "\n\n" in result

    def test_empty_list(self):
        """Test with empty list."""
        result = _build_system_reminder_block([])
        assert result == ""

    def test_multiline_message(self):
        """Test wrapping a message that spans multiple lines."""
        result = _build_system_reminder_block(["line1\nline2\nline3"])
        assert result == "<system-reminder>\nline1\nline2\nline3\n</system-reminder>"

    def test_preserves_message_order(self):
        """Test that multiple messages preserve their order."""
        result = _build_system_reminder_block(["first", "second", "third"])
        first_pos = result.index("first")
        second_pos = result.index("second")
        third_pos = result.index("third")
        assert first_pos < second_pos < third_pos


class TestPendingSystemMessages:
    """Tests for the pending system message queue on ClaudeAgent."""

    def test_agent_has_empty_pending_messages(self):
        """Test that a new agent starts with no pending system messages."""
        agent = ClaudeAgent("/tmp/test")
        assert agent._pending_system_messages == []

    def test_pending_messages_is_a_list(self):
        """Test that pending messages is a list we can append to."""
        agent = ClaudeAgent("/tmp/test")
        agent._pending_system_messages.append("test message")
        assert len(agent._pending_system_messages) == 1
        assert agent._pending_system_messages[0] == "test message"

    def test_multiple_pending_messages(self):
        """Test queuing multiple system messages."""
        agent = ClaudeAgent("/tmp/test")
        agent._pending_system_messages.append("msg1")
        agent._pending_system_messages.append("msg2")
        agent._pending_system_messages.append("msg3")
        assert len(agent._pending_system_messages) == 3
        assert agent._pending_system_messages == ["msg1", "msg2", "msg3"]

    def test_clear_pending_messages(self):
        """Test clearing the pending messages queue."""
        agent = ClaudeAgent("/tmp/test")
        agent._pending_system_messages.append("msg1")
        agent._pending_system_messages.append("msg2")
        agent._pending_system_messages.clear()
        assert agent._pending_system_messages == []


class TestFilterSystemReminders:
    """Tests for the _filter_system_reminders method."""

    def test_removes_single_reminder(self):
        """Test removing a single system-reminder block."""
        agent = ClaudeAgent("/tmp/test")
        text = "Hello\n<system-reminder>\nsome reminder\n</system-reminder>\nWorld"
        result = agent._filter_system_reminders(text)
        assert "some reminder" not in result
        assert "system-reminder" not in result
        assert "Hello" in result
        assert "World" in result

    def test_removes_multiple_reminders(self):
        """Test removing multiple system-reminder blocks."""
        agent = ClaudeAgent("/tmp/test")
        text = (
            "Start\n"
            "<system-reminder>\nfirst\n</system-reminder>\n"
            "Middle\n"
            "<system-reminder>\nsecond\n</system-reminder>\n"
            "End"
        )
        result = agent._filter_system_reminders(text)
        assert "first" not in result
        assert "second" not in result
        assert "Start" in result
        assert "Middle" in result
        assert "End" in result

    def test_no_reminders_unchanged(self):
        """Test that text without reminders passes through unchanged."""
        agent = ClaudeAgent("/tmp/test")
        text = "Just normal text with no reminders"
        result = agent._filter_system_reminders(text)
        assert result == text

    def test_empty_string(self):
        """Test filtering an empty string."""
        agent = ClaudeAgent("/tmp/test")
        assert agent._filter_system_reminders("") == ""


class TestNoHooksConfigParameter:
    """Tests verifying that the old hooks_config infrastructure is removed."""

    def test_no_hooks_config_param(self):
        """Test that ClaudeAgent.__init__ does not accept hooks_config."""
        sig = inspect.signature(ClaudeAgent.__init__)
        assert "hooks_config" not in sig.parameters

    def test_no_system_hooks_attribute(self):
        """Test that ClaudeAgent does not have a system_hooks registry."""
        agent = ClaudeAgent("/tmp/test")
        assert not hasattr(agent, "system_hooks")

    def test_agent_init_with_work_dir_only(self):
        """Test that agent can be created with just work_dir."""
        agent = ClaudeAgent("/tmp/test")
        assert agent is not None


class TestSystemMessageEventEmission:
    """Tests that system_start/system_text/system_end events are emitted immediately on receipt."""

    def test_no_events_when_no_messages(self):
        """No system events should be emitted when no messages arrive."""
        agent = ClaudeAgent("/tmp/test")
        emitted = []
        agent._emit = lambda msg: emitted.append(msg)
        # No system messages received — nothing emitted
        assert len(emitted) == 0

    def test_single_message_emits_events_immediately(self):
        """A system message should emit start/text/end at receipt time."""
        agent = ClaudeAgent("/tmp/test")
        emitted = []
        agent._emit = lambda msg: emitted.append(msg)
        # Simulate what read_stdin does when it receives a system_message
        text = "Test reminder"
        agent._emit({"type": "system_start"})
        agent._emit({"type": "system_text", "text": text})
        agent._emit({"type": "system_end"})
        agent._pending_system_messages.append(text)
        # Display events emitted immediately
        assert len(emitted) == 3
        assert emitted[0] == {"type": "system_start"}
        assert emitted[1] == {"type": "system_text", "text": "Test reminder"}
        assert emitted[2] == {"type": "system_end"}
        # Message also queued for injection
        assert agent._pending_system_messages == ["Test reminder"]

    def test_multiple_messages_emit_separate_blocks(self):
        """Each system message should get its own start/text/end block on receipt."""
        agent = ClaudeAgent("/tmp/test")
        emitted = []
        agent._emit = lambda msg: emitted.append(msg)
        # Simulate two system_message arrivals via read_stdin
        for text in ["First message", "Second message"]:
            agent._emit({"type": "system_start"})
            agent._emit({"type": "system_text", "text": text})
            agent._emit({"type": "system_end"})
            agent._pending_system_messages.append(text)
        assert len(emitted) == 6
        # First block
        assert emitted[0] == {"type": "system_start"}
        assert emitted[1] == {"type": "system_text", "text": "First message"}
        assert emitted[2] == {"type": "system_end"}
        # Second block
        assert emitted[3] == {"type": "system_start"}
        assert emitted[4] == {"type": "system_text", "text": "Second message"}
        assert emitted[5] == {"type": "system_end"}
        # Both queued for injection
        assert agent._pending_system_messages == ["First message", "Second message"]

    def test_multiline_message_emits_full_text(self):
        """Multiline messages should be emitted in full, not truncated."""
        agent = ClaudeAgent("/tmp/test")
        emitted = []
        agent._emit = lambda msg: emitted.append(msg)
        long_msg = "Line 1\nLine 2\nLine 3\nThis is a very long message"
        agent._emit({"type": "system_start"})
        agent._emit({"type": "system_text", "text": long_msg})
        agent._emit({"type": "system_end"})
        agent._pending_system_messages.append(long_msg)
        assert emitted[1]["text"] == long_msg

    def test_display_decoupled_from_injection(self):
        """Display events happen at receipt; injection happens at send_user_message time."""
        agent = ClaudeAgent("/tmp/test")
        emitted = []
        agent._emit = lambda msg: emitted.append(msg)
        # Receipt: display events emitted, message queued
        text = "Reminder text"
        agent._emit({"type": "system_start"})
        agent._emit({"type": "system_text", "text": text})
        agent._emit({"type": "system_end"})
        agent._pending_system_messages.append(text)
        assert len(emitted) == 3
        # Later: send_user_message consumes queue without re-emitting display events
        from claude_agent.agent import _build_system_reminder_block
        reminder_block = _build_system_reminder_block(agent._pending_system_messages)
        full_message = f"user msg\n\n{reminder_block}"
        agent._pending_system_messages.clear()
        # No new display events emitted
        assert len(emitted) == 3
        # But the message was built correctly for Claude
        assert "<system-reminder>" in full_message
        assert "Reminder text" in full_message
        assert agent._pending_system_messages == []
