"""Tests for _clear_plan_mode_on_exit hook."""

import asyncio
import time
from unittest.mock import AsyncMock, MagicMock

import pytest

from claude_agent.agent import ClaudeAgent


@pytest.fixture
def agent(tmp_path):
    """Create a ClaudeAgent with block_direct_edit=False for testing."""
    return ClaudeAgent(
        work_dir=str(tmp_path),
        block_direct_edit=False,
    )


@pytest.fixture
def agent_with_client(tmp_path):
    """Create a ClaudeAgent with a mock SDK client for testing set_permission_mode."""
    a = ClaudeAgent(
        work_dir=str(tmp_path),
        block_direct_edit=False,
    )
    mock_client = MagicMock()
    mock_client.set_permission_mode = AsyncMock()
    a._client = mock_client
    return a


@pytest.fixture
def plans_dir(tmp_path, monkeypatch):
    """Create a temporary plans directory and patch HOME to use it."""
    fake_home = tmp_path / "fakehome"
    fake_home.mkdir()
    plans = fake_home / ".claude" / "plans"
    plans.mkdir(parents=True)
    monkeypatch.setenv("HOME", str(fake_home))
    return plans


def _make_hook_input(tool_name="ExitPlanMode", tool_response=None):
    """Helper to build a PostToolUse hook_input dict."""
    if tool_response is None:
        tool_response = {"content": "Plan approved", "is_error": False}
    return {
        "session_id": "test-session",
        "transcript_path": "/tmp/test.jsonl",
        "cwd": "/tmp",
        "hook_event_name": "PostToolUse",
        "tool_name": tool_name,
        "tool_input": {},
        "tool_response": tool_response,
    }


def test_deletes_most_recent_plan_file(agent, plans_dir):
    """ExitPlanMode should delete the most recently modified plan file."""
    old_plan = plans_dir / "old-plan.md"
    old_plan.write_text("# Old plan")
    time.sleep(0.05)
    new_plan = plans_dir / "new-plan.md"
    new_plan.write_text("# New plan")

    result = asyncio.run(
        agent._clear_plan_mode_on_exit(_make_hook_input(), None, {"signal": None})
    )

    assert result == {}
    assert not new_plan.exists()
    assert old_plan.exists()


def test_ignores_non_exit_plan_mode(agent, plans_dir):
    """Hook should be a no-op for tools other than ExitPlanMode."""
    plan = plans_dir / "test-plan.md"
    plan.write_text("# Test plan")

    result = asyncio.run(
        agent._clear_plan_mode_on_exit(
            _make_hook_input(tool_name="Bash"), None, {"signal": None}
        )
    )

    assert result == {}
    assert plan.exists()


def test_skips_on_error(agent, plans_dir):
    """Hook should not delete plan files if ExitPlanMode returned an error."""
    plan = plans_dir / "test-plan.md"
    plan.write_text("# Test plan")

    result = asyncio.run(
        agent._clear_plan_mode_on_exit(
            _make_hook_input(tool_response={"content": "Error", "is_error": True}),
            None,
            {"signal": None},
        )
    )

    assert result == {}
    assert plan.exists()


def test_handles_missing_plans_dir(agent, tmp_path, monkeypatch):
    """Hook should handle missing ~/.claude/plans/ gracefully."""
    fake_home = tmp_path / "emptyhome"
    fake_home.mkdir()
    monkeypatch.setenv("HOME", str(fake_home))

    result = asyncio.run(
        agent._clear_plan_mode_on_exit(_make_hook_input(), None, {"signal": None})
    )
    assert result == {}


def test_handles_empty_plans_dir(agent, plans_dir):
    """Hook should handle empty plans directory gracefully."""
    result = asyncio.run(
        agent._clear_plan_mode_on_exit(_make_hook_input(), None, {"signal": None})
    )
    assert result == {}


def test_handles_non_dict_tool_response(agent, plans_dir):
    """Hook should handle non-dict tool_response (e.g., a string)."""
    plan = plans_dir / "test-plan.md"
    plan.write_text("# Test plan")

    result = asyncio.run(
        agent._clear_plan_mode_on_exit(
            _make_hook_input(tool_response="Plan approved. You can now start coding."),
            None,
            {"signal": None},
        )
    )

    assert result == {}
    assert not plan.exists()


# ── Tests for set_permission_mode (the primary fix) ──────────────────


def test_calls_set_permission_mode_on_success(agent_with_client, plans_dir):
    """ExitPlanMode should call set_permission_mode('default') on the SDK client."""
    result = asyncio.run(
        agent_with_client._clear_plan_mode_on_exit(
            _make_hook_input(), None, {"signal": None}
        )
    )

    assert result == {}
    agent_with_client._client.set_permission_mode.assert_awaited_once_with("default")


def test_no_set_permission_mode_on_error(agent_with_client, plans_dir):
    """Should not call set_permission_mode if ExitPlanMode returned an error."""
    plan = plans_dir / "test-plan.md"
    plan.write_text("# Test plan")

    result = asyncio.run(
        agent_with_client._clear_plan_mode_on_exit(
            _make_hook_input(tool_response={"content": "Error", "is_error": True}),
            None,
            {"signal": None},
        )
    )

    assert result == {}
    agent_with_client._client.set_permission_mode.assert_not_awaited()
    assert plan.exists()


def test_no_set_permission_mode_for_other_tools(agent_with_client, plans_dir):
    """Should not call set_permission_mode for tools other than ExitPlanMode."""
    result = asyncio.run(
        agent_with_client._clear_plan_mode_on_exit(
            _make_hook_input(tool_name="Bash"), None, {"signal": None}
        )
    )

    assert result == {}
    agent_with_client._client.set_permission_mode.assert_not_awaited()


def test_handles_set_permission_mode_failure(agent_with_client, plans_dir):
    """Hook should handle set_permission_mode failure gracefully and still clean up files."""
    agent_with_client._client.set_permission_mode = AsyncMock(
        side_effect=Exception("Control request failed")
    )
    plan = plans_dir / "test-plan.md"
    plan.write_text("# Test plan")

    result = asyncio.run(
        agent_with_client._clear_plan_mode_on_exit(
            _make_hook_input(), None, {"signal": None}
        )
    )

    assert result == {}
    # set_permission_mode was called (and failed)
    agent_with_client._client.set_permission_mode.assert_awaited_once_with("default")
    # Plan file should still be cleaned up even if set_permission_mode fails
    assert not plan.exists()
