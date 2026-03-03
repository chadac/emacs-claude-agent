"""DEPRECATED: This file should be deleted.

The auto-reject functionality has been removed from the MCP server.
Path-based permission rules are now handled by Emacs via the unified
permission system in claude-agent-permissions.el.

DELETE THIS FILE after merging the permissions refactoring branch.
"""

import asyncio
import pytest
from unittest.mock import AsyncMock, patch

from emacs_mcp import server


@pytest.fixture(autouse=True)
def reset_auto_reject():
    """Reset AUTO_REJECT_PATH_PREFIXES before each test."""
    original = server.AUTO_REJECT_PATH_PREFIXES
    server.AUTO_REJECT_PATH_PREFIXES = []
    yield
    server.AUTO_REJECT_PATH_PREFIXES = original


class TestCheckAutoReject:
    """Tests for the check_auto_reject function."""

    @pytest.mark.asyncio
    async def test_no_reject_when_no_prefixes(self):
        """No rejection when AUTO_REJECT_PATH_PREFIXES is empty."""
        server.AUTO_REJECT_PATH_PREFIXES = []
        result = await server.check_auto_reject(
            "lock", {"file_path": "/any/path.el"}
        )
        assert result is None

    @pytest.mark.asyncio
    async def test_no_reject_for_non_file_tools(self):
        """Non-file-modifying tools should never be rejected."""
        server.AUTO_REJECT_PATH_PREFIXES = ["/home/user/main-repo/"]
        # read_file is not in AUTO_REJECT_TOOLS
        result = await server.check_auto_reject(
            "read_file", {"file_path": "/home/user/main-repo/foo.el"}
        )
        assert result is None

    @pytest.mark.asyncio
    async def test_lock_rejected_with_file_path_in_main_repo(self):
        """lock tool with file_path matching auto-reject prefix is rejected."""
        server.AUTO_REJECT_PATH_PREFIXES = ["/home/user/main-repo/"]
        result = await server.check_auto_reject(
            "lock", {"file_path": "/home/user/main-repo/foo.el"}
        )
        assert result is not None
        assert "REJECTED" in result
        assert "/home/user/main-repo/" in result

    @pytest.mark.asyncio
    async def test_lock_allowed_with_file_path_in_worktree(self):
        """lock tool with file_path in worktree is allowed."""
        server.AUTO_REJECT_PATH_PREFIXES = ["/home/user/main-repo/"]
        result = await server.check_auto_reject(
            "lock", {"file_path": "/home/user/worktree/foo.el"}
        )
        assert result is None

    @pytest.mark.asyncio
    async def test_edit_rejected_with_file_path(self):
        """edit tool with file_path in auto-rejected prefix is rejected."""
        server.AUTO_REJECT_PATH_PREFIXES = ["/home/user/main-repo/"]
        result = await server.check_auto_reject(
            "edit", {"file_path": "/home/user/main-repo/bar.py", "content": "new"}
        )
        assert result is not None
        assert "REJECTED" in result

    @pytest.mark.asyncio
    async def test_locks_rejected_with_file_path(self):
        """locks (batch) tool with file_path in auto-rejected prefix is rejected."""
        server.AUTO_REJECT_PATH_PREFIXES = ["/home/user/main-repo/"]
        result = await server.check_auto_reject(
            "locks", {"file_path": "/home/user/main-repo/foo.el",
                      "regions": [{"start_line": 1, "end_line": 5}]}
        )
        assert result is not None
        assert "REJECTED" in result

    @pytest.mark.asyncio
    async def test_edits_rejected_with_file_path(self):
        """edits (batch) tool with file_path in auto-rejected prefix is rejected."""
        server.AUTO_REJECT_PATH_PREFIXES = ["/home/user/main-repo/"]
        result = await server.check_auto_reject(
            "edits", {"file_path": "/home/user/main-repo/foo.el",
                      "edits": [{"lock_id": "abc", "content": "x"}]}
        )
        assert result is not None
        assert "REJECTED" in result

    @pytest.mark.asyncio
    async def test_multiple_prefixes(self):
        """Multiple auto-reject prefixes are all checked."""
        server.AUTO_REJECT_PATH_PREFIXES = [
            "/home/user/main-repo/",
            "/home/user/other-repo/",
        ]
        # First prefix
        result = await server.check_auto_reject(
            "lock", {"file_path": "/home/user/main-repo/foo.el"}
        )
        assert result is not None

        # Second prefix
        result = await server.check_auto_reject(
            "lock", {"file_path": "/home/user/other-repo/bar.py"}
        )
        assert result is not None

        # Neither prefix
        result = await server.check_auto_reject(
            "lock", {"file_path": "/home/user/worktree/baz.rs"}
        )
        assert result is None

    @pytest.mark.asyncio
    async def test_lock_with_buffer_name_resolves_file_path(self):
        """lock with buffer_name (no file_path) resolves to buffer's file for checking."""
        server.AUTO_REJECT_PATH_PREFIXES = ["/home/user/main-repo/"]

        # Mock Emacs call to resolve buffer file path
        with patch.object(
            server.lib, "call_emacs_async",
            new_callable=AsyncMock,
            return_value='"/home/user/main-repo/todo.el"',
        ):
            result = await server.check_auto_reject(
                "lock", {"buffer_name": "todo.el", "start_line": 1, "end_line": 10}
            )
        assert result is not None
        assert "REJECTED" in result
        assert "todo.el" in result

    @pytest.mark.asyncio
    async def test_lock_with_buffer_name_allowed_for_worktree_file(self):
        """lock with buffer_name resolving to worktree file is allowed."""
        server.AUTO_REJECT_PATH_PREFIXES = ["/home/user/main-repo/"]

        with patch.object(
            server.lib, "call_emacs_async",
            new_callable=AsyncMock,
            return_value='"/home/user/worktree/foo.el"',
        ):
            result = await server.check_auto_reject(
                "lock", {"buffer_name": "foo.el", "start_line": 1, "end_line": 10}
            )
        assert result is None

    @pytest.mark.asyncio
    async def test_lock_with_buffer_name_emacs_error_allows_through(self):
        """If Emacs can't resolve buffer path, allow the call (fail open)."""
        server.AUTO_REJECT_PATH_PREFIXES = ["/home/user/main-repo/"]

        with patch.object(
            server.lib, "call_emacs_async",
            new_callable=AsyncMock,
            side_effect=RuntimeError("Emacs error"),
        ):
            result = await server.check_auto_reject(
                "lock", {"buffer_name": "nonexistent.el", "start_line": 1, "end_line": 5}
            )
        # Should allow through (fail open) since we couldn't resolve
        assert result is None

    @pytest.mark.asyncio
    async def test_lock_with_buffer_name_nil_result(self):
        """If buffer has no file (e.g. scratch), allow through."""
        server.AUTO_REJECT_PATH_PREFIXES = ["/home/user/main-repo/"]

        with patch.object(
            server.lib, "call_emacs_async",
            new_callable=AsyncMock,
            return_value="nil",
        ):
            result = await server.check_auto_reject(
                "lock", {"buffer_name": "*scratch*", "start_line": 1, "end_line": 5}
            )
        assert result is None

    @pytest.mark.asyncio
    async def test_file_path_takes_precedence_over_buffer_name(self):
        """When file_path is provided, buffer_name is not resolved."""
        server.AUTO_REJECT_PATH_PREFIXES = ["/home/user/main-repo/"]

        # file_path is in worktree (allowed), buffer_name would resolve to main repo
        with patch.object(
            server.lib, "call_emacs_async",
            new_callable=AsyncMock,
        ) as mock_emacs:
            result = await server.check_auto_reject(
                "lock", {
                    "file_path": "/home/user/worktree/foo.el",
                    "buffer_name": "todo.el",
                    "start_line": 1,
                    "end_line": 10,
                }
            )
        # file_path is allowed, so result should be None
        assert result is None
        # buffer_name should NOT have been resolved via Emacs
        mock_emacs.assert_not_called()

    @pytest.mark.asyncio
    async def test_subdirectory_paths_match(self):
        """Paths in subdirectories of rejected prefix are also rejected."""
        server.AUTO_REJECT_PATH_PREFIXES = ["/home/user/main-repo/"]
        result = await server.check_auto_reject(
            "lock", {"file_path": "/home/user/main-repo/deep/nested/dir/file.el"}
        )
        assert result is not None
        assert "REJECTED" in result

    @pytest.mark.asyncio
    async def test_prefix_must_match_from_start(self):
        """Path prefix must match from the start, not a substring."""
        server.AUTO_REJECT_PATH_PREFIXES = ["/home/user/main-repo/"]
        # This path contains "main-repo" but doesn't start with the prefix
        result = await server.check_auto_reject(
            "lock", {"file_path": "/other/home/user/main-repo/file.el"}
        )
        assert result is None

    @pytest.mark.asyncio
    async def test_empty_file_path_not_rejected(self):
        """Empty file_path should not be rejected."""
        server.AUTO_REJECT_PATH_PREFIXES = ["/home/user/main-repo/"]
        result = await server.check_auto_reject(
            "lock", {"file_path": "", "buffer_name": "", "start_line": 1, "end_line": 5}
        )
        assert result is None

    @pytest.mark.asyncio
    async def test_no_file_path_no_buffer_name_allowed(self):
        """Tool call without file_path or buffer_name is allowed."""
        server.AUTO_REJECT_PATH_PREFIXES = ["/home/user/main-repo/"]
        # edit with only lock_id and content (no file_path or buffer_name)
        result = await server.check_auto_reject(
            "edit", {"lock_id": "abc123", "content": "new content"}
        )
        assert result is None


class TestAutoRejectPathPrefixesInit:
    """Tests for loading AUTO_REJECT_PATH_PREFIXES from environment."""

    def test_parse_unit_separator_delimited(self):
        """Environment variable is parsed correctly."""
        paths = "/home/a/\x1f/home/b/"
        result = [p for p in paths.split("\x1f") if p]
        assert result == ["/home/a/", "/home/b/"]

    def test_empty_env_var(self):
        """Empty environment variable produces empty list."""
        paths = ""
        result = [p for p in paths.split("\x1f") if p]
        assert result == []

    def test_single_path(self):
        """Single path without separator works."""
        paths = "/home/user/main-repo/"
        result = [p for p in paths.split("\x1f") if p]
        assert result == ["/home/user/main-repo/"]
