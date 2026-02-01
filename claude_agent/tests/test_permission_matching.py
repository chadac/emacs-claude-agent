"""Tests for permission pattern matching."""

import pytest
from claude_agent.agent import ClaudeAgent


class TestPermissionMatching:
    """Tests for _pattern_matches and _matches_permission methods."""

    def test_bash_wildcard_with_colon_separator(self):
        """Test that Bash(ls:*) matches commands starting with 'ls'."""
        agent = ClaudeAgent("/tmp/test")

        # Pattern: Bash(ls:*)
        pattern = "Bash(ls:*)"
        tool_name = "Bash"

        # Should match commands starting with "ls"
        assert agent._pattern_matches(pattern, tool_name, {"command": "ls"})
        assert agent._pattern_matches(pattern, tool_name, {"command": "ls -la"})
        assert agent._pattern_matches(pattern, tool_name, {"command": "ls -lh /tmp"})

        # Should NOT match commands that don't start with "ls"
        assert not agent._pattern_matches(pattern, tool_name, {"command": "echo ls"})
        assert not agent._pattern_matches(pattern, tool_name, {"command": "lsof"})

    def test_bash_wildcard_without_colon(self):
        """Test that Bash(echo *) works without colon."""
        agent = ClaudeAgent("/tmp/test")

        pattern = "Bash(echo *)"
        tool_name = "Bash"

        # Should match "echo " exactly
        assert agent._pattern_matches(pattern, tool_name, {"command": "echo hello"})
        assert agent._pattern_matches(pattern, tool_name, {"command": "echo "})

        # Should NOT match without the space
        assert not agent._pattern_matches(pattern, tool_name, {"command": "echo"})

    def test_bash_exact_match(self):
        """Test exact command matching."""
        agent = ClaudeAgent("/tmp/test")

        pattern = "Bash(pwd)"
        tool_name = "Bash"

        # Exact match
        assert agent._pattern_matches(pattern, tool_name, {"command": "pwd"})

        # Should NOT match with args
        assert not agent._pattern_matches(pattern, tool_name, {"command": "pwd -P"})

    def test_read_file_wildcard(self):
        """Test file path wildcards for Read."""
        agent = ClaudeAgent("/tmp/test")

        pattern = "Read(/home/user/*)"
        tool_name = "Read"

        # Should match files under /home/user/
        assert agent._pattern_matches(pattern, tool_name, {"file_path": "/home/user/file.txt"})
        assert agent._pattern_matches(pattern, tool_name, {"file_path": "/home/user/dir/file.txt"})

        # Should NOT match files outside
        assert not agent._pattern_matches(pattern, tool_name, {"file_path": "/home/other/file.txt"})

    def test_tool_name_only(self):
        """Test matching just the tool name allows all uses."""
        agent = ClaudeAgent("/tmp/test")

        pattern = "Bash"
        tool_name = "Bash"

        # Should match any Bash command
        assert agent._pattern_matches(pattern, tool_name, {"command": "ls"})
        assert agent._pattern_matches(pattern, tool_name, {"command": "echo hello"})
        assert agent._pattern_matches(pattern, tool_name, {"command": "anything"})

    def test_session_permissions_match(self):
        """Test that session permissions are checked correctly."""
        agent = ClaudeAgent("/tmp/test")

        # Add permission to session
        agent.state.session_permissions.add("Bash(git:*)")

        # Should match
        assert agent._matches_permission("Bash", {"command": "git status"})
        assert agent._matches_permission("Bash", {"command": "git add ."})

        # Should NOT match
        assert not agent._matches_permission("Bash", {"command": "rm -rf /"})

    def test_always_permissions_match(self):
        """Test that always permissions are checked correctly."""
        agent = ClaudeAgent("/tmp/test")

        # Add permission to always
        agent.state.always_permissions.add("Read(/tmp/*)")

        # Should match
        assert agent._matches_permission("Read", {"file_path": "/tmp/file.txt"})
        assert agent._matches_permission("Read", {"file_path": "/tmp/dir/file.txt"})

        # Should NOT match
        assert not agent._matches_permission("Read", {"file_path": "/home/file.txt"})

    def test_multiple_patterns(self):
        """Test that multiple patterns are checked."""
        agent = ClaudeAgent("/tmp/test")

        agent.state.session_permissions.add("Bash(ls:*)")
        agent.state.session_permissions.add("Bash(pwd)")
        agent.state.always_permissions.add("Read(/tmp/*)")

        # Should match from session permissions
        assert agent._matches_permission("Bash", {"command": "ls -la"})
        assert agent._matches_permission("Bash", {"command": "pwd"})

        # Should match from always permissions
        assert agent._matches_permission("Read", {"file_path": "/tmp/test.txt"})

        # Should NOT match
        assert not agent._matches_permission("Bash", {"command": "rm file"})
        assert not agent._matches_permission("Read", {"file_path": "/home/test.txt"})

    def test_bash_colon_with_exact_match(self):
        """Test that Bash patterns without wildcards work correctly."""
        agent = ClaudeAgent("/tmp/test")

        # This should match the exact command "git status"
        pattern = "Bash(git status)"
        tool_name = "Bash"

        assert agent._pattern_matches(pattern, tool_name, {"command": "git status"})
        assert not agent._pattern_matches(pattern, tool_name, {"command": "git status --short"})

    def test_bash_echo_colon_star(self):
        """Regression test: ensure Bash(echo:*) matches 'echo' commands."""
        agent = ClaudeAgent("/tmp/test")

        pattern = "Bash(echo:*)"
        tool_name = "Bash"

        assert agent._pattern_matches(pattern, tool_name, {"command": "echo"})
        assert agent._pattern_matches(pattern, tool_name, {"command": "echo hello"})
        assert agent._pattern_matches(pattern, tool_name, {"command": "echo -n test"})
        assert not agent._pattern_matches(pattern, tool_name, {"command": "ls"})

    def test_mcp_emacs_lock_path_scoped(self):
        """Test that mcp__emacs__lock can be path-scoped to a worktree."""
        agent = ClaudeAgent("/tmp/test")

        pattern = "mcp__emacs__lock(/home/user/worktree/*)"
        tool_name = "mcp__emacs__lock"

        # Should match files within the worktree
        assert agent._pattern_matches(pattern, tool_name, {"file_path": "/home/user/worktree/foo.el"})
        assert agent._pattern_matches(pattern, tool_name, {"file_path": "/home/user/worktree/src/bar.py"})

        # Should NOT match files outside the worktree
        assert not agent._pattern_matches(pattern, tool_name, {"file_path": "/home/user/main-repo/foo.el"})
        assert not agent._pattern_matches(pattern, tool_name, {"file_path": "/tmp/other.el"})

    def test_mcp_emacs_lock_no_file_path(self):
        """Test that mcp__emacs__lock with path pattern doesn't match when no file_path."""
        agent = ClaudeAgent("/tmp/test")

        pattern = "mcp__emacs__lock(/home/user/worktree/*)"
        tool_name = "mcp__emacs__lock"

        # No file_path in input — should not match
        assert not agent._pattern_matches(pattern, tool_name, {"buffer_name": "foo.el", "start_line": 1, "end_line": 5})

    def test_mcp_emacs_lock_unconditional(self):
        """Test that bare mcp__emacs__lock matches any invocation."""
        agent = ClaudeAgent("/tmp/test")

        pattern = "mcp__emacs__lock"
        tool_name = "mcp__emacs__lock"

        assert agent._pattern_matches(pattern, tool_name, {"file_path": "/any/path.el"})
        assert agent._pattern_matches(pattern, tool_name, {"buffer_name": "foo"})

    def test_mcp_emacs_edit_unconditional(self):
        """Test that mcp__emacs__edit matches unconditionally (safe — requires prior lock)."""
        agent = ClaudeAgent("/tmp/test")

        pattern = "mcp__emacs__edit"
        tool_name = "mcp__emacs__edit"

        assert agent._pattern_matches(pattern, tool_name, {"content": "new stuff"})
        assert agent._pattern_matches(pattern, tool_name, {"file_path": "/any/path.el", "content": "x"})

    def test_mcp_emacs_read_file_path_scoped(self):
        """Test that mcp__emacs__read_file can be path-scoped."""
        agent = ClaudeAgent("/tmp/test")

        pattern = "mcp__emacs__read_file(/home/user/worktree/*)"
        tool_name = "mcp__emacs__read_file"

        assert agent._pattern_matches(pattern, tool_name, {"file_path": "/home/user/worktree/foo.el"})
        assert not agent._pattern_matches(pattern, tool_name, {"file_path": "/home/user/main/foo.el"})


class TestAutoRejectRules:
    """Tests for auto-reject rules (worktree confinement)."""

    def test_path_prefix_rejects_main_repo(self):
        """Test that path_prefix rule rejects tools targeting main repo."""
        agent = ClaudeAgent("/tmp/test")

        rule = {
            "path_prefix": "/home/user/main-repo/",
            "message": "REJECTED: Edit in worktree instead.",
        }

        # Should reject tools targeting the main repo
        assert agent._matches_auto_reject(rule, "mcp__emacs__lock", {"file_path": "/home/user/main-repo/foo.el"})
        assert agent._matches_auto_reject(rule, "mcp__emacs__lock", {"file_path": "/home/user/main-repo/src/bar.py"})
        assert agent._matches_auto_reject(rule, "Read", {"file_path": "/home/user/main-repo/README.md"})

        # Should NOT reject tools targeting the worktree
        assert not agent._matches_auto_reject(rule, "mcp__emacs__lock", {"file_path": "/home/user/worktree/foo.el"})
        assert not agent._matches_auto_reject(rule, "Read", {"file_path": "/tmp/other.txt"})

    def test_path_prefix_no_file_path(self):
        """Test that path_prefix rule doesn't reject tools without file_path."""
        agent = ClaudeAgent("/tmp/test")

        rule = {
            "path_prefix": "/home/user/main-repo/",
            "message": "REJECTED",
        }

        # Tools without file_path should NOT be rejected
        assert not agent._matches_auto_reject(rule, "Bash", {"command": "ls"})
        assert not agent._matches_auto_reject(rule, "mcp__emacs__edit", {"content": "new stuff"})

    def test_extract_file_path_for_various_tools(self):
        """Test _extract_file_path for different tool types."""
        agent = ClaudeAgent("/tmp/test")

        assert agent._extract_file_path("Read", {"file_path": "/a/b"}) == "/a/b"
        assert agent._extract_file_path("Write", {"file_path": "/a/b"}) == "/a/b"
        assert agent._extract_file_path("Edit", {"file_path": "/a/b"}) == "/a/b"
        assert agent._extract_file_path("mcp__emacs__lock", {"file_path": "/a/b"}) == "/a/b"
        assert agent._extract_file_path("mcp__emacs__read_file", {"file_path": "/a/b"}) == "/a/b"
        assert agent._extract_file_path("Glob", {"path": "/a/b"}) == "/a/b"
        assert agent._extract_file_path("Grep", {"path": "/a/b"}) == "/a/b"
        assert agent._extract_file_path("Bash", {"command": "ls"}) is None
