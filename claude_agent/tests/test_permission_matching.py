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
