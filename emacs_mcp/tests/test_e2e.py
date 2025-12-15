#!/usr/bin/env python3
"""End-to-end integration tests for MCP server tool loading from Emacs.

These tests require a running Emacs server with claude-mcp.el loaded.

Run with:
  uv run pytest tests/test_e2e.py -v -m integration

Or from project root:
  cd emacs_mcp && uv run pytest -v -m integration
"""

import json
import os
import pytest
import subprocess

from emacs_mcp import lib


pytestmark = pytest.mark.integration  # Mark all tests in this module


@pytest.fixture
def emacs_available():
    """Check if Emacs server is available."""
    try:
        lib.get_socket_path()
        return True
    except Exception:
        pytest.skip("Emacs server not available")


def test_export_tools_returns_valid_json(emacs_available):
    """Test that (claude-mcp-export-tools) returns valid JSON."""
    result = lib.call_emacs("(claude-mcp-export-tools)")

    # emacsclient returns JSON wrapped in quotes
    assert result.startswith('"') and result.endswith('"'), \
        f"Expected quoted JSON, got: {result[:100]}"

    # Strip outer quotes and unescape
    json_str = result[1:-1].replace('\\"', '"').replace('\\n', '\n')

    # Should parse as JSON
    tools = json.loads(json_str)
    assert isinstance(tools, dict), "Expected tools to be a dict"

    print(f"\nFound {len(tools)} tools")
    return tools


def test_tools_have_expected_format(emacs_available):
    """Test that exported tools have the expected structure."""
    result = lib.call_emacs("(claude-mcp-export-tools)")
    json_str = result[1:-1].replace('\\"', '"').replace('\\n', '\n')
    tools = json.loads(json_str)

    # Should have at least the 6 tools we registered
    assert len(tools) >= 6, f"Expected at least 6 tools, got {len(tools)}"

    # Check for our test tools (with underscores in JSON)
    expected_tools = [
        "get_buffer_content",
        "list_buffers",
        "buffer_info",
        "search_buffer",
        "get_region",
        "clear_buffer"
    ]

    for tool_name in expected_tools:
        assert tool_name in tools, f"Missing tool: {tool_name}"

        tool = tools[tool_name]
        assert "description" in tool, f"{tool_name}: missing description"
        assert "function" in tool, f"{tool_name}: missing function"
        assert "safe" in tool, f"{tool_name}: missing safe flag"
        assert "args" in tool, f"{tool_name}: missing args"

        # Verify args structure
        assert isinstance(tool["args"], dict), f"{tool_name}: args should be dict"

    print(f"\nAll {len(expected_tools)} expected tools found with correct structure")


def test_tool_args_converted_correctly(emacs_available):
    """Test that tool arguments are converted from dashes to underscores."""
    result = lib.call_emacs("(claude-mcp-export-tools)")
    json_str = result[1:-1].replace('\\"', '"').replace('\\n', '\n')
    tools = json.loads(json_str)

    # Check get_buffer_content args
    tool = tools["get_buffer_content"]
    args = tool["args"]

    # Should have buffer_name (not buffer-name)
    assert "buffer_name" in args, "Expected buffer_name arg"
    assert "tail_lines" in args, "Expected tail_lines arg"
    assert "head_lines" in args, "Expected head_lines arg"

    # Check arg properties
    buffer_name_arg = args["buffer_name"]
    assert buffer_name_arg["type"] == "string"
    assert buffer_name_arg["required"] is True
    assert "description" in buffer_name_arg

    print("\nArg conversion (dash to underscore) working correctly")


def test_load_tools_function():
    """Test that load_tools() can query Emacs successfully."""
    from emacs_mcp import server

    # This will call emacsclient
    tools = server.load_tools()

    assert isinstance(tools, dict), "load_tools should return a dict"
    assert len(tools) >= 6, f"Expected at least 6 tools, got {len(tools)}"

    # Verify TOOL_DEFS was populated
    assert len(server.TOOL_DEFS) >= 6

    print(f"\nload_tools() successfully loaded {len(tools)} tools")


def test_safe_tools_flagged_correctly(emacs_available):
    """Test that safe tools have correct boolean values."""
    result = lib.call_emacs("(claude-mcp-export-tools)")
    json_str = result[1:-1].replace('\\"', '"').replace('\\n', '\n')
    tools = json.loads(json_str)

    # get_buffer_content should be safe
    assert tools["get_buffer_content"]["safe"] is True

    # clear_buffer should NOT be safe
    assert tools["clear_buffer"]["safe"] is False

    print("\nSafe flags correctly set")


if __name__ == "__main__":
    # Run tests with verbose output
    pytest.main([__file__, "-v", "-s"])
