"""Tests for validate_tool_result function."""

import pytest
from claude_agent.agent import validate_tool_result


class TestValidateToolResult:
    """Tests for the validate_tool_result function."""

    def test_non_error_result_unchanged(self):
        """Non-error results should pass through unchanged."""
        tool_response = {
            "content": "some result",
            "is_error": False,
        }
        result = validate_tool_result(tool_response, "TestTool")
        assert result == tool_response

    def test_error_with_valid_content_unchanged(self):
        """Error results with valid content should pass through unchanged."""
        tool_response = {
            "content": [{"type": "text", "text": "Error occurred"}],
            "is_error": True,
        }
        result = validate_tool_result(tool_response, "TestTool")
        assert result == tool_response

    def test_error_with_none_content_gets_placeholder(self):
        """Error with None content should get placeholder."""
        tool_response = {
            "content": None,
            "is_error": True,
        }
        result = validate_tool_result(tool_response, "TestTool")
        assert result["is_error"] is True
        assert isinstance(result["content"], list)
        assert len(result["content"]) == 1
        assert result["content"][0]["type"] == "text"
        assert "TestTool" in result["content"][0]["text"]
        assert "failed without providing error details" in result["content"][0]["text"]

    def test_error_with_empty_string_gets_placeholder(self):
        """Error with empty string content should get placeholder."""
        tool_response = {
            "content": "",
            "is_error": True,
        }
        result = validate_tool_result(tool_response, "MyTool")
        assert result["is_error"] is True
        assert isinstance(result["content"], list)
        assert "MyTool" in result["content"][0]["text"]

    def test_error_with_whitespace_string_gets_placeholder(self):
        """Error with whitespace-only string should get placeholder."""
        tool_response = {
            "content": "   \n\t  ",
            "is_error": True,
        }
        result = validate_tool_result(tool_response, "TestTool")
        assert isinstance(result["content"], list)
        assert "TestTool" in result["content"][0]["text"]

    def test_error_with_empty_list_gets_placeholder(self):
        """Error with empty list content should get placeholder."""
        tool_response = {
            "content": [],
            "is_error": True,
        }
        result = validate_tool_result(tool_response, "TestTool")
        assert isinstance(result["content"], list)
        assert len(result["content"]) == 1
        assert result["content"][0]["type"] == "text"

    def test_error_with_list_of_empty_text_blocks_gets_placeholder(self):
        """Error with list of empty text blocks should get placeholder."""
        tool_response = {
            "content": [
                {"type": "text", "text": ""},
                {"type": "text", "text": "  "},
            ],
            "is_error": True,
        }
        result = validate_tool_result(tool_response, "TestTool")
        # Should have placeholder since all text is empty/whitespace
        assert len(result["content"]) == 1
        assert "failed without providing error details" in result["content"][0]["text"]

    def test_error_with_list_containing_valid_text_unchanged(self):
        """Error with at least one valid text block should be unchanged."""
        tool_response = {
            "content": [
                {"type": "text", "text": ""},
                {"type": "text", "text": "Actual error message"},
                {"type": "text", "text": "  "},
            ],
            "is_error": True,
        }
        result = validate_tool_result(tool_response, "TestTool")
        # Should be unchanged since there's valid content
        assert result == tool_response

    def test_error_with_string_in_list_unchanged(self):
        """Error with string items in list (legacy format) should work."""
        tool_response = {
            "content": ["Error message here"],
            "is_error": True,
        }
        result = validate_tool_result(tool_response, "TestTool")
        assert result == tool_response

    def test_error_with_empty_strings_in_list_gets_placeholder(self):
        """Error with only empty strings in list should get placeholder."""
        tool_response = {
            "content": ["", "  ", "\n"],
            "is_error": True,
        }
        result = validate_tool_result(tool_response, "TestTool")
        assert len(result["content"]) == 1
        assert "failed without providing error details" in result["content"][0]["text"]

    def test_default_tool_name(self):
        """Should use 'unknown' when tool name not provided."""
        tool_response = {
            "content": None,
            "is_error": True,
        }
        result = validate_tool_result(tool_response)  # No tool_name arg
        assert "unknown" in result["content"][0]["text"]

    def test_missing_content_key(self):
        """Should handle missing content key gracefully."""
        tool_response = {
            "is_error": True,
        }
        result = validate_tool_result(tool_response, "TestTool")
        assert isinstance(result["content"], list)
        assert "TestTool" in result["content"][0]["text"]

    def test_sdk_permission_denial_format(self):
        """Should fix the exact format from SDK permission denials."""
        # This is the exact format that causes the bug
        tool_response = {
            "content": "",
            "is_error": True,
        }
        result = validate_tool_result(tool_response, "Bash")
        assert result["is_error"] is True
        assert isinstance(result["content"], list)
        assert result["content"][0]["type"] == "text"
        assert len(result["content"][0]["text"]) > 0
        assert "Bash" in result["content"][0]["text"]
