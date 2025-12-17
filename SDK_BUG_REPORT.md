# SDK Bug: Empty Error Content on Permission Denial

## Summary

The `claude-agent-sdk` has a bug where permission denials (from timeout or user denial) result in tool results with empty content and `is_error=true`, which violates the Anthropic API requirement that error results must have non-empty content.

## Root Cause

**When a permission is denied:**
1. `_can_use_tool()` returns `PermissionResultDeny(message="Permission request timed out after 5 minutes")`
2. The SDK receives this denial and should create a `ToolResultBlock` with the deny message as content
3. **BUG**: Instead, SDK creates `ToolResultBlock(content='', is_error=True)` - completely ignoring the message
4. SDK sends this back to the API
5. API rejects it: `"messages.150.content.0.tool_result: content cannot be empty if is_error is true"`
6. Conversation is permanently corrupted

## Evidence

From `/home/chadac/.emacs.d/claude-agent/claude-agent.log`:

```
[2025-12-15T15:14:06.751935] EMIT: {"type": "session_message_text", "text": "Permission request timed out"}
[2025-12-15T15:14:06.765819] RECV: {"type": "UserMessage", "data": "UserMessage(content=[ToolResultBlock(tool_use_id='toolu_019jb3cSK8N2JEXCegnR1gYz', content='', is_error=True)]"}
[2025-12-15T15:14:07.261625] RECV: {"type": "AssistantMessage", "data": "AssistantMessage(content=[TextBlock(text='API Error: 400 {\"type\":\"error\",\"error\":{\"type\":\"invalid_request_error\",\"message\":\"messages.150.content.0.tool_result: content cannot be empty if `is_error` is true\""}
```

## Why the PostToolUse Hook Doesn't Fix It

The `_fix_empty_error_content` hook (agent.py:259-302) is registered as a `PostToolUse` hook, but:
- This hook only runs AFTER a tool executes
- When a permission is denied, the tool NEVER executes
- The SDK generates the error result internally without invoking any hooks

## Fixes Implemented

### 1. Centralized Validation Function (agent.py:35-87)
Created `validate_tool_result()` - a comprehensive validation function that ensures ALL tool results with `is_error=True` have non-empty content. This function:
- Handles all content formats (str, list, None)
- Checks for empty strings, empty lists, and lists with only empty text blocks
- Inserts a descriptive placeholder when content is missing
- Is the single source of truth for tool result validation

### 2. PostToolUse Hook (agent.py:314-343)
Uses `validate_tool_result()` to fix tool results from tools that actually execute. This catches most cases but NOT permission denials (since denied tools never execute).

### 3. UserMessage Handler Safety Net (agent.py:617-643)
Double validation in the message handler using `validate_tool_result()`. This catches SDK bugs that bypass the hook, including permission denials.

### 4. API Error Handler (agent.py:657-677)
Catches the specific API error and cleanly terminates the session with a helpful message, preventing further corruption.

**Defense-in-Depth Strategy:**
All tool results are now gated through `validate_tool_result()` at multiple points:
1. Hook layer (for normal execution)
2. Message layer (for display safety)
3. Error layer (for graceful degradation)

**Important Limitation:**
The UserMessage handler validation (#3) occurs AFTER the SDK has sent the message to the API, so it cannot prevent API errors from the SDK bug. It only ensures we display reasonable error messages to the user. The SDK permission denial bug can still cause session corruption - we just handle it more gracefully now.

## Required SDK Fix

The SDK needs to properly handle `PermissionResultDeny.message`:

**Current behavior:**
```python
# SDK receives PermissionResultDeny(message="Permission timed out")
# SDK creates: ToolResultBlock(content='', is_error=True)  # BUG!
```

**Expected behavior:**
```python
# SDK receives PermissionResultDeny(message="Permission timed out")
# SDK should create: ToolResultBlock(
#     content=[{"type": "text", "text": "Permission timed out"}],
#     is_error=True
# )
```

## Temporary Workaround for Users

If you hit this error:
1. The session is corrupted and cannot continue
2. Run `claude-agent-quit` or restart the buffer
3. Start a new session
4. Consider allowing tools at session or "always" scope to avoid frequent permission prompts

## Next Steps

- [ ] File issue with `claude-agent-sdk` team
- [ ] Add unit test for permission denial flow
- [ ] Consider adding PreToolUse hook as additional safeguard
- [ ] Update SDK dependency when fix is released
