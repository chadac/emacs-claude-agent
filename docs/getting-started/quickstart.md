# Quick Start

Get up and running with emacs-claude-agent in 5 minutes.

## Starting Your First Session

1. Open any source file in Emacs
2. Press `C-c C-e` to open the transient menu
3. Press `s` to start a Claude session

You'll see Claude Code start in a terminal buffer, ready for interaction.

## Basic Commands

### The Transient Menu

Press `C-c C-e` from any buffer to access the command menu:

| Key | Command | Description |
|-----|---------|-------------|
| `s` | Start session | Start or switch to Claude session |
| `r` | Resume session | Start with resume flag |
| `t` | Toggle buffer | Show/hide Claude buffer |
| `k` | Kill session | End current session |

### Sending Context to Claude

| Key | Command | Description |
|-----|---------|-------------|
| `x` | Execute with context | Send request with file/line context |
| `X` | Execute plain | Send request without context |
| `f` | Add file | Add file reference (@file) |
| `F` | Add current file | Add current buffer's file |
| `a` | Add context | Add file:line reference |

### Quick Actions

| Key | Command | Description |
|-----|---------|-------------|
| `e` | Fix error | Send flycheck error to Claude |
| `i` | Implement comment | Extract comment and implement |
| `y` | Yes | Send approval (RET) |
| `n` | No | Send rejection (ESC) |

## Example Workflow

### 1. Fix a Bug

```
1. Navigate to code with an error
2. C-c C-e e        ; Fix error at point
3. Claude analyzes and suggests fix
4. C-c C-e y        ; Approve the fix
```

### 2. Implement from Comment

```elisp
;; TODO: Add validation for email format
(defun process-email (email)
  email)
```

```
1. Place cursor on the comment
2. C-c C-e i        ; Implement comment
3. Claude generates validation code
```

### 3. Ask About Code

```
1. Select a region of code
2. C-c C-e x        ; Execute with context
3. Type: "Explain what this code does"
4. Claude explains with full context
```

## Working with MCP Tools

When MCP is enabled (default), Claude has direct access to your buffers:

```
User: Read the current buffer and suggest improvements

Claude: I can see you have a function that... [reads directly]
        Here are my suggestions... [can edit directly]
```

Claude can:

- Read any buffer content
- Search for patterns
- Edit files directly
- Execute elisp commands

## Oneshot Agents

For quick, targeted edits without a full conversation:

| Key | Scope | Description |
|-----|-------|-------------|
| `C-c c c` | Line/region | Edit only selected region |
| `C-c c b` | Buffer | Edit current buffer |
| `C-c c d` | Directory | Edit files in current dir |
| `C-c c p` | Project | Edit any project file |

Example:

```
1. Select a function
2. C-c c c          ; Oneshot with line scope
3. Type: "Add docstring"
4. Agent adds docstring, then exits
```

## Tips for Effective Use

### Save Before Asking

Claude uses files on disk as source of truth. Always save your buffers before sending requests:

```elisp
;; Tip: Enable auto-save
(auto-save-visited-mode t)
```

### Use Specific Context

The more context you provide, the better:

```
Bad:  "Fix this"
Good: "Fix the null pointer exception in the validate function"
```

### Approve Changes Carefully

When Claude proposes changes, review them before approving. Use `n` to reject if needed.

## Next Steps

- [Configuration](configuration.md) - Customize keybindings and behavior
- [MCP Features](../features/mcp.md) - Learn about buffer operations
- [Multi-Agent System](../features/multi-agent.md) - Run parallel agents
