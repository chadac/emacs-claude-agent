# MCP Integration

The Model Context Protocol (MCP) integration gives Claude direct access to Emacs operations through native tools.

## Overview

MCP replaces the need for Claude to use bash commands or copy-paste for Emacs interactions. Instead, Claude calls tools that execute directly in your Emacs instance.

## Buffer Operations

### Reading Buffers

Claude can read any buffer content:

```python
# Read entire buffer
mcp__emacs__get_buffer_content(buffer_name="main.py")

# Read specific lines
mcp__emacs__get_buffer_content(
    buffer_name="main.py",
    start_line=10,
    end_line=50
)

# Read last N lines
mcp__emacs__get_buffer_content(
    buffer_name="*compilation*",
    tail_lines=20
)
```

### Listing Buffers

```python
# Get all open buffers
mcp__emacs__list_buffers()
```

### Buffer Information

```python
# Get metadata about a buffer
mcp__emacs__buffer_info(buffer_name="main.py")
# Returns: file path, major mode, size, cursor position, etc.
```

### Searching Buffers

```python
# Search with regex
mcp__emacs__search_buffer(
    buffer_name="main.py",
    pattern="def\\s+\\w+",
    context_before=2,
    context_after=5
)
```

### Editing Buffers

```python
# Replace text
mcp__emacs__edit_buffer(
    buffer_name="main.py",
    old_string="def old_name(",
    new_string="def new_name("
)

# Replace all occurrences
mcp__emacs__edit_buffer(
    buffer_name="main.py",
    old_string="foo",
    new_string="bar",
    replace_all=True
)
```

## File Operations

### Reading Files

```python
# Read file with diagnostics
mcp__emacs__read_file(file_path="/path/to/file.py")

# Read with offset
mcp__emacs__read_file(
    file_path="/path/to/large/file.py",
    offset=100,
    limit=50
)
```

### Editing Files

```python
# Edit file directly
mcp__emacs__edit_file(
    file_path="/path/to/file.py",
    old_string="old code",
    new_string="new code"
)
```

## Elisp Execution

Claude can execute any elisp expression:

```python
# Simple expression
mcp__emacs__eval(expression="(buffer-name)")

# Complex operations
mcp__emacs__eval(expression="""
  (progn
    (find-file "/path/to/file.el")
    (goto-char (point-min))
    (search-forward "defun")
    (point))
""")
```

## Context-Aware Execution

MCP tools automatically detect the right context from arguments:

| Argument Pattern | Context Type |
|-----------------|--------------|
| `file_path`, `file` | Opens file, runs in its buffer |
| `buffer_name`, `buffer` | Runs in specified buffer |
| `directory`, `dir` | Sets `default-directory` |

### Forcing Context

```python
# Force file context
mcp__emacs__eval(
    expression="major-mode",
    __file="/path/to/file.py"
)

# Force directory context
mcp__emacs__eval(
    expression="(compile \"make\")",
    __dir="/project/root"
)
```

## Watch Functions

Non-blocking async operations for monitoring changes:

### Wait for Stability

```python
# Wait until buffer stops changing
mcp__emacs__watch_buffer(
    buffer_name="*compilation*",
    stable_time=0.5,
    timeout=30
)
```

### Wait for Pattern

```python
# Wait for specific text to appear
mcp__emacs__watch_for_pattern(
    buffer_name="*shell*",
    pattern="\\$\\s*$",  # Shell prompt
    timeout=60
)
```

### Wait for Any Change

```python
# Detect any modification
mcp__emacs__watch_for_change(
    buffer_name="file.py",
    timeout=10
)
```

## Available Tools

| Tool | Description | Safe |
|------|-------------|------|
| `get_buffer_content` | Read buffer contents | Yes |
| `list_buffers` | List all buffers | Yes |
| `buffer_info` | Get buffer metadata | Yes |
| `search_buffer` | Regex search in buffer | Yes |
| `get_region` | Get selected region | Yes |
| `edit_buffer` | Modify buffer text | No |
| `write_buffer` | Create/overwrite buffer | No |
| `clear_buffer` | Clear terminal buffer | No |
| `read_file` | Read file with diagnostics | Yes |
| `edit_file` | Modify file | No |
| `eval` | Execute elisp | No |
| `watch_buffer` | Wait for stability | Yes |
| `watch_for_pattern` | Wait for pattern | Yes |
| `watch_for_change` | Wait for change | Yes |

!!! note "Safe Tools"
    Tools marked "Safe" are read-only and can be auto-approved when `claude-auto-allow-cli-reads` is enabled.

## Configuration

```elisp
;; Enable MCP (default: t)
(setq claude-use-mcp t)

;; Auto-approve safe tools (default: t)
(setq claude-auto-allow-cli-reads t)
```

## Custom Tools

Create project-specific MCP tools in `.claude/claudemacs-tools.yaml`:

```yaml
tools:
  run_tests:
    description: "Run project test suite"
    safe: false
    context: dir
    elisp: [compile, "npm test"]
    args:
      directory:
        type: string
        required: true
```

See [Custom Tools Guide](../guides/custom-tools.md) for more details.
