# Interactive Prompts

Native Emacs interfaces for Claude to interact with you.

## Overview

Instead of text-based prompts in the terminal, Claude uses native Emacs UI components:

- Choice prompts with keyboard navigation
- Multi-select with checkboxes
- File and directory pickers
- Proposal buffers for review

## Choice Prompts

Claude can present numbered options for you to choose from.

### Appearance

```
┌─────────────────────────────────────┐
│ Which database should we use?       │
│                                     │
│ [1] PostgreSQL - Robust, full SQL   │
│ [2] SQLite - Simple, file-based     │
│ [3] MongoDB - Document store        │
│                                     │
│ [o] Other (custom input)            │
└─────────────────────────────────────┘
```

### Navigation

| Key | Action |
|-----|--------|
| `j` / `↓` | Move down |
| `k` / `↑` | Move up |
| `RET` | Select current |
| `1-9` | Direct select by number |
| `o` | Enter custom text (if enabled) |
| `q` | Cancel |

### MCP Tool

```python
mcp__emacs__prompt_choice(
    prompt="Which database should we use?",
    options=["PostgreSQL", "SQLite", "MongoDB"],
    include_other=True  # Allow custom input
)
# Returns: "PostgreSQL" or custom text or "cancelled"
```

## Multi-Select

For selecting multiple items from a list.

### Appearance

```
┌─────────────────────────────────────┐
│ Which features should be enabled?   │
│                                     │
│ [x] Authentication                  │
│ [ ] Rate limiting                   │
│ [x] Logging                         │
│ [ ] Caching                         │
│                                     │
│ Press SPC to toggle, RET to confirm │
└─────────────────────────────────────┘
```

### Navigation

| Key | Action |
|-----|--------|
| `j` / `↓` | Move down |
| `k` / `↑` | Move up |
| `SPC` / `x` | Toggle selection |
| `a` | Select all |
| `u` | Unselect all |
| `RET` | Confirm selection |
| `q` | Cancel |

### MCP Tool

```python
mcp__emacs__multiselect(
    prompt="Which features should be enabled?",
    options="Authentication\nRate limiting\nLogging\nCaching"
)
# Returns: ["Authentication", "Logging"] or "cancelled"
```

## Confirmation Dialogs

Simple yes/no prompts.

### Appearance

```
┌─────────────────────────────────────┐
│ Delete all test files?              │
│                                     │
│ [y] Yes    [n] No                   │
└─────────────────────────────────────┘
```

### Navigation

| Key | Action |
|-----|--------|
| `y` | Yes |
| `n` | No |
| `q` | Cancel |

### MCP Tool

```python
mcp__emacs__confirm(prompt="Delete all test files?")
# Returns: "yes", "no", or "cancelled"
```

## File Picker

Select files from your project.

### Appearance

```
┌─────────────────────────────────────┐
│ Select file:                        │
│                                     │
│   src/                              │
│     main.py                         │
│     utils.py                        │
│   tests/                            │
│     test_main.py                    │
│                                     │
└─────────────────────────────────────┘
```

### Navigation

Standard completion navigation, or j/k in popup.

### MCP Tool

```python
mcp__emacs__pick_file(
    prompt="Select file to edit",
    directory="/project/src"  # Optional starting dir
)
# Returns: "/project/src/main.py" or "cancelled"
```

## Directory Picker

Select directories from your project.

### MCP Tool

```python
mcp__emacs__pick_directory(
    prompt="Select output directory",
    directory="/project"
)
# Returns: "/project/dist" or "cancelled"
```

## Proposal Buffers

For reviewing and optionally editing Claude's suggestions before applying.

### Appearance

```
┌─────────────────────────────────────┐
│ Proposed: New function              │
├─────────────────────────────────────┤
│ def new_function(x, y):             │
│     """Add two numbers."""          │
│     return x + y                    │
│                                     │
├─────────────────────────────────────┤
│ C-c C-c: Accept  C-c C-k: Reject    │
└─────────────────────────────────────┘
```

### Actions

| Key | Action |
|-----|--------|
| Edit | Modify the proposal freely |
| `C-c C-c` | Accept (with any edits) |
| `C-c C-k` | Reject |

### MCP Tool

```python
mcp__emacs__show_proposal(
    title="New function",
    content="def new_function(x, y):\n    return x + y",
    mode="python-mode"  # Optional syntax highlighting
)
# Returns:
#   "ACCEPTED\n<content>" if accepted
#   "REJECTED" if rejected without changes
#   "REJECTED_WITH_CHANGES\n<diff>" if rejected after editing
```

## Progress Indicators

Visual feedback for long-running operations.

### Starting Progress

```python
progress_id = mcp__emacs__progress_start(
    message="Installing dependencies..."
)
```

### Updating Progress

```python
mcp__emacs__progress_update(
    id=progress_id,
    message="Installing: numpy",
    percent=50  # Optional percentage
)
```

### Completing Progress

```python
mcp__emacs__progress_stop(
    id=progress_id,
    final_message="Dependencies installed successfully"
)
```

### Appearance

```
Installing dependencies...
████████████░░░░░░░░░ 50%
```

## Usage Examples

### Guided Setup

```python
# Get user preferences
db = mcp__emacs__prompt_choice(
    prompt="Database type?",
    options=["PostgreSQL", "MySQL", "SQLite"]
)

features = mcp__emacs__multiselect(
    prompt="Features to enable:",
    options="Auth\nCaching\nLogging"
)

if mcp__emacs__confirm("Generate config file?") == "yes":
    # Generate with selected options
    pass
```

### File Operations

```python
# Let user pick target file
target = mcp__emacs__pick_file(
    prompt="Select file to refactor"
)

if target != "cancelled":
    # Read and process file
    content = mcp__emacs__read_file(file_path=target)
    # ... make changes ...

    # Show proposal
    result = mcp__emacs__show_proposal(
        title="Refactored code",
        content=new_content,
        mode="python-mode"
    )

    if result.startswith("ACCEPTED"):
        # Apply changes
        pass
```

## Tips

1. **Use appropriate prompts** - Choice for single select, multiselect for multiple
2. **Provide context** - Clear prompts help users decide
3. **Allow "Other"** - When options might not cover all cases
4. **Use proposals for changes** - Let users review before applying
5. **Show progress** - For operations taking > 1 second
