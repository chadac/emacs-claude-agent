# Oneshot Agents

Lightweight background agents for quick, targeted edits without disrupting your workflow.

## Overview

Oneshot agents are designed for:

- Quick, focused edits
- Permission-scoped operations
- Background execution
- Automatic cleanup

Unlike full Claude sessions, oneshot agents:

- Don't take over your terminal
- Run in the background
- Auto-terminate when done
- Can only edit what you specify

## Permission Scopes

Oneshot agents have four permission levels:

| Key | Scope | What Can Be Edited |
|-----|-------|-------------------|
| `C-c c c` | Line/Region | Only the selected line or region |
| `C-c c b` | Buffer | Current buffer only |
| `C-c c d` | Directory | Files in current directory |
| `C-c c p` | Project | Any file in project |

### Scope Hierarchy

```
Project (least constrained)
    └── Directory
        └── Buffer
            └── Line/Region (most constrained)
```

!!! tip "Start Small"
    Always use the smallest scope that accomplishes your task. This prevents unintended changes.

## Usage

### Line/Region Scope

Select a line or region, then:

```
C-c c c
```

Prompt: "Add type hints to this function"

The agent can only modify your selection.

### Buffer Scope

With cursor in any buffer:

```
C-c c b
```

Prompt: "Add docstrings to all functions"

The agent can edit the entire buffer but nothing else.

### Directory Scope

```
C-c c d
```

Prompt: "Update all import statements to use absolute paths"

The agent can edit any file in the current directory (non-recursive).

### Project Scope

```
C-c c p
```

Prompt: "Rename the User class to UserAccount across the project"

The agent can edit any file in the project.

## Visual Feedback

### Target Highlighting

When you start a oneshot agent, the target region is highlighted:

- Yellow background for the target area
- Border lines marking the scope
- Header showing agent status

### Fringe Indicator

A visual marker appears in the fringe to indicate:

- Agent is running
- Target region boundaries

### Status Updates

The header line shows:

- Agent name/ID
- Current status (working, waiting, done)
- Elapsed time

## Agent Lifecycle

```
1. User selects scope (C-c c X)
2. User enters prompt
3. Agent spawns in background
4. Target region highlighted
5. Agent works on task
6. Agent signals completion
7. Highlighting removed
8. Agent buffer cleaned up
```

## Configuration

### Model Selection

```elisp
;; Choose model for oneshot agents
;; "haiku" = fast/cheap, "sonnet" = balanced, "opus" = powerful
(setq claude-oneshot-model "sonnet")
```

### Timeout

```elisp
;; Maximum time before auto-kill (seconds)
(setq claude-oneshot-timeout 300)  ; 5 minutes
```

### Debug Mode

```elisp
;; Keep agent buffer after completion for debugging
(setq claude-oneshot-debug t)
```

When debug mode is enabled, completed agent buffers are renamed with a `-done` suffix instead of being killed.

## The Done Tool

Oneshot agents use a special tool to signal completion:

```python
mcp__emacs__done(message="Added type hints to 3 functions")
```

This:

1. Notifies the user of completion
2. Removes target highlighting
3. Triggers agent cleanup

## Updating Target

Agents can adjust their highlighted region:

```python
# Expand or narrow the target region
mcp__emacs__update_target(
    file_path="/path/to/file.py",
    start_line=10,
    end_line=50
)
```

## Example Workflows

### Quick Docstring Addition

```
1. Select a function
2. C-c c c
3. "Add a comprehensive docstring"
4. Agent adds docstring
5. Done notification appears
```

### Buffer-Wide Formatting

```
1. Open a messy file
2. C-c c b
3. "Clean up formatting and add missing type hints"
4. Agent reformats entire file
5. Review changes
```

### Cross-File Rename

```
1. In any project file
2. C-c c p
3. "Rename MyClass to BetterClassName everywhere"
4. Agent finds and updates all references
5. Verify changes across files
```

## Tips

1. **Be specific** - Clear prompts get better results
2. **Start narrow** - Use line scope before buffer scope
3. **Check the diff** - Review changes before accepting
4. **Use for repetitive tasks** - Oneshot excels at bulk edits
5. **Enable debug for troubleshooting** - See what the agent did

## Comparison with Full Sessions

| Aspect | Oneshot Agent | Full Session |
|--------|--------------|--------------|
| Interaction | Single prompt | Conversation |
| Terminal | Background | Foreground |
| Scope | Permission-limited | Project-wide |
| Duration | Auto-terminates | Manual close |
| Best for | Quick edits | Complex tasks |

## Limitations

- Cannot ask clarifying questions
- Limited to specified scope
- No conversation history
- Single-shot execution
