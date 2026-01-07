# Configuration Reference

Complete reference for all configuration options.

## Core Variables

### claude-program

Path to Claude Code executable.

| Property | Value |
|----------|-------|
| Type | string |
| Default | `"claude"` |
| Safe local | No |

```elisp
(setq claude-program "/usr/local/bin/claude")
```

---

### claude-program-switches

Command line arguments for Claude.

| Property | Value |
|----------|-------|
| Type | list of strings |
| Default | `nil` |
| Safe local | No |

```elisp
(setq claude-program-switches '("--verbose" "--add-dir" "../libs"))
```

---

### claude-use-mcp

Enable MCP integration.

| Property | Value |
|----------|-------|
| Type | boolean |
| Default | `t` |
| Safe local | Yes |

```elisp
(setq claude-use-mcp t)
```

---

### claude-auto-allow-cli-reads

Auto-approve read-only MCP operations.

| Property | Value |
|----------|-------|
| Type | boolean |
| Default | `t` |
| Safe local | Yes |

```elisp
(setq claude-auto-allow-cli-reads t)
```

---

### claude-prefer-projectile-root

Use projectile root instead of git root.

| Property | Value |
|----------|-------|
| Type | boolean |
| Default | `nil` |
| Safe local | Yes |

```elisp
(setq claude-prefer-projectile-root t)
```

---

### claude-additional-tools-files

Extra tool definition files.

| Property | Value |
|----------|-------|
| Type | list of file paths |
| Default | `nil` |
| Safe local | Yes |

```elisp
(setq claude-additional-tools-files
      '("~/tools/common.yaml" "~/tools/python.yaml"))
```

## Buffer Behavior

### claude-switch-to-buffer-on-create

Switch to Claude buffer when creating session.

| Property | Value |
|----------|-------|
| Type | boolean |
| Default | `t` |
| Safe local | Yes |

---

### claude-switch-to-buffer-on-toggle

Switch to Claude buffer when toggling.

| Property | Value |
|----------|-------|
| Type | boolean |
| Default | `t` |
| Safe local | Yes |

---

### claude-switch-to-buffer-on-file-add

Switch to buffer when adding file reference.

| Property | Value |
|----------|-------|
| Type | boolean |
| Default | `nil` |
| Safe local | Yes |

---

### claude-switch-to-buffer-on-send-error

Switch when sending error fix.

| Property | Value |
|----------|-------|
| Type | boolean |
| Default | `nil` |
| Safe local | Yes |

---

### claude-switch-to-buffer-on-add-context

Switch when adding context.

| Property | Value |
|----------|-------|
| Type | boolean |
| Default | `t` |
| Safe local | Yes |

## Input Behavior

### claude-m-return-is-submit

Swap RET and M-RET behavior.

| Property | Value |
|----------|-------|
| Type | boolean |
| Default | `nil` |
| Safe local | Yes |

When `t`:
- RET creates newline
- M-RET submits

---

### claude-shift-return-newline

Enable Shift-Return for newlines.

| Property | Value |
|----------|-------|
| Type | boolean |
| Default | `t` |
| Safe local | Yes |

---

### claude-use-shell-env

Load shell environment for Claude.

| Property | Value |
|----------|-------|
| Type | boolean |
| Default | `nil` |
| Safe local | No |

When `t`, invokes Claude through your shell to get PATH and environment.

## Notifications

### claude-notify-on-await

Show notifications when Claude awaits input.

| Property | Value |
|----------|-------|
| Type | boolean |
| Default | `t` |
| Safe local | Yes |

---

### claude-notification-sound-mac

macOS notification sound.

| Property | Value |
|----------|-------|
| Type | string |
| Default | `"Submarine"` |
| Safe local | Yes |

Available sounds: Basso, Blow, Bottle, Frog, Funk, Glass, Hero, Morse, Ping, Pop, Purr, Sosumi, Submarine, Tink

---

### claude-notification-auto-dismiss-linux

Auto-dismiss Linux notifications.

| Property | Value |
|----------|-------|
| Type | boolean |
| Default | `t` |
| Safe local | Yes |

---

### claude-notification-sound-linux

Linux notification sound ID.

| Property | Value |
|----------|-------|
| Type | string |
| Default | `"bell"` |
| Safe local | Yes |

Common IDs: message-new-instant, bell, dialog-error, dialog-warning

## Agent Configuration

### claude-agent-python-command

Python command for agent wrapper.

| Property | Value |
|----------|-------|
| Type | string |
| Default | `"uv"` |
| Safe local | No |

---

### claude-agent-enable-mcp

Enable MCP for agent sessions.

| Property | Value |
|----------|-------|
| Type | boolean |
| Default | `t` |
| Safe local | Yes |

---

### claude-agent-disallowed-tools

Tools to disable for agents.

| Property | Value |
|----------|-------|
| Type | list of strings |
| Default | `nil` |
| Safe local | Yes |

```elisp
(setq claude-agent-disallowed-tools '("WebSearch"))
```

## Oneshot Configuration

### claude-oneshot-model

Model for oneshot agents.

| Property | Value |
|----------|-------|
| Type | choice: haiku, sonnet, opus |
| Default | `"sonnet"` |
| Safe local | Yes |

---

### claude-oneshot-timeout

Timeout in seconds.

| Property | Value |
|----------|-------|
| Type | integer |
| Default | `300` |
| Safe local | Yes |

---

### claude-oneshot-debug

Keep buffers after completion.

| Property | Value |
|----------|-------|
| Type | boolean |
| Default | `nil` |
| Safe local | Yes |

## Faces

### claude-agent-header-face

Header section face.

Default: Cyan italic

---

### claude-agent-user-header-face

User message header face.

Default: Blue bold

---

### claude-agent-user-face

User message text face.

Default: Off-white

---

### claude-agent-assistant-header-face

Assistant message header face.

Default: Purple bold

---

### claude-agent-assistant-face

Assistant message text face.

Default: Near-white

---

### claude-agent-tool-face

Tool call indicator face.

Default: Yellow italic

---

### claude-oneshot-target-face

Oneshot target region face.

Default: Yellow tinted background

---

### claude-oneshot-header-face

Oneshot indicator header face.

Default: Yellow on dark background, bold

## Hooks

### claude-startup-hook

Run after session initialization.

```elisp
(add-hook 'claude-startup-hook
          (lambda ()
            (message "Session started in %s" claude--cwd)))
```

## Using customize

All options available via:

```
M-x customize-group RET claude-agent RET
```

## .dir-locals.el Example

```elisp
((nil . ((claude-program-switches . ("--add-dir" "../common"))
         (claude-additional-tools-files . (".claude/tools.yaml"))
         (claude-prefer-projectile-root . t)
         (claude-oneshot-model . "haiku"))))
```

## Environment Variables

| Variable | Description |
|----------|-------------|
| `ANTHROPIC_API_KEY` | API key (used by Claude Code) |
| `CLAUDE_HOME` | Claude configuration directory |

## File Locations

| File | Purpose |
|------|---------|
| `.claude/claudemacs-tools.yaml` | Project MCP tools |
| `.claude/claudemacs-notes.org` | Project notes |
| `.claude/kb/` | Knowledge base entries |
| `.dir-locals.el` | Per-project Emacs settings |
