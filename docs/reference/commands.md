# Commands Reference

Complete reference for all emacs-claude-agent commands.

## Transient Menu Commands

Access via `C-c C-e` (or your configured keybinding).

### Session Management

| Key | Command | Description |
|-----|---------|-------------|
| `s` | `claude-start` | Start or switch to Claude session |
| `r` | `claude-start-resume` | Start with resume flag |
| `t` | `claude-toggle` | Toggle Claude buffer visibility |
| `k` | `claude-kill` | Kill active Claude session |

### Context Actions

| Key | Command | Description |
|-----|---------|-------------|
| `x` | `claude-execute-with-context` | Send request with file/line context |
| `X` | `claude-execute-plain` | Send request without context |
| `f` | `claude-add-file` | Add file reference (@file) |
| `F` | `claude-add-current-file` | Add current file reference |
| `a` | `claude-add-context` | Add file:line reference |

### Quick Actions

| Key | Command | Description |
|-----|---------|-------------|
| `e` | `claude-fix-error` | Fix flycheck error at point |
| `i` | `claude-implement-comment` | Implement comment at point |
| `y` | `claude-yes` | Send "Yes" (RET) |
| `n` | `claude-no` | Send "No" (ESC) |

### Maintenance

| Key | Command | Description |
|-----|---------|-------------|
| `u` | `claude-unstick` | Reset buffer tracking |

## Oneshot Agent Commands

### Default Keybindings

| Key | Command | Scope |
|-----|---------|-------|
| `C-c c c` | `claude-oneshot-line` | Line/region only |
| `C-c c b` | `claude-oneshot-buffer` | Current buffer |
| `C-c c d` | `claude-oneshot-directory` | Current directory |
| `C-c c p` | `claude-oneshot-project` | Entire project |

## Multi-Agent Commands

### From Transient Menu

| Key | Command | Description |
|-----|---------|-------------|
| `S` | `claude-spawn-agent` | Spawn agent in directory |

### Programmatic

```elisp
;; Spawn agent programmatically
(claude-spawn-agent "/path/to/directory" "agent-name")

;; List all agents
(claude-list-agents)

;; Message another agent
(claude-message-agent buffer-name message)
```

## M-x Commands

Commands available via `M-x`:

| Command | Description |
|---------|-------------|
| `claude-transient-menu` | Open main transient menu |
| `claude-start` | Start Claude session |
| `claude-start-resume` | Start with resume |
| `claude-toggle` | Toggle buffer visibility |
| `claude-kill` | Kill session |
| `claude-setup-bell-handler` | Re-setup notifications |
| `claude-oneshot-line` | Oneshot with line scope |
| `claude-oneshot-buffer` | Oneshot with buffer scope |
| `claude-oneshot-directory` | Oneshot with directory scope |
| `claude-oneshot-project` | Oneshot with project scope |
| `claude-spawn-agent` | Spawn new agent |

## Buffer Commands

In Claude agent buffers:

### Navigation

| Key | Action |
|-----|--------|
| `C-c C-e` | Enter Emacs mode (for selection) |
| `C-c C-j` | Return to semi-char mode |
| `C-v` | Paste image from clipboard |

### eat-mode Specific

| Key | Action |
|-----|--------|
| `C-c C-e` | Toggle between modes |
| Standard Emacs | Available in Emacs mode |

## Interactive Prompt Keys

### Choice Prompts

| Key | Action |
|-----|--------|
| `j` / `↓` | Move down |
| `k` / `↑` | Move up |
| `RET` | Select current |
| `1-9` | Direct select |
| `o` | Custom input |
| `q` | Cancel |

### Multi-Select

| Key | Action |
|-----|--------|
| `j` / `↓` | Move down |
| `k` / `↑` | Move up |
| `SPC` / `x` | Toggle selection |
| `a` | Select all |
| `u` | Unselect all |
| `RET` | Confirm |
| `q` | Cancel |

### Confirmation

| Key | Action |
|-----|--------|
| `y` | Yes |
| `n` | No |
| `q` | Cancel |

### Proposal Buffers

| Key | Action |
|-----|--------|
| Edit | Modify freely |
| `C-c C-c` | Accept |
| `C-c C-k` | Reject |

## Customizing Keybindings

### Main Menu Keybinding

```elisp
;; Per-mode (recommended)
(define-key prog-mode-map (kbd "C-c C-e") #'claude-transient-menu)
(define-key text-mode-map (kbd "C-c C-e") #'claude-transient-menu)

;; Global
(global-set-key (kbd "C-c C-e") #'claude-transient-menu)
```

### Oneshot Keybindings

```elisp
;; Customize oneshot prefix
(define-key global-map (kbd "C-c a c") #'claude-oneshot-line)
(define-key global-map (kbd "C-c a b") #'claude-oneshot-buffer)
(define-key global-map (kbd "C-c a d") #'claude-oneshot-directory)
(define-key global-map (kbd "C-c a p") #'claude-oneshot-project)
```

### Input Behavior

```elisp
;; Swap RET and M-RET
(setq claude-m-return-is-submit t)

;; Enable Shift-Return for newlines
(setq claude-shift-return-newline t)
```

## Command Functions

### Starting Sessions

```elisp
;; Start session in current project
(claude-start)

;; Start with resume
(claude-start-resume)

;; Start in specific directory
(claude-start-in-directory "/path/to/project")
```

### Sending Input

```elisp
;; Send text to Claude
(claude-send-text "Your message here")

;; Send with context
(claude-execute-with-context "Fix this bug")

;; Send file reference
(claude-add-file "/path/to/file.py")
```

### Session Management

```elisp
;; Get current session buffer
(claude-get-buffer)

;; Check if session is active
(claude-session-active-p)

;; Kill session
(claude-kill)
```

## Hooks

```elisp
;; Run after session starts
(add-hook 'claude-startup-hook
          (lambda ()
            (message "Claude session started in %s" claude--cwd)))

;; Custom initialization
(add-hook 'claude-startup-hook #'my-claude-setup)
```
