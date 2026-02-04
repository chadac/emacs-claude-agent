# Commands Reference

Complete reference for all emacs-claude-agent commands.

## Main Transient Menu (`claude-transient-menu`)

Access via `C-c C-e` (or your configured keybinding). This is the top-level menu.

### Session Management

| Key | Command | Description |
|-----|---------|-------------|
| `s` | `claude-run` | Start or switch to Claude session |
| `r` | `claude-resume` | Start with resume flag |
| `R` | `claude-restart` | Restart current session |
| `k` | `claude-kill` | Kill active Claude session |
| `t` | `claude-toggle-buffer` | Toggle Claude buffer visibility |
| `l` | `claude-list-sessions` | List all sessions |

### Actions

| Key | Command | Description |
|-----|---------|-------------|
| `e` | `claude-fix-error-at-point` | Fix flycheck/flymake error at point |
| `x` | `claude-execute-request` | Send request with file/line context |
| `X` | `claude-ask-without-context` | Send request without context |
| `i` | `claude-implement-comment` | Implement CLAUDE: comment at point |
| `c` | `claude-generate-commit-message` | Generate a commit message |
| `f` | `claude-add-file-reference` | Add file reference (@file) |
| `F` | `claude-add-current-file-reference` | Add current file reference |
| `a` | `claude-add-context` | Add file:line context |
| `p` | `claude-paste-context-to-shell` | Paste context to shell |

### Quick Responses

| Key | Command | Description |
|-----|---------|-------------|
| `y` | `claude-send-yes` | Send "Yes" |
| `n` | `claude-send-no` | Send "No" |

## Pair Programming Menu (`claude-pair-menu`)

Accessed via `claude-menu` from non-agent buffers (or bound to your preferred key). This menu provides oneshot agents and pair programming features.

### Oneshot Agents

| Key | Command | Scope |
|-----|---------|-------|
| `c` | `claude-transient-oneshot-line-or-region` | Line or active region |
| `b` | `claude-transient-oneshot-buffer` | Current buffer |
| `d` | `claude-transient-oneshot-directory` | Current directory |
| `p` | `claude-transient-oneshot-project` | Entire project |
| `o` | `claude-transient-oneshot-list` | List active oneshots |
| `y` | `claude-transient-dismiss-tooltips` | Dismiss oneshot tips |

### Sessions

| Key | Command | Description |
|-----|---------|-------------|
| `s` | `claude-transient-start-session` | Start new session |
| `r` | `claude-transient-resume-session` | Resume previous session |
| `w` | `claude-transient-switch-session` | Switch between sessions |
| `l` | `claude-list-sessions` | List all sessions |

### Actions

| Key | Command | Description |
|-----|---------|-------------|
| `x` | `claude-pair-point-action` | Action at point |
| `t` | `claude-pair-point-action-test` | Generate test for point |
| `D` | `claude-pair-point-action-doc` | Document at point |
| `f` | `claude-pair-point-action-fix` | Fix at point |

### Comments

| Key | Command | Description |
|-----|---------|-------------|
| `C` | `claude-pair-send-comments` | Send CLAUDE: comments |
| `P` | (project-wide) | Send all project CLAUDE: comments |

## Agent Buffer Menu (`claude-agent-menu`)

Accessed via `claude-menu` when inside a Claude agent buffer.

### Model & Cost

| Key | Command | Description |
|-----|---------|-------------|
| `m` | `claude-agent-set-model` | Change model |
| `$` | `claude-agent-show-cost` | Show cost/tokens |

### MCP Servers (M prefix)

| Key | Command | Description |
|-----|---------|-------------|
| `M l` | `claude-agent-mcp-list` | List MCP servers |
| `M s` | `claude-agent-show-mcp-status` | Show MCP status |
| `M a` | `claude-agent-mcp-add` | Add MCP server |
| `M r` | `claude-agent-mcp-remove` | Remove MCP server |

### Session

| Key | Command | Description |
|-----|---------|-------------|
| `c` | `claude-agent-compact` | Compact history |
| `C` | `claude-agent-clear` | Clear history |
| `q` | `claude-agent-quit` | Quit session |
| `k` | `claude-agent-interrupt` | Interrupt current operation |

### View

| Key | Command | Description |
|-----|---------|-------------|
| `p` | `claude-agent-toggle-progress` | Toggle progress visibility |
| `t` | `claude-agent-toggle-todos` | Toggle todos visibility |
| `w` | `claude-mcp-watch-mode-toggle` | Toggle watch mode |

### Navigation

| Key | Command | Description |
|-----|---------|-------------|
| `i` / `RET` | `claude-agent-goto-input` | Jump to input area |

### Git

| Key | Command | Description |
|-----|---------|-------------|
| `g` | `claude-mcp-magit-commit-approve` | Approve proposed commit |

## Unified Dispatcher

`claude-menu` automatically selects the right menu based on context:

- In a Claude agent buffer → shows `claude-agent-menu`
- In any other buffer → shows `claude-pair-menu`

## Buffer-Local Keybindings

In Claude session buffers (`claude-mode`):

| Key | Command | Description |
|-----|---------|-------------|
| `C-c t` | `claude-clear-buffer` | Clear/trim buffer |
| `C-c s` | `claude-spawn-agent` | Spawn new agent |

## M-x Commands

All interactive commands available via `M-x`:

| Command | Description |
|---------|-------------|
| `claude-transient-menu` | Open main transient menu |
| `claude-menu` | Context-aware transient menu |
| `claude-run` | Start Claude session |
| `claude-resume` | Resume previous session |
| `claude-restart` | Restart session |
| `claude-kill` | Kill session |
| `claude-toggle-buffer` | Toggle buffer visibility |
| `claude-list-sessions` | List all sessions |
| `claude-execute-request` | Send request with context |
| `claude-ask-without-context` | Send request without context |
| `claude-fix-error-at-point` | Fix error at point |
| `claude-implement-comment` | Implement CLAUDE: comment |
| `claude-add-file-reference` | Add file reference |
| `claude-add-current-file-reference` | Add current file |
| `claude-add-context` | Add file:line context |
| `claude-send-yes` | Send "Yes" |
| `claude-send-no` | Send "No" |
| `claude-oneshot-line-or-region` | Oneshot: line/region scope |
| `claude-oneshot-buffer` | Oneshot: buffer scope |
| `claude-oneshot-directory` | Oneshot: directory scope |
| `claude-oneshot-project` | Oneshot: project scope |
| `claude-spawn-agent` | Spawn new agent |

## Interactive Prompt Keys

### Choice Prompts

| Key | Action |
|-----|--------|
| `j` / `↓` | Move down |
| `k` / `↑` | Move up |
| `RET` | Select current |
| `1-9` | Direct select |
| `o` | Custom input (if enabled) |
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
| Free editing | Modify the proposal freely |
| `C-c C-c` | Accept proposal |
| `C-c C-k` | Reject proposal |

## Customizing Keybindings

### Main Menu

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
(define-key global-map (kbd "C-c a c") #'claude-oneshot-line-or-region)
(define-key global-map (kbd "C-c a b") #'claude-oneshot-buffer)
(define-key global-map (kbd "C-c a d") #'claude-oneshot-directory)
(define-key global-map (kbd "C-c a p") #'claude-oneshot-project)
```

### Input Behavior

```elisp
;; Swap RET and M-RET (default: nil)
;; When t: RET creates newline, M-RET submits
(setq claude-m-return-is-submit t)

;; Enable Shift-Return for newlines (default: t)
(setq claude-shift-return-newline t)
```

## Extending the Transient Menus

You can add custom items to the agent and pair menus:

```elisp
;; Add to agent menu
(claude-transient-register-agent-item
 "z" "My Command" #'my-claude-command)

;; Add to pair menu
(claude-transient-register-pair-item
 "z" "My Pair Action" #'my-pair-action)
```

## Hooks

```elisp
;; Run after session starts
(add-hook 'claude-startup-hook
          (lambda ()
            (message "Claude session started in %s" claude--cwd)))
```
