# Configuration

Customize emacs-claude-agent to match your workflow.

## Core Settings

### Claude Executable

```elisp
;; Custom Claude Code executable path (default: "claude")
(setq claude-program "/usr/local/bin/claude")

;; Command line switches
(setq claude-program-switches '("--verbose"))

;; Skip permission prompts (use with caution!)
(setq claude-program-switches '("--dangerously-skip-permissions"))
```

### MCP Integration

```elisp
;; Enable MCP for buffer operations (default: t)
(setq claude-use-mcp t)

;; Auto-allow read-only operations (default: t)
(setq claude-auto-allow-cli-reads t)
```

### Project Root Detection

```elisp
;; Use projectile root instead of git root (default: nil)
(setq claude-prefer-projectile-root t)
```

This is useful for monorepos where you want Claude to access files across multiple git repos.

## Window Behavior

### Buffer Display

```elisp
;; Display Claude in a side window
(add-to-list 'display-buffer-alist
             '("^\\*claude"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.4)))

;; Or at bottom
(add-to-list 'display-buffer-alist
             '("^\\*claude"
               (display-buffer-in-side-window)
               (side . bottom)
               (window-height . 0.3)))
```

### Buffer Switching

```elisp
;; Switch to buffer when creating session (default: t)
(setq claude-switch-to-buffer-on-create t)

;; Switch when toggling visibility (default: t)
(setq claude-switch-to-buffer-on-toggle t)

;; Switch when adding file references (default: nil)
(setq claude-switch-to-buffer-on-file-add nil)

;; Switch when sending error fixes (default: nil)
(setq claude-switch-to-buffer-on-send-error nil)

;; Switch when adding context (default: t)
(setq claude-switch-to-buffer-on-add-context t)
```

## Keybindings

### Main Menu

```elisp
;; Bind to specific modes (recommended)
(define-key prog-mode-map (kbd "C-c C-e") #'claude-transient-menu)
(define-key text-mode-map (kbd "C-c C-e") #'claude-transient-menu)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") #'claude-transient-menu)

;; Or globally
(global-set-key (kbd "C-c C-e") #'claude-transient-menu)
```

### Input Behavior

```elisp
;; Swap RET and M-RET behavior (default: nil)
;; When t: RET creates newline, M-RET submits
(setq claude-m-return-is-submit t)

;; Enable Shift-Return for newlines (default: t)
(setq claude-shift-return-newline t)
```

## System Notifications

### General Settings

```elisp
;; Enable notifications when Claude awaits input (default: t)
(setq claude-notify-on-await t)
```

### macOS

First, set up Claude Code for terminal bell notifications:

```bash
claude config set --global preferredNotifChannel terminal_bell
```

Then configure the sound:

```elisp
;; Available: Basso, Blow, Bottle, Frog, Funk, Glass, Hero,
;;            Morse, Ping, Pop, Purr, Sosumi, Submarine, Tink
(setq claude-notification-sound-mac "Submarine")
```

### Linux

```elisp
;; Auto-dismiss notifications (default: t)
(setq claude-notification-auto-dismiss-linux t)

;; Notification sound (requires canberra-gtk-play)
;; Common IDs: "message-new-instant", "bell", "dialog-error"
(setq claude-notification-sound-linux "message-new-instant")

;; Disable sound
(setq claude-notification-sound-linux "")
```

## Shell Environment

```elisp
;; Load shell environment (.zshrc, .bashrc) for PATH (default: nil)
;; Useful if Claude can't find commands in your shell PATH
(setq claude-use-shell-env t)
```

!!! note
    Changes only apply to new sessions. Kill and restart to take effect.

## Oneshot Agents

```elisp
;; Default model for oneshot agents
;; Options: "haiku" (fast/cheap), "sonnet", "opus"
(setq claude-oneshot-model "sonnet")

;; Timeout in seconds (default: 300 = 5 minutes)
(setq claude-oneshot-timeout 300)

;; Keep buffers after completion for debugging (default: nil)
(setq claude-oneshot-debug t)
```

## Terminal Settings

```elisp
;; Increase scrollback for history searching
(with-eval-after-load 'eat
  (setq eat-term-scrollback-size 400000))
```

## Startup Hook

Run code when a Claude session initializes:

```elisp
(add-hook 'claude-startup-hook
          (lambda ()
            ;; Example: Auto-detect project type
            (when (file-exists-p (expand-file-name "package.json" claude--cwd))
              (message "Node.js project detected"))

            ;; Example: Set project-specific switches
            (when (string-match "my-special-project" claude--cwd)
              (setq-local claude-program-switches '("--verbose")))))
```

## Project-Specific Configuration

Use `.dir-locals.el` for per-project settings:

```elisp
;; In your project's .dir-locals.el
((nil . ((claude-program-switches . ("--add-dir" "../libs"))
         (claude-additional-tools-files . ("tools/custom-tools.yaml")))))
```

## Customization Interface

All variables can be customized via Emacs's customize interface:

```
M-x customize-group RET claude-agent RET
```

## Complete Example Configuration

```elisp
(use-package claude
  :vc (:url "https://github.com/chadac/emacs-claude-agent")
  :config
  (require 'claude-mcp)
  (require 'claude-agent)

  ;; Keybindings
  (define-key prog-mode-map (kbd "C-c C-e") #'claude-transient-menu)
  (define-key text-mode-map (kbd "C-c C-e") #'claude-transient-menu)

  ;; Window display
  (add-to-list 'display-buffer-alist
               '("^\\*claude"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.4)))

  ;; Notifications
  (setq claude-notify-on-await t)
  (setq claude-notification-sound-mac "Submarine")

  ;; Behavior
  (setq claude-switch-to-buffer-on-create t)
  (setq claude-switch-to-buffer-on-file-add nil)
  (setq claude-m-return-is-submit t)

  ;; Oneshot agents
  (setq claude-oneshot-model "sonnet"))

;; Auto-revert for Claude file modifications
(global-auto-revert-mode t)

;; Terminal scrollback
(with-eval-after-load 'eat
  (setq eat-term-scrollback-size 400000))
```

## Next Steps

- [Features Overview](../features/overview.md) - Explore all capabilities
- [Custom Tools](../guides/custom-tools.md) - Create project-specific MCP tools
- [Commands Reference](../reference/commands.md) - Complete command listing
