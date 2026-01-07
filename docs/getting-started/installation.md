# Installation

This guide covers all installation methods for emacs-claude-agent.

## Prerequisites

Before installing emacs-claude-agent, ensure you have:

1. **Emacs 28.1+** - Required for proper functionality
2. **[Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code/overview)** - The official Claude terminal client
3. **[eat](https://codeberg.org/akib/emacs-eat)** - Terminal emulator for Emacs
4. **Python 3.8+** - Required for the MCP server
5. **[uv](https://github.com/astral-sh/uv)** - Fast Python package installer (recommended)
6. **Emacs server** - Must be running for MCP integration (`M-x server-start`)

## Installation Methods

### Doom Emacs

Add to your `packages.el`:

```elisp
(package! emacs-claude-agent
  :recipe (:host github :repo "chadac/emacs-claude-agent"))
```

Then in your `config.el`:

```elisp
(use-package! claude
  :config
  (require 'claude-mcp)
  (require 'claude-agent))
```

Run `doom sync` to install.

### use-package with built-in :vc (Emacs 30+)

```elisp
(use-package claude
  :vc (:url "https://github.com/chadac/emacs-claude-agent")
  :config
  (require 'claude-mcp)
  (require 'claude-agent))
```

### use-package with vc-use-package

```elisp
(use-package claude
  :vc (:fetcher github :repo "chadac/emacs-claude-agent")
  :config
  (require 'claude-mcp)
  (require 'claude-agent))
```

### straight.el

```elisp
(straight-use-package
 '(claude :type git :host github :repo "chadac/emacs-claude-agent"))

(require 'claude)
(require 'claude-mcp)
(require 'claude-agent)
```

### Manual Installation

1. Clone the repository:

    ```bash
    git clone https://github.com/chadac/emacs-claude-agent.git ~/.emacs.d/site-lisp/emacs-claude-agent
    ```

2. Add to your Emacs configuration:

    ```elisp
    (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-claude-agent")
    (require 'claude)
    (require 'claude-mcp)
    (require 'claude-agent)
    ```

## Post-Installation Setup

### Start the Emacs Server

The MCP integration requires the Emacs server to be running:

```elisp
;; Add to your init.el
(server-start)
```

Or run manually with `M-x server-start`.

### Set Up Keybindings

Choose your preferred keybinding for the transient menu:

```elisp
;; Recommended: bind to prog-mode and text-mode
(define-key prog-mode-map (kbd "C-c C-e") #'claude-transient-menu)
(define-key text-mode-map (kbd "C-c C-e") #'claude-transient-menu)

;; Or globally (may override useful keybindings)
(global-set-key (kbd "C-c C-e") #'claude-transient-menu)
```

### Enable Auto-Revert Mode

Claude modifies files directly, so auto-revert ensures you see changes:

```elisp
(global-auto-revert-mode t)
```

### Configure Terminal Scrollback

Increase scrollback for searching conversation history:

```elisp
(with-eval-after-load 'eat
  (setq eat-term-scrollback-size 400000))
```

## Verifying Installation

1. Open any source file
2. Press `C-c C-e` (or your chosen keybinding)
3. Press `s` to start a Claude session
4. You should see a Claude Code session start in a new buffer

If you see MCP tools available (Claude mentions it can read/edit buffers), the MCP integration is working.

## Troubleshooting

### Claude CLI not found

Ensure Claude Code is installed and in your PATH:

```bash
which claude
# Should show path like /usr/local/bin/claude
```

If not found, install from [Claude Code documentation](https://docs.anthropic.com/en/docs/claude-code/overview).

### MCP tools not available

1. Ensure Emacs server is running: `M-x server-start`
2. Check that `server-name` is set correctly
3. Verify Python/uv is available

### eat package not found

Install eat from MELPA or directly:

```elisp
;; From MELPA
(use-package eat
  :ensure t)

;; Or with straight
(straight-use-package 'eat)
```

## Next Steps

- [Quick Start Guide](quickstart.md) - Learn basic usage
- [Configuration](configuration.md) - Customize your setup
- [Features Overview](../features/overview.md) - Explore all capabilities
