# emacs-claude-agent

An Emacs integration for Claude Code that turns your editor into a collaborative AI programming environment.

> **Fair warning:** This project is highly vibe-coded. The code quality is... let's say "enthusiastic rather than disciplined." It works, often surprisingly well, but don't expect clean architecture or comprehensive test coverage. That said, it provides capabilities that no other Emacs+Claude integration currently offers.

emacs-claude-agent gives Claude deep access to your Emacs session through [MCP](https://modelcontextprotocol.io/) (Model Context Protocol). Instead of copy-pasting between your editor and a chat window, Claude can directly read your buffers, make targeted edits with visual feedback, run shell commands, interact with Magit, and coordinate multiple agents — all while you keep working.

This project evolved from a fork of [cpoile/claudemacs](https://github.com/cpoile/claudemacs). While it shares very little code with the original at this point, claudemacs provided the initial inspiration and terminal integration approach that made this possible.

## What Makes This Different

Most Claude-in-Emacs integrations give you a chat buffer and maybe some file context. emacs-claude-agent goes further:

- **Claude edits your buffers directly** — through a lock-region workflow that shows you exactly what's being changed, with visual highlighting
- **Multiple agents at once** — spawn parallel Claude sessions that can message each other and work on different parts of your codebase simultaneously
- **Oneshot background agents** — quick, scoped edits (line, buffer, directory, or project level) that run in the background without interrupting your flow
- **Native Emacs UI** — choice menus, file pickers, proposal review, and progress indicators that feel like Emacs, not a web app
- **Self-extending** — Claude can define new MCP tools on the fly using `eval` and `claude-mcp-deftool`, tailoring its own capabilities to your project
- **Git integration** — stage, diff, commit through Magit; Claude proposes commits for your review
- **Knowledge base** — persistent project-specific learnings stored as org-roam nodes
- **TODO/worktree system** — org-roam based task management with git worktree support for isolated agent work

## Quick Start

### Prerequisites

- Emacs 28.1+
- [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code/overview)
- [eat](https://codeberg.org/akib/emacs-eat) terminal emulator package
- Python 3.12+ with [uv](https://github.com/astral-sh/uv)
- Emacs server running (`M-x server-start`)

### Installation

#### Doom Emacs

In `packages.el`:

```elisp
(package! emacs-claude-agent
  :recipe (:host github :repo "chadac/emacs-claude-agent"))
```

In `config.el`:

```elisp
(use-package! claude-agent
  :config
  (require 'claude-mcp)
  (require 'claude-agent))
```

#### use-package + :vc (Emacs 30+)

```elisp
(use-package claude-agent
  :vc (:url "https://github.com/chadac/emacs-claude-agent")
  :config
  (require 'claude-mcp)
  (require 'claude-agent))
```

#### straight.el

```elisp
(straight-use-package
 '(claude-agent :type git :host github :repo "chadac/emacs-claude-agent"))
```

#### Manual

```elisp
(add-to-list 'load-path "/path/to/emacs-claude-agent")
(require 'claude-agent)
(require 'claude-mcp)
```

### Setup

```elisp
;; Bind the transient menu to your preferred key
(define-key prog-mode-map (kbd "C-c C-e") #'claude-transient-menu)

;; Recommended: auto-revert buffers when Claude modifies files on disk
(global-auto-revert-mode t)
```

Then `C-c C-e s` to start a session.

## Features

### Buffer Operations via MCP

Claude interacts with your Emacs buffers through 40+ MCP tools. The core editing workflow uses region locking for safety:

1. Claude reads a file with `read_file` (sees line numbers + flycheck/flymake diagnostics)
2. Locks a region with `lock` (highlighted in your buffer, protected from your edits)
3. Replaces the content with `edit` (auto-saves if buffer was clean)

Other buffer tools include `search_buffer` (regex with context lines), `list_buffers`, `buffer_info`, `get_buffer_content` (with head/tail/range support), and `eval` for arbitrary elisp.

### Oneshot Agents

Quick background edits with permission scoping:

| Keybinding | Scope | What Claude Can Touch |
|------------|-------|----------------------|
| `C-c c c` | Line/region | Only the selected line(s) |
| `C-c c b` | Buffer | Current buffer only |
| `C-c c d` | Directory | Files in current directory |
| `C-c c p` | Project | Any file in the project |

Oneshot agents run in the background, show visual highlighting of their target, and auto-terminate when done. Configure the model with `claude-oneshot-model` (default: `"sonnet"`).

### Multi-Agent System

Spawn multiple Claude sessions and coordinate them:

```
C-c C-e s    Start a new session
C-c C-e S    Spawn in another directory
```

Agents communicate through a message queue system — `spawn_agent`, `message_agent`, `check_messages`, `list_agents`. Useful for divide-and-conquer workflows on large codebases.

### Interactive Prompts

Claude can ask for your input through native Emacs interfaces:

- **Choice menus** — navigate with j/k, confirm with RET
- **Multi-select** — toggle items with SPC, select all with 'a'
- **File/directory pickers** — quick selection from project files
- **Proposals** — review and edit Claude's suggestions before applying (C-c C-c to accept, C-c C-k to reject)
- **Confirmations** — simple y/n popups

### Git Integration

Git operations through Magit:

- `magit_status`, `magit_stage`, `magit_unstage`, `magit_diff`, `magit_log`
- `magit_commit_propose` — Claude proposes a commit message; you review and sign it

### Knowledge Base

Store project learnings as org-roam nodes (requires org-roam):

- **Types**: `gotcha`, `architecture`, `pattern`, `reference`
- **Operations**: `kb_create`, `kb_search`, `kb_get`, `kb_update`, `kb_list`
- Searchable by text, file, module, or concept

### TODO / Worktree System

Org-roam based task management with git worktree isolation (requires org-roam):

- Create TODOs with `org-roam-todo-capture` (`C-c n t t`)
- Spawn a worktree + Claude session with `org-roam-todo-create-worktree` (`C-c c w`)
- Track acceptance criteria and progress
- Auto-commit and optional auto-push on completion

### Custom Tools

Define project-specific MCP tools in elisp:

```elisp
(claude-mcp-deftool project-test
  "Run the project test suite."
  :function (lambda ()
              (compile "make test"))
  :safe t
  :needs-session-cwd t)
```

Key options: `:function`, `:safe` (auto-approve), `:needs-session-cwd`, `:args` for typed parameters.

### Self-Extending

Claude has full access to `eval` and can define new tools mid-conversation. Need a project-specific command? Claude can write and register it, reload elisp files, inspect Emacs state — whatever the workflow requires.

## Configuration

### Core

```elisp
(setq claude-program "/usr/local/bin/claude")     ; CLI path
(setq claude-program-switches '("--verbose"))      ; Extra CLI args
(setq claude-use-mcp t)                            ; MCP integration (default: t)
(setq claude-prefer-projectile-root t)             ; Use projectile over git root
```

### Agent Behavior

```elisp
;; Block direct Edit/Write tools (use lock-region workflow instead)
;; This is the default — Claude edits through Emacs, not around it
(setq claude-agent-disallowed-tools '("Edit" "Write"))

;; Auto-reject rules for safety
(setq claude-agent-auto-reject-rules-extra
      '((:pattern "Bash(rm -rf:*)" :message "No recursive deletes")))

;; Extra system prompt for project-specific instructions
(setq claude-agent-extra-system-prompt
      "Always run tests before committing.")
```

### Window Behavior

```elisp
(add-to-list 'display-buffer-alist
             '("^\\*claude"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.4)))

(setq claude-switch-to-buffer-on-create t)
(setq claude-switch-to-buffer-on-toggle t)
```

### Notifications

```elisp
(setq claude-notify-on-await t)                              ; Notify when Claude needs input
(setq claude-notification-sound-mac "Submarine")             ; macOS sound
(setq claude-notification-auto-dismiss-linux t)              ; Auto-dismiss on Linux
(setq claude-notification-sound-linux "message-new-instant") ; Linux sound
```

## Key Commands

| Key | Command | Description |
|-----|---------|-------------|
| `C-c C-e s` | `claude-run` | Start new session |
| `C-c C-e r` | `claude-resume` | Resume previous session |
| `C-c C-e t` | `claude-toggle-buffer` | Show/hide Claude buffer |
| `C-c C-e k` | `claude-kill` | Kill session |
| `C-c C-e x` | `claude-execute-request` | Send request with file context |
| `C-c C-e e` | `claude-fix-error-at-point` | Fix flycheck error at point |
| `C-c C-e i` | `claude-implement-comment` | Implement CLAUDE: comment |
| `C-c C-e f` | `claude-add-file-reference` | Add file to conversation |
| `C-c C-e F` | `claude-add-current-file-reference` | Add current file |
| `C-c C-e a` | `claude-add-context` | Add context at point/region |
| `C-c C-e y` | `claude-send-yes` | Send "Yes" |
| `C-c C-e n` | `claude-send-no` | Send "No" |
| `C-c c c` | `claude-oneshot-line-or-region` | Oneshot: edit line/region |
| `C-c c b` | `claude-oneshot-buffer` | Oneshot: edit buffer |
| `C-c c d` | `claude-oneshot-directory` | Oneshot: edit directory |
| `C-c c p` | `claude-oneshot-project` | Oneshot: edit project |

## Documentation

Full documentation is available in the `docs/` directory and can be built as a website with MkDocs:

```bash
pip install mkdocs mkdocs-material mkdocstrings
mkdocs serve  # Local dev server at http://localhost:8000
```

See also:
- [MCP Integration Overview](MCP-INTEGRATION.md)
- [Development Guide](CLAUDE.md)

## Requirements

- Emacs 28.1+
- [eat](https://codeberg.org/akib/emacs-eat) terminal emulator
- [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code/overview)
- Python 3.12+ (for MCP server and agent wrapper)
- [uv](https://github.com/astral-sh/uv) (Python dependency management)
- **Optional**: [org-roam](https://www.orgroam.com/) (for knowledge base and TODO system)
- **Optional**: [magit](https://magit.vc/) (for git integration)
- **Optional**: [flycheck](https://www.flycheck.org/) or flymake (for error-at-point features)

## Credits

- Original [claudemacs](https://github.com/cpoile/claudemacs) by Christopher Poile
- Inspired by [Aidermacs](https://github.com/MatthewZMD/aidermacs) by Matthew Zeng
- Inspired by [claude-code.el](https://github.com/stevemolitor/claude-code.el) by Steve Molitor
- Inspired by [claude-code-ide](https://github.com/CrazyForks/claude-code-ide)

## License

MIT License. See [LICENSE](LICENSE) for details.
