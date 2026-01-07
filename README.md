# emacs-claude-agent

A Claude/Emacs integration designed for collaborative pair programming.

emacs-claude-agent provides two core components:

1. *claude-agent* — an interactive buffer with Claude that features org-mode-like capabilities; and
2. *emacs-mcp* — an MCP server enabling agents to interact with the editor and access richer contextual information.

> *Note:* This project evolved from a fork of [cpoile/claudemacs](https://github.com/cpoile/claudemacs) into its own thing that shares effectively no code; nonetheless, it was still influenced/motivated by claudemacs.

## Features

- **Seamless buffer integration** - Claude reads, modifies, and navigates your buffers through MCP without context loss
- **Org-mode-like editing** - Natural syntax for formatting, context, and special commands within conversation
- **Multi-agent coordination** - Spawn parallel agents for complex tasks and communicate between them
- **Scoped background editing** - Oneshot agents for permission-controlled, targeted edits
- **Native Emacs prompts** - Choice menus, file pickers, and proposal review interfaces
- **Git operations** - Stage, diff, and commit directly through Magit
- **Progress tracking** - Visual indicators for long-running operations
- **Knowledge base** - Store and retrieve project-specific learnings and patterns
- **Self-extending** - Claude can define new tools, evaluate elisp, and expand its own capabilities on the fly
- **Extensible tools** - Create custom MCP tools for project-specific workflows

## Quick Start

### Prerequisites

1. Install [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code/overview)
2. Install the [eat](https://codeberg.org/akib/emacs-eat) package in Emacs
3. Ensure Emacs server is running (`M-x server-start`)

### Installation

#### Doom Emacs

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

#### use-package with built-in :vc (Emacs 30+)

```elisp
(use-package claude
  :vc (:url "https://github.com/chadac/emacs-claude-agent")
  :config
  (require 'claude-mcp)
  (require 'claude-agent))
```

#### straight.el

```elisp
(straight-use-package
 '(claude :type git :host github :repo "chadac/emacs-claude-agent"))
```

#### Manual Installation

```elisp
(add-to-list 'load-path "/path/to/emacs-claude-agent")
(require 'claude)
(require 'claude-mcp)
(require 'claude-agent)
```

### Basic Setup

```elisp
;; Set your preferred keybinding for the transient menu
(define-key prog-mode-map (kbd "C-c C-e") #'claude-transient-menu)

;; Enable global auto-revert (recommended - Claude modifies files)
(global-auto-revert-mode t)

;; Increase terminal scrollback for history searching
(with-eval-after-load 'eat
  (setq eat-term-scrollback-size 400000))
```

## Core Features

### MCP-Powered Buffer Operations

Claude can directly interact with your Emacs buffers through MCP tools:

| Tool | Description |
|------|-------------|
| `get_buffer_content` | Read buffer contents with line ranges |
| `edit_buffer` | Modify buffer text directly |
| `list_buffers` | List all open buffers |
| `search_buffer` | Search with regex and context lines |
| `eval` | Execute arbitrary elisp expressions |

### Multi-Agent System

Spawn and coordinate multiple Claude agents for complex tasks:

```
C-c C-e s    Start a new Claude agent
C-c C-e S    Spawn agent in another directory
```

Agents can communicate via message passing:

- `spawn_agent` - Create new agents in different directories
- `message_agent` - Send messages between agents
- `list_agents` - View all running agents
- `check_messages` - Check message queue

### Oneshot Background Agents

Lightweight agents for quick, targeted edits:

```
C-c c c    Edit current line/region (most constrained)
C-c c b    Edit current buffer
C-c c d    Edit files in current directory
C-c c p    Edit any file in project (least constrained)
```

Oneshot agents:
- Run in the background without taking over your terminal
- Auto-terminate when done
- Have permission-scoped editing (can only touch what you specify)
- Show visual highlighting of their target region

### Interactive Prompts and Proposals

Claude can ask for your input through native Emacs interfaces:

- **Choice prompts**: Navigate with j/k, confirm with RET
- **Multi-select**: Toggle with SPC, select all with 'a'
- **File/directory pickers**: Quick selection from project files
- **Proposals**: Review and edit Claude's suggestions before applying

### Progress Tracking

Visual progress indicators for long-running operations:

```elisp
;; Claude uses these automatically
(mcp__emacs__progress_start "Building project...")
(mcp__emacs__progress_update "Compiling..." 50)
(mcp__emacs__progress_stop "Build complete!")
```

### Magit Integration

Git operations through Emacs's Magit:

| Tool | Description |
|------|-------------|
| `magit_status` | View git status |
| `magit_stage` | Stage files for commit |
| `magit_diff` | View diffs |
| `magit_commit_propose` | Propose commits for your approval |

### Knowledge Base

Store and retrieve learnings about your codebase:

```elisp
;; Claude can store gotchas, patterns, and architecture notes
(mcp__emacs__kb_create
  :title "Database connection gotcha"
  :kb_type "gotcha"
  :summary "Always close connections in finally block")
```

## Usage Examples

### Basic Pair Programming

```
C-c C-e s    Start Claude session
C-c C-e x    Send request with file/region context
C-c C-e e    Fix error at point (uses flycheck)
C-c C-e i    Implement comment at point
```

### Working with Context

```elisp
;; Add current file to conversation
C-c C-e F

;; Add specific file reference
C-c C-e f

;; Send context (file:line or file:line-range)
C-c C-e a
```

### Quick Responses

```
C-c C-e y    Send "Yes" (approve)
C-c C-e n    Send "No" (reject)
```

## Configuration

### Core Settings

```elisp
;; Custom Claude executable path
(setq claude-program "/usr/local/bin/claude")

;; Command line switches
(setq claude-program-switches '("--verbose"))

;; Enable MCP integration (default: t)
(setq claude-use-mcp t)

;; Prefer projectile root over git root
(setq claude-prefer-projectile-root t)
```

### Window Behavior

```elisp
;; Display Claude in a side window
(add-to-list 'display-buffer-alist
             '("^\\*claude"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.4)))

;; Control buffer switching behavior
(setq claude-switch-to-buffer-on-create t)
(setq claude-switch-to-buffer-on-toggle t)
```

### System Notifications

```elisp
;; Enable notifications when Claude needs input
(setq claude-notify-on-await t)

;; Mac notification sound
(setq claude-notification-sound-mac "Submarine")

;; Linux notification settings
(setq claude-notification-auto-dismiss-linux t)
(setq claude-notification-sound-linux "message-new-instant")
```

## Defining Custom Tools

Custom MCP tools are defined in Elisp using `claude-mcp-deftool`:

```elisp
(claude-mcp-deftool project-build
  "Build the project using make."
  :function #'my-project-build
  :safe nil
  :args ((directory string :required "Build directory")))

(defun my-project-build (directory)
  "Build project in DIRECTORY."
  (let ((default-directory directory))
    (compile "make")))
```

Tools automatically become available to Claude agents. Key options:

- `:function` - The elisp function to call
- `:safe t` - Mark as safe to run (no need to request permissions)
- `:needs-session-cwd t` - Bind `default-directory` to session's working directory
- `:args` - Argument definitions: `(name type [:required] "description")`

See the [Custom Tools Guide](docs/guides/custom-tools.md) for more examples.

## Self-Extending Capabilities

One of the unique advantages of emacs-claude-agent is that Claude has full access to Emacs's introspection and extension capabilities. Through the `eval` MCP tool, Claude can:

- **Define new tools on the fly** - Need a project-specific command? Claude can write and register it using `claude-mcp-deftool`
- **Inspect and modify Emacs state** - Query variables, check modes, examine buffers
- **Reload modified code** - After editing elisp files, Claude can reload them to test changes immediately
- **Extend its own capabilities** - If Claude needs functionality that doesn't exist, it can create it

This makes emacs-claude-agent particularly powerful for:

- **Rapid prototyping** - Try new tools without leaving the conversation
- **Project-specific automation** - Claude builds exactly the tools your workflow needs
- **Self-improvement** - Claude can enhance its own integration with your Emacs setup

```elisp
;; Example: Claude can define a tool mid-conversation
(claude-mcp-deftool check-package-json
  "Read and parse the project's package.json."
  :function (lambda ()
              (with-temp-buffer
                (insert-file-contents "package.json")
                (json-parse-buffer)))
  :safe t
  :needs-session-cwd t)
```

## Documentation

- [Custom Tools Guide](docs/guides/custom-tools.md) - Create project-specific MCP tools with `claude-mcp-deftool`
- [MCP Overview](MCP-INTEGRATION.md) - Overview of MCP features (buffer ops, multi-agent, etc.)
- [Full Documentation](docs/) - Comprehensive documentation (can be built with MkDocs)

### Building the Documentation Site

The `docs/` directory contains a full documentation site that can be built with MkDocs:

```bash
# Install mkdocs and dependencies
pip install mkdocs mkdocs-material mkdocstrings

# Serve locally
mkdocs serve

# Build static site
mkdocs build
```

## History

This project began as a fork of [cpoile/claudemacs](https://github.com/cpoile/claudemacs), which provides a clean terminal-based Claude Code integration for Emacs. emacs-claude-agent extends that foundation with:

- **MCP integration** for direct buffer manipulation
- **Multi-agent architecture** for parallel operations
- **Oneshot agents** for quick, scoped edits
- **Interactive prompts** through native Emacs UI
- **Knowledge base** for persistent learnings

The original claudemacs philosophy of simplicity remains - we just added superpowers.

## Requirements

- Emacs 28.1+
- [eat](https://codeberg.org/akib/emacs-eat) terminal emulator
- [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code/overview)
- Python 3.8+ (for MCP server)
- [uv](https://github.com/astral-sh/uv) (for Python dependency management)

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## Credits

- Original [claudemacs](https://github.com/cpoile/claudemacs) by Christopher Poile
- Inspired by [Aidermacs](https://github.com/MatthewZMD/aidermacs) by Matthew Zeng
- Inspired by [claude-code.el](https://github.com/stevemolitor/claude-code.el) by Steve Molitor
- Inspired by [claude-code-ide](https://github.com/CrazyForks/claude-code-ide)

## License

MIT License. See [LICENSE](LICENSE) file for details.
