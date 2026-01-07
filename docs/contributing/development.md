# Development Setup

Set up your environment for contributing to emacs-claude-agent.

## Prerequisites

- Emacs 28.1+
- Python 3.8+
- [uv](https://github.com/astral-sh/uv) (Python package manager)
- Git
- Claude Code CLI

## Getting Started

### 1. Clone the Repository

```bash
git clone https://github.com/chadac/emacs-claude-agent.git
cd emacs-claude-agent
```

### 2. Install Python Dependencies

```bash
cd emacs_mcp
uv pip install -e .
```

### 3. Load in Emacs

```elisp
;; Add to load-path
(add-to-list 'load-path "/path/to/emacs-claude-agent")

;; Load the packages
(require 'claude)
(require 'claude-mcp)
(require 'claude-agent)

;; Set up keybinding
(global-set-key (kbd "C-c C-e") #'claude-transient-menu)
```

### 4. Start Emacs Server

```elisp
(server-start)
```

## Project Structure

```
emacs-claude-agent/
├── claude.el              # Main entry, configuration
├── claude-agent.el        # Agent buffer UI
├── claude-mcp.el          # MCP tool definitions
├── claude-mcp-process.el  # MCP server management
├── claude-mcp-magit.el    # Git/Magit integration
├── claude-mcp-messaging.el # Inter-agent messaging
├── claude-mcp-notes.el    # Project notes
├── claude-oneshot.el      # Oneshot agents
├── claude-sessions.el     # Session management
├── claude-transient.el    # Transient menu
├── claude-kb.el           # Knowledge base
├── claude-pair.el         # Pair programming helpers
├── emacs_mcp/             # Python MCP server
│   ├── server.py          # MCP protocol
│   └── __init__.py
├── claude_agent/          # Python agent wrapper
│   └── agent.py           # Claude SDK wrapper
├── test/                  # Test files
│   ├── claude-test.el
│   └── ...
└── docs/                  # Documentation
```

## Development Workflow

### Making Changes

1. Create a branch:
   ```bash
   git checkout -b feature/my-feature
   ```

2. Make changes

3. Test your changes:
   ```bash
   make test
   ```

4. Submit PR

### Testing

#### Run All Tests

```bash
make test
```

#### Run Specific Tests

```elisp
(ert-run-tests-batch-and-exit "claude-")
```

#### Interactive Testing

```elisp
;; Reload after changes
(load-file "claude.el")
(load-file "claude-mcp.el")
;; etc.
```

### Debugging

#### Enable Debug Logging

```elisp
(setq claude-program-switches '("--verbose"))
```

#### Check MCP Server Logs

```bash
tail -f /tmp/claude-mcp.log
```

#### Oneshot Debug Mode

```elisp
(setq claude-oneshot-debug t)
```

Keeps agent buffers after completion.

## Adding Features

### Adding an MCP Tool

1. Define the tool in `claude-mcp.el`:

```elisp
(claude-mcp-deftool my-new-tool
  "Description of what this tool does"
  :function #'claude-mcp-my-new-tool
  :safe t  ; or nil if it modifies state
  :args ((arg1 string :required "First argument")
         (arg2 integer "Optional argument")))
```

2. Implement the function:

```elisp
(defun claude-mcp-my-new-tool (arg1 &optional arg2)
  "Implementation of my-new-tool.
ARG1 is the first argument.
ARG2 is optional."
  ;; Implementation
  (format "Result: %s %s" arg1 (or arg2 "default")))
```

3. Test it:

```elisp
(claude-mcp-my-new-tool "test" 42)
```

### Adding a Command

1. Define the command:

```elisp
(defun claude-my-command ()
  "Description of command."
  (interactive)
  ;; Implementation
  )
```

2. Add to transient menu in `claude-transient.el`:

```elisp
["My Section"
 ("m" "My Command" claude-my-command)]
```

### Adding Configuration

1. Define the variable:

```elisp
(defcustom claude-my-option nil
  "Description of option."
  :type 'boolean
  :group 'claude-agent)
```

2. Use in code:

```elisp
(when claude-my-option
  ;; Do something
  )
```

## Code Style

### Elisp Style

- Use `cl-` prefixed functions from `cl-lib`
- Document all public functions
- Use meaningful variable names
- Follow Emacs Lisp conventions

### Python Style

- Follow PEP 8
- Use type hints
- Document functions with docstrings

## Pull Request Guidelines

1. **Clear description** - Explain what and why
2. **Tests** - Add tests for new features
3. **Documentation** - Update relevant docs
4. **Small PRs** - One feature/fix per PR
5. **Clean history** - Rebase if needed

## Common Tasks

### Regenerate Tool Exports

```elisp
(claude-mcp-export-tools)
```

### Reset MCP Server

```elisp
(claude-mcp-restart)
```

### Check Tool Registry

```elisp
(hash-table-keys claude-mcp-tools)
```

## Getting Help

- File issues on GitHub
- Check existing issues and PRs
- Read the codebase comments
