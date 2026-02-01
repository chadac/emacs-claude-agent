# Claude-Agent Development Guide

## Project Overview

Claude-agent is an Emacs package that integrates Claude Code with Emacs via MCP (Model Context Protocol). It has two architectures:

1. **eat-mode backend** (`claude-agent.el`): Runs `claude` CLI in an eat terminal inside Emacs
2. **REPL/agent backend** (`claude-agent-repl.el` + `claude-mcp-process.el`): Runs a Python wrapper (`claude_agent/`) that manages the Claude SDK directly, with a structured buffer UI

Both backends use the same MCP server (`emacs_mcp/`) to give Claude access to Emacs buffers.

## Repository Structure

```
├── claude-agent.el          # Main entry point: eat-mode backend, session management, keybindings
├── claude-agent-repl.el     # REPL backend: structured buffer UI, input/output parsing
├── claude-mcp-process.el    # REPL backend: process spawning, lifecycle management
├── claude-mcp.el            # MCP tool definitions (buffer ops, locking, eval, etc.)
├── claude-mcp-magit.el      # Git/magit MCP tools
├── claude-mcp-messaging.el  # Inter-agent messaging MCP tools
├── claude-mcp-notes.el      # Org-mode notes MCP tools
├── claude-kb.el             # Knowledge base MCP tools
├── claude-oneshot.el        # One-shot agent spawning (targeted edits)
├── claude-pair.el           # Pair programming utilities
├── claude-comment.el        # Comment detection for "implement this comment"
├── claude-sessions.el       # Session management and persistence
├── claude-transient.el      # Transient menu UI
├── todo.el                  # Org-roam TODO integration with worktree support
├── claude-agent-prompt.md   # System prompt appended to Claude when running inside Emacs
├── claude_agent/            # Python package: Claude SDK wrapper
│   ├── claude_agent/        # Python source
│   └── pyproject.toml
├── emacs_mcp/               # Python package: MCP server
│   ├── emacs_mcp/           # Python source
│   ├── tools.yaml           # Tool definitions and safety annotations
│   └── pyproject.toml
├── scripts/                 # Utility scripts (pretrust-directory.py, etc.)
├── test/                    # ERT test files
└── Makefile                 # Test runner
```

## Key Architecture Concepts

### Path Resolution (`claude--package-root`)

All paths in the package are derived from a single root directory via `claude--package-root`:

- `<root>/claude_agent/` - Python agent wrapper
- `<root>/emacs_mcp/` - MCP server
- `<root>/scripts/` - Utility scripts
- `<root>/claude-agent-prompt.md` - System prompt

The root is resolved in this priority order:
1. `claude-agent-root-directory` defcustom (user override)
2. `claude--package-dir` (captured at load time from `load-file-name`)
3. `locate-library` fallbacks

**When adding new paths derived from the package root, always use `(claude--package-root)`.** Do not use ad-hoc `load-file-name` / `locate-library` patterns.

### MCP Server

The MCP server (`emacs_mcp/`) communicates with Emacs via `emacsclient` over a Unix socket. Tools are defined in `emacs_mcp/tools.yaml` and implemented as elisp functions registered in `claude-mcp.el`.

Key env vars passed to the MCP server:
- `CLAUDE_AGENT_CWD` - Working directory for the session
- `CLAUDE_AGENT_BUFFER_NAME` - Buffer name for routing
- `CLAUDE_AGENT_SOCKET` - Emacs server socket path

### Two Buffer Naming Schemes

- eat backend: `*claude:/full/path*` or `*claude:/full/path:agent-name*`
- REPL backend: `*claude:dirname*` or `*claude:dirname:agent-name*`

## Development Workflow

### Working in Git Worktrees

When developing in a worktree, `load-file-name` and `locate-library` resolve to the *installed* version, not the worktree. Set the override:

```elisp
(setq claude-agent-root-directory
      "/path/to/worktrees/claude-agent/my-branch/")
```

This makes all spawned agents use the worktree's Python agent, MCP server, and scripts.

### Reloading Elisp Changes

Use `mcp__emacs__reload_file` to reload files in dependency order:

```
claude-mcp.el → claude-agent-repl.el → claude-mcp-process.el → claude-agent.el
```

The `claude-restart` command (or `mcp__emacs__restart_session`) kills the current session, reloads elisp, and restarts with `--continue`.

### Running Tests

```bash
make test          # Unit tests (default)
make test-unit     # Unit tests only
make test-all      # Unit + integration + e2e
make test-tdd      # TDD batch-mode tests
```

### Python Development

Both Python packages use `uv` for dependency management:

```bash
# MCP server
cd emacs_mcp && uv run pytest

# Python agent
cd claude_agent && uv run pytest
```

## File Dependency Graph

```
claude-agent.el (main entry point)
├── requires: claude-pair.el
├── requires: claude-mcp.el
│   ├── requires: claude-mcp-messaging.el
│   ├── requires: claude-mcp-magit.el
│   ├── requires: claude-mcp-notes.el
│   └── requires: claude-kb.el
├── requires: claude-sessions.el
├── requires: todo.el
└── requires: claude-agent-repl.el
    ├── requires: claude-mcp.el (shared)
    └── requires: claude-transient.el

claude-mcp-process.el (standalone, loaded separately)
├── requires: claude-agent-repl.el
├── requires: claude-mcp.el
└── requires: claude-sessions.el
```

Note: `claude-agent.el` defines `claude--package-root` and the defcustom. Files loaded before it (`claude-mcp.el`, `todo.el`) use `fboundp` guards to safely call `claude--package-root` when available.
