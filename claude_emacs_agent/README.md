# Claude Emacs Agent

Python wrapper for Claude CLI that provides a clean line-based protocol for Emacs integration.

## Overview

This agent spawns Claude CLI in headless JSON streaming mode and translates between:
- **Claude CLI**: JSON streaming via stdin/stdout
- **Emacs**: Simple line protocol via stdin/stdout

## Protocol

### Commands (Emacs → Agent)

```
USER:<message>           # Send user message
RESUME:<session-id>      # Resume session (must be sent before first USER)
CONTINUE                 # Continue most recent session
APPROVE:<tool-use-id>    # Approve pending tool call
DENY:<tool-use-id>       # Deny pending tool call
INTERRUPT                # Send interrupt signal to Claude
QUIT                     # Graceful shutdown
```

### Events (Agent → Emacs)

```
INIT:<json>              # Session initialized (model, tools, session_id)
STATUS:<status>          # Status change: ready, thinking, waiting_approval, dead
ASSISTANT:<text>         # Assistant response text (may be partial)
TOOL_CALL:<json>         # Tool call request (name, input, id)
TOOL_RESULT:<json>       # Tool call completed (id, output)
COST:<json>              # Cost update (total_usd, tokens)
RESULT:<json>            # Turn complete (final result, duration)
ERROR:<message>          # Error occurred
```

## Usage

```bash
# Start agent for a directory
uv run python -m claude_emacs_agent --work-dir /path/to/project

# Resume a specific session
uv run python -m claude_emacs_agent --work-dir /path/to/project --resume <session-id>

# Continue most recent session
uv run python -m claude_emacs_agent --work-dir /path/to/project --continue

# With MCP config
uv run python -m claude_emacs_agent --work-dir /path/to/project --mcp-config /path/to/config.json
```

## Development

```bash
# Run tests
uv run pytest

# Type checking
uv run mypy claude_emacs_agent
```
