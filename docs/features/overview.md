# Features Overview

emacs-claude-agent provides a comprehensive suite of features for AI-assisted programming in Emacs.

## Core Capabilities

### Direct Buffer Access

Unlike terminal-only integrations, emacs-claude-agent gives Claude direct access to your Emacs environment through MCP (Model Context Protocol):

- **Read buffers** without copy-paste
- **Edit files** directly with proper undo support
- **Search content** with regex patterns
- **Execute elisp** in the correct context

[Learn more about MCP Integration](mcp.md)

### Multi-Agent Architecture

Run multiple Claude instances that can:

- Work in different directories simultaneously
- Communicate via message passing
- Coordinate on complex tasks
- Share session context

[Learn more about Multi-Agent System](multi-agent.md)

### Oneshot Background Agents

Lightweight agents for quick, scoped operations:

- Run without taking over your terminal
- Automatically terminate when done
- Permission-scoped to specific files/regions
- Visual highlighting of target area

[Learn more about Oneshot Agents](oneshot.md)

### Native Emacs UI

All interactions feel native to Emacs:

- **Choice prompts** with j/k navigation
- **Multi-select** with checkbox toggles
- **File pickers** for project files
- **Proposals** for review-before-apply

[Learn more about Interactive Prompts](prompts.md)

### Git Integration

Seamless git operations through Magit:

- View status and diffs
- Stage and unstage files
- Propose commits for your approval
- Full Magit power available to Claude

[Learn more about Magit Integration](magit.md)

### Knowledge Base

Persistent storage for project learnings:

- **Gotchas**: Bugs and edge cases
- **Architecture**: Design decisions
- **Patterns**: Reusable code patterns
- **References**: Documentation links

[Learn more about Knowledge Base](knowledge-base.md)

### Self-Extending Capabilities

Claude can extend its own capabilities during a conversation:

- **Define new tools** on the fly using `claude-mcp-deftool`
- **Evaluate elisp** to inspect or modify Emacs state
- **Reload code** after editing elisp files
- **Build project-specific automation** as needs arise

This tight integration with Emacs's extension system means Claude isn't limited to predefined tools—it can create exactly the functionality your workflow needs.

[Learn more about Custom Tools](../guides/custom-tools.md)

## Feature Matrix

| Feature | Description | Default |
|---------|-------------|---------|
| MCP Buffer Ops | Direct buffer read/write | Enabled |
| Multi-Agent | Spawn parallel agents | Available |
| Oneshot Agents | Quick background edits | Available |
| Magit Integration | Git through Emacs | Enabled |
| Knowledge Base | Persistent learnings | Enabled |
| Notifications | System alerts when waiting | Enabled |
| Progress Tracking | Visual indicators | Enabled |
| Custom Tools | Project-specific MCP tools | Configurable |
| Self-Extension | Claude defines new tools on the fly | Available |

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                     Emacs                               │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐ │
│  │ claude.el   │  │ claude-mcp  │  │ claude-agent    │ │
│  │ (core)      │  │ (tools)     │  │ (UI buffer)     │ │
│  └─────────────┘  └─────────────┘  └─────────────────┘ │
│         │              │                  │            │
│         └──────────────┼──────────────────┘            │
│                        │                               │
│                 ┌──────┴──────┐                        │
│                 │ MCP Server  │                        │
│                 │ (Python)    │                        │
│                 └──────┬──────┘                        │
└────────────────────────┼────────────────────────────────┘
                         │
                 ┌───────┴───────┐
                 │  Claude Code  │
                 │  (CLI)        │
                 └───────────────┘
```

## Comparison with Other Tools

| Feature | emacs-claude-agent | claudemacs | claude-code.el |
|---------|-------------------|------------|----------------|
| Terminal interface | ✓ | ✓ | ✓ |
| Direct buffer access | ✓ (MCP) | ✗ | ✗ |
| Multi-agent | ✓ | ✗ | ✗ |
| Oneshot agents | ✓ | ✗ | ✗ |
| Custom MCP tools | ✓ | ✗ | ✗ |
| Self-extension | ✓ | ✗ | ✗ |
| Magit integration | ✓ (native) | ✗ | ✗ |
| Progress indicators | ✓ | ✗ | ✗ |

## Getting Started

1. [Install emacs-claude-agent](../getting-started/installation.md)
2. [Configure your setup](../getting-started/configuration.md)
3. [Start your first session](../getting-started/quickstart.md)
4. Explore specific features in the pages linked above
