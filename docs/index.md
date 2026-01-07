# emacs-claude-agent

**A powerful Emacs integration for Claude Code with MCP-powered buffer operations and multi-agent support.**

## What is emacs-claude-agent?

emacs-claude-agent connects [Claude Code](https://docs.anthropic.com/en/docs/claude-code/overview) with Emacs through the Model Context Protocol (MCP), giving Claude direct access to your buffers, files, and editor state.

Unlike simple terminal wrappers, this integration lets Claude:

- **Read and modify buffers directly** - No more copy-pasting code back and forth
- **Execute elisp expressions** - Claude can run any Emacs command with proper context
- **Spawn background agents** - Run multiple Claude instances for parallel tasks
- **Track progress visually** - See real-time progress indicators and status updates
- **Integrate with Git** - Stage, diff, and commit through Magit

## Key Features

<div class="grid cards" markdown>

-   :material-buffer:{ .lg .middle } **MCP Buffer Operations**

    ---

    Claude can directly read, search, and modify your Emacs buffers without copy-pasting.

    [:octicons-arrow-right-24: Learn more](features/mcp.md)

-   :material-account-group:{ .lg .middle } **Multi-Agent System**

    ---

    Spawn multiple Claude agents for complex parallel tasks with inter-agent messaging.

    [:octicons-arrow-right-24: Learn more](features/multi-agent.md)

-   :material-lightning-bolt:{ .lg .middle } **Oneshot Agents**

    ---

    Lightweight background agents for quick, permission-scoped edits.

    [:octicons-arrow-right-24: Learn more](features/oneshot.md)

-   :material-git:{ .lg .middle } **Magit Integration**

    ---

    Git operations through Emacs's Magit, with commit proposals for your approval.

    [:octicons-arrow-right-24: Learn more](features/magit.md)

</div>

## Quick Start

```elisp
;; Install with use-package (Emacs 30+)
(use-package claude
  :vc (:url "https://github.com/chadac/emacs-claude-agent")
  :config
  (require 'claude-mcp)
  (require 'claude-agent))

;; Set your keybinding
(define-key prog-mode-map (kbd "C-c C-e") #'claude-transient-menu)
```

Then press `C-c C-e s` to start your first Claude session!

[:octicons-arrow-right-24: Full installation guide](getting-started/installation.md)

## History

This project is a fork of [cpoile/claudemacs](https://github.com/cpoile/claudemacs) that has evolved into a comprehensive agent framework. While claudemacs provides an excellent terminal-based Claude integration, emacs-claude-agent extends it with MCP-powered buffer operations, multi-agent support, and deep Emacs integration.

## Getting Help

- Check the [Getting Started](getting-started/installation.md) guide
- Browse the [Features](features/overview.md) documentation
- File issues on [GitHub](https://github.com/chadac/emacs-claude-agent/issues)
