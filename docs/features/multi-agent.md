# Multi-Agent System

Run multiple Claude agents that can work in parallel and communicate with each other.

## Overview

The multi-agent system allows you to:

- Spawn agents in different directories
- Run agents in parallel
- Send messages between agents
- Coordinate complex tasks

## Spawning Agents

### From the Transient Menu

```
C-c C-e s    Start agent in current project
C-c C-e S    Spawn agent in another directory
```

### Programmatically

```python
# Spawn an agent in a specific directory
mcp__emacs__spawn_agent(
    directory="/path/to/project",
    agent_name="backend"  # Optional identifier
)
```

## Agent Identification

Each agent has a buffer name that serves as its identity:

- Without name: `*claudemacs:/path/to/project*`
- With name: `*claudemacs:/path/to/project:backend*`

Get your own identity:

```python
mcp__emacs__whoami()
# Returns: "*claudemacs:/home/user/project:main*"
```

## Listing Agents

```python
# Get all running agents
mcp__emacs__list_agents()
# Returns: [
#   {"buffer": "*claudemacs:/project1*", "directory": "/project1"},
#   {"buffer": "*claudemacs:/project2:test*", "directory": "/project2"}
# ]
```

## Inter-Agent Communication

### Sending Messages

```python
# Send a message to another agent
mcp__emacs__message_agent(
    buffer_name="*claudemacs:/project:backend*",
    message="Please update the API endpoint for user auth"
)
```

### Checking Messages

```python
# Check your inbox
mcp__emacs__check_messages(
    buffer_name="*claudemacs:/project:frontend*"
)
# Returns formatted messages with sender info

# Clear after reading
mcp__emacs__check_messages(
    buffer_name="*claudemacs:/project:frontend*",
    clear=True
)
```

### Message Board Summary

```python
# See all message activity
mcp__emacs__message_board_summary()
# Shows counts by sender/recipient pairs
```

## Use Cases

### Parallel Development

```
Agent 1 (backend):   Implements API endpoints
Agent 2 (frontend):  Builds UI components
Agent 3 (tests):     Writes integration tests
```

### Code Review Pipeline

```
Agent 1: Makes code changes
Agent 2: Reviews changes and suggests improvements
Agent 1: Implements feedback
```

### Monorepo Work

```
Main agent: Coordinates overall task
Subagent 1: Works in /packages/core
Subagent 2: Works in /packages/ui
Subagent 3: Works in /apps/web
```

## Example Workflow

### 1. Start the Main Agent

```elisp
;; C-c C-e s in your project root
```

### 2. Spawn Specialized Agents

```
User: Spawn an agent for the backend and one for frontend

Claude: I'll spawn two agents for parallel work.
        [Spawns backend agent in /project/backend]
        [Spawns frontend agent in /project/frontend]
```

### 3. Coordinate Work

```
User: Have the backend agent create the user API,
      then tell frontend to build the login form

Claude: [Sends task to backend agent]
        [Waits for completion]
        [Messages frontend with API details]
```

### 4. Monitor Progress

```python
# Check all agents
mcp__emacs__list_agents()

# Check message flow
mcp__emacs__message_board_summary()
```

## Buffer Management

Agent buffers follow the naming convention:

```
*claudemacs:workspace-name*     ; With workspace
*claudemacs:/path/to/project*   ; Without workspace
*claudemacs:/path:agent-name*   ; With custom name
```

### Switching Between Agents

```elisp
;; Use standard buffer switching
C-x b *claudemacs:backend* RET
```

### Killing Agents

```
C-c C-e k    ; Kill current agent from transient menu
```

## Configuration

```elisp
;; Enable MCP agent tools (required)
(setq claude-agent-enable-mcp t)
```

## Tips

1. **Name your agents** - Use descriptive names like "backend", "tests", "docs"
2. **Check messages regularly** - Agents should poll their inbox
3. **Use clear handoffs** - Include context when messaging between agents
4. **Monitor the board** - Use `message_board_summary` to track coordination

## Limitations

- Agents share no state beyond explicit messages
- Each agent has its own conversation history
- Spawned agents start fresh (no memory of parent's context)
- Message delivery is asynchronous
