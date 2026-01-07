# Architecture

Technical overview of emacs-claude-agent's design.

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                         User                                 │
│                          │                                   │
│                    ┌─────┴─────┐                            │
│                    │  Emacs    │                            │
│                    │  (UI)     │                            │
│                    └─────┬─────┘                            │
│                          │                                   │
│          ┌───────────────┼───────────────┐                  │
│          │               │               │                  │
│    ┌─────┴─────┐   ┌─────┴─────┐   ┌─────┴─────┐          │
│    │ claude.el │   │ claude-   │   │ claude-   │          │
│    │ (core)    │   │ agent.el  │   │ oneshot   │          │
│    └─────┬─────┘   │ (buffer)  │   │ (bg agent)│          │
│          │         └─────┬─────┘   └─────┬─────┘          │
│          │               │               │                  │
│          └───────────────┼───────────────┘                  │
│                          │                                   │
│                    ┌─────┴─────┐                            │
│                    │ claude-   │                            │
│                    │ mcp.el    │                            │
│                    │ (tools)   │                            │
│                    └─────┬─────┘                            │
│                          │                                   │
│                    ┌─────┴─────┐                            │
│                    │ MCP       │                            │
│                    │ Server    │                            │
│                    │ (Python)  │                            │
│                    └─────┬─────┘                            │
└──────────────────────────┼──────────────────────────────────┘
                           │
                     ┌─────┴─────┐
                     │ Claude    │
                     │ Code CLI  │
                     └─────┬─────┘
                           │
                     ┌─────┴─────┐
                     │ Anthropic │
                     │ API       │
                     └───────────┘
```

## Component Overview

### claude.el (Core)

The main entry point that:
- Defines configuration variables
- Loads dependencies
- Provides public API functions
- Manages session state

### claude-agent.el (Buffer Interface)

Provides the agent buffer UI:
- Renders conversation history
- Handles user input
- Displays status/progress
- Manages buffer lifecycle

### claude-mcp.el (MCP Tools)

Defines MCP tools:
- Tool registration system
- Buffer operations
- File operations
- Elisp execution
- Tool export for Python server

### claude-mcp-process.el (Server Management)

Manages the MCP server process:
- Server startup/shutdown
- Process communication
- Error handling
- Restart logic

### claude-oneshot.el (Background Agents)

Implements oneshot agents:
- Scope-based permissions
- Visual target highlighting
- Background execution
- Auto-cleanup

### claude-sessions.el (Session Management)

Handles session lifecycle:
- Buffer naming
- Workspace integration
- Session persistence

### emacs_mcp/ (Python MCP Server)

MCP protocol implementation:
- Tool dispatch
- Context handling
- Communication with Claude Code

### claude_agent/ (Python Agent Wrapper)

Wraps the Claude SDK:
- Session management
- Streaming handling
- Error recovery

## Data Flow

### Starting a Session

```
1. User calls (claude-start)
2. claude.el determines project root
3. claude-sessions.el creates buffer
4. claude-mcp-process.el starts MCP server
5. Python agent process spawned
6. Claude Code CLI invoked with MCP config
7. Agent buffer displays conversation
```

### Tool Execution

```
1. Claude calls MCP tool (e.g., get_buffer_content)
2. Python MCP server receives request
3. Server calls Emacs via emacsclient
4. Elisp function executes
5. Result returned to Python
6. Python returns to Claude
7. Claude continues reasoning
```

### Oneshot Agent Flow

```
1. User selects region and invokes oneshot
2. claude-oneshot.el creates overlay
3. Background agent spawned
4. Agent works with constrained tools
5. Agent calls done() when finished
6. Overlay removed, buffer cleaned up
```

## MCP Tool System

### Tool Registration

```elisp
(claude-mcp-deftool tool-name
  "Description"
  :function #'implementation-function
  :safe t/nil
  :needs-session-cwd t/nil
  :args ((arg1 type :required "desc")
         (arg2 type "desc")))
```

### Tool Export

Tools are exported as JSON for the Python server:

```json
{
  "tool_name": {
    "description": "Description",
    "function": "implementation-function",
    "safe": true,
    "args": {
      "arg1": {
        "type": "string",
        "required": true,
        "description": "desc"
      }
    }
  }
}
```

### Context System

Tools can run in different contexts:

1. **File context**: Opens file, runs in buffer
2. **Buffer context**: Uses existing buffer
3. **Directory context**: Sets default-directory
4. **Auto context**: Infers from argument names

## Multi-Agent Architecture

### Agent Identification

Each agent has a unique buffer name:

```
*claudemacs:/project/path*
*claudemacs:/project/path:name*
```

### Message Passing

```elisp
;; Message queue stored in hash table
claude-mcp--message-queues
;; Key: buffer-name, Value: list of messages
```

### Agent Coordination

```
Agent A                     Agent B
   │                           │
   ├── message_agent(B, msg) ──┤
   │                           │
   │                     check_messages()
   │                           │
   │                     process message
   │                           │
   │◄── message_agent(A, reply)│
   │                           │
check_messages()               │
   │                           │
```

## Session State

### Buffer-Local Variables

```elisp
claude--cwd           ; Working directory
claude--buffer-name   ; Buffer name
claude--project-root  ; Project root
claude--session-id    ; Unique session ID
```

### Global State

```elisp
claude-mcp-tools          ; Tool registry
claude-mcp--server-process ; MCP server process
claude-mcp--message-queues ; Inter-agent messages
```

## Extension Points

### Adding Tools

1. Use `claude-mcp-deftool`
2. Implement the function
3. Tools automatically available

### Claude Self-Extension

Claude can extend its own capabilities during a conversation:

```elisp
;; Claude uses the eval tool to define new tools
(claude-mcp-deftool my-helper
  "A helper tool defined mid-conversation."
  :function #'my-helper-impl
  :safe t)

(defun my-helper-impl ()
  "Implementation.")
```

This enables:
- Rapid prototyping of new tools
- Project-specific automation
- Self-improving agent capabilities

### Custom Prompts

Use interactive prompt tools:
- `prompt_choice`
- `multiselect`
- `confirm`
- `show_proposal`

### Hooks

- `claude-startup-hook` - After session init

## Security Model

### Permission Scoping

Oneshot agents have constrained permissions:
- Line scope: Only target line/region
- Buffer scope: Only current buffer
- Directory scope: Only current directory
- Project scope: Any project file

### Safe vs Unsafe Tools

- Safe tools: Read-only, can be auto-approved
- Unsafe tools: Modify state, require approval

### Context Isolation

Each agent has its own:
- Working directory
- Conversation history
- Tool permissions

## Performance Considerations

### Large Buffers

- Use `tail_lines` or line ranges
- Clear terminal buffers with `clear_buffer`
- Watch functions use async polling

### Many Files

- Project-level operations batch work
- MCP server caches file operations

### Long Sessions

- Scrollback configurable
- Terminal buffer clearable
- Session state minimal

## Testing

### Unit Tests

```elisp
;; In test/claude-test.el
(ert-deftest claude-test-example ()
  (should (equal expected actual)))
```

### Integration Tests

```elisp
;; Test full tool execution
(ert-deftest claude-mcp-test-buffer-ops ()
  (with-temp-buffer
    (insert "test content")
    (should (string-match "test"
                          (claude-mcp-get-buffer-content
                           (buffer-name))))))
```

## Future Directions

- WebSocket MCP transport
- Tool marketplace
- Session replay
- Collaborative editing
- Performance analytics
