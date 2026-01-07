# Elisp API Reference

Functions and variables for programmatic use.

## Session Management

### claude-start

Start or switch to a Claude session.

```elisp
(claude-start)
```

Starts a session in the current project directory.

---

### claude-start-resume

Start session with resume flag.

```elisp
(claude-start-resume)
```

---

### claude-start-in-directory

Start session in specific directory.

```elisp
(claude-start-in-directory "/path/to/project")
```

---

### claude-kill

Kill the active Claude session.

```elisp
(claude-kill)
```

---

### claude-session-active-p

Check if a session is active.

```elisp
(if (claude-session-active-p)
    (message "Session is running")
  (message "No active session"))
```

---

### claude-get-buffer

Get the current Claude session buffer.

```elisp
(let ((buf (claude-get-buffer)))
  (when buf
    (switch-to-buffer buf)))
```

## Sending Input

### claude-send-text

Send text to the Claude session.

```elisp
(claude-send-text "Your message here")
```

---

### claude-execute-with-context

Send request with file/line context.

```elisp
(claude-execute-with-context "Explain this function")
```

Uses current buffer, point, and region if active.

---

### claude-execute-plain

Send request without context.

```elisp
(claude-execute-plain "What is a monad?")
```

---

### claude-add-file

Add a file reference to the conversation.

```elisp
(claude-add-file "/path/to/file.py")
```

---

### claude-add-current-file

Add current buffer's file.

```elisp
(claude-add-current-file)
```

---

### claude-add-context

Add file:line reference.

```elisp
(claude-add-context)
```

Uses current buffer and point.

## Quick Responses

### claude-yes

Send approval.

```elisp
(claude-yes)
```

---

### claude-no

Send rejection.

```elisp
(claude-no)
```

## Buffer Operations

### claude-toggle

Toggle Claude buffer visibility.

```elisp
(claude-toggle)
```

---

### claude-unstick

Reset buffer tracking.

```elisp
(claude-unstick)
```

Fixes scroll and input issues.

## Oneshot Agents

### claude-oneshot-line

Start oneshot with line/region scope.

```elisp
(claude-oneshot-line)
```

Prompts for task.

---

### claude-oneshot-buffer

Start oneshot with buffer scope.

```elisp
(claude-oneshot-buffer)
```

---

### claude-oneshot-directory

Start oneshot with directory scope.

```elisp
(claude-oneshot-directory)
```

---

### claude-oneshot-project

Start oneshot with project scope.

```elisp
(claude-oneshot-project)
```

---

### claude-oneshot-with-prompt

Start oneshot with predefined prompt.

```elisp
(claude-oneshot-with-prompt "buffer" "Add docstrings to all functions")
```

Scopes: "line", "buffer", "directory", "project"

## Multi-Agent

### claude-spawn-agent

Spawn a new agent.

```elisp
(claude-spawn-agent "/path/to/dir" "agent-name")
```

Returns buffer name.

---

### claude-list-agents

List all running agents.

```elisp
(claude-list-agents)
;; Returns: (("*claude:/project*" . "/project") ...)
```

---

### claude-message-agent

Send message to another agent.

```elisp
(claude-message-agent "*claude:/other*" "Please update the API")
```

## Notifications

### claude-setup-bell-handler

Re-setup notification handler.

```elisp
(claude-setup-bell-handler)
```

Call if notifications stop working.

## Variables

### claude--cwd

Current session's working directory.

```elisp
(message "Working in: %s" claude--cwd)
```

Buffer-local in Claude buffers.

---

### claude--buffer-name

Name of the Claude buffer.

```elisp
claude--buffer-name
;; e.g., "*claudemacs:/project*"
```

---

### claude--project-root

Project root directory.

```elisp
claude--project-root
```

## Hooks

### claude-startup-hook

Run after session initialization.

```elisp
(add-hook 'claude-startup-hook
          (lambda ()
            ;; Access claude--cwd here
            (when (file-exists-p (expand-file-name "package.json" claude--cwd))
              (message "Node project detected"))))
```

## MCP Tool Functions

These can be called programmatically:

### claude-mcp-get-buffer-content

```elisp
(claude-mcp-get-buffer-content "main.py" nil nil nil nil)
```

---

### claude-mcp-list-buffers

```elisp
(claude-mcp-list-buffers)
```

---

### claude-mcp-buffer-info

```elisp
(claude-mcp-buffer-info "main.py")
```

---

### claude-mcp-search-buffer

```elisp
(claude-mcp-search-buffer "main.py" "def " nil 5 nil nil)
```

---

### claude-mcp-edit-buffer

```elisp
(claude-mcp-edit-buffer "main.py" "old" "new" nil)
```

## Tool Registration

### claude-mcp-deftool

Define a new MCP tool.

```elisp
(claude-mcp-deftool my-tool
  "Description of my tool"
  :function #'my-tool-function
  :safe t
  :args ((arg1 string :required "First argument")
         (arg2 integer "Optional second argument")))
```

---

### claude-mcp-remove-tool

Remove a tool from registry.

```elisp
(claude-mcp-remove-tool "my_tool")
```

---

### claude-mcp-export-tools

Export tools as JSON.

```elisp
(claude-mcp-export-tools)
```

Used by MCP server.

## Transient Interface

### claude-transient-menu

Open the transient menu.

```elisp
(claude-transient-menu)
```

Bind to your preferred key.

## Example: Custom Commands

### Auto-fix All Errors

```elisp
(defun my/claude-fix-all-errors ()
  "Send all flycheck errors to Claude."
  (interactive)
  (let ((errors (flycheck-overlay-errors-in (point-min) (point-max))))
    (when errors
      (claude-send-text
       (format "Fix these errors:\n%s"
               (mapconcat #'flycheck-error-message errors "\n"))))))
```

### Quick Docstring

```elisp
(defun my/claude-add-docstring ()
  "Ask Claude to add docstring to function at point."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((start (point)))
      (end-of-defun)
      (claude-oneshot-with-prompt
       "line"
       "Add a comprehensive docstring to this function"))))
```

### Session with Custom Switches

```elisp
(defun my/claude-start-verbose ()
  "Start Claude with verbose logging."
  (interactive)
  (let ((claude-program-switches '("--verbose")))
    (claude-start)))
```

### Batch File Add

```elisp
(defun my/claude-add-files (files)
  "Add multiple FILES to Claude conversation."
  (dolist (file files)
    (claude-add-file file)))

(my/claude-add-files '("src/main.py" "src/utils.py" "tests/test_main.py"))
```
