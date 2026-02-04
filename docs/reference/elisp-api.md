# Elisp API Reference

Functions and variables for programmatic use.

## Session Management

### claude-run

Start or switch to a Claude session.

```elisp
(claude-run)
```

Starts a session in the current project directory. With a prefix argument, prompts for a session slug.

---

### claude-resume

Start session with resume flag.

```elisp
(claude-resume)
```

---

### claude-kill

Kill the active Claude session.

```elisp
(claude-kill)
```

---

### claude-toggle-buffer

Toggle Claude buffer visibility.

```elisp
(claude-toggle-buffer)
```

## Sending Input

### claude-execute-request

Send request with file/line context.

```elisp
(claude-execute-request)
```

Uses current buffer, point, and region if active to provide context.

---

### claude-ask-without-context

Send request without file context.

```elisp
(claude-ask-without-context)
```

---

### claude-add-file-reference

Add a file reference to the conversation.

```elisp
(claude-add-file-reference)
```

Prompts for file path interactively.

---

### claude-add-current-file-reference

Add current buffer's file.

```elisp
(claude-add-current-file-reference)
```

---

### claude-add-context

Add file:line reference at point.

```elisp
(claude-add-context)
```

Uses current buffer and point position.

## Quick Responses

### claude-send-yes

Send approval ("Yes").

```elisp
(claude-send-yes)
```

---

### claude-send-no

Send rejection ("No").

```elisp
(claude-send-no)
```

## Quick Actions

### claude-fix-error-at-point

Send flycheck/flymake error at point to Claude for fixing.

```elisp
(claude-fix-error-at-point)
```

---

### claude-implement-comment

Implement a CLAUDE: comment at point.

```elisp
(claude-implement-comment)
```

---

### claude-generate-commit-message

Generate a commit message from staged changes.

```elisp
(claude-generate-commit-message)
```

## Oneshot Agents

### claude-oneshot-line-or-region

Start oneshot with line/region scope.

```elisp
(claude-oneshot-line-or-region)
```

Prompts for task description.

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

## Multi-Agent

### claude-spawn-agent

Spawn a new Claude agent in a directory.

```elisp
(claude-spawn-agent "/path/to/dir" "agent-name")
```

Returns buffer name. Available as `C-c s` in Claude buffers.

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

Key options:

- `:function` — The elisp function to call
- `:safe` — If `t`, can be auto-approved (read-only operations)
- `:needs-session-cwd` — If `t`, receives session working directory
- `:args` — Parameter definitions with types

---

### claude-mcp-remove-tool

Remove a tool from the registry.

```elisp
(claude-mcp-remove-tool "my_tool")
```

---

### claude-mcp-export-tools

Export all registered tools as JSON (used by the Python MCP server).

```elisp
(claude-mcp-export-tools)
```

## Transient Menu Extension

### claude-transient-register-agent-item

Add a custom item to the agent buffer transient menu.

```elisp
(claude-transient-register-agent-item
 "z" "My Command" #'my-claude-command)
```

---

### claude-transient-register-pair-item

Add a custom item to the pair programming transient menu.

```elisp
(claude-transient-register-pair-item
 "z" "My Pair Action" #'my-pair-action)
```

## Example: Custom Commands

### Auto-fix All Errors

```elisp
(defun my/claude-fix-all-errors ()
  "Send all flycheck errors to Claude."
  (interactive)
  (let ((errors (flycheck-overlay-errors-in (point-min) (point-max))))
    (when errors
      (claude-execute-request
       (format "Fix these errors:\n%s"
               (mapconcat #'flycheck-error-message errors "\n"))))))
```

### Quick Docstring

```elisp
(defun my/claude-add-docstring ()
  "Ask Claude to add docstring to function at point."
  (interactive)
  (claude-oneshot-line-or-region))
```

### Session with Custom Switches

```elisp
(defun my/claude-start-verbose ()
  "Start Claude with verbose logging."
  (interactive)
  (let ((claude-program-switches '("--verbose")))
    (claude-run)))
```
