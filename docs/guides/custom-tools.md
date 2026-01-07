# Custom Tools

Create project-specific MCP tools for Claude using Elisp.

## Overview

Custom tools extend Claude's capabilities for your specific project. Define them using `claude-mcp-deftool` in Elisp and Claude can call them like built-in tools.

## Quick Start

Add to your Emacs configuration or a project-specific file:

```elisp
(claude-mcp-deftool run-tests
  "Run the project test suite."
  :function #'my-run-tests
  :safe nil)

(defun my-run-tests ()
  "Run project tests."
  (compile "npm test"))
```

That's it! Claude can now use `run_tests` in your project.

## Tool Definition

### Basic Structure

```elisp
(claude-mcp-deftool tool-name
  "Description of what this tool does."
  :function #'implementation-function
  :safe t|nil
  :needs-session-cwd t|nil
  :args ((arg-name type [:required] "description")
         ...))
```

### Required Fields

| Field | Description |
|-------|-------------|
| `tool-name` | Symbol name (dashes converted to underscores for MCP) |
| `"Description"` | Docstring explaining what the tool does |
| `:function` | The elisp function to call |

### Optional Fields

| Field | Default | Description |
|-------|---------|-------------|
| `:safe` | `nil` | When `t`, tool is read-only (can be auto-approved) |
| `:needs-session-cwd` | `nil` | When `t`, binds `default-directory` to session's working dir |
| `:args` | `nil` | List of argument definitions |

## Argument Definitions

Arguments are defined as lists: `(name type [:required] "description")`

### Types

| Type | Description |
|------|-------------|
| `string` | Text value |
| `integer` | Whole number |
| `boolean` | true/false |

### Required vs Optional

```elisp
:args ((required-arg string :required "This argument must be provided")
       (optional-arg string "This argument is optional"))
```

## Examples

### Simple Tool (No Arguments)

```elisp
(claude-mcp-deftool show-current-time
  "Display the current time."
  :function #'current-time-string
  :safe t)
```

### Tool with Arguments

```elisp
(claude-mcp-deftool greet-user
  "Greet a user by name."
  :function #'my-greet-user
  :safe t
  :args ((name string :required "Name to greet")))

(defun my-greet-user (name)
  "Greet NAME."
  (format "Hello, %s!" name))
```

### Project Builder

```elisp
(claude-mcp-deftool project-build
  "Build the project using make."
  :function #'my-project-build
  :safe nil
  :needs-session-cwd t
  :args ((target string "Make target (default: all)")))

(defun my-project-build (&optional target)
  "Build project with optional TARGET."
  (compile (if target
               (format "make %s" target)
             "make")))
```

### Test Runner

```elisp
(claude-mcp-deftool run-pytest
  "Run pytest with optional file filter."
  :function #'my-run-pytest
  :safe nil
  :needs-session-cwd t
  :args ((file-path string "Specific test file to run")
         (verbose boolean "Run with verbose output")))

(defun my-run-pytest (&optional file-path verbose)
  "Run pytest, optionally on FILE-PATH with VERBOSE output."
  (let ((cmd (concat "pytest"
                     (when verbose " -v")
                     (when file-path (format " %s" file-path)))))
    (compile cmd)))
```

### Buffer Analysis

```elisp
(claude-mcp-deftool count-buffer-lines
  "Count lines in a buffer."
  :function #'my-count-buffer-lines
  :safe t
  :args ((buffer-name string :required "Name of the buffer")))

(defun my-count-buffer-lines (buffer-name)
  "Count lines in BUFFER-NAME."
  (with-current-buffer buffer-name
    (count-lines (point-min) (point-max))))
```

### Docker Integration

```elisp
(claude-mcp-deftool docker-build
  "Build a Docker image."
  :function #'my-docker-build
  :safe nil
  :needs-session-cwd t
  :args ((image-name string :required "Name for the Docker image")
         (dockerfile string "Path to Dockerfile")))

(defun my-docker-build (image-name &optional dockerfile)
  "Build Docker image IMAGE-NAME from DOCKERFILE."
  (let ((cmd (format "docker build -t %s %s ."
                     image-name
                     (if dockerfile (format "-f %s" dockerfile) ""))))
    (compile cmd)))
```

### Database Operations

```elisp
(claude-mcp-deftool db-migrate
  "Run database migrations."
  :function #'my-db-migrate
  :safe nil
  :needs-session-cwd t)

(defun my-db-migrate ()
  "Run database migrations."
  (compile "python manage.py migrate"))

(claude-mcp-deftool db-shell
  "Open interactive database shell."
  :function #'my-db-shell
  :safe nil
  :needs-session-cwd t)

(defun my-db-shell ()
  "Open database shell."
  (term "python manage.py dbshell"))
```

## Loading Project-Specific Tools

### Via .dir-locals.el

Create a file with your tool definitions and load it via `.dir-locals.el`:

```elisp
;; In project/.dir-locals.el
((nil . ((eval . (load-file "tools/project-tools.el")))))
```

### Via Hook

```elisp
(add-hook 'claude-startup-hook
          (lambda ()
            (when (file-exists-p (expand-file-name "tools.el" claude--cwd))
              (load-file (expand-file-name "tools.el" claude--cwd)))))
```

## Managing Tools

### List Registered Tools

```elisp
(claude-mcp-list-tools)
;; Returns: ("count_buffer_lines" "docker_build" "greet_user" ...)
```

### Remove a Tool

```elisp
(claude-mcp-remove-tool "tool_name")
;; or
(claude-mcp-remove-tool 'tool-name)
```

### View Tool Definition

```elisp
(gethash "tool_name" claude-mcp-tools)
```

## Session Context

When `:needs-session-cwd t` is set, the tool function runs with `default-directory` bound to the Claude session's working directory. This is useful for:

- Running compile commands
- File operations relative to project root
- Shell commands that need correct CWD

```elisp
(claude-mcp-deftool list-project-files
  "List files in the project root."
  :function #'my-list-project-files
  :safe t
  :needs-session-cwd t)

(defun my-list-project-files ()
  "List files in current directory."
  ;; default-directory is automatically set to session CWD
  (directory-files default-directory nil "^[^.]"))
```

## Best Practices

1. **Use descriptive names** - `run-python-tests` not `rpt`
2. **Write clear docstrings** - Help Claude understand when to use it
3. **Mark safe tools correctly** - Only `:safe t` for read-only operations
4. **Use `:needs-session-cwd`** - For tools that need project context
5. **Handle optional arguments** - Use `&optional` in function signatures
6. **Return useful values** - Claude sees the return value
7. **Keep tools focused** - One tool, one purpose

## Troubleshooting

### Tool Not Available

- Ensure `claude-mcp-deftool` was evaluated
- Check `(claude-mcp-list-tools)` for registered tools
- Restart Claude session after adding tools

### Wrong Working Directory

- Add `:needs-session-cwd t` to the tool definition
- Or explicitly set `default-directory` in your function

### Argument Type Errors

- Verify argument types match function expectations
- Use `&optional` for optional arguments in function signature

### Testing Tools

Test your tool function directly:

```elisp
;; Test the implementation
(my-project-build "release")

;; Check tool is registered
(gethash "project_build" claude-mcp-tools)
```

## Claude Self-Extension

One of the powerful features of emacs-claude-agent is that Claude can extend its own capabilities during a conversation. Using the `eval` MCP tool, Claude can define new tools on the fly.

### Example: Claude Creates a Tool

If you ask Claude to help with a task that would benefit from a custom tool, it can create one:

```elisp
;; Claude evaluates this via the eval MCP tool
(claude-mcp-deftool analyze-imports
  "Analyze JavaScript imports in a file."
  :function #'my-analyze-imports
  :safe t
  :args ((file-path string :required "Path to JS file")))

(defun my-analyze-imports (file-path)
  "Extract import statements from FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let (imports)
      (goto-char (point-min))
      (while (re-search-forward "^import .+ from ['\"]\\(.+\\)['\"]" nil t)
        (push (match-string 1) imports))
      (nreverse imports))))
```

After evaluation, Claude can immediately use `analyze_imports` for the rest of the conversation.

### Use Cases

- **Project-specific helpers** - Claude creates tools tailored to your codebase
- **Workflow automation** - Build tools as needs arise during a session
- **Rapid iteration** - Test and refine tools without restarting
- **Self-improvement** - Claude enhances its own capabilities

### Reloading Modified Code

Claude can also reload elisp files after editing them:

```elisp
;; After Claude edits a tool file
(load-file "tools/project-tools.el")
```

This enables a tight feedback loop: edit code, reload, test, iterate.

## Security Notes

- Tools run with your Emacs permissions
- `:safe nil` tools can modify files and run commands
- Review tools before loading in projects
- Don't include sensitive data in tool definitions
- Claude-defined tools persist only for the session unless saved to a file
