# Project Configuration

Configure emacs-claude-agent for specific projects.

## Overview

Project-specific configuration allows you to:

- Set custom Claude switches per project
- Define project-specific MCP tools in Elisp
- Configure tool permissions
- Customize behavior per project

## Configuration Methods

### .dir-locals.el

The standard Emacs way to configure per-directory settings:

```elisp
;; In your project's .dir-locals.el
((nil . ((claude-program-switches . ("--verbose"))
         (claude-prefer-projectile-root . t)
         (eval . (load-file "tools/project-tools.el")))))
```

### Project Tools File

Create an Elisp file with project-specific tool definitions:

```elisp
;; tools/project-tools.el
(claude-mcp-deftool project-build
  "Build the project."
  :function #'my-project-build
  :safe nil
  :needs-session-cwd t)

(defun my-project-build ()
  (compile "npm run build"))
```

Load it via `.dir-locals.el`:

```elisp
((nil . ((eval . (load-file "tools/project-tools.el")))))
```

## Data Storage

### Knowledge Base

KB entries are stored in your org-roam directory:

```
org-roam-directory/
└── kb/
    └── {project-name}/
        ├── kb-entry-1.org
        └── kb-entry-2.org
```

### TODOs

TODOs are stored as org-roam nodes:

```
org-roam-directory/
└── projects/
    └── {project-name}/
        ├── todo-feature-x.org
        └── todo-bug-fix.org
```

## Configuration Options

### Command Line Switches

```elisp
;; Add directories to Claude's allowed paths
((nil . ((claude-program-switches . ("--add-dir" "../libs" "--add-dir" "../shared")))))

;; Enable verbose mode for debugging
((nil . ((claude-program-switches . ("--verbose")))))

;; Skip permission prompts (use with caution!)
((nil . ((claude-program-switches . ("--dangerously-skip-permissions")))))
```

### Project Root

```elisp
;; Use projectile root instead of git root
((nil . ((claude-prefer-projectile-root . t))))
```

This is useful for monorepos:

```
monorepo/
├── .projectile          # Project root marker
├── backend/
│   └── .git/
└── frontend/
    └── .git/
```

With `claude-prefer-projectile-root`, Claude can access both `backend/` and `frontend/`.

### MCP Settings

```elisp
;; Disable MCP for this project
((nil . ((claude-use-mcp . nil))))

;; Disable auto-approval of read operations
((nil . ((claude-auto-allow-cli-reads . nil))))
```

## Custom MCP Tools

Define tools using `claude-mcp-deftool` in an Elisp file:

```elisp
;; tools/project-tools.el

;; Project-specific build
(claude-mcp-deftool project-build
  "Build the project."
  :function #'my-project-build
  :safe nil
  :needs-session-cwd t)

(defun my-project-build ()
  (compile "npm run build"))

;; Project-specific test runner
(claude-mcp-deftool project-test
  "Run tests with coverage."
  :function #'my-project-test
  :safe nil
  :needs-session-cwd t)

(defun my-project-test ()
  (compile "npm run test:coverage"))

;; Deploy to staging
(claude-mcp-deftool deploy-staging
  "Deploy to staging environment."
  :function #'my-deploy-staging
  :safe nil
  :needs-session-cwd t)

(defun my-deploy-staging ()
  (compile "npm run deploy:staging"))
```

## Environment-Based Configuration

### Different Configs for Different Branches

Use `.dir-locals.el` with conditionals:

```elisp
((nil . ((eval . (when (string-match "feature/" (magit-get-current-branch))
                   (setq-local claude-program-switches '("--verbose")))))))
```

### Per-Environment Tools

Create separate tool files and load conditionally:

```elisp
;; In .dir-locals.el
((nil . ((eval . (load-file
                  (if (getenv "CI")
                      "tools/ci-tools.el"
                    "tools/dev-tools.el"))))))
```

## Monorepo Configuration

### Root Configuration

```elisp
;; monorepo/.dir-locals.el
((nil . ((claude-prefer-projectile-root . t)
         (claude-program-switches . ("--add-dir" "packages/*")))))
```

### Package-Specific Configuration

```elisp
;; monorepo/packages/api/.dir-locals.el
((nil . ((eval . (load-file "../../tools/api-tools.el")))))
```

### Shared Tools

```elisp
;; monorepo/tools/common-tools.el
(claude-mcp-deftool lint-all
  "Lint all packages."
  :function #'my-lint-all
  :safe t
  :needs-session-cwd t)

(defun my-lint-all ()
  (compile "npm run lint --workspaces"))
```

## Security Considerations

### Reviewing .dir-locals.el

Emacs prompts before applying `.dir-locals.el`. Review carefully:

- Check `eval` expressions
- Verify tool file paths
- Review command switches

### Safe Defaults

```elisp
;; Allow safe variables without prompting
(setq safe-local-variable-values
      '((claude-prefer-projectile-root . t)))
```

### Marking Variables Safe

```elisp
;; In your init.el
(put 'claude-program-switches 'safe-local-variable #'listp)
```

## Examples

### Node.js Project

```elisp
;; .dir-locals.el
((nil . ((eval . (load-file "tools/node-tools.el")))))
```

```elisp
;; tools/node-tools.el
(claude-mcp-deftool npm-install
  "Install dependencies."
  :function (lambda () (compile "npm install"))
  :safe nil
  :needs-session-cwd t)

(claude-mcp-deftool npm-start
  "Start development server."
  :function (lambda () (async-shell-command "npm start"))
  :safe nil
  :needs-session-cwd t)

(claude-mcp-deftool npm-test
  "Run tests."
  :function (lambda () (compile "npm test"))
  :safe nil
  :needs-session-cwd t)
```

### Python Project

```elisp
;; tools/python-tools.el
(claude-mcp-deftool run-pytest
  "Run pytest."
  :function (lambda () (compile "pytest"))
  :safe nil
  :needs-session-cwd t)

(claude-mcp-deftool run-mypy
  "Run type checking."
  :function (lambda () (compile "mypy ."))
  :safe t
  :needs-session-cwd t)
```

### Rust Project

```elisp
;; tools/rust-tools.el
(claude-mcp-deftool cargo-build
  "Build with cargo."
  :function (lambda () (compile "cargo build"))
  :safe nil
  :needs-session-cwd t)

(claude-mcp-deftool cargo-test
  "Run cargo tests."
  :function (lambda () (compile "cargo test"))
  :safe nil
  :needs-session-cwd t)

(claude-mcp-deftool cargo-clippy
  "Run clippy lints."
  :function (lambda () (compile "cargo clippy"))
  :safe t
  :needs-session-cwd t)
```

## Troubleshooting

### Config Not Applied

1. Check `.dir-locals.el` syntax
2. Verify file is in project root
3. Run `M-x revert-buffer` to reload
4. Check `*Messages*` for errors

### Tools Not Loading

1. Ensure `claude-mcp-deftool` was evaluated
2. Check `(claude-mcp-list-tools)` for registered tools
3. Restart Claude session
4. Verify the tool file path in `.dir-locals.el`

### Permission Denied

1. Review `.dir-locals.el` prompt
2. Mark variables as safe if trusted
3. Check file permissions
