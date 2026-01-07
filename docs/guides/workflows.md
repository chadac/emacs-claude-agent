# Workflows

Common workflows and patterns for using emacs-claude-agent effectively.

## Daily Development

### Starting Your Day

1. Open your project in Emacs
2. Start a Claude session: `C-c C-e s`
3. Resume previous context: `C-c C-e r`

### Code Review with Claude

```
1. Open the file to review
2. C-c C-e x
3. "Review this code for potential issues"
4. Discuss findings with Claude
```

### Quick Bug Fix

```
1. Navigate to the buggy code
2. Select the problematic region
3. C-c c c (oneshot with line scope)
4. "Fix the null pointer issue here"
5. Review and accept the change
```

## Feature Development

### Planning

```
User: I need to add user authentication to this app

Claude: Let me understand the current codebase first.
        [Reads relevant files via MCP]

        Here's my suggested approach:
        1. Add User model
        2. Create auth endpoints
        3. Add middleware
        4. Update routes

        Should I proceed with this plan?
```

### Implementation

```
User: Yes, start with the User model

Claude: I'll create the User model.
        [Creates file via MCP]
        [Proposes commit via Magit]

        Ready for the next step?
```

### Testing

```
User: Add tests for the User model

Claude: [Creates test file]
        [Runs tests via custom tool]

        All tests pass. Here's the coverage report.
```

## Parallel Tasks with Multi-Agent

### Setup

```
User: I need to update both the API and the frontend

Claude: I'll spawn two agents for parallel work.
        [Spawns API agent]
        [Spawns frontend agent]

        API agent is working on endpoints.
        Frontend agent is updating components.
```

### Coordination

```
Claude (main): API agent reports: "Auth endpoints ready at /api/auth/*"
               Forwarding to frontend agent...

Claude (frontend): Received API spec. Updating fetch calls now.
```

### Completion

```
Claude (main): Both agents have completed.
               - API: 3 new endpoints
               - Frontend: 5 components updated

               Ready to review changes?
```

## Refactoring Workflows

### Rename Across Project

```
1. C-c c p (oneshot with project scope)
2. "Rename UserService to AuthService everywhere"
3. Agent finds and updates all references
4. Review changes
```

### Extract Function

```
1. Select code block
2. C-c c b (oneshot with buffer scope)
3. "Extract this into a function called validateInput"
4. Agent creates function and updates call site
```

### Add Documentation

```
1. Open file with undocumented functions
2. C-c c b
3. "Add comprehensive docstrings to all functions"
4. Review generated documentation
```

## Debugging Workflows

### Investigate Error

```
User: I'm getting a NullPointerException in the UserController

Claude: Let me investigate.
        [Searches codebase via MCP]
        [Reads UserController]
        [Reads related files]

        Found the issue! Line 45 doesn't check for null user.
        Here's the fix: [proposes change]
```

### Add Logging

```
1. C-c c d (oneshot with directory scope)
2. "Add debug logging to all public methods"
3. Agent adds logging statements
4. Review and approve
```

### Performance Analysis

```
User: This function is slow

Claude: Let me analyze it.
        [Reads function]
        [Checks Knowledge Base for related gotchas]

        I see two issues:
        1. N+1 query pattern
        2. Missing index

        Want me to fix the query pattern first?
```

## Documentation Workflows

### Generate API Docs

```
1. Open API module
2. C-c C-e x
3. "Generate OpenAPI documentation for these endpoints"
4. Claude produces YAML spec
```

### Update README

```
User: The README is outdated

Claude: [Reads current README]
        [Scans project structure]
        [Checks recent commits]

        Here's an updated README with:
        - New installation steps
        - Updated feature list
        - Current API examples

        [Shows proposal for review]
```

### Knowledge Base Maintenance

```
User: Record what we learned about the caching issue

Claude: [Creates KB entry]
        Title: "Redis cache invalidation timing"
        Type: gotcha
        Summary: "Must invalidate before database update"
        Files: [cache.py, models.py]
```

## Git Workflows

### Feature Branch

```
User: Create a branch for the new feature

Claude: [via Magit]
        Created branch: feature/user-auth
        Switched to new branch.

        Ready to start implementing?
```

### Commit Changes

```
User: Commit these changes

Claude: [Stages relevant files]
        [Shows diff for review]

        Proposed commit message:
        "feat: add user authentication

        - Add User model with password hashing
        - Create login/logout endpoints
        - Add JWT middleware"

        [Awaits approval]
```

### Review Before Push

```
User: Review changes before pushing

Claude: [Shows git log]
        [Shows diff against main]

        3 commits ready to push:
        1. feat: add User model
        2. feat: add auth endpoints
        3. test: add auth tests

        All tests passing. Safe to push?
```

## CI/CD Workflows

### Pre-commit Checks

```yaml
# .claude/claudemacs-tools.yaml
tools:
  pre_commit:
    description: "Run pre-commit checks"
    safe: false
    elisp: [compile, "pre-commit run --all-files"]
```

```
User: Run pre-commit

Claude: [Runs pre-commit tool]
        3 checks passed
        1 check failed: trailing whitespace

        Want me to fix the whitespace issues?
```

### Deployment

```yaml
tools:
  deploy_staging:
    description: "Deploy to staging"
    safe: false
    elisp: [compile, "npm run deploy:staging"]
```

```
User: Deploy to staging

Claude: [Runs deployment]
        Deployment started...
        [Watches for completion]
        Deployed successfully to staging.
        URL: https://staging.example.com
```

## Tips for Effective Workflows

### 1. Use the Right Scope

| Task | Recommended Scope |
|------|-------------------|
| Fix one line | Line (C-c c c) |
| Add docstrings | Buffer (C-c c b) |
| Rename in directory | Directory (C-c c d) |
| Cross-file refactor | Project (C-c c p) |

### 2. Save Before Asking

Claude uses files on disk. Always save:

```elisp
(global-auto-revert-mode t)
(auto-save-visited-mode t)
```

### 3. Review Before Accepting

Always review Claude's changes before approving, especially for:

- Multi-file changes
- Database modifications
- Deployment actions

### 4. Use Knowledge Base

```
Before: "How do we handle authentication here?"
After: Search KB, find documented pattern, apply consistently
```

### 5. Leverage Custom Tools

Create tools for repetitive tasks:

- Build commands
- Test runners
- Deployment scripts
- Code generators
