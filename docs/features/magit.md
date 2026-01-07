# Magit Integration

Git operations through Emacs's Magit interface.

## Overview

emacs-claude-agent integrates with Magit to provide:

- Status and diff viewing
- File staging/unstaging
- Commit proposals for user approval
- Full git history access

## Available Tools

### Status

View current git status:

```python
mcp__emacs__magit_status()
# Returns: branch name, staged files, unstaged files, untracked files
```

Example response:

```json
{
  "branch": "feature/new-feature",
  "staged": ["src/main.py"],
  "unstaged": ["src/utils.py", "tests/test_main.py"],
  "untracked": ["notes.txt"]
}
```

### Staging Files

Stage files for commit:

```python
mcp__emacs__magit_stage(
    files=["src/main.py", "src/utils.py"]
)
```

### Unstaging Files

Remove files from staging:

```python
mcp__emacs__magit_unstage(
    files=["src/utils.py"]
)
```

### Viewing Diffs

Get diff output:

```python
# All unstaged changes
mcp__emacs__magit_diff()

# Staged changes only
mcp__emacs__magit_diff(staged=True)

# Specific file
mcp__emacs__magit_diff(file="src/main.py")
```

### Commit History

View recent commits:

```python
mcp__emacs__magit_log(count=10)
```

Returns commit hashes, authors, dates, and messages.

### Proposing Commits

Claude doesn't commit directly. Instead, it proposes commits:

```python
mcp__emacs__magit_commit_propose(
    message="Add user authentication\n\nImplements JWT-based auth with refresh tokens."
)
```

This creates a proposal that you must approve manually.

### Checking Proposal Status

```python
mcp__emacs__magit_commit_status()
# Returns whether there's a pending proposal
```

## Commit Workflow

### 1. Claude Makes Changes

Claude edits files using MCP tools.

### 2. Claude Stages Files

```python
mcp__emacs__magit_stage(files=["src/auth.py", "tests/test_auth.py"])
```

### 3. Claude Proposes Commit

```python
mcp__emacs__magit_commit_propose(
    message="feat: add JWT authentication\n\n- Add login endpoint\n- Add token refresh\n- Add tests"
)
```

### 4. User Reviews and Approves

You see the proposal in Emacs and can:

- Accept the commit
- Modify the message
- Reject the proposal

### 5. Commit is Created

Only after your approval is the actual commit created.

!!! important "GPG Signing"
    If you have GPG signing enabled, your signature is used - not Claude's. This ensures commit authorship is properly attributed.

## Working Directory

All git operations use the session's working directory by default. You can specify a different directory:

```python
mcp__emacs__magit_status(directory="/path/to/other/repo")
```

## Example Workflow

```python
# 1. Check current status
status = mcp__emacs__magit_status()
print(f"On branch: {status['branch']}")
print(f"Unstaged: {status['unstaged']}")

# 2. Review changes
diff = mcp__emacs__magit_diff()

# 3. Stage relevant files
mcp__emacs__magit_stage(files=["src/new_feature.py"])

# 4. View staged diff
staged_diff = mcp__emacs__magit_diff(staged=True)

# 5. Propose commit
mcp__emacs__magit_commit_propose(
    message="feat: implement new feature\n\nAdds the new feature as discussed."
)

# 6. Check if user approved
status = mcp__emacs__magit_commit_status()
if not status['pending']:
    print("Commit was approved and created!")
```

## Benefits Over Direct Git

| Aspect | Direct Git | Magit Integration |
|--------|-----------|-------------------|
| Approval | Automatic | User must approve |
| Signing | Claude's key | Your GPG key |
| Review | After commit | Before commit |
| Visibility | In terminal | In Emacs |
| Undo | git reset | Reject proposal |

## Configuration

The Magit integration uses your existing Magit configuration. No additional setup is needed beyond having Magit installed.

```elisp
;; Optional: Customize commit proposal appearance
(setq magit-commit-show-diff t)
```

## Tips

1. **Review proposals carefully** - Claude can't commit without your approval
2. **Use conventional commits** - Ask Claude to follow your commit message style
3. **Stage incrementally** - Stage related changes together
4. **Check the diff** - Always review what's being committed
5. **Use with caution on main** - Consider working on branches
