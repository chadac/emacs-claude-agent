# Knowledge Base

Persistent storage for project-specific learnings and context.

## Overview

The Knowledge Base (KB) allows Claude to:

- Store learnings about your codebase
- Record gotchas and edge cases
- Document architectural decisions
- Save reusable patterns
- Link related knowledge

## Entry Types

### Gotchas

Bugs, edge cases, and things to watch out for:

```python
mcp__emacs__kb_create(
    title="Timezone handling in user timestamps",
    kb_type="gotcha",
    summary="User timestamps are stored in UTC but displayed in local time. Always convert when comparing.",
    files=["src/models/user.py", "src/utils/time.py"],
    concepts=["timezone", "datetime", "utc"]
)
```

### Architecture

Design decisions and system structure:

```python
mcp__emacs__kb_create(
    title="Event sourcing pattern for orders",
    kb_type="architecture",
    summary="Orders use event sourcing. Never modify order state directly - emit events instead.",
    details="""
## Event Types
- OrderCreated
- OrderUpdated
- OrderCancelled

## Projections
Events are projected into OrderSummary for queries.
    """,
    modules=["orders", "events"]
)
```

### Patterns

Reusable code patterns:

```python
mcp__emacs__kb_create(
    title="Repository pattern for data access",
    kb_type="pattern",
    summary="All database access goes through repository classes.",
    details="""
```python
class UserRepository:
    def find_by_id(self, id: int) -> User:
        ...
    def save(self, user: User) -> None:
        ...
```
    """,
    concepts=["repository", "database", "data-access"]
)
```

### References

Links to documentation and resources:

```python
mcp__emacs__kb_create(
    title="API documentation",
    kb_type="reference",
    summary="Official API docs at https://api.example.com/docs",
    concepts=["api", "documentation"]
)
```

## Searching Knowledge

### By Text

```python
mcp__emacs__kb_search(query="timezone")
# Finds entries mentioning "timezone"
```

### By File

```python
mcp__emacs__kb_search(file="src/models/user.py")
# Finds entries related to this file
```

### By Module

```python
mcp__emacs__kb_search(module="orders")
# Finds entries about the orders module
```

### By Concept

```python
mcp__emacs__kb_search(concept="authentication")
# Finds entries tagged with this concept
```

### By Type

```python
mcp__emacs__kb_search(kb_type="gotcha")
# Finds all gotcha entries
```

### Combined Search

```python
mcp__emacs__kb_search(
    concept="database",
    kb_type="pattern",
    limit=5
)
```

## Retrieving Entries

### Get Full Content

```python
mcp__emacs__kb_get(kb_id="kb-001")
# or by title
mcp__emacs__kb_get(kb_id="Timezone handling in user timestamps")
```

### Include Related Entries

```python
mcp__emacs__kb_get(
    kb_id="kb-001",
    include_related=True
)
# Also returns summaries of linked entries
```

## Updating Knowledge

### Append Details

```python
mcp__emacs__kb_update(
    kb_id="kb-001",
    append_details="\n## Update 2024-01-15\nAlso affects the reporting module."
)
```

### Add Associations

```python
mcp__emacs__kb_update(
    kb_id="kb-001",
    add_files=["src/reports/time_report.py"],
    add_concepts=["reporting"],
    add_related_ids=["kb-015"]
)
```

## Listing All Knowledge

```python
mcp__emacs__kb_list()
# Returns all entries for current project

mcp__emacs__kb_list(kb_type="architecture")
# Filter by type

mcp__emacs__kb_list(project="*")
# All entries across all projects
```

## Project Scoping

Knowledge is scoped to projects by default:

- Searches only return entries for current project
- Creating entries associates them with current project
- Use `project="*"` to search across all projects

## Use Cases

### Before Editing a File

```python
# Check for relevant knowledge
kb = mcp__emacs__kb_search(file="src/models/user.py")
# Review any gotchas or patterns before making changes
```

### After Discovering an Issue

```python
# Record the learning
mcp__emacs__kb_create(
    title="Cache invalidation race condition",
    kb_type="gotcha",
    summary="Must invalidate cache BEFORE updating database to avoid stale reads",
    files=["src/cache.py", "src/models/product.py"]
)
```

### During Code Review

```python
# Check for architectural guidelines
arch = mcp__emacs__kb_search(
    kb_type="architecture",
    module="authentication"
)
# Ensure changes align with design decisions
```

## Storage

Knowledge is stored in the project's `.claude/` directory:

```
.claude/
└── kb/
    ├── kb-001.org
    ├── kb-002.org
    └── ...
```

Entries are org-mode files for easy manual editing.

## Tips

1. **Search before coding** - Check KB before modifying unfamiliar code
2. **Record gotchas immediately** - When you hit an edge case, document it
3. **Link related entries** - Build a knowledge graph
4. **Use descriptive titles** - Makes searching easier
5. **Tag with concepts** - Enables cross-cutting queries
6. **Include code examples** - Patterns are more useful with examples
