# Claude.el Integration

You are running inside Emacs via claude.el. The MCP server provides enhanced Emacs integration.

## File Editing - CRITICAL

**⚠️ DO NOT USE the standard `Edit` or `Write` tools. They bypass Emacs and break the pair programming experience.**

**ALWAYS use the Emacs MCP lock/write workflow instead:**

### Workflow: lock-region → write-region

1. **Read the file** with =mcp__emacs__read_file= to see content with line numbers
2. **Lock the region** with =mcp__emacs__lock_region= specifying buffer name and line range
   - This highlights the region in Emacs and prevents user edits
   - Returns a lock ID for tracking
3. **Write new content** with =mcp__emacs__write_region= to replace the locked region
   - Auto-saves if the buffer was unmodified before locking
   - Shows a green flash animation so the user can see the change

### Why This Matters

- The user can see exactly what you're editing in real-time
- Watch mode (=C-c c w=) lets users follow your edits across files
- Prevents conflicts - locked regions can't be edited by the user
- Visual feedback with highlighted regions and lock labels

### Why NOT Standard Edit/Write Tools

The standard `Edit` and `Write` tools bypass Emacs entirely:
- **No visual feedback** - user can't see what you're changing
- **No lock protection** - user might edit the same region, causing conflicts
- **Buffer out of sync** - Emacs buffer won't reflect changes until revert
- **Watch mode broken** - can't track edits across files
- **Poor pair programming** - defeats the purpose of IDE integration

**Bottom line**: If you're editing a file, use `lock_region` → `write_region`. Always.

### Example

#+begin_example
# 1. Read the file
mcp__emacs__read_file(file_path="/path/to/file.py")

# 2. Lock lines 10-15
mcp__emacs__lock_region(buffer_name="file.py", start_line=10, end_line=15)

# 3. Write new content (include trailing newline if replacing whole lines)
mcp__emacs__write_region(buffer_name="file.py", content="new content here\n")
#+end_example

### Multiple Edits

You can lock multiple non-overlapping regions in the same buffer, or regions in different buffers simultaneously. Each lock has a unique ID.

If you need to cancel an edit, use =mcp__emacs__unlock_region= to release the lock without changes.

## Bash Execution

**Prefer `mcp__emacs__bash` over the standard Bash tool** for running shell commands. Benefits:
- Output is visible in Emacs (in an eat terminal buffer)
- User can see command execution in real-time
- Better integration with Emacs workflow

Use the standard Bash tool only when:
- You need specific timeout handling beyond what MCP bash provides
- Running very long background processes

## Notes System (Org-Mode)

The notes MCP tools store persistent notes in `.claude/claude.el-notes.org` (org-mode format). Use these tools to:

- **Track context and progress**: Record what you've learned about the codebase, decisions made, and work completed
- **Organize TODOs**: Create structured task lists with `notes_add_todo_item`
- **Document code**: Use `notes_add_documentation` for file/API docs
- **Record concepts**: Use `notes_add_concept` for architecture and patterns
- **Track tools**: Use `notes_add_tool` for useful commands and scripts
- **Summarize sessions**: Use `notes_add_summary` to record what was accomplished

### Structured Sections

The notes file is organized into standard sections. Use section-specific tools for database-like access:

| Section | Query Tool | Add Tool | Purpose |
|---------|-----------|----------|---------|
| SUMMARIES | `notes_get_summaries` | `notes_add_summary` | Session summaries, work history |
| TODOS | `notes_get_todos` | `notes_add_todo_item` | Tasks, bugs, enhancements |
| CONCEPTS | `notes_get_concepts` | `notes_add_concept` | Architecture, patterns, key understanding |
| TOOLS | `notes_get_tools` | `notes_add_tool` | Commands, scripts, project helpers |
| DOCUMENTATION | `notes_get_documentation` | `notes_add_documentation` | File/directory docs, API notes |
| ARCHIVE | - | - | Completed/archived items |

### Query Tools

- `notes_list_sections` - See all sections with item counts
- `notes_query_by_tag` - Find entries by tag across sections
- `notes_get_entry` - Get full content of a specific entry
- `notes_get_recent` - Find recently added items

### Recommended Usage Patterns

1. **Starting a session**: Use `notes_list_sections` to see what context exists, then query relevant sections
2. **Learning about code**: Add to CONCEPTS when you understand important patterns or architecture
3. **Modifying files**: Update DOCUMENTATION for any files you significantly change (see below)
4. **Finding useful commands**: Add to TOOLS when you discover helpful commands for the project
5. **Completing work**: Add to SUMMARIES at end of significant work sessions
6. **Tracking issues**: Add to TODOS for bugs found or enhancements identified

### Keeping Documentation Updated

**IMPORTANT**: When you make significant changes to files in this codebase, update the DOCUMENTATION section with relevant information:

```
notes_add_documentation(
  title="filename.el",
  body="Description of what the file does, key functions, and any important notes",
  tags=":elisp:category:",
  file_path="/full/path/to/filename.el"
)
```

For this project (claude.el), key files to document:
- `claude.el.el` - Main entry point, session management
- `claude.el-ai.el` - MCP tool implementations (buffer ops, watching, agents)
- `claude.el-ai-notes.el` - Structured org-mode notes system
- `claude.el-ai-messaging.el` - Inter-agent messaging
- `claude.el-ai-magit.el` - Magit section querying
- `claude.el_mcp/` - Python MCP server

### Tag Guidelines

- Tags must match `[[:alnum:]_@#%]+` (no hyphens allowed)
- Use underscores instead: `:package_manager:` not `:package-manager:`
- Common tags: `:bug:`, `:enhancement:`, `:elisp:`, `:python:`, `:claude.el:`
- Require 2+ spaces before tags in org headings for recognition

### Org-Mode Formatting Tips

- Use `*` for headings (more stars = deeper nesting)
- Use `- [ ]` for checkboxes, `- [X]` for checked
- Use `TODO`/`DONE` keywords after heading stars
- Use `:tag:` syntax at end of headings for tags (with 2+ spaces before)
- Use `#+begin_src lang ... #+end_src` for code blocks
- Use `:PROPERTIES:` drawer for metadata
- Timestamps: `<2024-01-15 Mon>` (active) or `[2024-01-15 Mon]` (inactive)
