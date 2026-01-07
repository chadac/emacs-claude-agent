# MCP Tools Reference

Complete reference for all built-in MCP tools.

## Buffer Operations

### get_buffer_content

Read content from an Emacs buffer.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `buffer_name` | string | Yes | Name of the buffer |
| `head_lines` | integer | No | Get only first N lines |
| `tail_lines` | integer | No | Get only last N lines |
| `start_line` | integer | No | Start line (1-indexed) |
| `end_line` | integer | No | End line (1-indexed) |

**Returns:** Buffer content with line numbers.

**Example:**

```python
mcp__emacs__get_buffer_content(buffer_name="main.py")
mcp__emacs__get_buffer_content(buffer_name="main.py", start_line=10, end_line=50)
mcp__emacs__get_buffer_content(buffer_name="*compilation*", tail_lines=20)
```

---

### list_buffers

List all open buffers in Emacs.

**Parameters:** None

**Returns:** List of buffer names with metadata.

**Example:**

```python
mcp__emacs__list_buffers()
```

---

### buffer_info

Get detailed information about a buffer.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `buffer_name` | string | Yes | Name of the buffer |

**Returns:** File path, size, major mode, cursor position, etc.

**Example:**

```python
mcp__emacs__buffer_info(buffer_name="main.py")
```

---

### search_buffer

Search for a pattern in a buffer.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `buffer_name` | string | Yes | Buffer to search |
| `pattern` | string | Yes | Regex pattern |
| `context_before` | integer | No | Lines before match |
| `context_after` | integer | No | Lines after match |
| `case_insensitive` | boolean | No | Ignore case |
| `limit` | integer | No | Max matches |

**Returns:** Matches with context lines.

**Example:**

```python
mcp__emacs__search_buffer(
    buffer_name="main.py",
    pattern="def\\s+\\w+",
    context_after=5
)
```

---

### get_region

Get content from a specific region.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `buffer_name` | string | Yes | Name of the buffer |
| `start` | integer | Yes | Start position (1-indexed) |
| `end` | integer | Yes | End position (1-indexed) |

**Returns:** Content between positions.

---

### edit_buffer

Modify buffer content by replacing text.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `buffer_name` | string | Yes | Buffer to edit |
| `old_string` | string | Yes | Text to replace |
| `new_string` | string | Yes | Replacement text |
| `replace_all` | boolean | No | Replace all occurrences |

**Returns:** Success status and diff.

**Example:**

```python
mcp__emacs__edit_buffer(
    buffer_name="main.py",
    old_string="def old_name(",
    new_string="def new_name("
)
```

---

### write_buffer

Create or overwrite a buffer with content.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `buffer_name` | string | Yes | Name for the buffer |
| `content` | string | Yes | Content to write |
| `mode` | string | No | Major mode to apply |

**Returns:** Success status.

---

### clear_buffer

Clear terminal buffer content.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `buffer_name` | string | Yes | Buffer to clear |

**Returns:** Success status.

## File Operations

### read_file

Read a file with IDE diagnostics.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `file_path` | string | Yes | Path to file |
| `offset` | integer | No | Start line (1-indexed) |
| `limit` | integer | No | Number of lines |

**Returns:** File content with diagnostics.

---

### edit_file

Edit a file by replacing text.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `file_path` | string | Yes | Path to file |
| `old_string` | string | Yes | Text to replace |
| `new_string` | string | Yes | Replacement text |
| `replace_all` | boolean | No | Replace all occurrences |

**Returns:** Success status and diff.

## Elisp Execution

### eval

Execute an Emacs Lisp expression.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `expression` | string | Yes | Elisp to evaluate |

**Returns:** Expression result.

**Example:**

```python
mcp__emacs__eval(expression="(buffer-name)")
mcp__emacs__eval(expression="(+ 1 2 3)")
```

## Watch Functions

### watch_buffer

Wait for buffer to stabilize (stop changing).

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `buffer_name` | string | Yes | Buffer to watch |
| `stable_time` | number | No | Seconds of no change (default: 0.5) |
| `timeout` | integer | No | Max wait seconds (default: 30) |

**Returns:** "stabilized" or "timeout"

---

### watch_for_pattern

Wait for regex pattern to appear.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `buffer_name` | string | Yes | Buffer to watch |
| `pattern` | string | Yes | Regex pattern |
| `timeout` | integer | No | Max wait seconds (default: 30) |

**Returns:** Match info or null on timeout.

---

### watch_for_change

Wait for any buffer change.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `buffer_name` | string | Yes | Buffer to watch |
| `timeout` | integer | No | Max wait seconds (default: 30) |

**Returns:** "changed" or "timeout"

---

### send_and_watch

Send input and wait for completion.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `buffer_name` | string | Yes | Buffer name |
| `input` | string | Yes | Text/command to send |
| `done_pattern` | string | No | Completion regex |
| `timeout` | integer | No | Max wait seconds (default: 30) |

**Returns:** New content after input.

## Interactive Prompts

### prompt_choice

Present numbered choices to user.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `prompt` | string | Yes | Question to display |
| `options` | array | Yes | Choice strings |
| `include_other` | boolean | No | Allow custom input |

**Returns:** Selected option or "cancelled"

---

### multiselect

Present checkboxes for multi-selection.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `prompt` | string | Yes | Question to display |
| `options` | string | Yes | Newline-separated options |

**Returns:** Array of selected items or "cancelled"

---

### confirm

Yes/no confirmation dialog.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `prompt` | string | Yes | Confirmation question |

**Returns:** "yes", "no", or "cancelled"

---

### pick_file

File picker for project files.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `prompt` | string | No | Prompt text |
| `directory` | string | No | Starting directory |

**Returns:** Selected file path or "cancelled"

---

### pick_directory

Directory picker.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `prompt` | string | No | Prompt text |
| `directory` | string | No | Starting directory |

**Returns:** Selected directory or "cancelled"

---

### show_proposal

Show proposal for review and approval.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `title` | string | Yes | Proposal title |
| `content` | string | Yes | Content to review |
| `mode` | string | No | Major mode for highlighting |

**Returns:** "ACCEPTED\n<content>", "REJECTED", or "REJECTED_WITH_CHANGES\n<diff>"

## Progress Tracking

### progress_start

Start a progress indicator.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `message` | string | Yes | Progress message |
| `id` | string | No | Custom identifier |

**Returns:** Progress ID

---

### progress_update

Update progress indicator.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `id` | string | Yes | Progress ID |
| `message` | string | Yes | New message |
| `percent` | number | No | Progress 0-100 |

---

### progress_stop

Stop progress indicator.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `id` | string | Yes | Progress ID |
| `final_message` | string | No | Completion message |

## Git/Magit

### magit_status

Get git status.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `directory` | string | No | Repository directory |

**Returns:** Branch, staged, unstaged, untracked files.

---

### magit_stage

Stage files for commit.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `files` | array | Yes | File paths to stage |
| `directory` | string | No | Repository directory |

---

### magit_unstage

Unstage files.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `files` | array | Yes | File paths to unstage |
| `directory` | string | No | Repository directory |

---

### magit_diff

Get diff output.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `file` | string | No | Specific file |
| `staged` | boolean | No | Show staged diff |
| `directory` | string | No | Repository directory |

---

### magit_log

Get commit history.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `count` | integer | No | Number of entries (default: 5) |
| `directory` | string | No | Repository directory |

---

### magit_commit_propose

Propose a commit for approval.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `message` | string | Yes | Commit message |
| `directory` | string | No | Repository directory |

---

### magit_commit_status

Check for pending commit proposal.

**Parameters:** None

**Returns:** Pending proposal status.

## Multi-Agent

### spawn_agent

Spawn a new Claude agent.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `directory` | string | Yes | Working directory |
| `agent_name` | string | No | Agent identifier |

**Returns:** Buffer name for monitoring.

---

### list_agents

List all running agents.

**Parameters:** None

**Returns:** Array of (buffer, directory) pairs.

---

### message_agent

Send message to another agent.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `buffer_name` | string | Yes | Target agent buffer |
| `message` | string | Yes | Message to send |
| `from_buffer` | string | No | Sender buffer |

---

### check_messages

Check queued messages.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `buffer_name` | string | Yes | Agent buffer |
| `clear` | boolean | No | Clear after reading |

---

### message_board_summary

Get message activity summary.

**Parameters:** None

**Returns:** Message counts by sender/recipient.

---

### whoami

Get current agent identity.

**Parameters:** None

**Returns:** Buffer name.

## Knowledge Base

### kb_create

Create a new KB entry.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `title` | string | Yes | Entry title |
| `kb_type` | string | Yes | gotcha/architecture/pattern/reference |
| `summary` | string | Yes | One-paragraph summary |
| `details` | string | No | Detailed explanation |
| `files` | array | No | Related file paths |
| `modules` | array | No | Related modules |
| `concepts` | array | No | Concept keywords |
| `related_ids` | array | No | Linked KB IDs |

---

### kb_search

Search KB entries.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `query` | string | No | Text search |
| `file` | string | No | Find by file |
| `module` | string | No | Find by module |
| `concept` | string | No | Find by concept |
| `kb_type` | string | No | Filter by type |
| `limit` | integer | No | Max results |
| `project` | string | No | Project scope (* for all) |

---

### kb_get

Get full KB entry content.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `kb_id` | string | Yes | Entry ID or title |
| `include_related` | boolean | No | Fetch linked entries |

---

### kb_update

Update a KB entry.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `kb_id` | string | Yes | Entry ID or title |
| `append_details` | string | No | Text to append |
| `add_files` | array | No | Files to add |
| `add_modules` | array | No | Modules to add |
| `add_concepts` | array | No | Concepts to add |
| `add_related_ids` | array | No | Links to add |

---

### kb_list

List all KB entries.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `kb_type` | string | No | Filter by type |
| `limit` | integer | No | Max entries |
| `project` | string | No | Project scope |

## Session Management

### restart_session

Restart MCP server and continue.

**Parameters:** None

---

### reload_file

Reload elisp files.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `file_path` | string | No | Single file |
| `file_paths` | array | No | Multiple files |

## Oneshot Tools

### done

Signal oneshot task completion.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `message` | string | No | Completion message |

---

### update_target

Update oneshot target highlighting.

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `file_path` | string | Yes | Target file |
| `start_line` | integer | No | Start line |
| `end_line` | integer | No | End line |
