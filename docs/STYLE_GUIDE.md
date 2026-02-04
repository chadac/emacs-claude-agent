# Claude-Agent Visual Style Guide

All agents and contributors must follow this guide for visual/UX consistency.
Every user-visible element — faces, colors, overlays, prompts, dialogs — should
conform to the patterns documented here.

## Color Palette

### Canonical Palette: Atom One Dark

All UI colors must come from this palette. Do not introduce new hex values
without updating this guide.

| Hex       | Name          | Semantic Role                        | Light Equivalent |
|-----------|---------------|--------------------------------------|------------------|
| `#282c34` | `bg-dark`     | Dark bg, text-on-color labels        | `#fafafa`        |
| `#3e4451` | `bg-region`   | Regions, borders, separators         | `#e5e5e6`        |
| `#5c6370` | `fg-comment`  | Hints, placeholders, disabled text   | `#a0a1a7`        |
| `#abb2bf` | `fg-default`  | Default text, options, popup fg      | `#383a42`        |
| `#c8ccd4` | `fg-user`     | User message body text               | `#383a42`        |
| `#e5e5e5` | `fg-bright`   | Assistant message body text          | `#1a1a1a`        |
| `#e06c75` | `red`         | Errors, deny, destructive actions    | `#e45649`        |
| `#98c379` | `green`       | Success, accept, thinking indicator  | `#50a14f`        |
| `#e5c07b` | `amber`       | Tools, warnings, watch mode          | `#986801`        |
| `#61afef` | `blue`        | User header, progress, titles        | `#4078f2`        |
| `#c678dd` | `purple`      | Assistant header                     | `#a626a4`        |
| `#56b6c2` | `cyan`        | Session info, status bar             | `#0184bc`        |

**Additional derived colors** (used for subtle backgrounds):

| Hex       | Name              | Usage                     | Light Equivalent |
|-----------|-------------------|---------------------------|------------------|
| `#2e4a2e` | `bg-written`      | Written/flash overlay bg  | `#e6ffe6`        |
| `#1e1e1e` | `bg-posframe`     | Posframe/tooltip bg       | `#ffffff`        |

### `[REJECTED]` Saturated "Raw" Colors

The following colors were previously used in `claude-sessions.el` and `todo.el`.
They are **rejected** — replace with the Atom One Dark equivalents shown:

| Raw Hex    | Was Used For        | Replace With (Dark) | Replace With (Light) |
|------------|---------------------|---------------------|----------------------|
| `#00ff00`  | Ready / Done        | `#98c379` (green)   | `#50a14f`            |
| `#ff0000`  | Dead                | `#e06c75` (red)     | `#e45649`            |
| `#ffaa00`  | Thinking / Active   | `#e5c07b` (amber)   | `#986801`            |
| `#66ccff`  | Typing              | `#61afef` (blue)    | `#4078f2`            |
| `#ff66ff`  | Waiting             | `#c678dd` (purple)  | `#a626a4`            |
| `#88aaff`  | Project column      | `#61afef` (blue)    | `#4078f2`            |
| `#aaaaff`  | Buffer column       | `#c678dd` (purple)  | `#a626a4`            |
| `#aaffaa`  | Label column        | `#98c379` (green)   | `#50a14f`            |
| `#888888`  | Draft               | `#5c6370` (comment) | `#a0a1a7`            |
| `#ff4444`  | Rejected            | `#e06c75` (red)     | `#e45649`            |
| `#4488ff`  | Review              | `#61afef` (blue)    | `#4078f2`            |
| `#aaccff`  | TODO title          | `#61afef` (blue)    | `#4078f2`            |


## Face Definitions

### Rules

1. **Always use `defface`** — never use inline `propertize` with hardcoded hex
   colors. Every color visible to the user must be a named face that can be
   customized via `M-x customize-face`.

2. **Always provide light AND dark variants** — use the `((class color)
   (background dark))` / `((class color) (background light))` display spec
   pattern. Never use bare `(t ...)` specs.

3. **Use semantic face names** — face names should describe their purpose, not
   their appearance. For example, `claude-agent-error-face` not
   `claude-agent-red-face`.

4. **Group faces** — set `:group` to the appropriate customization group for
   the module.

### Template

```elisp
(defface claude-example-face
  '(((class color) (background dark))
     (:foreground "#61afef" :weight bold))
    ((class color) (background light))
     (:foreground "#4078f2" :weight bold)))
  "One-line description of what this face is used for."
  :group 'claude-agent)
```

### Unused Faces

`claude-repl-face` is defined as `nil` in `claude-agent.el` and is unused.
It should be removed.


## Height Scaling

### `[PREFERRED]` Standardized Scale

Use these fixed height tiers. Do not introduce intermediate values.

| Tier     | `:height` | Usage                                      |
|----------|-----------|--------------------------------------------|
| Label    | `0.85`    | Lock labels, inline badges, annotations    |
| Body     | `1.0`     | Normal text (implicit, do not set)         |
| Subtitle | `1.1`     | Proposal titles, secondary headings        |
| Title    | `1.2`     | Dialog/popup/picker titles                 |

### `[REJECTED]` Non-standard heights

- `1.3` for confirmation titles — **use `1.2` (Title tier)** instead
- `1.1` for confirmation body — **use `1.0` (Body tier)** instead


## REPL Buffer Formatting

### Message Headers

#### `[PREFERRED]` Live REPL style

```
you> Hello, Claude!
claude> Hello! How can I help?
```

- `you> ` — blue `#61afef` bold (`claude-agent-user-header-face`)
- `claude> ` — purple `#c678dd` bold (`claude-agent-assistant-header-face`)
- System — gray `#5c6370` bold header, italic body

#### `[REJECTED]` Box-drawing style

```
┌─ You
  Hello, Claude!
┌─ Claude
  Hello! How can I help?
```

Session history replay should use the same `you> ` / `claude> ` format as the
live REPL, not box-drawing characters.

### Session Chrome

- Header/footer bars: `━` (heavy horizontal), cyan `#56b6c2` italic
- Status bar: `Model: X | Cost: $X.XXXX | Session: X` between `━` bars
- History markers: `─── Previous Conversation History ───`

### Thinking Indicator

- Braille spinner sequence: `⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏` at 100ms interval
- Format: `<spinner> <status> <elapsed> (+in/-out) (C-c C-k to interrupt)`
- Face: green `#98c379` bold (amber when compacting)

### Progress Bars

- Format: `Working... (45%) 12s` + `▐████████████░░░░░░░░▌`
- Color: blue `#61afef`
- Width: 30 chars in REPL, 10 chars in minibuffer

### Tool Calls

- Format: `<status> toolname› arguments`
- Status icons: `○` pending, `✓` success, `✗` error, `🚫` denied
- Tool name: amber bold, `›` separator amber, arguments styled by type

### Inline TODO List

- `[ ]` — gray (pending)
- `[-]` — blue bold (in-progress)
- `[X]` — green with strike-through (completed)

### Queued Messages

#### `[PREFERRED]` Grayed prompt style

Display queued messages with a dimmed `you> ` header, consistent with the
live prompt style.

#### `[REJECTED]` Dotted separator style

```
┄┄┄ Queued ┄┄┄
```


## Overlays

### Locked Regions

- Background: `#3e4451` (`bg-region`) with `:extend t`
- Label: ` 🔒 Locked by <agent> ` — blue bg `#61afef`, dark fg `#282c34`,
  bold, height `0.85`
- Written flash: green bg `#2e4a2e`, fades after brief delay

### Oneshot Target Regions

#### `[REJECTED]` Yellow-tinted with border strings

The old style used yellow bg `#4a4a2e`, `-- claude oneshot -----` border
strings, fringe bitmaps, priority 100.

#### `[PREFERRED]` Locked-region style

Reuse the locked-region visual pattern:

- **Region scope**: highlight with `bg-region` face (`#3e4451` bg), same as
  lock overlays
- **File/buffer scope**: highlight entire buffer with `bg-region` face
- **Directory/project scope**: no special highlighting needed

### Oneshot Completion Tooltip

#### `[REJECTED]` Box-drawn tooltip

The old style used `┌─┐ │ └─┘` with `✓` checkmark, green bg `#2d4a2d`,
priority 200.

#### `[PREFERRED]` Label style (matches lock labels)

Use a label like: ` ✓ Completed by *claude:oneshot* ` with blue bg `#61afef`,
dark fg `#282c34`, bold, height `0.85` — same visual pattern as lock overlay
labels.


## Dialogs & Popups

### Prompt Choice

- Title: blue bold, height `1.2` (`claude-mcp-prompt-title-face`)
- Selected item: green bg `#98c379`, dark fg, bold
- Hints: gray `#5c6370` italic

### Proposal Buffer

- Header-line: `C-c C-c accept | C-c C-k reject | Edit freely` — green bold
- Title: blue bold, height `1.1` (Subtitle tier)
- Separator: 60x `─` in `#3e4451`

### Confirmation Dialog

- Title: amber `#e5c07b` bold, height `1.2` (Title tier, not 1.3)
- Body: `#abb2bf`, height `1.0` (Body tier, not 1.1)
- `[y]` green, `[n]` red

### File/Directory Picker

- Title: blue bold, height `1.2`
- Directory path: gray, items: `#abb2bf`

### Multiselect

- Title: blue bold, height `1.2`
- Unchecked items: gray
- Checked items: green bold

### Permission Dialog (Full)

- Box borders: `─` characters
- Options: `[X] Allow once`, etc.
- Faces: amber box border, blue selected, gray options

### Permission Dialog (Compact/Inline)

- Format: `⚡ Bash(ls)` + `[1:once] [2:session] [3:always] [4:deny]`

### Tool Result Posframe

- Background: `#1e1e1e`, foreground: `#abb2bf`, border: `#5c6370`
- Max display: 15 lines / 1000 chars


## Modeline / Headerline

| Component      | Text                                          | Face                              |
|----------------|-----------------------------------------------|-----------------------------------|
| Watch mode     | ` 👁 WATCH `                                  | amber bg, dark fg, bold           |
| Oneshot active | ` ⚡ Claude oneshot active (N agents) `       | amber bg, dark fg, bold (dark+light) |
| Proposal       | `C-c C-c accept \| C-c C-k reject`           | green bold (inline)               |
| TODO help      | `TODO List Help -- press a key`               | `mode-line-emphasis` (standard)   |


## Tabulated Lists

### Columns

#### Sessions List (`claude-sessions.el`)

| Column    | Width | Face                            |
|-----------|-------|---------------------------------|
| Project   | 20    | blue `#61afef`                  |
| Label     | 15    | green `#98c379`                 |
| Status    | 10    | (per-status face)               |
| Worktree  | 30    | default                         |
| Directory | 40    | default                         |

#### TODO List (`todo.el`)

| Column    | Width | Face                            |
|-----------|-------|---------------------------------|
| Status    | 12    | (per-status face)               |
| Title     | 50    | blue `#61afef`                  |
| Claude    | 10    | (delegates to session faces)    |
| Created   | 12    | default                         |
| Project   | 20    | blue `#61afef`                  |

### Status Colors (Both Lists)

| Status    | Color                    | Weight        | Extra              |
|-----------|--------------------------|---------------|--------------------|
| Ready     | green `#98c379`          | bold          |                    |
| Thinking  | amber `#e5c07b`          | bold          |                    |
| Typing    | blue `#61afef`           | bold          |                    |
| Waiting   | purple `#c678dd`         | bold          |                    |
| Dead      | red `#e06c75`            | bold          |                    |
| Draft     | comment `#5c6370`        | normal        |                    |
| Active    | amber `#e5c07b`          | bold          |                    |
| Review    | blue `#61afef`           | bold          |                    |
| Done      | green `#98c379`          | bold          |                    |
| Rejected  | red `#e06c75`            | normal        | `:strike-through t` |


## Unicode / Special Characters

### Approved Characters

| Category       | Characters                          | Usage                                    |
|----------------|-------------------------------------|------------------------------------------|
| Box drawing    | `━ ─ ┌ ┐ └ ┘ │ ┄`                  | Headers, borders, continuations          |
| Progress       | `▐ ▌ █ ░`                          | Progress bar fill/empty                  |
| Status icons   | `○ ✓ ✗`                            | Tool call pending/success/error          |
| Emoji          | `🔒 👁 🚫 ⚡`                       | Lock labels, watch mode, denied, oneshot |
| Spinner        | `⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏`            | Thinking animation (braille dots)        |
| Arrows         | `→ › ↑ ↓`                          | Line numbers, tool separator, nav hints  |

### Rules

- Prefer Unicode box-drawing over ASCII dashes/pipes for visual structure
- Use emoji sparingly — only for labels and modeline indicators
- Status icons (`○ ✓ ✗`) are preferred over emoji for inline status


## Anti-Patterns

### Do NOT:

1. **Use raw hex colors in `propertize`**
   ```elisp
   ;; BAD
   (propertize text 'face '(:foreground "#61afef"))

   ;; GOOD
   (propertize text 'face 'claude-agent-user-header-face)
   ```

2. **Define faces without light/dark variants**
   ```elisp
   ;; BAD
   (defface my-face
     '((t :foreground "#61afef"))
     "...")

   ;; GOOD
   (defface my-face
     '(((class color) (background dark))
       (:foreground "#61afef"))
      ((class color) (background light))
       (:foreground "#4078f2")))
     "...")
   ```

3. **Use colors outside the canonical palette** — if you need a new color,
   add it to the palette table in this guide first.

4. **Use non-standard height values** — stick to the four tiers: `0.85`,
   `1.0`, `1.1`, `1.2`.

5. **Mix visual patterns** — lock overlays, oneshot targets, and completion
   tooltips should all follow the same label + region pattern.
