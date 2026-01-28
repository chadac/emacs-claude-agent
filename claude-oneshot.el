;;; claude-oneshot.el --- One-shot background agents for Claude -*- lexical-binding: t; -*-
;; Author: Claude
;; Keywords: tools, ai
;; Package-Requires: ((emacs "28.1"))
;; PERMISSION TEST: Read/edit works
;;; Commentary:
;; Provides lightweight, background agents for quick one-off tasks.
;; These agents:
;; - Run in parallel with the main agent
;; - Share session context via --continue
;; - Auto-terminate when done
;; - Stay invisible unless interaction is needed
;;
;; Permission-scoped keybindings:
;; - C-c c c: Line/region scope - can only edit target line/region
;; - C-c c b: Buffer scope - can edit current buffer
;; - C-c c d: Directory scope - can edit files in current directory
;; - C-c c p: Project scope - can edit any file in project
;;; Code:
(require 'claude-agent-repl)
(require 'claude-mcp)

;;;; Customization

(defgroup claude-oneshot nil
  "Claude oneshot agent settings."
  :group 'claude-agent)

(defcustom claude-oneshot-model "sonnet"
  "Default model to use for oneshot agents.
Haiku is recommended for quick, low-cost operations."
  :type '(choice (const "haiku")
                 (const "sonnet")
                 (const "opus"))
  :group 'claude-oneshot)

(defcustom claude-oneshot-timeout 300
  "Timeout in seconds for oneshot agents (300 seconds = 5 minutes).
Agent will be killed if it doesn't complete within this time."
  :type 'integer
  :group 'claude-oneshot)

(defcustom claude-oneshot-debug nil
  "When non-nil, keep oneshot agent buffers after completion for debugging.
The buffer will be renamed with a '-done' suffix instead of being killed."
  :type 'boolean
  :group 'claude-oneshot)

;;;; Faces

(defface claude-oneshot-target-face
  '((((class color) (background dark))
     :background "#4a4a2e" :extend t)
    (((class color) (background light))
     :background "#ffffcc" :extend t)
    (t :inverse-video t :extend t))
  "Face for highlighting the target region of a oneshot agent.
Uses a yellow tint to indicate the region being edited."
  :group 'claude-oneshot)

(defface claude-oneshot-target-border-face
  '((((class color) (background dark))
     :foreground "#b8a520" :background "#3a3a20" :extend t)
    (((class color) (background light))
     :foreground "#8b7500" :background "#eeeeaa" :extend t)
    (t :weight bold :extend t))
  "Face for the border lines of a oneshot target region.
The :extend property ensures the background stretches across the buffer."
  :group 'claude-oneshot)

(defface claude-oneshot-header-face
  '((((class color) (background dark))
     :foreground "#e5c07b" :background "#3e3d32" :weight bold)
    (((class color) (background light))
     :foreground "#986801" :background "#fffacd" :weight bold)
    (t :weight bold :inverse-video t))
  "Face for the oneshot agent indicator header line."
  :group 'claude-oneshot)

(defface claude-oneshot-fringe-face
  '((((class color) (background dark))
     :foreground "#e5c07b")
    (((class color) (background light))
     :foreground "#986801"))
  "Face for the fringe indicator of oneshot target regions."
  :group 'claude-oneshot)

(defface claude-oneshot-tooltip-face
  '((((class color) (background dark))
     :background "#2d4a2d" :foreground "#98c379" :extend t)
    (((class color) (background light))
     :background "#e6ffe6" :foreground "#2d5a2d" :extend t)
    (t :inverse-video t :extend t))
  "Face for oneshot completion tooltip messages.
Uses a green tint to indicate success/completion."
  :group 'claude-oneshot)

;;;; Variables

(defvar claude-oneshot--counter 0
  "Counter for generating unique oneshot buffer names.")

(defvar claude-oneshot--active-agents (make-hash-table :test 'equal)
  "Hash table of active oneshot agents.
Key is buffer name, value is a plist with agent metadata.")

(defvar-local claude-oneshot--target-overlay nil
  "Overlay highlighting the target region for this oneshot agent.")

(defvar-local claude-oneshot--source-buffer nil
  "The source buffer that this oneshot agent is targeting.")

(defvar-local claude-oneshot--scope nil
  "The scope of this oneshot agent: `line', `region', `buffer', `directory', or `project'.")

(defvar-local claude-oneshot--target-info nil
  "Plist with target information: :file, :start-line, :end-line, :content.")

(defvar-local claude-oneshot--timeout-timer nil
  "Timer for the oneshot timeout.")

(defvar-local claude-oneshot--is-oneshot nil
  "Non-nil if this buffer is a oneshot agent.")

;; Variables for source buffers (where oneshot is targeting)
(defvar-local claude-oneshot--source-agents nil
  "List of oneshot agent buffers targeting this buffer.
Used by source buffers to track which oneshot agents are working on them.")

(defvar-local claude-oneshot--saved-header-line nil
  "Saved header-line-format before oneshot indicator was added.")

(defvar-local claude-oneshot--tooltip-overlays nil
  "List of tooltip overlays in this buffer from completed oneshot agents.")

(defvar-local claude-oneshot--target-position nil
  "Position where the oneshot was invoked (for tooltip placement).")

;;;; Fringe Bitmap

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'claude-oneshot-fringe-indicator
    [#b11111111
     #b11111111
     #b11000011
     #b11000011
     #b11000011
     #b11000011
     #b11111111
     #b11111111]
    nil nil 'center))

;;;; Core Functions

(defun claude-oneshot--generate-buffer-name (scope)
  "Generate a unique buffer name for a oneshot agent with SCOPE."
  (cl-incf claude-oneshot--counter)
  (format "*claude:oneshot-%s-%d*" scope claude-oneshot--counter))

(defun claude-oneshot--get-project-root ()
  "Get the project root directory."
  (or (when-let ((proj (project-current)))
        (project-root proj))
      (vc-git-root default-directory)
      default-directory))

(defun claude-oneshot--get-scope-system-prompt (scope target-info)
  "Generate a system prompt explaining the SCOPE and TARGET-INFO to the agent."
  (let ((file (plist-get target-info :file))
        (buffer-name (plist-get target-info :buffer-name))
        (start-line (plist-get target-info :start-line))
        (end-line (plist-get target-info :end-line))
        (content (plist-get target-info :content))
        (directory (plist-get target-info :directory))
        (project (plist-get target-info :project)))
    (concat
     "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
     "!!!              STOP! READ THIS CAREFULLY!                !!!\n"
     "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n"
     "YOU ARE NOW A ONESHOT AGENT. THIS IS A COMPLETELY NEW TASK.\n\n"
     "IGNORE EVERYTHING ABOVE THIS MESSAGE!\n"
     "- You are NOT the main agent anymore\n"
     "- You are NOT continuing previous work\n"
     "- You are NOT watching any buffers\n"
     "- FORGET what you were doing before\n\n"
     "YOUR ONLY JOB: Complete the single task below, then call mcp__emacs__done.\n\n"
     "CRITICAL: YOUR OUTPUT IS INVISIBLE TO THE USER!\n"
     "- Your text responses go to a HIDDEN buffer the user cannot see\n"
     "- The ONLY way to communicate with the user is via mcp__emacs__done\n"
     "- If the user asks you to explain something, PUT THE EXPLANATION in the done message\n"
     "- The done message appears as a tooltip in their buffer\n\n"
     "ONESHOT RULES:\n"
     "1. Do the ONE task described below - nothing else\n"
     "2. Call mcp__emacs__done with a message when finished (REQUIRED!)\n"
     "3. If the user asked for an explanation, include it in the done message\n"
     "4. If you need user input, use mcp__emacs__prompt_choice\n"
     "5. Do NOT use watch_buffer, watch_for_pattern, or similar tools\n\n"
     "TOOL ACCESS:\n"
     "- For FILE-BACKED BUFFERS: Use the SDK's Edit/Write/Read tools (pre-authorized within your scope)\n"
     "- For NON-FILE BUFFERS (like *scratch*, capture buffers, etc.):\n"
     "  * Use mcp__emacs__edit_buffer(buffer_name, old_string, new_string) to edit\n"
     "  * Use mcp__emacs__read_buffer(buffer_name) to read\n"
     "  * The SDK Edit tool will NOT work on non-file buffers!\n"
     "- You can also use mcp__emacs__edit_file and mcp__emacs__read_file for files\n\n"
     "═══════════════════════════════════════════════════════════════\n"
     "SCOPE: " (upcase (symbol-name scope)) "\n"
     (pcase scope
       ('line
        (if file
            (format "You may ONLY modify line %d in file %s.\n\nTarget content:\n```\n%s\n```\n"
                    start-line file content)
          (format "You may ONLY modify line %d in buffer %s (not a file).\n\nTarget content:\n```\n%s\n```\n"
                  start-line buffer-name content)))
       ('region
        (if file
            (format "You may ONLY modify lines %d-%d in file %s.\n\nTarget content:\n```\n%s\n```\n"
                    start-line end-line file content)
          (format "You may ONLY modify lines %d-%d in buffer %s (not a file).\n\nTarget content:\n```\n%s\n```\n"
                  start-line end-line buffer-name content)))
       ('buffer
        (if file
            (format "You may ONLY modify the file %s.\n" file)
          (format "You may ONLY modify buffer %s (not a file).\n\nBuffer content:\n```\n%s\n```\n"
                  buffer-name content)))
       ('directory
        (format "You may ONLY modify files in directory: %s\n" directory))
       ('project
        (format "You may modify any file in project: %s\n" project))
       (_ "")))))

(defun claude-oneshot--get-allowed-tools-for-scope (scope target-info)
  "Return list of tools to pre-authorize based on SCOPE and TARGET-INFO.
Includes SDK tools (Edit, Write, Read, Glob) and MCP tools for buffer editing.

The format follows Claude Code's allowed tools syntax:
- ToolName(path) for specific files
- ToolName(dir/*) for directory
- ToolName(dir/**) for recursive
- mcp__server__tool(arg) for MCP tools"
  (let ((file (plist-get target-info :file))
        (buffer-name (plist-get target-info :buffer-name))
        (directory (plist-get target-info :directory))
        (project (plist-get target-info :project)))
    (append
     ;; Always allow done tool for oneshot completion
     ;; NOTE: Plain tool names work; parameterized format (tool(arg)) doesn't seem to work
     ;; for MCP tools. TODO: Investigate if SDK supports parameterized MCP tool permissions
     (list "mcp__emacs__done"
           "mcp__emacs__edit_buffer"
           "mcp__emacs__read_buffer")
     ;; SDK and MCP tools for file/scope editing
     (pcase scope
       ;; For line/region/buffer scope, allow Edit for that specific file
       ((or 'line 'region 'buffer)
        (when file
          (list (format "Edit(%s)" file)
                (format "Write(%s)" file)
                (format "Read(%s)" file)
                (format "Glob(%s)" (file-name-directory file))
                (format "mcp__emacs__edit_file(%s)" file)
                (format "mcp__emacs__read_file(%s)" file))))
       ;; For directory scope, allow Edit for files in that directory
       ('directory
        (when directory
          (list (format "Edit(%s*)" directory)
                (format "Write(%s*)" directory)
                (format "Read(%s*)" directory)
                (format "Glob(%s)" directory)
                (format "mcp__emacs__edit_file(%s*)" directory)
                (format "mcp__emacs__read_file(%s*)" directory))))
       ;; For project scope, allow Edit anywhere in project
       ('project
        (when project
          (list (format "Edit(%s**)" project)
                (format "Write(%s**)" project)
                (format "Read(%s**)" project)
                (format "Glob(%s)" project)
                (format "mcp__emacs__edit_file(%s**)" project)
                (format "mcp__emacs__read_file(%s**)" project))))
       (_ nil)))))

(defun claude-oneshot--create-border-string (label)
  "Create a border string with LABEL that stretches across the window.
Returns a string like \"-- claude oneshot ----------------\" that fills the width."
  (let* ((prefix (concat "-- " label " "))
         (prefix-len (length prefix))
         ;; Use a reasonable width, will extend with :extend face property
         (fill-width (max 0 (- 80 prefix-len)))
         (dashes (make-string fill-width ?-)))
    (concat prefix dashes)))

(defun claude-oneshot--create-target-overlay (buffer start end)
  "Create an overlay in BUFFER from START to END to highlight the target."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((ov (make-overlay start end)))
        (overlay-put ov 'face 'claude-oneshot-target-face)
        (overlay-put ov 'claude-oneshot t)
        (overlay-put ov 'priority 100)
        ;; Add before-string with simple dashed border that extends
        (overlay-put ov 'before-string
                     (propertize (concat (claude-oneshot--create-border-string "claude oneshot") "\n")
                                 'face 'claude-oneshot-target-border-face
                                 'display '(space-width 1)))
        ;; Add after-string with simple dashed border that extends
        (overlay-put ov 'after-string
                     (propertize (concat "\n" (make-string 80 ?-))
                                 'face 'claude-oneshot-target-border-face))
        ov))))

(defun claude-oneshot--clear-target-overlay (buffer)
  "Clear any oneshot target overlays in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'claude-oneshot)
          (delete-overlay ov))))))

;;;; Header Line Indicator

(defun claude-oneshot--update-header-line (buffer)
  "Update the header line in BUFFER to show active oneshot agents."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if claude-oneshot--source-agents
          ;; Show header with active agents
          (let* ((count (length claude-oneshot--source-agents))
                 (scopes (mapcar (lambda (agent-buf)
                                   (when (buffer-live-p agent-buf)
                                     (with-current-buffer agent-buf
                                       claude-oneshot--scope)))
                                 claude-oneshot--source-agents))
                 (scope-str (mapconcat (lambda (s) (symbol-name (or s 'unknown)))
                                       (delq nil scopes) ", ")))
            (setq header-line-format
                  (propertize
                   (format " ⚡ Claude oneshot active (%d agent%s: %s) - C-c c o to list "
                           count (if (= count 1) "" "s") scope-str)
                   'face 'claude-oneshot-header-face)))
        ;; Restore original header line
        (setq header-line-format claude-oneshot--saved-header-line)))))

(defun claude-oneshot--register-in-source (source-buffer agent-buffer)
  "Register AGENT-BUFFER as targeting SOURCE-BUFFER."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      ;; Save original header line if this is the first agent
      (unless claude-oneshot--source-agents
        (setq claude-oneshot--saved-header-line header-line-format))
      ;; Add to list
      (cl-pushnew agent-buffer claude-oneshot--source-agents)
      ;; Update header
      (claude-oneshot--update-header-line source-buffer))))

(defun claude-oneshot--unregister-from-source (source-buffer agent-buffer)
  "Unregister AGENT-BUFFER from SOURCE-BUFFER."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      ;; Remove from list
      (setq claude-oneshot--source-agents
            (delq agent-buffer claude-oneshot--source-agents))
      ;; Update header (will restore if no agents left)
      (claude-oneshot--update-header-line source-buffer))))

;;;; Tooltip Overlay

(defun claude-oneshot--wrap-text (text width)
  "Wrap TEXT to WIDTH characters, returning a list of lines."
  (with-temp-buffer
    (insert text)
    (let ((fill-column width))
      (fill-region (point-min) (point-max)))
    (split-string (buffer-string) "\n" t)))

(defun claude-oneshot--create-tooltip (buffer position message)
  "Create a tooltip overlay in BUFFER at POSITION with MESSAGE.
The tooltip shows the completion message and a hint to dismiss it.
Long messages are wrapped to fit within the box."
  (when (and (buffer-live-p buffer) message (not (string-empty-p message)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char position)
        ;; Move to end of line to place tooltip after content
        (end-of-line)
        (let* ((ov (make-overlay (point) (point)))
               (box-width 64)
               ;; Inner width: box-width minus "│ " (2) and " │" (2) = 4
               (content-width (- box-width 4))  ; Width for text between │ and │
               (border-line (make-string (- box-width 2) ?─))
               (hint-text "Press C-c c y to dismiss")
               ;; Wrap the message - account for "✓ " prefix on first line
               (wrapped-lines (claude-oneshot--wrap-text message (- content-width 2)))
               ;; Build the content lines - first line gets checkmark
               (first-line (car wrapped-lines))
               (rest-lines (cdr wrapped-lines))
               ;; Build all lines as plain strings
               (lines
                (append
                 ;; Top border
                 (list (concat "┌" border-line "┐"))
                 ;; First line with checkmark
                 (list (concat "│ ✓ " first-line
                               (make-string (max 0 (- content-width 2 (length first-line))) ? )
                               " │"))
                 ;; Rest of wrapped lines (indented to align with first line)
                 (mapcar (lambda (line)
                           (concat "│   " line
                                   (make-string (max 0 (- content-width 2 (length line))) ? )
                                   " │"))
                         rest-lines)
                 ;; Hint line
                 (list (concat "│ " hint-text
                               (make-string (max 0 (- content-width (length hint-text))) ? )
                               " │"))
                 ;; Bottom border
                 (list (concat "└" border-line "┘"))))
               ;; Join all lines and apply single face to entire string
               ;; Start with plain newline, then propertized content with trailing newline
               (tooltip-content
                (concat "\n"
                        (propertize (concat (mapconcat #'identity lines "\n") "\n")
                                    'face 'claude-oneshot-tooltip-face))))
          (overlay-put ov 'after-string tooltip-content)
          (overlay-put ov 'claude-oneshot-tooltip t)
          (overlay-put ov 'priority 200)
          ;; Track this overlay in the buffer
          (push ov claude-oneshot--tooltip-overlays)
          ov)))))

(defun claude-oneshot--clear-tooltip (overlay)
  "Remove a single tooltip OVERLAY."
  (when (overlayp overlay)
    (let ((buf (overlay-buffer overlay)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq claude-oneshot--tooltip-overlays
                (delq overlay claude-oneshot--tooltip-overlays))))
      (delete-overlay overlay))))

(defun claude-oneshot--clear-all-tooltips (&optional buffer)
  "Clear all tooltip overlays in BUFFER or current buffer."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (dolist (ov claude-oneshot--tooltip-overlays)
          (when (overlayp ov)
            (delete-overlay ov)))
        (setq claude-oneshot--tooltip-overlays nil)))))

;;;###autoload
(defun claude-oneshot-dismiss-tooltips ()
  "Dismiss all oneshot completion tooltips in the current buffer."
  (interactive)
  (let ((count (length claude-oneshot--tooltip-overlays)))
    (claude-oneshot--clear-all-tooltips)
    (message "Dismissed %d tooltip%s" count (if (= count 1) "" "s"))))

(defun claude-oneshot--add-fringe-indicator (overlay)
  "Add a fringe indicator to OVERLAY."
  (when overlay
    (overlay-put overlay 'line-prefix
                 (propertize " " 'display
                             '(left-fringe claude-oneshot-fringe-indicator
                                           claude-oneshot-fringe-face)))))

(defun claude-oneshot--get-line-region ()
  "Get the start and end positions of the current line."
  (cons (line-beginning-position) (line-end-position)))

(defun claude-oneshot--get-region-info ()
  "Get information about the current region or line.
Returns a plist with :start, :end, :start-line, :end-line, :content.
Trims trailing newlines from the region to keep visual highlighting clean."
  (let* ((has-region (use-region-p))
         (start (if has-region (region-beginning) (line-beginning-position)))
         (end (if has-region (region-end) (line-end-position))))
    ;; Trim trailing newlines from the end position
    (save-excursion
      (goto-char end)
      (while (and (> end start)
                  (eq (char-before end) ?\n))
        (setq end (1- end))))
    (let* ((start-line (line-number-at-pos start))
           (end-line (line-number-at-pos end))
           (content (buffer-substring-no-properties start end)))
      (list :start start
            :end end
            :start-line start-line
            :end-line end-line
            :content content))))

;;;; Agent Lifecycle

(defun claude-oneshot--start (scope prompt &optional target-info)
  "Start a oneshot agent with SCOPE, PROMPT, and optional TARGET-INFO.
Returns the buffer of the new agent."
  ;; Clear any existing oneshot overlays in this buffer first
  (claude-oneshot--clear-target-overlay (current-buffer))
  (let* ((work-dir (claude-oneshot--get-project-root))
         (buf-name (claude-oneshot--generate-buffer-name scope))
         (source-buffer (current-buffer))
         (source-file (buffer-file-name))
         ;; Build target info based on scope
         (target (or target-info
                     (pcase scope
                       ((or 'line 'region)
                        (let ((region-info (claude-oneshot--get-region-info)))
                          (list :file source-file
                                :buffer-name (buffer-name source-buffer)
                                :start-line (plist-get region-info :start-line)
                                :end-line (plist-get region-info :end-line)
                                :content (plist-get region-info :content)
                                :start (plist-get region-info :start)
                                :end (plist-get region-info :end))))
                       ('buffer
                        (list :file source-file
                              :buffer-name (buffer-name source-buffer)
                              :content (unless source-file
                                         (buffer-substring-no-properties
                                          (point-min) (point-max)))))
                       ('directory
                        (list :directory (file-name-directory
                                          (or source-file default-directory))))
                       ('project
                        (list :project work-dir)))))
         ;; Create the buffer
         (buf (get-buffer-create buf-name)))

    ;; Set up the buffer
    (with-current-buffer buf
      (claude-agent-mode)
      (claude-agent--init-buffer (format "oneshot-%s" scope))
      (setq claude-agent--parse-state nil
            claude-agent--pending-output ""
            claude-agent--session-info nil
            claude-agent--has-conversation nil
            claude-agent--work-dir work-dir
            default-directory work-dir
            ;; Oneshot-specific variables
            claude-oneshot--is-oneshot t
            claude-oneshot--source-buffer source-buffer
            claude-oneshot--scope scope
            claude-oneshot--target-info target
            ;; Save position for tooltip placement (use start of target or current point)
            claude-oneshot--target-position (or (plist-get target :start)
                                                (with-current-buffer source-buffer (point)))))

    ;; Create visual highlight in source buffer for line/region scope
    (when (and (memq scope '(line region))
               (plist-get target :start)
               (plist-get target :end))
      (let ((ov (claude-oneshot--create-target-overlay
                 source-buffer
                 (plist-get target :start)
                 (plist-get target :end))))
        ;; Add fringe indicator
        (claude-oneshot--add-fringe-indicator ov)
        (with-current-buffer buf
          (setq claude-oneshot--target-overlay ov))))

    ;; Register this agent in the source buffer (for header line indicator)
    (claude-oneshot--register-in-source source-buffer buf)

    ;; Start the process with system prompt (no --continue to avoid context bleed)
    ;; Pass scope-appropriate allowed tools so the agent can edit without permission prompts
    (let* ((system-prompt (claude-oneshot--get-scope-system-prompt scope target))
           (allowed-tools (claude-oneshot--get-allowed-tools-for-scope scope target))
           (proc (claude-agent--start-process
                  work-dir buf nil nil claude-oneshot-model system-prompt allowed-tools)))
      (with-current-buffer buf
        (setq claude-agent--process proc)
        ;; Set up timeout timer
        (setq claude-oneshot--timeout-timer
              (run-with-timer claude-oneshot-timeout nil
                              #'claude-oneshot--handle-timeout buf)))

      ;; Register in active agents
      (puthash buf-name
               (list :buffer buf
                     :scope scope
                     :source-buffer source-buffer
                     :start-time (current-time))
               claude-oneshot--active-agents)

      ;; Send the user request after a short delay
      ;; System prompt is handled by the SDK via --system-prompt flag
      (run-with-timer
       1.5 nil
       (lambda (buffer user-prompt)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and claude-agent--process
                        (process-live-p claude-agent--process))
               ;; Send just the user request - system prompt is already set
               (process-send-string
                claude-agent--process
                (concat (json-encode
                         `((type . "message")
                           (text . ,user-prompt)))
                        "\n"))))))
       buf prompt))

    ;; Return the buffer (but don't display it)
    buf))

(defun claude-oneshot--handle-timeout (buffer)
  "Handle timeout for oneshot agent in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (message "Oneshot agent timed out after %d seconds" claude-oneshot-timeout)
      (claude-oneshot--cleanup buffer "Timeout"))))

(defun claude-oneshot--handle-ready-state (buffer)
  "Handle when a oneshot agent enters Ready state.
Sends a reminder to call done or ask for input."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when claude-oneshot--is-oneshot
        ;; Send reminder after a delay
        (run-with-timer
         3 nil
         (lambda (buf)
           (when (and (buffer-live-p buf)
                      (with-current-buffer buf
                        (and claude-oneshot--is-oneshot
                             (not claude-agent--thinking-status))))
             (with-current-buffer buf
               (when (and claude-agent--process
                          (process-live-p claude-agent--process))
                 (process-send-string
                  claude-agent--process
                  (concat (json-encode
                           '((type . "message")
                             (text . "REMINDER: You're in oneshot mode. Call mcp__emacs__done if finished, or use mcp__emacs__prompt_choice/mcp__emacs__confirm if you need user input.")))
                          "\n"))))))
         buffer)))))

(defun claude-oneshot--cleanup (buffer &optional result)
  "Clean up oneshot agent BUFFER with optional RESULT message."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Cancel timeout timer
      (when claude-oneshot--timeout-timer
        (cancel-timer claude-oneshot--timeout-timer)
        (setq claude-oneshot--timeout-timer nil))

      ;; Clear target overlay in source buffer and unregister
      (when claude-oneshot--source-buffer
        (claude-oneshot--clear-target-overlay claude-oneshot--source-buffer)
        ;; Unregister from source buffer (removes header line if no agents left)
        (claude-oneshot--unregister-from-source claude-oneshot--source-buffer buffer))

      ;; Kill the process if still running
      (when (and claude-agent--process (process-live-p claude-agent--process))
        (delete-process claude-agent--process))

      ;; Remove from active agents
      (remhash (buffer-name buffer) claude-oneshot--active-agents)

      ;; Show result message
      (when result
        (message "Oneshot agent: %s" result))

      ;; Kill or rename the buffer based on debug flag
      (if claude-oneshot-debug
          ;; Debug mode: rename buffer and keep it for inspection
          (let ((new-name (concat (buffer-name buffer) "-done")))
            (rename-buffer new-name t)
            (message "Debug: oneshot buffer kept as %s" new-name))
        ;; Normal mode: kill the buffer
        (kill-buffer buffer)))))

;;;; MCP Tools for Oneshot

(defun claude-mcp-done (&optional message)
  "Signal that a oneshot agent has completed its task.
Optional MESSAGE is displayed to the user and shown as a tooltip in the source buffer.
This tool should be called by oneshot agents when they finish."
  (let ((buf (current-buffer)))
    ;; Check if we're actually in a oneshot buffer
    (if (and (boundp 'claude-oneshot--is-oneshot)
             claude-oneshot--is-oneshot)
        (let ((source-buf claude-oneshot--source-buffer)
              (tooltip-pos claude-oneshot--target-position)
              (tooltip-msg message))
          ;; Create tooltip in source buffer before cleanup
          (when (and source-buf tooltip-pos tooltip-msg)
            (claude-oneshot--create-tooltip source-buf tooltip-pos tooltip-msg))
          ;; Clean up the oneshot agent
          (claude-oneshot--cleanup buf (or message "Task completed"))
          "Oneshot agent terminated successfully")
      ;; Not a oneshot buffer - just show the message
      (when message
        (message "Claude: %s" message))
      "done (not a oneshot agent)")))

(claude-mcp-deftool done
  "Signal completion of a oneshot task. Call this when you have finished your assigned task. The oneshot agent will be terminated and the user notified."
  :function #'claude-mcp-done
  :safe t
  :needs-session-cwd t
  :args ((message string "Optional completion message to show the user")))

(defun claude-mcp-update-target (file-path &optional start-line end-line)
  "Update the visual highlighting for the current oneshot target.
FILE-PATH is the file being worked on.
START-LINE and END-LINE define the target region (optional)."
  (when (and (boundp 'claude-oneshot--is-oneshot)
             claude-oneshot--is-oneshot
             claude-oneshot--source-buffer)
    ;; Clear old overlay
    (claude-oneshot--clear-target-overlay claude-oneshot--source-buffer)
    ;; Create new overlay if we have line info
    (when (and start-line end-line)
      (with-current-buffer claude-oneshot--source-buffer
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- start-line))
          (let ((start (point)))
            (forward-line (- end-line start-line))
            (end-of-line)
            (let ((end (point)))
              (setq claude-oneshot--target-overlay
                    (claude-oneshot--create-target-overlay
                     claude-oneshot--source-buffer start end))))))))
  "Target updated")

(claude-mcp-deftool update-target
  "Update the visual highlighting showing where the oneshot agent is working. Use this to narrow or expand the scope as you work."
  :function #'claude-mcp-update-target
  :safe t
  :needs-session-cwd t
  :args ((file-path string :required "Path to the file being targeted")
         (start-line integer "Start line of the target region")
         (end-line integer "End line of the target region")))

;;;; Interactive Commands

;;;###autoload
(defun claude-oneshot-line-or-region ()
  "Start a oneshot agent scoped to the current line or region.
Prompts for what you want done.  Works on any buffer, not just file-visiting ones."
  (interactive)
  (let* ((has-region (use-region-p))
         (scope (if has-region 'region 'line))
         (prompt (read-string (format "What should Claude do with this %s? "
                                      (if has-region "region" "line")))))
    (when (string-empty-p (string-trim prompt))
      (error "Prompt cannot be empty"))
    (claude-oneshot--start scope prompt)
    ;; Deactivate region so the oneshot overlay is visible
    (deactivate-mark)
    (message "Oneshot agent started (scope: %s)" scope)))

;;;###autoload
(defun claude-oneshot-buffer ()
  "Start a oneshot agent scoped to the current buffer.
Prompts for what you want done.  Works on any buffer, not just file-visiting ones."
  (interactive)
  (let ((prompt (read-string (format "What should Claude do with buffer %s? "
                                     (buffer-name)))))
    (when (string-empty-p (string-trim prompt))
      (error "Prompt cannot be empty"))
    (claude-oneshot--start 'buffer prompt)
    (message "Oneshot agent started (scope: buffer)")))

;;;###autoload
(defun claude-oneshot-directory ()
  "Start a oneshot agent scoped to the current directory.
Prompts for what you want done."
  (interactive)
  (let ((prompt (read-string (format "What should Claude do in %s? "
                                     (file-name-directory
                                      (or (buffer-file-name) default-directory))))))
    (when (string-empty-p (string-trim prompt))
      (error "Prompt cannot be empty"))
    (claude-oneshot--start 'directory prompt)
    (message "Oneshot agent started (scope: directory)")))

;;;###autoload
(defun claude-oneshot-project ()
  "Start a oneshot agent scoped to the current project.
Prompts for what you want done."
  (interactive)
  (let ((prompt (read-string (format "What should Claude do in project %s? "
                                     (claude-oneshot--get-project-root)))))
    (when (string-empty-p (string-trim prompt))
      (error "Prompt cannot be empty"))
    (claude-oneshot--start 'project prompt)
    (message "Oneshot agent started (scope: project)")))

;;;###autoload
(defun claude-oneshot-list ()
  "List all active oneshot agents."
  (interactive)
  (if (= (hash-table-count claude-oneshot--active-agents) 0)
      (message "No active oneshot agents")
    (let ((msg "Active oneshot agents:\n"))
      (maphash
       (lambda (name info)
         (let* ((scope (plist-get info :scope))
                (start-time (plist-get info :start-time))
                (elapsed (float-time (time-subtract (current-time) start-time))))
           (setq msg (concat msg
                             (format "  %s (scope: %s, elapsed: %.0fs)\n"
                                     name scope elapsed)))))
       claude-oneshot--active-agents)
      (message "%s" msg))))

;;;###autoload
(defun claude-oneshot-cancel-all ()
  "Cancel all active oneshot agents."
  (interactive)
  (when (yes-or-no-p "Cancel all active oneshot agents? ")
    (let ((count 0))
      (maphash
       (lambda (_name info)
         (let ((buf (plist-get info :buffer)))
           (when (buffer-live-p buf)
             (claude-oneshot--cleanup buf "Cancelled")
             (cl-incf count))))
       claude-oneshot--active-agents)
      (message "Cancelled %d oneshot agent(s)" count))))

;;;; Hook into agent Ready state

(defun claude-oneshot--check-ready-state ()
  "Check if we need to send a reminder for oneshot agents.
Called from the agent's ready message handler."
  (when claude-oneshot--is-oneshot
    (claude-oneshot--handle-ready-state (current-buffer))))

;; Advice to hook into the ready message handling
(defun claude-oneshot--advice-dispatch-ready (orig-fun msg-type msg)
  "Advice for `claude-agent--dispatch-message' to handle oneshot ready state.
ORIG-FUN is the original function, MSG-TYPE and MSG are the arguments."
  (funcall orig-fun msg-type msg)
  ;; After handling ready message, check if we need to remind oneshot
  (when (equal msg-type "ready")
    (claude-oneshot--check-ready-state)))

(advice-add 'claude-agent--dispatch-message :around #'claude-oneshot--advice-dispatch-ready)

;; Auto-deny permission requests from oneshot agents
(defun claude-oneshot--advice-auto-deny-permission (orig-fun data)
  "Advice for `claude-agent--show-permission-prompt' to auto-deny for oneshot agents.
ORIG-FUN is the original function, DATA is the permission request data."
  (if claude-oneshot--is-oneshot
      ;; Auto-deny and notify the user, but let the agent continue
      (let ((tool-name (cdr (assq 'tool_name data))))
        (message "Oneshot agent requested permission for '%s' - auto-denied (agent continues)" tool-name)
        ;; Send deny response directly - agent will continue and may try another approach
        (when (and claude-agent--process (process-live-p claude-agent--process))
          (process-send-string
           claude-agent--process
           (concat (json-encode
                    `((type . "permission_response")
                      (action . "deny")))
                   "\n"))))
    ;; Not a oneshot - proceed normally
    (funcall orig-fun data)))

(advice-add 'claude-agent--show-permission-prompt :around #'claude-oneshot--advice-auto-deny-permission)

(provide 'claude-oneshot)
;;; claude-oneshot.el ends here
