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
(require 'claude-agent-permissions)
(require 'claude-mcp)

;; Forward declarations for ACP backend
(defvar claude-mcp-backend)
(defvar claude-acp--backend-active)
(declare-function claude-acp-start-session "claude-acp")
(declare-function claude-acp--process-live-p "claude-acp")
(declare-function claude-acp-shutdown "claude-acp")

;;;; Customization

(defgroup claude-oneshot nil
  "Claude oneshot agent settings."
  :group 'claude-agent)

(defcustom claude-oneshot-model "sonnet"
  "Default model to use for oneshot agents.
Any model alias accepted by the Claude SDK (e.g. \"sonnet\", \"haiku\",
\"opus\") or a full model name (e.g. \"claude-sonnet-4-5-20250929\")."
  :type '(choice (const "haiku")
                 (const "sonnet")
                 (const "opus")
                 (const "default")
                 (string :tag "Other model"))
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
     (:background "#3e4451" :extend t))
    (((class color) (background light))
     (:background "#e5e5e6" :extend t)))
  "Face for highlighting the target region of a oneshot agent.
Matches the lock region face for visual consistency."
  :group 'claude-oneshot)

(defface claude-oneshot-header-face
  '((((class color) (background dark))
     (:foreground "#282c34" :background "#e5c07b" :weight bold))
    (((class color) (background light))
     (:foreground "#fafafa" :background "#986801" :weight bold)))
  "Face for the oneshot agent indicator header line.
Uses amber background matching the modeline style guide."
  :group 'claude-oneshot)

(defface claude-oneshot-label-face
  '((((class color) (background dark))
     (:background "#61afef" :foreground "#282c34" :weight bold :height 0.85))
    (((class color) (background light))
     (:background "#4078f2" :foreground "#fafafa" :weight bold :height 0.85)))
  "Face for oneshot overlay labels (e.g. \" ⚡ Oneshot (region) \").
Matches the lock label face for visual consistency."
  :group 'claude-oneshot)

(defface claude-oneshot-completed-label-face
  '((((class color) (background dark))
     (:background "#61afef" :foreground "#282c34" :weight bold :height 0.85))
    (((class color) (background light))
     (:background "#4078f2" :foreground "#fafafa" :weight bold :height 0.85)))
  "Face for oneshot completion labels (e.g. \" ✓ Completed by *claude:oneshot* \").
Matches the lock label face for visual consistency."
  :group 'claude-oneshot)

(defface claude-oneshot-written-face
  '((((class color) (background dark))
     (:background "#2e4a2e" :extend t))
    (((class color) (background light))
     (:background "#e6ffe6" :extend t)))
  "Face for briefly highlighting newly written content by oneshot."
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
     "2. Call mcp__emacs__done when finished (REQUIRED!)\n"
     "3. If the user asked for an explanation, include it in the done message\n"
     "4. If you need user input, use mcp__emacs__prompt_choice\n"
     "5. Do NOT use watch_buffer, watch_for_pattern, or similar tools\n\n"
     ;; Tool access instructions vary by scope
     (pcase scope
       ((or 'line 'region)
        ;; Line/region scopes: done() replaces the target automatically
        (concat
         "HOW TO MAKE CHANGES:\n"
         "For LINE/REGION scope, the done tool handles everything:\n"
         "  mcp__emacs__done(message, replacement_text)\n"
         "- Pass your NEW content as replacement_text\n"
         "- The target region will be replaced automatically\n"
         "- Do NOT use lock_file or edit tools\n\n"))
       ('buffer
        ;; Buffer scope: use lock/edit workflow
        (concat
         "HOW TO MAKE CHANGES:\n"
         "Use the lock_file → edit workflow:\n"
         "  1. mcp__emacs__lock_file(file_path, start_line, end_line)\n"
         "  2. mcp__emacs__edit(content=\"new content\")\n"
         "The lock auto-releases after edit. File is auto-saved.\n"
         "Call mcp__emacs__done(message) when finished.\n\n"))
       ((or 'directory 'project)
        ;; Directory/project scope: use lock/edit workflow
        (concat
         "HOW TO MAKE CHANGES:\n"
         "Use the lock_file → edit workflow:\n"
         "  1. mcp__emacs__lock_file(file_path, start_line, end_line)\n"
         "  2. mcp__emacs__edit(content=\"new content\")\n"
         "The lock auto-releases after edit. File is auto-saved.\n"
         "You can read files with mcp__emacs__read_file(file_path).\n"
         "Call mcp__emacs__done(message) when finished.\n\n"))
       (_ ""))
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
For line/region scopes, the done() tool handles replacement automatically.
For buffer/directory/project scopes, agents use the lock/edit workflow.

The format follows Claude Code's allowed tools syntax:
- ToolName(path) for specific files (built-in tools only)
- ToolName(dir/*) for directory (built-in tools only)
- ToolName(dir/**) for recursive (built-in tools only)

NOTE: Parameterized permissions do NOT work for MCP tools.  The Claude CLI
only parses parameters for built-in tools (Bash, Edit, Read, etc.).  MCP
tools like `mcp__emacs__lock_file' must be authorized without parameters,
which grants access to all paths.  Path-based restrictions for MCP tools
should be enforced at the MCP server level instead."
  (let ((file (plist-get target-info :file))
        (buffer-name (plist-get target-info :buffer-name))
        (directory (plist-get target-info :directory))
        (project (plist-get target-info :project)))
    (append
     ;; Core MCP tools that all oneshot agents need
     ;; NOTE: MCP tools cannot have parameterized restrictions - the CLI ignores them
     (list
      ;; Completion tool - always needed (handles replacement for line/region)
      "mcp__emacs__done"
      ;; User interaction
      "mcp__emacs__prompt_choice"
      "mcp__emacs__confirm")
     ;; Scope-specific tools
     (pcase scope
       ;; Line/region scope: done() handles replacement, no lock/edit needed
       ((or 'line 'region)
        ;; Allow reading the file for context
        ;; Built-in Read tool supports path restriction; MCP read_file does not
        (when file
          (list (format "Read(%s)" file)
                "mcp__emacs__read_file")))
       ;; Buffer scope: needs lock/edit workflow
       ('buffer
        (append
         ;; MCP tools: plain names only (no parameterization supported)
         (list "mcp__emacs__edit" "mcp__emacs__unlock"
               "mcp__emacs__lock_file" "mcp__emacs__lock_buffer"
               "mcp__emacs__read_file" "mcp__emacs__read_buffer")
         ;; Built-in tools: can use path restrictions
         (when file
           (list (format "Read(%s)" file)
                 (format "Glob(%s)" (file-name-directory file))))))
       ;; Directory scope: needs lock/edit workflow
       ('directory
        (when directory
          (list ;; MCP tools: plain names (path restrictions not supported)
                "mcp__emacs__edit" "mcp__emacs__unlock"
                "mcp__emacs__lock_file" "mcp__emacs__read_file"
                ;; Built-in tools: can use path restrictions
                (format "Read(%s*)" directory)
                (format "Glob(%s)" directory))))
       ;; Project scope: needs lock/edit workflow
       ('project
        (when project
          (list ;; MCP tools: plain names (path restrictions not supported)
                "mcp__emacs__edit" "mcp__emacs__unlock"
                "mcp__emacs__lock_file" "mcp__emacs__read_file"
                ;; Built-in tools: can use path restrictions
                (format "Read(%s**)" project)
                (format "Glob(%s)" project))))
       (_ nil)))))

(defun claude-oneshot--create-target-overlay (buffer2 start end &optional scope)
  "Create an overlay in BUFFER2 from START to END to highlight the target.
SCOPE is the oneshot scope (line, region, buffer, etc.) for the label."
  (when (buffer-live-p buffer2)
    (with-current-buffer buffer2
      (let* ((ov (make-overlay start end))
             (scope-str (if scope (symbol-name scope) "oneshot"))
             ;; Create label using the defined face (matches lock overlay style)
             (label (propertize (format " ⚡ Oneshot (%s) " scope-str)
                                'face 'claude-oneshot-label-face)))
        (overlay-put ov 'face 'claude-oneshot-target-face)
        (overlay-put ov 'claude-oneshot t)
        (overlay-put ov 'priority 100)
        ;; Add before-string with label (matching lock style)
        (overlay-put ov 'before-string (concat label "\n"))
        (overlay-put ov 'help-echo (format "Oneshot agent target (%s scope)" scope-str))
        ;; Protect the region from user edits while oneshot is working
        (overlay-put ov 'modification-hooks
                     (list (lambda (_ov after-p _beg _end &optional _len)
                             (unless after-p
                               (error "This region is being edited by a oneshot agent")))))
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
The tooltip shows the completion message using a label style matching lock overlays.
Uses the label style per STYLE_GUIDE.md instead of box-drawn tooltips."
  (when (and (buffer-live-p buffer) message (not (string-empty-p message)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char position)
        ;; Move to end of line to place tooltip after content
        (end-of-line)
        (let* ((ov (make-overlay (point) (point)))
               ;; Truncate long messages for the label
               (truncated-msg (if (> (length message) 60)
                                  (concat (substring message 0 57) "...")
                                message))
               ;; Create label in the style of lock overlays
               (label (propertize (format " ✓ %s " truncated-msg)
                                  'face 'claude-oneshot-completed-label-face))
               ;; Add hint as a separate dimmed line
               (hint (propertize " (C-c c y to dismiss)"
                                 'face '(:foreground "#5c6370" :height 0.85)))
               (tooltip-content (concat "\n" label hint)))
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
                                                (with-current-buffer source-buffer (point))))
      ;; Set up permission rules for oneshot agents
      ;; Auto-deny all permission requests - oneshot agents use pre-authorized tools only
      (setq-local claude-agent-permission-rules-local
                  `((:match t
                     :action :deny
                     :reason ,(format "Oneshot agent (%s scope) - use pre-authorized tools only"
                                      scope)))))
    ;; Create visual highlight in source buffer for line/region scope
    (when (and (memq scope '(line region))
               (plist-get target :start)
               (plist-get target :end))
      (let ((ov (claude-oneshot--create-target-overlay
                 source-buffer
                 (plist-get target :start)
                 (plist-get target :end)
                 scope)))
        (with-current-buffer buf
          (setq claude-oneshot--target-overlay ov))))

    ;; Register this agent in the source buffer (for header line indicator)
    (claude-oneshot--register-in-source source-buffer buf)

    ;; Start the process (no --continue to avoid context bleed)
    ;; Pass scope-appropriate allowed tools so the agent can edit without permission prompts
    ;; System prompt is sent via stdin as system_message (injected into first user message)
    (let* ((system-prompt (claude-oneshot--get-scope-system-prompt scope target))
           (allowed-tools (claude-oneshot--get-allowed-tools-for-scope scope target)))
      ;; Start appropriate backend
      (if (eq (bound-and-true-p claude-mcp-backend) 'acp)
          ;; ACP backend
          (with-current-buffer buf
            (require 'claude-acp)
            (setq-local claude-acp--backend-active t)
            (claude-acp-start-session buf work-dir buf-name
                                      nil system-prompt claude-oneshot-model))
        ;; Python backend
        (let ((proc (claude-agent--start-process
                     work-dir buf nil nil claude-oneshot-model nil allowed-tools)))
          (with-current-buffer buf
            (setq claude-agent--process proc))))

      (with-current-buffer buf
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

      ;; Send system message first, then user prompt after a short delay
      (run-with-timer
       1.5 nil
       (lambda (buffer sys-prompt user-prompt)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (claude-agent--backend-alive-p)
               ;; Send system message (queued for ACP, sent directly for Python)
               (claude-agent--backend-send-json
                `((type . "system_message")
                  (text . ,sys-prompt)))
               ;; Then send the user request
               (claude-agent--backend-send-json
                `((type . "message")
                  (text . ,user-prompt)))))))
       buf system-prompt prompt))

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
               (when (claude-agent--backend-alive-p)
                 (claude-agent--backend-send-json
                  '((type . "message")
                    (text . "REMINDER: You're in oneshot mode. Call mcp__emacs__done if finished, or use mcp__emacs__prompt_choice/mcp__emacs__confirm if you need user input.")))))))
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

      ;; Kill the backend if still running
      (claude-agent--backend-shutdown)

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

(defun claude-mcp-done (&optional message replacement-text)
  "Signal that a oneshot agent has completed its task.
Optional MESSAGE is displayed to the user and shown as a tooltip in the source buffer.
Optional REPLACEMENT-TEXT replaces the target region (for line/region scopes).
This tool should be called by oneshot agents when they finish."
  (let ((buf (current-buffer)))
    ;; Check if we're actually in a oneshot buffer
    (if (and (boundp 'claude-oneshot--is-oneshot)
             claude-oneshot--is-oneshot)
        (let ((source-buf claude-oneshot--source-buffer)
              (tooltip-pos claude-oneshot--target-position)
              (tooltip-msg message)
              (target-ov claude-oneshot--target-overlay))
          ;; Replace target region if replacement text provided
          (when (and replacement-text source-buf target-ov
                     (overlay-buffer target-ov))
            (with-current-buffer source-buf
              ;; Save window configuration to preserve cursor position
              (let ((windows (get-buffer-window-list source-buf nil t)))
                (dolist (win windows)
                  (with-selected-window win
                    (let ((saved-point (point))
                          (saved-window-start (window-start)))
                      ;; Replace the overlay region
                      (let ((start (overlay-start target-ov))
                            (end (overlay-end target-ov)))
                        (save-excursion
                          (goto-char start)
                          (delete-region start end)
                          (insert replacement-text)))
                      ;; Restore cursor position if it was before the change
                      (when (< saved-point (overlay-start target-ov))
                        (goto-char saved-point))
                      (set-window-start win saved-window-start t)))))))
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
  "Signal completion of a oneshot task. For LINE/REGION scopes, pass replacement_text to replace the target. The oneshot agent will be terminated and the user notified."
  :function #'claude-mcp-done
  :safe t
  :needs-session-cwd t
  :args ((message string "Completion message to show the user")
         (replacement_text string "New content to replace the target region (for line/region scopes)")))

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
                     claude-oneshot--source-buffer start end 'updated))))))))
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
(defun claude-oneshot-visit ()
  "Visit an active oneshot agent buffer with completion.
If only one agent is active, switch to it directly.
If no agents are active, show a message."
  (interactive)
  ;; First, clean up any dead agents from the hash table
  (let ((dead-agents '()))
    (maphash
     (lambda (name info)
       (unless (buffer-live-p (plist-get info :buffer))
         (push name dead-agents)))
     claude-oneshot--active-agents)
    (dolist (name dead-agents)
      (remhash name claude-oneshot--active-agents)))
  ;; Now show the live agents
  (if (= (hash-table-count claude-oneshot--active-agents) 0)
      (message "No active oneshot agents")
    (let ((agents '()))
      ;; Build list of agent names with annotations
      (maphash
       (lambda (name info)
         (let* ((buf (plist-get info :buffer))
                (scope (plist-get info :scope))
                (start-time (plist-get info :start-time))
                (elapsed (float-time (time-subtract (current-time) start-time)))
                (annotation (format " [%s, %.0fs]" scope elapsed)))
           (push (cons name (list :annotation annotation :buffer buf))
                 agents)))
       claude-oneshot--active-agents)
      (if (= (length agents) 1)
          ;; Only one agent, switch directly
          (switch-to-buffer (plist-get (cdar agents) :buffer))
        ;; Multiple agents, prompt with completion
        (let* ((completion-extra-properties
                `(:annotation-function
                  ,(lambda (name)
                     (plist-get (cdr (assoc name agents)) :annotation))))
               (selected (completing-read "Visit oneshot agent: "
                                          (mapcar #'car agents)
                                          nil t))
               (buf (plist-get (cdr (assoc selected agents)) :buffer)))
          (switch-to-buffer buf))))))

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



(provide 'claude-oneshot)
;;; claude-oneshot.el ends here
