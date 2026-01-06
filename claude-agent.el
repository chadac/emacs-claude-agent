;;; claude-agent.el --- Claude interaction buffer -*- lexical-binding: t; -*-

;; This file is part of Claude.
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This module provides a single-buffer interface for interacting with Claude.
;; The buffer is organized into distinct sections:
;;
;; 1. Header Section: Session name (read-only)
;; 2. Log Section: Conversation history (read-only, append-only)
;; 3. Status Section: Model/cost/session info + thinking indicator (read-only)
;; 4. Input Section: Header line (read-only) + editable typing area
;;
;; The Python agent outputs structured markers which are parsed and formatted.

;;; Code:

(require 'ansi-color)
(require 'org)
(require 'transient)
(require 'claude-mcp)
(require 'claude-transient)

;;;; Customization

(defgroup claude-agent nil
  "Claude interaction buffer."
  :group 'Claude)

(defcustom claude-agent-python-command "uv"
  "Command to run Python for the agent wrapper."
  :type 'string
  :group 'claude-agent)

(defcustom claude-agent-enable-mcp t
  "Whether to enable the Emacs MCP server for Claude sessions.
When non-nil, Claude can interact with Emacs buffers via MCP tools.
Requires the Emacs server to be running (`server-start')."
  :type 'boolean
  :group 'claude-agent)

(defcustom claude-agent-disallowed-tools '()
  "List of tools that should be disallowed for the Claude agent.
Example: Setting to \\='(\"WebSearch\") would disable web search."
  :type '(repeat string)
  :group 'claude-agent)

;;;; Faces

(defface claude-agent-header-face
  '((t :foreground "#56b6c2" :slant italic))
  "Face for the header section."
  :group 'claude-agent)

(defface claude-agent-user-header-face
  '((t :foreground "#61afef" :weight bold))
  "Face for user message headers."
  :group 'claude-agent)

(defface claude-agent-user-face
  '((t :foreground "#c8ccd4"))  ; Slightly off-white (lighter than default)
  "Face for user message text."
  :group 'claude-agent)

(defface claude-agent-assistant-header-face
  '((t :foreground "#c678dd" :weight bold))
  "Face for assistant message headers."
  :group 'claude-agent)

(defface claude-agent-assistant-face
  '((t :foreground "#e5e5e5"))
  "Face for assistant message text."
  :group 'claude-agent)

(defface claude-agent-tool-face
  '((t :foreground "#e5c07b" :slant italic))
  "Face for tool call indicators."
  :group 'claude-agent)

(defface claude-agent-status-face
  '((t :foreground "#56b6c2" :slant italic))
  "Face for status info section (model, cost, session)."
  :group 'claude-agent)

(defface claude-agent-thinking-face
  '((t :foreground "#98c379" :weight bold))
  "Face for thinking indicator."
  :group 'claude-agent)

(defface claude-agent-progress-face
  '((t :foreground "#61afef"))
  "Face for progress indicators."
  :group 'claude-agent)

(defface claude-agent-progress-header-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for progress section header."
  :group 'claude-agent)

(defface claude-agent-compacting-face
  '((t :foreground "#e5c07b" :weight bold :slant italic))
  "Face for compacting indicator (yellow/warning color)."
  :group 'claude-agent)

(defface claude-agent-todo-pending-face
  '((t :foreground "#5c6370"))
  "Face for pending todo items."
  :group 'claude-agent)

(defface claude-agent-todo-in-progress-face
  '((t :foreground "#61afef" :weight bold))
  "Face for in-progress todo items."
  :group 'claude-agent)

(defface claude-agent-todo-completed-face
  '((t :foreground "#98c379" :strike-through t))
  "Face for completed todo items."
  :group 'claude-agent)

(defface claude-agent-error-face
  '((t :foreground "#e06c75" :weight bold))
  "Face for error messages."
  :group 'claude-agent)

(defface claude-agent-session-face
  '((t :foreground "#56b6c2" :slant italic))
  "Face for session info messages."
  :group 'claude-agent)

(defface claude-agent-input-header-face
  '((t :foreground "#5c6370" :weight bold))
  "Face for the input area header."
  :group 'claude-agent)

(defface claude-agent-diff-removed
  '((t :inherit diff-refine-removed))
  "Face for removed lines in diff display."
  :group 'claude-agent)

(defface claude-agent-diff-added
  '((t :inherit diff-refine-added))
  "Face for added lines in diff display."
  :group 'claude-agent)

(defface claude-agent-diff-header
  '((t :foreground "#5c6370"))
  "Face for diff box drawing characters."
  :group 'claude-agent)

(defface claude-agent-file-link
  '((t :inherit link :underline t))
  "Face for clickable file paths."
  :group 'claude-agent)

(defface claude-agent-line-number
  '((t :foreground "#5c6370"))
  "Face for line numbers in file content display."
  :group 'claude-agent)


;;;; Buffer-local variables - Section markers
;;
;; Buffer has 3 zones with different update semantics:
;;
;;   [STATIC]  - Header + completed conversation turns
;;               Append-only, never modified after written
;;               Ends at `static-end-marker`
;;
;;   [DYNAMIC] - Current in-progress turn + status bar
;;               Fully deleted and rebuilt on each update
;;               Content stored in variables, rendered fresh each time
;;
;;   [INPUT]   - User typing area
;;               Preserved across dynamic rebuilds
;;               Starts at `input-start-marker` (set after each rebuild)

(defvar-local claude-agent--process nil
  "The agent process for this session.")

(defvar-local claude-agent--static-end-marker nil
  "Marker for end of static section (start of dynamic section).
Everything before this is completed content that never changes.")

(defvar-local claude-agent--input-start-marker nil
  "Marker for start of input section (where user types).
Set fresh after each dynamic section rebuild.")

;;;; Buffer-local variables - State

(defvar-local claude-agent--parse-state nil
  "Current parsing state: nil, user, assistant, tool, error, session.")

(defvar-local claude-agent--pending-output ""
  "Buffer for incomplete lines from process output.")

(defvar-local claude-agent--session-info nil
  "Plist with session info: :model :session-id :cost.")

(defvar-local claude-agent--mcp-server-status nil
  "List of MCP server status objects from the agent.
Each element is an alist with keys: name, status.")

(defvar-local claude-agent--input-history nil
  "History of inputs sent to Claude.")

(defvar-local claude-agent--input-history-index 0
  "Current position in input history.")

;;;; Buffer-local variables - Thinking status

(defconst claude-agent--spinner-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames for the spinner animation.")



(defvar-local claude-agent--spinner-index 0
  "Current index in spinner frames.")

(defvar-local claude-agent--spinner-timer nil
  "Timer for spinner animation.")

(defvar-local claude-agent--thinking-start-time nil
  "Time when thinking started, for elapsed time display.")

(defvar-local claude-agent--thinking-status nil
  "Current thinking status text, or nil if not thinking.")

(defvar-local claude-agent--input-tokens 0
  "Input token count for current turn.")

(defvar-local claude-agent--output-tokens 0
  "Output token count for current turn.")

;;;; Buffer-local variables - Progress indicators

(defvar-local claude-agent--progress-indicators nil
  "Hash table of active progress indicators.
Keys are progress IDs, values are plists with :message and :start-time.")

(defvar-local claude-agent--progress-visible t
  "Whether the progress section is visible.")

(defvar-local claude-agent--compacting nil
  "Non-nil when the conversation is being compacted.
This is set when Claude is summarizing the conversation history.")

(defvar-local claude-agent--todos-visible t
  "Whether the todo list section is visible.")

(defvar-local claude-agent--todos nil
  "List of current todo items.
Each item is an alist with keys: content, status, activeForm.")

(defvar-local claude-agent--has-conversation nil
  "Non-nil if conversation has started (first message sent).")

(defvar-local claude-agent--tool-results nil
  "Alist mapping tool call positions to their results.
Each entry is (MARKER NAME . RESULT-STRING).")

(defvar-local claude-agent--last-tool-marker nil
  "Marker for the last tool call, used to associate results.")

(defvar-local claude-agent--last-tool-name nil
  "Name of the last tool call, used to associate results.")

(defvar-local claude-agent--placeholder-overlay nil
  "Overlay for the placeholder text in empty input area.")

(defvar-local claude-agent--work-dir nil
  "The working directory for this Claude session.")

(defconst claude-agent--placeholder-text "Enter your message... (C-c C-c to send)"
  "Placeholder text shown when input area is empty.")

(defface claude-agent-placeholder-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for placeholder text in empty input area."
  :group 'claude-agent)

;;;; Buffer-local variables - Input mode

(defvar-local claude-agent--input-mode 'text
  "Current input mode: `text' for normal input, `permission' for permission prompt.")

(defvar-local claude-agent--saved-input ""
  "Saved input text when switching away from text mode.")

;;;; Buffer-local variables - Message queue

(defvar-local claude-agent--message-queue nil
  "List of messages queued while agent is busy. Each is a string.")


(defface claude-agent-queued-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for queued messages (grayed out)."
  :group 'claude-agent)

(defface claude-agent-queued-header-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for queued message headers."
  :group 'claude-agent)

;;;; Mode definition

(defvar claude-agent-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Standard Emacs-style bindings (work everywhere)
    (define-key map (kbd "C-c C-c") #'claude-agent-send)
    (define-key map (kbd "C-<return>") #'claude-agent-send)
    (define-key map (kbd "C-c C-k") #'claude-agent-interrupt)
    (define-key map (kbd "C-c C-q") #'claude-agent-quit)
    (define-key map (kbd "M-p") #'claude-agent-previous-input)
    (define-key map (kbd "M-n") #'claude-agent-next-input)
    ;; Transient menu (C-c c for "claude")
    (define-key map (kbd "C-c c") #'claude-menu)
    map)
  "Keymap for `claude-agent-mode'.")

(defvar claude-agent-log-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Magit-style single-key bindings for the log/read-only area
    ;; Model
    (define-key map (kbd "m") #'claude-agent-set-model)
    (define-key map (kbd "$") #'claude-agent-show-cost)
    ;; MCP
    (define-key map (kbd "M l") #'claude-agent-mcp-list)
    (define-key map (kbd "M s") #'claude-agent-show-mcp-status)
    (define-key map (kbd "M a") #'claude-agent-mcp-add)
    (define-key map (kbd "M r") #'claude-agent-mcp-remove)
    ;; Session control
    (define-key map (kbd "c") #'claude-agent-compact)
    (define-key map (kbd "C") #'claude-agent-clear)
    (define-key map (kbd "q") #'claude-agent-quit)
    (define-key map (kbd "k") #'claude-agent-interrupt)
    ;; Navigation - go to input
    (define-key map (kbd "i") #'claude-agent-goto-input)
    (define-key map (kbd "RET") #'claude-agent-goto-input)
    ;; Tool result viewing
    (define-key map (kbd "'") #'claude-agent-show-tool-result)
    (define-key map (kbd "TAB") #'claude-agent-toggle-tool-popup)
    ;; Navigation between messages/tool calls
    (define-key map (kbd "{") #'claude-agent-previous-section)
    (define-key map (kbd "}") #'claude-agent-next-section)
    ;; Help
    (define-key map (kbd "?") #'claude-menu)
    map)
  "Keymap for the read-only log area in `claude-agent-mode'.
These single-key bindings only apply outside the input area.")

(define-derived-mode claude-agent-mode fundamental-mode "Claude"
  "Major mode for Claude interaction buffer."
  :group 'claude-agent
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local buffer-read-only nil)
  (visual-line-mode 1)
  ;; Set up org-mode fontification without org-mode keybindings
  ;; This gives us org syntax highlighting (bold, italic, code, src blocks)
  (require 'org)
  (org-set-font-lock-defaults)
  (font-lock-mode 1)
  ;; Disable flycheck and company to prevent expensive syntax parsing
  (when (bound-and-true-p flycheck-mode)
    (flycheck-mode -1))
  (when (bound-and-true-p company-mode)
    (company-mode -1))
  ;; Ensure our keybindings are set (defvar doesn't reinit on re-eval)
  (use-local-map claude-agent-mode-map)
  ;; Re-define keys to ensure they're set
  (local-set-key (kbd "C-c C-c") #'claude-agent-send)
  (local-set-key (kbd "C-<return>") #'claude-agent-send)
  (local-set-key (kbd "C-c C-k") #'claude-agent-interrupt)
  (local-set-key (kbd "C-c C-q") #'claude-agent-quit)
  (local-set-key (kbd "M-p") #'claude-agent-previous-input)
  (local-set-key (kbd "M-n") #'claude-agent-next-input)
  (local-set-key (kbd "C-c '") #'claude-agent-show-tool-result)
  ;; Set up placeholder update hook
  (add-hook 'post-command-hook #'claude-agent--post-command-hook nil t)
  ;; Set up evil insert state entry hook to move to input area
  ;; Use after-change-major-mode-hook to ensure evil is loaded
  (add-hook 'evil-insert-state-entry-hook
            #'claude-agent--on-insert-state-entry nil t)
  ;; Add C-c c for transient menu (works in all states)
  (local-set-key (kbd "C-c c") #'claude-menu))

;;;; Helper functions

(defun claude-agent--in-input-area-p ()
  "Return t if point is in the input area."
  (and claude-agent--input-start-marker
       (>= (point) claude-agent--input-start-marker)))

(defun claude-agent--scroll-to-bottom ()
  "Scroll window to show maximum content with input area near bottom.
This eliminates empty space below the input area."
  (when-let ((win (get-buffer-window (current-buffer))))
    (with-selected-window win
      ;; Recenter with point near the bottom (negative arg = lines from bottom)
      (recenter -3))))

(defun claude-agent--add-tool-tooltip (marker content)
  "Add a tooltip with CONTENT preview to the tool call at MARKER."
  (when (and marker (marker-position marker))
    (let* ((line-end (save-excursion
                       (goto-char (marker-position marker))
                       (line-end-position)))
           ;; Truncate content for tooltip (first 500 chars, max 10 lines)
           (preview (if (> (length content) 500)
                        (concat (substring content 0 500) "\n...")
                      content))
           (preview (let ((lines (split-string preview "\n")))
                      (if (> (length lines) 10)
                          (concat (string-join (seq-take lines 10) "\n") "\n...")
                        preview))))
      ;; Create overlay for the whole tool call line
      (let ((ov (make-overlay (marker-position marker) line-end)))
        (overlay-put ov 'help-echo preview)
        (overlay-put ov 'claude-agent-tooltip t)
        (overlay-put ov 'evaporate t)))))

(defvar-local claude-agent--tool-popup-enabled t
  "When non-nil, show tool result popup automatically when on a tool line.")

(defvar claude-agent--tool-popup-buffer " *claude-tool-popup*"
  "Buffer name for the tool result posframe.")

(defun claude-agent-toggle-tool-popup ()
  "Toggle automatic tool result popup display."
  (interactive)
  (setq claude-agent--tool-popup-enabled (not claude-agent--tool-popup-enabled))
  (if claude-agent--tool-popup-enabled
      (message "Tool popup enabled")
    (claude-agent--hide-tool-popup)
    (message "Tool popup disabled")))

(defun claude-agent--hide-tool-popup ()
  "Hide the tool result posframe."
  (when (and (fboundp 'posframe-hide)
             (get-buffer claude-agent--tool-popup-buffer))
    (posframe-hide claude-agent--tool-popup-buffer)))

(defface claude-agent-popup-hint-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for hint text in tool popup."
  :group 'claude-agent)

(defun claude-agent--show-tool-popup (content)
  "Show CONTENT in a posframe below the current line."
  (when (fboundp 'posframe-show)
    (let* ((max-lines 15)
           (max-chars 1000)
           ;; Truncate content
           (preview (if (> (length content) max-chars)
                        (concat (substring content 0 max-chars) "\n...")
                      content))
           (lines (split-string preview "\n"))
           (preview (if (> (length lines) max-lines)
                        (concat (string-join (seq-take lines max-lines) "\n") "\n...")
                      preview))
           ;; Add hints at the bottom
           (hints "\n─────────────────────────────────\nC-c ' full result  |  TAB disable popup")
           (full-content (concat preview hints)))
      (posframe-show claude-agent--tool-popup-buffer
                     :string full-content
                     :position (line-end-position)
                     :background-color "#1e1e1e"
                     :foreground-color "#abb2bf"
                     :border-color "#5c6370"
                     :border-width 1
                     :left-fringe 8
                     :right-fringe 8))))

(defun claude-agent--update-tool-popup ()
  "Update tool popup based on current cursor position.
Called from `post-command-hook'."
  (when (and claude-agent--tool-popup-enabled
             ;; Don't update during buffer modifications
             (not inhibit-read-only)
             ;; Only in claude-agent buffers
             (eq major-mode 'claude-agent-mode))
    (condition-case nil
        (if-let ((result (claude-agent--find-tool-result-at-point)))
            (let* ((name (car result))
                   (content (cdr result))
                   (formatter (cdr (assoc name claude-agent-tool-formatters)))
                   (formatted (if formatter (funcall formatter content) content)))
              (claude-agent--show-tool-popup formatted))
          (claude-agent--hide-tool-popup))
      ;; Silently ignore errors to prevent buffer corruption
      (error (claude-agent--hide-tool-popup)))))

(defvar claude-agent-tool-formatters
  '(("mcp__claudemacs__edit" . claude-agent--format-diff-output)
    ("Edit" . claude-agent--format-diff-output))
  "Alist mapping tool names to formatter functions.
Each formatter takes a result string and returns a propertized string.")

(defun claude-agent--format-diff-output (content)
  "Format CONTENT as a diff with colored +/- lines."
  (let ((lines (split-string content "\n")))
    (mapconcat
     (lambda (line)
       (cond
        ((string-prefix-p "- " line)
         (propertize line 'face 'claude-agent-diff-removed))
        ((string-prefix-p "+ " line)
         (propertize line 'face 'claude-agent-diff-added))
        (t line)))
     lines "\n")))

(defun claude-agent--find-tool-result-at-point ()
  "Find the tool result for the tool call on the current line.
Returns (NAME . RESULT) cons or nil if not found."
  (let ((line-start (line-beginning-position))
        (line-end (line-end-position))
        (result nil))
    ;; Find a tool marker on this line
    (dolist (entry claude-agent--tool-results)
      (let ((marker (car entry)))
        (when (and (marker-position marker)
                   (>= (marker-position marker) line-start)
                   (<= (marker-position marker) line-end))
          (setq result entry))))
    ;; Return (NAME . RESULT) or nil
    (when result
      (cons (nth 1 result) (nth 2 result)))))

(defun claude-agent-show-tool-result ()
  "Show the result of the tool call at point in a popup buffer.
Like `org-edit-special' (C-c ') for source blocks."
  (interactive)
  (if-let ((result (claude-agent--find-tool-result-at-point)))
      (let* ((name (car result))
             (content (cdr result))
             (formatter (cdr (assoc name claude-agent-tool-formatters)))
             (formatted (if formatter (funcall formatter content) content))
             (buf (get-buffer-create "*claude-tool-result*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert formatted)
            (goto-char (point-min))
            (special-mode)))
        (display-buffer buf '(display-buffer-below-selected
                              (window-height . 0.4))))
    (message "No tool result found at point")))

(defun claude-agent-goto-input ()
  "Move point to the input area."
  (interactive)
  (when claude-agent--input-start-marker
    (goto-char claude-agent--input-start-marker)
    ;; Enter insert state if using evil
    (when (and (bound-and-true-p evil-local-mode)
               (fboundp 'evil-insert-state))
      (evil-insert-state))))

(defun claude-agent--section-header-p ()
  "Return non-nil if current line is a section header (message or tool call)."
  (save-excursion
    (beginning-of-line)
    (or (looking-at "^you> ")
        (looking-at "^claude> ")
        (looking-at "^[a-z-]+/[a-z-]+›")  ; MCP tool: server/tool›
        (looking-at "^[a-z]+›"))))          ; Built-in tool: edit›, grep›, etc.

(defun claude-agent-next-section ()
  "Move to the next message or tool call."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (claude-agent--section-header-p))
                (< (point) (or claude-agent--input-start-marker (point-max))))
      (forward-line 1))
    (if (or (eobp)
            (>= (point) (or claude-agent--input-start-marker (point-max))))
        (progn
          (goto-char start)
          (message "No more sections"))
      (beginning-of-line))))

(defun claude-agent-previous-section ()
  "Move to the previous message or tool call."
  (interactive)
  (let ((start (point)))
    (beginning-of-line)
    (when (claude-agent--section-header-p)
      (forward-line -1))
    (while (and (not (bobp))
                (not (claude-agent--section-header-p)))
      (forward-line -1))
    (if (bobp)
        (if (claude-agent--section-header-p)
            (beginning-of-line)
          (goto-char start)
          (message "No more sections"))
      (beginning-of-line))))

(defvar-local claude-agent--in-log-area nil
  "Non-nil when cursor is in the log area (not input area).
Used to track keymap state changes.")

(defun claude-agent--update-keymap ()
  "Update the active keymap based on cursor position.
When in the log area, enable magit-style single-key bindings.
When in the input area, use normal text input bindings."
  (let ((in-log (not (claude-agent--in-input-area-p))))
    (unless (eq in-log claude-agent--in-log-area)
      (setq claude-agent--in-log-area in-log)
      (if in-log
          ;; Entering log area - add log keymap as minor mode map
          (progn
            (setq-local minor-mode-overriding-map-alist
                        (cons (cons 'claude-agent--in-log-area claude-agent-log-mode-map)
                              (assq-delete-all 'claude-agent--in-log-area
                                               minor-mode-overriding-map-alist))))
        ;; Entering input area - remove log keymap
        (setq-local minor-mode-overriding-map-alist
                    (assq-delete-all 'claude-agent--in-log-area
                                     minor-mode-overriding-map-alist))))))

(defmacro claude-agent--in-base-buffer (&rest body)
  "Execute BODY in the base buffer (for polymode compatibility)."
  `(let ((base (or (buffer-base-buffer) (current-buffer))))
     (with-current-buffer base
       ,@body)))

;;;; Placeholder management

(defun claude-agent--input-empty-p ()
  "Return t if the input area is empty (only whitespace)."
  (and claude-agent--input-start-marker
       ;; string-blank-p returns match position (0) for empty, so convert to t
       (not (null (string-blank-p (buffer-substring-no-properties
                                   claude-agent--input-start-marker (point-max)))))))

(defun claude-agent--update-placeholder ()
  "Show or hide placeholder based on input area content."
  (when (and claude-agent--input-start-marker
             (marker-position claude-agent--input-start-marker))
    (if (claude-agent--input-empty-p)
        ;; Show placeholder at current input-start position
        (let ((pos (marker-position claude-agent--input-start-marker)))
          ;; Move existing overlay or create new one
          (if (and claude-agent--placeholder-overlay
                   (overlay-buffer claude-agent--placeholder-overlay))
              ;; Move to new position
              (move-overlay claude-agent--placeholder-overlay pos pos)
            ;; Create new overlay
            (setq claude-agent--placeholder-overlay (make-overlay pos pos))
            (overlay-put claude-agent--placeholder-overlay 'before-string
                         (propertize claude-agent--placeholder-text
                                     'face 'claude-agent-placeholder-face
                                     'cursor t))
            (overlay-put claude-agent--placeholder-overlay 'evaporate nil)))
      ;; Hide placeholder
      (when (and claude-agent--placeholder-overlay
                 (overlay-buffer claude-agent--placeholder-overlay))
        (delete-overlay claude-agent--placeholder-overlay)
        (setq claude-agent--placeholder-overlay nil)))))

(defun claude-agent--in-insert-state-p ()
  "Return t if evil-mode is active and in insert state."
  (and (bound-and-true-p evil-local-mode)
       (eq evil-state 'insert)))

(defun claude-agent--on-insert-state-entry ()
  "Hook called when entering evil insert state.
Moves cursor to input area if currently outside it."
  (when (and claude-agent--input-start-marker
             (marker-position claude-agent--input-start-marker)
             (< (point) claude-agent--input-start-marker))
    (goto-char claude-agent--input-start-marker)))

(defun claude-agent--post-command-hook ()
  "Hook run after each command to update placeholder visibility.
Also constrains cursor to input area when in evil insert state,
and switches keymaps based on cursor position."
  (claude-agent--update-placeholder)
  ;; In insert mode, keep cursor in input area
  (when (and claude-agent--input-start-marker
             (marker-position claude-agent--input-start-marker)
             (claude-agent--in-insert-state-p)
             (< (point) claude-agent--input-start-marker))
    (goto-char claude-agent--input-start-marker))
  ;; If input is empty and we're in the input area, keep cursor at prompt
  (when (and (claude-agent--input-empty-p)
             (claude-agent--in-input-area-p)
             claude-agent--input-start-marker)
    (goto-char claude-agent--input-start-marker))
  ;; Switch keymaps based on position (magit-style in log area)
  (claude-agent--update-keymap)
  ;; Update tool result popup
  (claude-agent--update-tool-popup))

;;;; Section management
;;
;; Two-zone architecture:
;; - Static section: Append-only (header + conversation log)
;; - Dynamic section: Re-rendered from state (status bar + input area)

(defun claude-agent--init-buffer (session-name)
  "Initialize buffer with section structure for SESSION-NAME."
  (let ((inhibit-read-only t))
    (erase-buffer)

    ;; === HEADER (part of static section) ===
    (let ((start (point)))
      (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
      (insert (format " Claude Session: %s\n" session-name))
      (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
      (claude-agent--apply-face start (point) 'claude-agent-header-face)
      ;; Mark as fontified to prevent org-mode font-lock from interfering
      (put-text-property start (point) 'fontified t))

    ;; === STATIC END MARKER ===
    ;; Everything before this is committed content that never changes
    (setq claude-agent--static-end-marker (point-marker))
    (set-marker-insertion-type claude-agent--static-end-marker nil)

    ;; === INPUT START MARKER ===
    ;; Set initially at same position, will be updated by rebuild
    (setq claude-agent--input-start-marker (point-marker))
    (set-marker-insertion-type claude-agent--input-start-marker nil)

    ;; Show placeholder and position cursor
    (claude-agent--update-placeholder)
    (goto-char claude-agent--input-start-marker)

    ;; Make everything before input read-only
    (claude-agent--update-read-only)))

(defun claude-agent--apply-face (start end face)
  "Apply FACE to region from START to END using overlay."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'priority 100)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'claude-agent-styled t))
  ;; Also apply to indirect buffers (polymode)
  (let ((base (current-buffer)))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (eq (buffer-base-buffer buf) base))
        (with-current-buffer buf
          (let ((ov (make-overlay start end)))
            (overlay-put ov 'face face)
            (overlay-put ov 'priority 100)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'claude-agent-styled t)))))))

(defun claude-agent--update-read-only ()
  "Update read-only text property to cover everything before prompt marker.
Optimized to only modify the dynamic section, not the entire buffer."
  ;; Use text properties for read-only (overlays don't enforce read-only)
  (when (and claude-agent--input-start-marker
             (> (marker-position claude-agent--input-start-marker) (point-min)))
    ;; Only remove read-only from the dynamic section (after static-end).
    ;; The static section already has read-only applied and doesn't change.
    ;; This is O(dynamic-section-size) instead of O(buffer-size).
    (when (and claude-agent--static-end-marker
               (marker-position claude-agent--static-end-marker))
      (remove-list-of-text-properties
       claude-agent--static-end-marker (point-max)
       '(read-only rear-nonsticky)))
    ;; Apply read-only to everything before prompt, with rear-nonsticky
    ;; so text inserted at the boundary is NOT read-only.
    ;; Note: This still applies to the full static section, but add-text-properties
    ;; is fast when properties already exist (it's a no-op for unchanged regions).
    (add-text-properties (point-min) claude-agent--input-start-marker
                         '(read-only t rear-nonsticky (read-only)))))

;;;; Dynamic section management
;;
;; Two-zone architecture:
;; - Static section: Append-only log content (header + conversation)
;; - Dynamic section: Re-rendered from state (status bar + input area)
;;
;; `append-to-static` appends to the static section directly.
;; `render-dynamic-section` clears and re-renders dynamic section from state.

(defun claude-agent--get-input-text ()
  "Get the current text in the input area."
  (if (and claude-agent--input-start-marker
           (marker-position claude-agent--input-start-marker))
      (buffer-substring-no-properties
       claude-agent--input-start-marker (point-max))
    ""))

(defun claude-agent--append-to-static (text)
  "Append TEXT to the static section and re-render dynamic section.
This is a convenience wrapper for `append-to-log' without styling."
  (claude-agent--append-to-log text nil nil))

(defun claude-agent--render-dynamic-section ()
  "Render the dynamic section (status bar + input area).
Clears everything after static-end-marker and re-renders from state.
Handles different input modes: text input vs permission prompt."
  (let* ((inhibit-read-only t)
         ;; Only save cursor offset if in text mode
         (cursor-offset (when (and (eq claude-agent--input-mode 'text)
                                   claude-agent--input-start-marker
                                   (marker-position claude-agent--input-start-marker)
                                   (>= (point) claude-agent--input-start-marker))
                          (- (point) claude-agent--input-start-marker)))
         ;; Only capture input if in text mode (permission mode uses saved-input)
         (input-to-restore (if (eq claude-agent--input-mode 'text)
                               (claude-agent--get-input-text)
                             "")))
    ;; Clear overlays in dynamic section
    (when (and claude-agent--static-end-marker
               (marker-position claude-agent--static-end-marker))
      (dolist (ov (overlays-in claude-agent--static-end-marker (point-max)))
        (when (or (overlay-get ov 'claude-agent-styled)
                  (overlay-get ov 'claude-permission-face))
          (delete-overlay ov)))
      ;; Delete everything from static-end to end of buffer
      (delete-region claude-agent--static-end-marker (point-max)))

    ;; Position at start of dynamic section
    (goto-char (or claude-agent--static-end-marker (point-max)))

    ;; === INSERT STATUS BAR ===
    (when claude-agent--has-conversation
      (claude-agent--insert-status-bar))

    ;; === RENDER INPUT AREA BASED ON MODE ===
    (pcase claude-agent--input-mode
      ('text
       ;; Normal text input mode
       (setq claude-agent--input-start-marker (point-marker))
       (set-marker-insertion-type claude-agent--input-start-marker nil)
       (unless (string-empty-p input-to-restore)
         (insert input-to-restore))
       ;; Update read-only and placeholder
       (claude-agent--update-read-only)
       (claude-agent--update-placeholder)
       ;; Restore cursor position within input area
       (if (and cursor-offset (>= cursor-offset 0))
           (goto-char (min (+ claude-agent--input-start-marker cursor-offset)
                           (point-max)))
         (goto-char claude-agent--input-start-marker))
       ;; Scroll to show maximum content - put input near bottom
       (claude-agent--scroll-to-bottom))

      ('empty
       ;; Empty mode - clear input area and switch to text mode
       ;; This discards any saved input and starts fresh
       (setq claude-agent--saved-input "")
       (setq claude-agent--input-mode 'text)
       (setq claude-agent--input-start-marker (point-marker))
       (set-marker-insertion-type claude-agent--input-start-marker nil)
       ;; Update read-only and placeholder (will show placeholder since empty)
       (claude-agent--update-read-only)
       (claude-agent--update-placeholder)
       (goto-char claude-agent--input-start-marker))

      ('permission
       ;; Permission prompt mode - render the permission dialog (legacy, replaces input)
       (setq claude-agent--input-start-marker (point-marker))
       (claude-agent--render-permission-content)
       (claude-agent--update-read-only))

      ('text-with-permission
       ;; Combined mode: permission dialog ABOVE preserved text input
       ;; First render the permission dialog
       (claude-agent--render-permission-content)
       ;; Add separator before input area
       (insert "\n")
       ;; Now render the text input area
       (setq claude-agent--input-start-marker (point-marker))
       (set-marker-insertion-type claude-agent--input-start-marker nil)
       (unless (string-empty-p input-to-restore)
         (insert input-to-restore))
       ;; Update read-only and placeholder
       (claude-agent--update-read-only)
       (claude-agent--update-placeholder)
       ;; Restore cursor position within input area
       (if (and cursor-offset (>= cursor-offset 0))
           (goto-char (min (+ claude-agent--input-start-marker cursor-offset)
                           (point-max)))
         (goto-char claude-agent--input-start-marker))))))

;;;; Status bar rendering

(defun claude-agent--format-elapsed-time (start-time)
  "Format elapsed time since START-TIME as Xm Ys."
  (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
         (minutes (floor (/ elapsed 60)))
         (seconds (floor (mod elapsed 60))))
    (if (> minutes 0)
        (format "%dm%ds" minutes seconds)
      (format "%ds" seconds))))

(defun claude-agent--insert-status-bar ()
  "Insert the status bar content at point.
Called by `render-dynamic-section'. Assumes point is positioned correctly."
  ;; === Thinking indicator (if active) ===
  (when claude-agent--thinking-status
    (let ((start (point))
          (spinner (nth claude-agent--spinner-index
                        claude-agent--spinner-frames))
          (elapsed (if claude-agent--thinking-start-time
                       (claude-agent--format-elapsed-time
                        claude-agent--thinking-start-time)
                     "0s"))
          (tokens (format "(+%d/-%d)"
                          claude-agent--input-tokens
                          claude-agent--output-tokens)))
      (insert (format "\n%s %s %s %s (C-c C-k to interrupt)\n"
                      spinner
                      claude-agent--thinking-status
                      elapsed
                      tokens))
      (claude-agent--apply-face start (point)
                                (if claude-agent--compacting
                                    'claude-agent-compacting-face
                                  'claude-agent-thinking-face))))

  ;; === Compacting indicator (standalone, when not also thinking) ===
  (when (and claude-agent--compacting (not claude-agent--thinking-status))
    (let ((start (point))
          (spinner (nth claude-agent--spinner-index
                        claude-agent--spinner-frames)))
      (insert (format "\n%s 📦 Compacting conversation...\n" spinner))
      (claude-agent--apply-face start (point) 'claude-agent-compacting-face)))

  ;; === Progress indicators (if any and visible) ===
  (when (and claude-agent--progress-indicators
             claude-agent--progress-visible
             (> (hash-table-count claude-agent--progress-indicators) 0))
    (maphash
     (lambda (_id info)
       (let* ((label (plist-get info :label))
              (percent (or (plist-get info :percent) 0))
              (start-time (plist-get info :start-time))
              (elapsed (if start-time
                           (claude-agent--format-elapsed-time start-time)
                         ""))
              (bar-width 30)
              (filled (round (* bar-width (/ (min (max percent 0) 100.0) 100.0))))
              (empty (- bar-width filled)))
         ;; Insert label with percentage and elapsed time
         (let ((start (point)))
           (insert (format "\n  %s (%d%%) %s\n"
                           (or label "Working...")
                           (round percent)
                           elapsed))
           (claude-agent--apply-face start (point) 'claude-agent-progress-face))
         ;; Insert progress bar
         (let ((start (point)))
           (insert (format "  ▐%s%s▌\n"
                           (make-string filled ?█)
                           (make-string empty ?░)))
           (claude-agent--apply-face start (point) 'claude-agent-progress-face))))
     claude-agent--progress-indicators))

  ;; === Todo list (if any active and visible) ===
  ;; Hide when all todos are completed
  (let ((has-active-todos (and claude-agent--todos
                               (seq-some (lambda (todo)
                                           (let ((status (cdr (assq 'status todo))))
                                             (not (equal status "completed"))))
                                         claude-agent--todos))))
    (when (and has-active-todos claude-agent--todos-visible)
      (insert "\n")
      (dolist (todo claude-agent--todos)
        (let* ((content (cdr (assq 'content todo)))
               (status (cdr (assq 'status todo)))
               (active-form (cdr (assq 'activeForm todo)))
               (checkbox (pcase status
                           ("completed" "[X]")
                           ("in_progress" "[-]")
                           (_ "[ ]")))
               (face (pcase status
                       ("completed" 'claude-agent-todo-completed-face)
                       ("in_progress" 'claude-agent-todo-in-progress-face)
                       (_ 'claude-agent-todo-pending-face)))
               (text (if (equal status "in_progress")
                         (or active-form content)
                       content))
               (start (point)))
          (insert (format "  - %s %s\n" checkbox text))
          (claude-agent--apply-face start (point) face)))))

  ;; === Queued messages (if any) ===
  (when claude-agent--message-queue
    (dolist (msg (reverse claude-agent--message-queue))
      (let ((msg-start (point)))
        (insert "\n┄┄┄ Queued ┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄\n")
        (claude-agent--apply-face msg-start (point) 'claude-agent-queued-header-face)
        (setq msg-start (point))
        (let ((lines (split-string msg "\n")))
          (dolist (line lines)
            (insert "  " line "\n")))
        (claude-agent--apply-face msg-start (point) 'claude-agent-queued-face))))

  ;; === Status info line ===
  (let* ((model (or (plist-get claude-agent--session-info :model) "..."))
         (cost (or (plist-get claude-agent--session-info :cost) 0))
         (session-id (or (plist-get claude-agent--session-info :session-id) "..."))
         (status-text (format " Model: %s  |  Cost: $%.4f  |  Session: %s "
                              model cost
                              (if (> (length session-id) 8)
                                  (substring session-id 0 8)
                                session-id)))
         (bar-length (length status-text))
         (bar (make-string bar-length ?━))
         (start (point)))
    (insert "\n")
    (insert bar "\n")
    (insert status-text "\n")
    (insert bar "\n")
    (insert "\n")
    (claude-agent--apply-face start (point) 'claude-agent-header-face)))

(defun claude-agent--spinner-tick ()
  "Advance spinner and update in-place (lightweight)."
  (when claude-agent--thinking-status
    (setq claude-agent--spinner-index
          (mod (1+ claude-agent--spinner-index)
               (length claude-agent--spinner-frames)))
    ;; Only update the spinner/elapsed time, don't rebuild everything
    (claude-agent--update-spinner-display)))

(defun claude-agent--update-spinner-display ()
  "Update spinner and elapsed time in-place without full rebuild."
  (when (and claude-agent--thinking-status
             claude-agent--static-end-marker
             (marker-position claude-agent--static-end-marker))
    (let ((inhibit-read-only t)
          (spinner (nth claude-agent--spinner-index
                        claude-agent--spinner-frames))
          (elapsed (if claude-agent--thinking-start-time
                       (claude-agent--format-elapsed-time
                        claude-agent--thinking-start-time)
                     "0s"))
          (tokens (format "(+%d/-%d)"
                          claude-agent--input-tokens
                          claude-agent--output-tokens)))
      (save-excursion
        ;; Find the thinking indicator line (starts after static-end-marker)
        (goto-char claude-agent--static-end-marker)
        ;; Skip the first newline
        (when (looking-at "\n")
          (forward-char 1))
        ;; Now we should be at the start of the spinner line
        (when (looking-at ".*?\\(C-c C-k to interrupt\\)")
          (let ((line-start (point))
                (line-end (line-end-position)))
            ;; Replace the line
            (delete-region line-start line-end)
            (insert (format "%s %s %s %s (C-c C-k to interrupt)"
                            spinner
                            claude-agent--thinking-status
                            elapsed
                            tokens))
            ;; Reapply the face
            (claude-agent--apply-face line-start (point) 'claude-agent-thinking-face)))))))

(defun claude-agent--set-thinking (status)
  "Set thinking STATUS, or clear if nil."
  ;; Cancel existing timer
  (when claude-agent--spinner-timer
    (cancel-timer claude-agent--spinner-timer)
    (setq claude-agent--spinner-timer nil))

  (setq claude-agent--thinking-status status)

  (if status
      (progn
        ;; Start timing if not already
        (unless claude-agent--thinking-start-time
          (setq claude-agent--thinking-start-time (current-time)))
        ;; Start spinner timer
        (setq claude-agent--spinner-timer
              (run-with-timer 0.1 0.1 #'claude-agent--spinner-tick)))
    ;; Clear timing when done
    (setq claude-agent--thinking-start-time nil))

  ;; Rebuild dynamic section (handles cursor positioning)
  (claude-agent--render-dynamic-section))

;;;; Content helpers

(defun claude-agent--count-diff-lines (old-string new-string)
  "Count lines removed and added from OLD-STRING and NEW-STRING.
Returns a cons cell (REMOVED . ADDED)."
  (let ((removed (if (and old-string (not (string-empty-p old-string)))
                     (length (split-string old-string "\n"))
                   0))
        (added (if (and new-string (not (string-empty-p new-string)))
                   (length (split-string new-string "\n"))
                 0)))
    (cons removed added)))

(defun claude-agent--format-diff-content (old-string new-string)
  "Format OLD-STRING and NEW-STRING as a diff string for storage.
Returns a string with - and + prefixed lines."
  (let ((result ""))
    ;; Old lines (removed)
    (when (and old-string (not (string-empty-p old-string)))
      (dolist (line (split-string old-string "\n"))
        (setq result (concat result "- " line "\n"))))
    ;; New lines (added)
    (when (and new-string (not (string-empty-p new-string)))
      (dolist (line (split-string new-string "\n"))
        (setq result (concat result "+ " line "\n"))))
    result))

(defun claude-agent--insert-diff (file-path old-string new-string)
  "Insert a diff display for FILE-PATH with OLD-STRING and NEW-STRING.
Inserts directly at point with proper faces and clickable link."
  (let ((inhibit-read-only t))
    ;; Tool header in new terse format (using overlays to survive font-lock)
    (let ((start (point)))
      (insert "edit")
      (claude-agent--apply-face start (point) 'claude-agent-tool-name-face))
    (let ((start (point)))
      (insert "› ")
      (claude-agent--apply-face start (point) 'claude-agent-tool-arrow-face))
    (insert-text-button file-path
                        'action (lambda (_btn)
                                  (find-file-other-window
                                   (button-get _btn 'file-path)))
                        'file-path file-path
                        'face 'claude-agent-tool-file-face
                        'help-echo "Click to open file"
                        'follow-link t)
    (insert "\n")
    ;; Old lines (removed)
    (when (and old-string (not (string-empty-p old-string)))
      (dolist (line (split-string old-string "\n"))
        (let ((line-start (point)))
          (insert "- " line "\n")
          (claude-agent--apply-face line-start (point) 'claude-agent-diff-removed))))
    ;; New lines (added)
    (when (and new-string (not (string-empty-p new-string)))
      (dolist (line (split-string new-string "\n"))
        (let ((line-start (point)))
          (insert "+ " line "\n")
          (claude-agent--apply-face line-start (point) 'claude-agent-diff-added))))))

(defun claude-agent--insert-edit-summary (file-path old-string new-string)
  "Insert a compact edit summary for FILE-PATH with line counts.
Shows format: edit› filename.el (+N/-M)
The full diff is stored in tool-results for popup display."
  (let* ((inhibit-read-only t)
         (counts (claude-agent--count-diff-lines old-string new-string))
         (removed (car counts))
         (added (cdr counts))
         (filename (file-name-nondirectory file-path))
         (summary (format "%s (+%d/-%d)" filename added removed)))
    ;; Tool header
    (let ((start (point)))
      (insert "edit")
      (claude-agent--apply-face start (point) 'claude-agent-tool-name-face))
    (let ((start (point)))
      (insert "› ")
      (claude-agent--apply-face start (point) 'claude-agent-tool-arrow-face))
    ;; Clickable filename with line counts
    (insert-text-button summary
                        'action (lambda (_btn)
                                  (find-file-other-window
                                   (button-get _btn 'file-path)))
                        'file-path file-path
                        'face 'claude-agent-tool-file-face
                        'help-echo "Click to open file, hover for diff preview"
                        'follow-link t)
    (insert "\n")))

(defface claude-agent-tool-name-face
  '((t :foreground "#e5c07b" :weight bold))
  "Face for tool names in tool calls (orange, like headers)."
  :group 'claude-agent)

(defface claude-agent-tool-arrow-face
  '((t :foreground "#e5c07b"))
  "Face for the arrow separator in tool calls."
  :group 'claude-agent)

(defface claude-agent-tool-cmd-face
  '((t :foreground "#abb2bf"))
  "Face for command text in tool calls."
  :group 'claude-agent)

(defface claude-agent-tool-file-face
  '((t :foreground "#61afef"))
  "Face for file paths in tool calls."
  :group 'claude-agent)

(defface claude-agent-tool-pattern-face
  '((t :foreground "#98c379"))
  "Face for patterns (glob, grep) in tool calls."
  :group 'claude-agent)

(defface claude-agent-tool-continuation-face
  '((t :foreground "#5c6370"))
  "Face for continuation markers in multi-line tool calls."
  :group 'claude-agent)

(defun claude-agent--format-bash-multiline (command)
  "Format a multi-line bash COMMAND with pipe continuations."
  (let ((lines (split-string command "\n")))
    (if (= (length lines) 1)
        ;; Single line - just return propertized command
        (propertize command 'face 'claude-agent-tool-cmd-face)
      ;; Multi-line - add continuation markers
      (let ((result ""))
        (dotimes (i (length lines))
          (let ((line (nth i lines)))
            (if (= i 0)
                (setq result (concat result (propertize line 'face 'claude-agent-tool-cmd-face) "\n"))
              (setq result (concat result
                                   (propertize "   │ " 'face 'claude-agent-tool-continuation-face)
                                   (propertize line 'face 'claude-agent-tool-cmd-face)
                                   "\n")))))
        ;; Remove trailing newline since caller adds it
        (substring result 0 -1)))))

(defun claude-agent--format-tool-name (tool-name)
  "Format TOOL-NAME for display.
Converts MCP tools like 'mcp__claudemacs__reload_file' to 'claudemacs/reload-file'."
  (if (string-prefix-p "mcp__" tool-name)
      ;; MCP tool: mcp__server__tool_name -> server/tool-name
      (let* ((without-prefix (substring tool-name 5))  ; Remove "mcp__"
             (parts (split-string without-prefix "__"))
             (server (car parts))
             (tool (mapconcat #'identity (cdr parts) "_")))
        (concat server "/" (replace-regexp-in-string "_" "-" tool)))
    ;; Regular tool: just lowercase
    (downcase tool-name)))

(defun claude-agent--insert-tool-call (tool-name args-string)
  "Insert a tool call display for TOOL-NAME with ARGS-STRING.
Uses terse format: toolname› args with appropriate faces."
  (let* ((inhibit-read-only t)
         (tool-lower (claude-agent--format-tool-name tool-name))
         (saved-input (claude-agent--get-input-text))
         (cursor-offset (when (and claude-agent--input-start-marker
                                   (marker-position claude-agent--input-start-marker)
                                   (>= (point) claude-agent--input-start-marker))
                          (- (point) claude-agent--input-start-marker))))
    ;; Delete dynamic section
    (delete-region claude-agent--static-end-marker (point-max))
    (goto-char claude-agent--static-end-marker)

    ;; Insert tool name with overlay (survives font-lock)
    (let ((start (point)))
      (insert tool-lower)
      (claude-agent--apply-face start (point) 'claude-agent-tool-name-face))
    ;; Insert arrow with overlay
    (let ((start (point)))
      (insert "› ")
      (claude-agent--apply-face start (point) 'claude-agent-tool-arrow-face))
    ;; Insert args with overlay based on tool type
    (let ((start (point))
          (face (cond
                 ((string= tool-name "Bash") nil)
                 ((member tool-name '("Read" "Write" "Edit")) 'claude-agent-tool-file-face)
                 ((member tool-name '("Glob" "Grep")) 'claude-agent-tool-pattern-face)
                 (t 'claude-agent-tool-cmd-face))))
      (if (string= tool-name "Bash")
          (insert (claude-agent--format-bash-multiline args-string))
        (insert args-string))
      (when face
        (claude-agent--apply-face start (point) face)))
    (insert "\n")

    ;; Update static marker
    (set-marker claude-agent--static-end-marker (point))

    ;; Rebuild dynamic section
    (when claude-agent--has-conversation
      (claude-agent--insert-status-bar))
    (setq claude-agent--input-start-marker (point-marker))
    (insert saved-input)
    (claude-agent--update-read-only)
    (claude-agent--update-placeholder)
    (goto-char (if (and cursor-offset (>= cursor-offset 0))
                   (min (+ claude-agent--input-start-marker cursor-offset) (point-max))
                 claude-agent--input-start-marker))))

(defun claude-agent--insert-tool-result-start ()
  "Insert the start of a tool result section."
  (claude-agent--append-to-log " #+begin_example\n" nil " "))

(defun claude-agent--insert-tool-result-end ()
  "Insert the end of a tool result section."
  (claude-agent--append-to-log " #+end_example\n" nil))

(defun claude-agent--insert-bash-tool (command)
  "Insert a Bash tool call with COMMAND."
  ;; Just use the standard tool call format, no special src block
  (claude-agent--insert-tool-call "Bash" command))

(defvar-local claude-agent--current-read-file nil
  "File path for current Read tool being displayed.")

(defvar-local claude-agent--current-write-file nil
  "File path for current Write tool being displayed.")

(defvar-local claude-agent--current-write-content nil
  "Content for current Write tool being displayed.")

(defun claude-agent--insert-read-tool (file-path)
  "Insert a Read tool call with FILE-PATH."
  (setq claude-agent--current-read-file file-path)
  ;; Just use the standard tool call format
  (claude-agent--insert-tool-call "Read" file-path))

(defun claude-agent--format-read-line (line)
  "Format a LINE from Read tool output with prettier line numbers.
Input format: '     N→content' where N is line number."
  (if (string-match "^\\( *\\)\\([0-9]+\\)→\\(.*\\)$" line)
      (let ((line-num (match-string 2 line))
            (content (match-string 3 line)))
        (cons (format "%4s│ " line-num) content))
    ;; Not a numbered line, return as-is
    (cons nil line)))

(defun claude-agent--insert-read-content (content)
  "Insert Read tool CONTENT with formatted line numbers.
Expects content in the format from Claude's Read tool."
  (let* ((inhibit-read-only t)
         (saved-input (if (eq claude-agent--input-mode 'text)
                          (claude-agent--get-input-text)
                        claude-agent--saved-input))
         (cursor-offset (when (and (eq claude-agent--input-mode 'text)
                                   claude-agent--input-start-marker
                                   (marker-position claude-agent--input-start-marker)
                                   (>= (point) claude-agent--input-start-marker))
                          (- (point) claude-agent--input-start-marker)))
         (lines (split-string content "\n"))
         ;; Remove trailing empty lines
         (trimmed-lines (let ((result lines))
                          (while (and result (string-empty-p (car (last result))))
                            (setq result (butlast result)))
                          result)))
    ;; Delete dynamic section
    (delete-region claude-agent--static-end-marker (point-max))
    (goto-char claude-agent--static-end-marker)
    ;; Insert each line with formatted line numbers
    (dolist (line trimmed-lines)
      (let ((parsed (claude-agent--format-read-line line)))
        (if (car parsed)
            ;; Line with number
            (progn
              (let ((num-start (point)))
                (insert " " (car parsed))
                (claude-agent--apply-face num-start (point) 'claude-agent-line-number))
              (insert (cdr parsed) "\n"))
          ;; Plain line (no number)
          (insert " " (cdr parsed) "\n"))))
    ;; Update static marker
    (set-marker claude-agent--static-end-marker (point))
    ;; Rebuild dynamic section
    (when claude-agent--has-conversation
      (claude-agent--insert-status-bar))
    (setq claude-agent--input-start-marker (point-marker))
    (insert saved-input)
    (claude-agent--update-read-only)
    (claude-agent--update-placeholder)
    (goto-char (if (and cursor-offset (>= cursor-offset 0))
                   (min (+ claude-agent--input-start-marker cursor-offset) (point-max))
                 claude-agent--input-start-marker))))

(defun claude-agent--insert-write-tool (file-path)
  "Insert a Write tool header with FILE-PATH as clickable link."
  (setq claude-agent--current-write-file file-path)
  ;; Use standard tool call format
  (claude-agent--insert-tool-call "Write" file-path))

(defun claude-agent--insert-write-content (content)
  "Insert Write tool CONTENT with line numbers showing additions.
Follows same pattern as `claude-agent--insert-read-content'."
  (setq claude-agent--current-write-content content)
  (let* ((inhibit-read-only t)
         (saved-input (if (eq claude-agent--input-mode 'text)
                          (claude-agent--get-input-text)
                        claude-agent--saved-input))
         (cursor-offset (when (and (eq claude-agent--input-mode 'text)
                                   claude-agent--input-start-marker
                                   (marker-position claude-agent--input-start-marker)
                                   (>= (point) claude-agent--input-start-marker))
                          (- (point) claude-agent--input-start-marker)))
         (lines (split-string content "\n"))
         ;; Remove trailing empty lines
         (trimmed-lines (let ((result lines))
                          (while (and result (string-empty-p (car (last result))))
                            (setq result (butlast result)))
                          result))
         (line-num 0))
    ;; Delete dynamic section
    (delete-region claude-agent--static-end-marker (point-max))
    (goto-char claude-agent--static-end-marker)
    ;; Insert each line with line numbers and + prefix
    (dolist (line trimmed-lines)
      (cl-incf line-num)
      (let ((num-start (point)))
        (insert (format " %4d│+ " line-num))
        (claude-agent--apply-face num-start (point) 'claude-agent-diff-added))
      (insert line "\n"))
    ;; Update static marker
    (set-marker claude-agent--static-end-marker (point))
    ;; Rebuild dynamic section
    (when claude-agent--has-conversation
      (claude-agent--insert-status-bar))
    (setq claude-agent--input-start-marker (point-marker))
    (insert saved-input)
    (claude-agent--update-read-only)
    (claude-agent--update-placeholder)
    (goto-char (if (and cursor-offset (>= cursor-offset 0))
                   (min (+ claude-agent--input-start-marker cursor-offset) (point-max))
                 claude-agent--input-start-marker))))

(defun claude-agent--append-to-log (text &optional face virtual-indent)
  "Append TEXT to the static section (conversation log).
If FACE is non-nil, apply it as an overlay to the inserted text.
If VIRTUAL-INDENT is non-nil, apply it as line-prefix/wrap-prefix."
  ;; We need to apply face/indent to the text being inserted.
  ;; Since append-to-static does complex operations, we'll handle styling here.
  (let* ((inhibit-read-only t)
         ;; In permission mode, use saved-input; in text mode, capture current input
         (saved-input (if (eq claude-agent--input-mode 'text)
                          (claude-agent--get-input-text)
                        claude-agent--saved-input))
         (cursor-offset (when (and (eq claude-agent--input-mode 'text)
                                   claude-agent--input-start-marker
                                   (marker-position claude-agent--input-start-marker)
                                   (>= (point) claude-agent--input-start-marker))
                          (- (point) claude-agent--input-start-marker))))
    ;; Delete everything from static-end to end
    (delete-region claude-agent--static-end-marker (point-max))
    ;; Insert new static content with styling
    (goto-char claude-agent--static-end-marker)
    (let ((start (point)))
      (insert text)
      ;; Apply face overlay if specified
      (when face
        (claude-agent--apply-face start (point) face))
      ;; Apply virtual indent if specified
      (when virtual-indent
        (put-text-property start (point) 'line-prefix virtual-indent)
        (put-text-property start (point) 'wrap-prefix virtual-indent))
      ;; Mark as fontified to prevent org-mode font-lock from interfering
      (put-text-property start (point) 'fontified t))
    (set-marker claude-agent--static-end-marker (point))
    ;; Insert status bar
    (when claude-agent--has-conversation
      (claude-agent--insert-status-bar))
    ;; Set input marker and restore input
    (setq claude-agent--input-start-marker (point-marker))
    (insert saved-input)
    ;; Finalize
    (claude-agent--update-read-only)
    (claude-agent--update-placeholder)
    ;; Restore cursor
    (goto-char (if (and cursor-offset (>= cursor-offset 0))
                   (min (+ claude-agent--input-start-marker cursor-offset) (point-max))
                 claude-agent--input-start-marker))))

;;;; Process filter - parsing NDJSON messages

(defun claude-agent--process-filter (proc output)
  "Process filter for agent PROC handling OUTPUT."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (claude-agent--handle-output output)))))

(defun claude-agent--handle-output (output)
  "Handle OUTPUT from the agent process, parsing NDJSON messages."
  (setq claude-agent--pending-output
        (concat claude-agent--pending-output output))

  ;; Process complete lines (each line is a JSON message)
  (while (string-match "\n" claude-agent--pending-output)
    (let ((line (substring claude-agent--pending-output 0 (match-beginning 0))))
      (setq claude-agent--pending-output
            (substring claude-agent--pending-output (match-end 0)))
      (claude-agent--process-json-line line))))

(defun claude-agent--process-json-line (line)
  "Process a single LINE of NDJSON output."
  (when (and line (not (string-empty-p (string-trim line))))
    (condition-case err
        (let* ((msg (json-read-from-string line))
               (msg-type (cdr (assq 'type msg))))
          (claude-agent--dispatch-message msg-type msg))
      (json-readtable-error
       (message "Claude agent: Invalid JSON: %s" line))
      (error
       (message "Claude agent: Error processing message: %s" (error-message-string err))))))

(defun claude-agent--dispatch-message (msg-type msg)
  "Dispatch message MSG based on MSG-TYPE."
  (pcase msg-type
    ;; Ready - clear thinking, send queued messages
    ("ready"
     (claude-agent--set-thinking nil)
     (when claude-agent--message-queue
       (claude-agent--send-next-queued)))

    ;; Session start
    ("session_start"
     nil)  ; Handled by buffer init

    ;; Session info - update model/session-id
    ("session_info"
     (when-let ((model (cdr (assq 'model msg))))
       (setq claude-agent--session-info
             (plist-put claude-agent--session-info :model model)))
     (when-let ((session-id (cdr (assq 'session_id msg))))
       (setq claude-agent--session-info
             (plist-put claude-agent--session-info :session-id session-id)))
     (claude-agent--render-dynamic-section))

    ;; Thinking status
    ("thinking"
     (let ((status (cdr (assq 'status msg))))
       (unless claude-agent--thinking-start-time
         (setq claude-agent--input-tokens 0
               claude-agent--output-tokens 0
               claude-agent--thinking-start-time (current-time)))
       (claude-agent--set-thinking (or status "Thinking..."))))

    ;; Progress - update token counts
    ("progress"
     (when-let ((input (cdr (assq 'input_tokens msg))))
       (setq claude-agent--input-tokens input))
     (when-let ((output (cdr (assq 'output_tokens msg))))
       (setq claude-agent--output-tokens output)))

    ;; Result - update cost
    ("result"
     (when-let ((cost (cdr (assq 'cost_usd msg))))
       (setq claude-agent--session-info
             (plist-put claude-agent--session-info :cost cost)))
     ;; Clear compacting status on result
     (setq claude-agent--compacting nil)
     (claude-agent--render-dynamic-section))

    ;; Compacting - conversation is being summarized
    ("compacting"
     (let ((status (cdr (assq 'status msg))))
       (if (equal status "start")
           (progn
             (setq claude-agent--compacting t)
             (claude-agent--append-to-log
              "\n📦 Compacting conversation history...\n"
              'claude-agent-compacting-face))
         ;; status is "end" or similar
         (setq claude-agent--compacting nil))
       (claude-agent--render-dynamic-section)))

    ;; Todo update - update todo list display
    ("todo_update"
     (let ((todos (cdr (assq 'todos msg))))
       ;; Convert vector to list if needed (JSON arrays come as vectors)
       (setq claude-agent--todos (if (vectorp todos)
                                     (append todos nil)
                                   todos))
       (claude-agent--render-dynamic-section)))

    ;; MCP status
    ("mcp_status"
     (let ((servers (cdr (assq 'servers msg))))
       (setq claude-agent--mcp-server-status servers)
       (let ((failed (seq-filter
                      (lambda (s) (not (equal (cdr (assq 'status s)) "connected")))
                      servers)))
         (when failed
           (claude-agent--append-to-log
            (format "\n⚠ MCP server issue: %s\n"
                    (mapconcat (lambda (s)
                                 (format "%s (%s)"
                                         (cdr (assq 'name s))
                                         (cdr (assq 'status s))))
                               failed ", "))
            'claude-agent-error-face)))))

    ;; Permission request
    ("permission_request"
     (claude-agent--set-thinking "Awaiting permission...")
     (claude-agent--show-permission-prompt msg))

    ;; Permission granted (informational)
    ("permission_granted"
     nil)  ; Could show notification if desired

    ;; User message start
    ("user_start"
     (setq claude-agent--parse-state 'user)
     (setq claude-agent--has-conversation t)
     (claude-agent--append-to-log "you> " 'claude-agent-user-header-face))

    ;; User message text
    ("user_text"
     (let ((text (cdr (assq 'text msg))))
       (claude-agent--append-to-log (concat text "\n") 'claude-agent-user-face "  ")))

    ;; User message end
    ("user_end"
     (setq claude-agent--parse-state nil))

    ;; Assistant message start
    ("assistant_start"
     (setq claude-agent--parse-state 'assistant)
     (claude-agent--append-to-log "claude> " 'claude-agent-assistant-header-face))

    ;; Assistant message text
    ("assistant_text"
     (let ((text (cdr (assq 'text msg))))
       (claude-agent--append-to-log (concat text "\n") nil "  ")))

    ;; Assistant message end
    ("assistant_end"
     (setq claude-agent--parse-state nil))

    ;; Tool call - all tools use the same simple format now
    ("tool_call"
     (let* ((name (cdr (assq 'name msg)))
            (input (cdr (assq 'input msg)))
            (args-str (claude-agent--format-tool-input-for-display name input)))
       (setq claude-agent--parse-state 'tool)
       ;; Mark position and name before inserting so we can associate result later
       (setq claude-agent--last-tool-marker
             (copy-marker (or claude-agent--static-end-marker (point-max))))
       (setq claude-agent--last-tool-name name)
       (claude-agent--insert-tool-call name args-str)))

    ;; Tool result - store for later viewing with C-c ' and add tooltip
    ("tool_result"
     (let ((content (cdr (assq 'content msg))))
       (when (and claude-agent--last-tool-marker content)
         (push (list claude-agent--last-tool-marker
                     claude-agent--last-tool-name
                     content)
               claude-agent--tool-results)
         ;; Add tooltip to the tool call line
         (claude-agent--add-tool-tooltip claude-agent--last-tool-marker content))))

    ;; Tool end
    ("tool_end"
     (setq claude-agent--parse-state nil)
     (claude-agent--set-thinking "Thinking..."))

    ;; Edit tool (compact summary display with diff in popup)
    ("edit_tool"
     (let ((file-path (cdr (assq 'file_path msg)))
           (old-string (cdr (assq 'old_string msg)))
           (new-string (cdr (assq 'new_string msg))))
       (setq claude-agent--parse-state 'tool)
       (claude-agent--set-thinking (format "Editing: %s" (file-name-nondirectory file-path)))
       ;; Mark position and name for tool result storage
       (setq claude-agent--last-tool-marker
             (copy-marker (or claude-agent--static-end-marker (point-max))))
       (setq claude-agent--last-tool-name "Edit")
       ;; Format diff content for storage in tool-results
       (let ((diff-content (claude-agent--format-diff-content old-string new-string)))
         ;; Store in tool-results for popup viewing (like other tools)
         (push (list claude-agent--last-tool-marker "Edit" diff-content)
               claude-agent--tool-results)
         ;; Insert compact summary instead of full diff
         (let* ((inhibit-read-only t)
                (saved-input (claude-agent--get-input-text))
                (cursor-offset (when (and claude-agent--input-start-marker
                                          (marker-position claude-agent--input-start-marker)
                                          (>= (point) claude-agent--input-start-marker))
                                 (- (point) claude-agent--input-start-marker))))
           (delete-region claude-agent--static-end-marker (point-max))
           (goto-char claude-agent--static-end-marker)
           (claude-agent--insert-edit-summary file-path old-string new-string)
           (set-marker claude-agent--static-end-marker (point))
           ;; Add tooltip to the summary line
           (claude-agent--add-tool-tooltip claude-agent--last-tool-marker diff-content)
           (when claude-agent--has-conversation
             (claude-agent--insert-status-bar))
           (setq claude-agent--input-start-marker (point-marker))
           (insert saved-input)
           (claude-agent--update-read-only)
           (claude-agent--update-placeholder)
           (goto-char (if (and cursor-offset (>= cursor-offset 0))
                          (min (+ claude-agent--input-start-marker cursor-offset) (point-max))
                        claude-agent--input-start-marker))))))

    ;; Write tool (special display)
    ("write_tool"
     (let ((file-path (cdr (assq 'file_path msg)))
           (content (cdr (assq 'content msg))))
       (setq claude-agent--parse-state 'tool)
       (claude-agent--set-thinking (format "Writing: %s" (file-name-nondirectory file-path)))
       ;; Mark position for tool result storage
       (setq claude-agent--last-tool-marker
             (copy-marker (or claude-agent--static-end-marker (point-max))))
       (claude-agent--insert-write-tool file-path)
       (claude-agent--insert-write-content content)))

    ;; Session message (system notifications)
    ("session_message_start"
     (setq claude-agent--parse-state 'session))

    ("session_message_text"
     (let ((text (cdr (assq 'text msg))))
       (claude-agent--append-to-log (concat text "\n") 'claude-agent-session-face)))

    ("session_message_end"
     (setq claude-agent--parse-state nil))

    ;; Error
    ("error"
     (let ((message-text (cdr (assq 'message msg)))
           (traceback (cdr (assq 'traceback msg))))
       (claude-agent--append-to-log
        (format "\n⚠ Error: %s\n" message-text)
        'claude-agent-error-face)
       (when traceback
         (claude-agent--append-to-log
          (format "Traceback:\n%s\n" traceback)
          'claude-agent-error-face))))

    ;; Unknown message type
    (_
     (message "Claude agent: Unknown message type: %s" msg-type))))

(defun claude-agent--format-tool-input-for-display (tool-name tool-input)
  "Format TOOL-INPUT for display based on TOOL-NAME."
  (cond
   ((member tool-name '("Read" "Write" "Edit"))
    (cdr (assq 'file_path tool-input)))
   ((string= tool-name "Bash")
    (cdr (assq 'command tool-input)))
   ((string= tool-name "Glob")
    (cdr (assq 'pattern tool-input)))
   ((string= tool-name "Grep")
    (let ((pattern (cdr (assq 'pattern tool-input)))
          (path (cdr (assq 'path tool-input))))
      (if path (format "%s, %s" pattern path) pattern)))
   ((string= tool-name "WebFetch")
    (cdr (assq 'url tool-input)))
   ((string= tool-name "Task")
    (cdr (assq 'description tool-input)))
   (t
    (let ((first-val (cdar tool-input)))
      (if first-val
          (format "%s" first-val)
        "")))))

;;;; Permission prompt UI

(defvar-local claude-agent--permission-data nil
  "Current permission request data.")

(defvar-local claude-agent--permission-selection 0
  "Currently selected option in permission prompt (0-3).")

(defface claude-agent-permission-box-face
  '((t :foreground "#e5c07b" :background "#3e4451" :box (:line-width 1 :color "#5c6370")))
  "Face for permission dialog box."
  :group 'claude-agent)

(defface claude-agent-permission-selected-face
  '((t :foreground "#282c34" :background "#61afef" :weight bold))
  "Face for selected option in permission dialog."
  :group 'claude-agent)

(defface claude-agent-permission-option-face
  '((t :foreground "#abb2bf"))
  "Face for unselected options in permission dialog."
  :group 'claude-agent)

(defun claude-agent--format-tool-input (tool-name tool-input)
  "Format TOOL-INPUT for display based on TOOL-NAME."
  (cond
   ((string= tool-name "Read")
    (cdr (assq 'file_path tool-input)))
   ((string= tool-name "Write")
    (cdr (assq 'file_path tool-input)))
   ((string= tool-name "Edit")
    (cdr (assq 'file_path tool-input)))
   ((string= tool-name "Bash")
    (cdr (assq 'command tool-input)))
   (t (format "%s" tool-input))))

(defun claude-agent--generate-permission-pattern (tool-name tool-input scope)
  "Generate permission pattern for TOOL-NAME with TOOL-INPUT at SCOPE level."
  (pcase scope
    ('once
     (pcase tool-name
       ("Read" (format "Read(%s)" (cdr (assq 'file_path tool-input))))
       ("Write" (format "Write(%s)" (cdr (assq 'file_path tool-input))))
       ("Edit" (format "Edit(%s)" (cdr (assq 'file_path tool-input))))
       ("Bash" (format "Bash(%s)" (cdr (assq 'command tool-input))))
       (_ (format "%s" tool-name))))
    ('session
     (pcase tool-name
       ("Read" (format "Read(%s)" (cdr (assq 'file_path tool-input))))
       ("Write" (format "Write(%s)" (cdr (assq 'file_path tool-input))))
       ("Edit" (format "Edit(%s)" (cdr (assq 'file_path tool-input))))
       ("Bash"
        (let* ((cmd (cdr (assq 'command tool-input)))
               (first-word (car (split-string cmd))))
          (format "Bash(%s:*)" first-word)))
       (_ (format "%s(*)" tool-name))))
    ('always
     (pcase tool-name
       ("Read"
        (let* ((path (cdr (assq 'file_path tool-input)))
               (dir (file-name-directory path)))
          (format "Read(%s*)" (or dir "/"))))
       ("Write"
        (let* ((path (cdr (assq 'file_path tool-input)))
               (dir (file-name-directory path)))
          (format "Write(%s*)" (or dir "/"))))
       ("Edit"
        (let* ((path (cdr (assq 'file_path tool-input)))
               (dir (file-name-directory path)))
          (format "Edit(%s*)" (or dir "/"))))
       ("Bash"
        (let* ((cmd (cdr (assq 'command tool-input)))
               (first-word (car (split-string cmd))))
          (format "Bash(%s:*)" first-word)))
       (_ (format "%s(*)" tool-name))))))

(defvar-local claude-agent--permission-overlay-specs nil
  "List of (start end face) specs for permission dialog overlays.")

(defun claude-agent--apply-permission-overlays ()
  "Apply permission overlays in the current buffer using saved specs."
  (when claude-agent--permission-overlay-specs
    ;; Remove existing permission overlays in this buffer
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'claude-permission-face)
        (delete-overlay ov)))
    ;; Apply new overlays
    (dolist (spec claude-agent--permission-overlay-specs)
      (let ((ov (make-overlay (nth 0 spec) (nth 1 spec))))
        (overlay-put ov 'face (nth 2 spec))
        (overlay-put ov 'priority 1000)
        (overlay-put ov 'evaporate nil)
        (overlay-put ov 'claude-permission-face t)))))

(defun claude-agent--render-permission-content ()
  "Insert the permission dialog content at point.
Called by `render-dynamic-section' when in permission mode.
Uses compact inline format when in text-with-permission mode."
  (when claude-agent--permission-data
    (let* ((tool-name (cdr (assq 'tool_name claude-agent--permission-data)))
           (tool-input (cdr (assq 'tool_input claude-agent--permission-data)))
           (input-str (claude-agent--format-tool-input tool-name tool-input))
           (sel claude-agent--permission-selection)
           (inhibit-read-only t)
           (compact (eq claude-agent--input-mode 'text-with-permission))
           (overlay-specs nil))
      ;; Helper to insert and record overlay spec
      (cl-flet ((insert-styled (text face)
                  (let ((start (point)))
                    (insert text)
                    (push (list start (point) face) overlay-specs))))
        (if compact
            ;; Compact 3-line format for inline display
            (let ((short-options '("once" "session" "always" "deny")))
              ;; Line 1: Tool being requested
              (insert-styled "⚡ " 'claude-agent-session-face)
              (insert-styled (format "%s(%s)" tool-name input-str) 'claude-agent-tool-face)
              (insert "\n")
              ;; Line 2: Options as inline buttons
              (insert "  ")
              (dotimes (i 4)
                (let* ((selected (= i sel))
                       (label (nth i short-options))
                       (face (if selected
                                 'claude-agent-permission-selected-face
                               'claude-agent-permission-option-face)))
                  (insert-styled (format "[%d:%s]" (1+ i) label) face)
                  (when (< i 3) (insert " "))))
              (insert "\n")
              ;; Line 3: Hint
              (insert-styled "  ↑↓ navigate, RET confirm, C-1..C-4 direct, C-g deny" 'claude-agent-session-face)
              (insert "\n"))
          ;; Full format for standalone permission mode
          (let ((options '("Allow once" "Allow for this session" "Always allow" "Deny")))
            ;; Header
            (insert-styled "── Permission Request " 'claude-agent-input-header-face)
            (insert-styled (make-string 40 ?─) 'claude-agent-input-header-face)
            (insert "\n")
            ;; Tool info
            (insert-styled " Claude wants to run:\n" 'claude-agent-session-face)
            (insert-styled (format " %s(%s)\n\n" tool-name input-str) 'claude-agent-tool-face)
            ;; Options
            (dotimes (i 4)
              (let* ((selected (= i sel))
                     (checkbox (if selected "[X]" "[ ]"))
                     (label (nth i options))
                     (face (if selected
                               'claude-agent-permission-selected-face
                             'claude-agent-permission-option-face)))
                (insert-styled (format " %d. %s %s\n" (1+ i) checkbox label) face)))
            ;; Footer
            (insert-styled (make-string 62 ?─) 'claude-agent-input-header-face)
            (insert "\n"))))
      ;; Save overlay specs and apply
      (setq claude-agent--permission-overlay-specs (nreverse overlay-specs))
      (claude-agent--apply-permission-overlays)
      ;; Apply in indirect buffers too
      (let ((base (current-buffer)))
        (dolist (buf (buffer-list))
          (when (and (buffer-live-p buf)
                     (eq (buffer-base-buffer buf) base))
            (with-current-buffer buf
              (setq claude-agent--permission-overlay-specs
                    (buffer-local-value 'claude-agent--permission-overlay-specs base))
              (claude-agent--apply-permission-overlays))))))))

(defun claude-agent--render-permission-dialog ()
  "Re-render the permission dialog (updates selection state).
This is called when the user navigates options."
  (claude-agent--render-dynamic-section))

(defun claude-agent--show-permission-prompt (data)
  "Show permission prompt for DATA above the input area.
Saves current input text and shows dialog while preserving input."
  ;; Save current input text before switching modes
  (setq claude-agent--saved-input (claude-agent--get-input-text))
  ;; Set permission state
  (setq claude-agent--permission-data data)
  (setq claude-agent--permission-selection 0)
  ;; Switch to combined mode: permission dialog + text input preserved
  (setq claude-agent--input-mode 'text-with-permission)
  ;; Render the dialog (which now uses render-dynamic-section)
  (claude-agent--render-permission-dialog)
  ;; Set up keyboard navigation
  (claude-agent--setup-permission-keymap))

(defun claude-agent--permission-select-next ()
  "Move selection down in permission dialog."
  (interactive)
  (claude-agent--in-base-buffer
   (when claude-agent--permission-data
     (setq claude-agent--permission-selection
           (mod (1+ claude-agent--permission-selection) 4))
     (claude-agent--render-permission-dialog))))

(defun claude-agent--permission-select-prev ()
  "Move selection up in permission dialog."
  (interactive)
  (claude-agent--in-base-buffer
   (when claude-agent--permission-data
     (setq claude-agent--permission-selection
           (mod (1- claude-agent--permission-selection) 4))
     (claude-agent--render-permission-dialog))))

(defun claude-agent--permission-confirm ()
  "Confirm the current selection in permission dialog."
  (interactive)
  (claude-agent--in-base-buffer
   (when claude-agent--permission-data
     (pcase claude-agent--permission-selection
       (0 (claude-agent--send-permission-response "allow_once"))
       (1 (claude-agent--send-permission-response "allow_session"))
       (2 (claude-agent--send-permission-response "allow_always"))
       (3 (claude-agent--send-permission-response "deny"))))))

;; Minor mode for permission dialog - uses chord keys to not interfere with typing
(defvar claude-agent-permission-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation with M-n/M-p and arrow keys
    (define-key map (kbd "M-p") #'claude-agent--permission-select-prev)
    (define-key map (kbd "M-n") #'claude-agent--permission-select-next)
    (define-key map (kbd "<up>") #'claude-agent--permission-select-prev)
    (define-key map (kbd "<down>") #'claude-agent--permission-select-next)
    ;; Confirm with RET or C-c C-c
    (define-key map (kbd "RET") #'claude-agent--permission-confirm)
    (define-key map (kbd "C-c C-c") #'claude-agent--permission-confirm)
    ;; Direct selection with C-1 through C-4
    (define-key map (kbd "C-1") #'claude-agent-permit-once)
    (define-key map (kbd "C-2") #'claude-agent-permit-session)
    (define-key map (kbd "C-3") #'claude-agent-permit-always)
    (define-key map (kbd "C-4") #'claude-agent-deny)
    ;; C-g to deny (standard Emacs cancel)
    (define-key map (kbd "C-g") #'claude-agent-deny)
    map)
  "Keymap for permission dialog mode.")

;; Use emulation-mode-map-alists to give permission keymap highest priority
(defvar claude-agent--permission-emulation-map-alist nil
  "Alist for `emulation-mode-map-alists' to override other keymaps during permission.")

(define-minor-mode claude-agent-permission-mode
  "Minor mode for permission dialog interaction.
Uses chord keys so typing is not affected."
  :lighter " Permit"
  :keymap claude-agent-permission-mode-map
  (if claude-agent-permission-mode
      (progn
        ;; Use emulation-mode-map-alists for higher priority
        (setq claude-agent--permission-emulation-map-alist
              `((claude-agent-permission-mode . ,claude-agent-permission-mode-map)))
        (add-to-list 'emulation-mode-map-alists 'claude-agent--permission-emulation-map-alist)
        (message "Permission: ↑↓ navigate, RET confirm, C-1..C-4 direct, C-g deny"))
    ;; Remove from emulation alist when disabling
    (setq emulation-mode-map-alists
          (delq 'claude-agent--permission-emulation-map-alist emulation-mode-map-alists))))

(defun claude-agent--setup-permission-keymap ()
  "Set up keymap for permission prompt interaction."
  ;; Enable permission mode in the base buffer
  (claude-agent-permission-mode 1)
  ;; For polymode: also enable in all indirect buffers sharing this base
  (let ((base (or (buffer-base-buffer) (current-buffer))))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (eq (buffer-base-buffer buf) base))
        (with-current-buffer buf
          (claude-agent-permission-mode 1))))))

(defun claude-agent--send-permission-response (action)
  "Send permission response with ACTION to the agent process.
Restores text input mode and any saved input."
  (claude-agent--in-base-buffer
   (when claude-agent--permission-data
     (let* ((tool-name (cdr (assq 'tool_name claude-agent--permission-data)))
            (tool-input (cdr (assq 'tool_input claude-agent--permission-data)))
            (scope (pcase action
                     ("allow_once" 'once)
                     ("allow_session" 'session)
                     ("allow_always" 'always)
                     (_ nil)))
            (pattern (when scope
                       (claude-agent--generate-permission-pattern
                        tool-name tool-input scope)))
            (response-msg `((type . "permission_response")
                            (action . ,action)
                            (pattern . ,pattern)))
            (saved-input claude-agent--saved-input))
       ;; Clear permission state and disable minor mode in all related buffers
       (setq claude-agent--permission-data nil)
       (setq claude-agent--permission-overlay-specs nil)
       (claude-agent-permission-mode -1)
       ;; For polymode: also disable in all indirect buffers sharing this base
       (let ((base (current-buffer)))
         (dolist (buf (buffer-list))
           (when (and (buffer-live-p buf)
                      (eq (buffer-base-buffer buf) base))
             (with-current-buffer buf
               (claude-agent-permission-mode -1)
               ;; Clear permission overlays in indirect buffers too
               (dolist (ov (overlays-in (point-min) (point-max)))
                 (when (overlay-get ov 'claude-permission-face)
                   (delete-overlay ov)))))))
       ;; Clear permission overlays in base buffer
       (dolist (ov (overlays-in (point-min) (point-max)))
         (when (overlay-get ov 'claude-permission-face)
           (delete-overlay ov)))
       ;; Switch to empty mode (clears input area completely)
       (setq claude-agent--input-mode 'empty)
       ;; Show thinking status (this rebuilds dynamic section with empty input)
       (claude-agent--set-thinking "Processing...")
       ;; Send JSON response to process
       (when (and claude-agent--process
                  (process-live-p claude-agent--process))
         (process-send-string claude-agent--process
                              (concat (json-encode response-msg) "\n")))))))

(defun claude-agent-permit-once ()
  "Allow the tool to run once."
  (interactive)
  (claude-agent--send-permission-response "allow_once"))

(defun claude-agent-permit-session ()
  "Allow the tool pattern for this session."
  (interactive)
  (claude-agent--send-permission-response "allow_session"))

(defun claude-agent-permit-always ()
  "Always allow this tool pattern (saves to settings)."
  (interactive)
  (claude-agent--send-permission-response "allow_always"))

(defun claude-agent-deny ()
  "Deny the permission request."
  (interactive)
  (claude-agent--send-permission-response "deny"))

;;;; Process management

(defun claude-agent--get-agent-dir ()
  "Get the directory containing the Python agent.
Returns the path to the claude_agent directory, or nil if not found."
  (when-let ((lib-file (locate-library "claude-agent")))
    (expand-file-name "claude_agent"
                      (file-name-directory lib-file))))

(defun claude-agent--generate-mcp-config (work-dir buffer-name)
  "Generate MCP config file for emacs_mcp server.
WORK-DIR is the session working directory.
BUFFER-NAME is the Claude buffer name for this session.
Returns the path to the generated config file."
  (let* ((agent-dir (claude-agent--get-agent-dir))
         (emacs-mcp-dir (expand-file-name "../emacs_mcp" agent-dir))
         (config-file (make-temp-file "claude-mcp-config-" nil ".json"))
         (server-socket (expand-file-name (or (bound-and-true-p server-name) "server")
                                          (or (bound-and-true-p server-socket-dir)
                                              (expand-file-name "emacs" (temporary-file-directory)))))
         (config `((mcpServers
                    . ((claudemacs
                        . ((command . "uv")
                           (args . ["run" "--directory" ,emacs-mcp-dir
                                    "python" "-m" "emacs_mcp.server"])
                           (env . ((CLAUDE_AGENT_SOCKET . ,server-socket)
                                   (CLAUDE_AGENT_CWD . ,(expand-file-name work-dir))
                                   (CLAUDE_AGENT_BUFFER_NAME . ,buffer-name))))))))))
    (with-temp-file config-file
      (insert (json-encode config)))
    config-file))

(defvar-local claude-agent--mcp-config-file nil
  "Path to the MCP config file for this session.")

(defun claude-agent--validate-prerequisites ()
  "Validate that all required commands and directories exist.
Returns nil if valid, or an error message string if validation fails."
  (let ((agent-dir (claude-agent--get-agent-dir)))
    (cond
     ;; Check if command exists in PATH
     ((not (executable-find claude-agent-python-command))
      (format "Command '%s' not found in PATH. Please install it or set `claude-agent-python-command' to the correct command.\n\nFor uv installation, see: https://docs.astral.sh/uv/getting-started/installation/"
              claude-agent-python-command))
     ;; Check if agent directory exists
     ((not agent-dir)
      "Could not locate claude-agent library using (locate-library \"claude-agent\").\n\nPlease ensure claude-agent is properly installed and in your `load-path'.")
     ((not (file-directory-p agent-dir))
      (format "Agent directory not found: %s\n\nThe claude_agent Python package should be in the same directory as claude-agent.el."
              agent-dir))
     ;; Check if Python module exists
     ((not (file-exists-p (expand-file-name "claude_agent/__init__.py" agent-dir)))
      (format "Python module 'claude_agent' not found in: %s\n\nPlease ensure the claude_agent package is properly installed."
              agent-dir))
     ;; All checks passed
     (t nil))))

(defun claude-agent--start-process (work-dir buffer &optional resume-session continue-session model)
  "Start the Python agent process for WORK-DIR with BUFFER.
Optional RESUME-SESSION is a session ID to resume.
Optional CONTINUE-SESSION, if non-nil, continues the most recent session.
Optional MODEL is the model to use (e.g., 'sonnet', 'opus', 'haiku')."
  ;; Validate prerequisites first
  (when-let ((error-msg (claude-agent--validate-prerequisites)))
    (error "Cannot start Claude agent:\n%s" error-msg))

  ;; Ensure Emacs server is running if MCP is enabled
  (when (and claude-agent-enable-mcp
             (not (bound-and-true-p server-process)))
    (message "Starting Emacs server for MCP...")
    (server-start))
  (let* ((agent-dir (claude-agent--get-agent-dir))
         (log-file (expand-file-name "claude-agent.log" work-dir))
         (buffer-name (buffer-name buffer))
         (mcp-config (when claude-agent-enable-mcp
                       (claude-agent--generate-mcp-config work-dir buffer-name)))
         (args (list "run" "--directory" agent-dir
                     "python" "-u" "-m" "claude_agent"  ; -u for unbuffered
                     "--work-dir" work-dir
                     "--log-file" log-file)))
    ;; Add MCP config if enabled
    (when mcp-config
      (setq args (append args (list "--mcp-config" mcp-config)))
      ;; Store MCP config path for cleanup
      (with-current-buffer buffer
        (setq claude-agent--mcp-config-file mcp-config)))
    ;; Add resume or continue flags
    (when resume-session
      (setq args (append args (list "--resume" resume-session))))
    (when continue-session
      (setq args (append args (list "--continue"))))
    ;; Add model if specified
    (when model
      (setq args (append args (list "--model" model))))
    ;; Use pipe (nil) instead of PTY to avoid focus-related buffering issues
    ;; Bind default-directory so the process starts in work-dir
    (let ((default-directory work-dir)
          (process-connection-type nil)
          (process-environment (cons "PYTHONUNBUFFERED=1" process-environment)))
      (condition-case err
          (let ((proc (apply #'start-process
                             "claude-agent"
                             buffer
                             claude-agent-python-command
                             args)))
            (set-process-coding-system proc 'utf-8 'utf-8)
            (set-process-filter proc #'claude-agent--process-filter)
            (set-process-sentinel proc #'claude-agent--process-sentinel)
            (set-process-query-on-exit-flag proc nil)
            proc)
        (error
         (error "Failed to start Claude agent process:\n\nCommand: %s %s\n\nError: %s\n\nPlease check that:\n- %s is installed and in your PATH\n- The agent directory exists: %s\n- Python is available"
                claude-agent-python-command
                (mapconcat #'identity args " ")
                (error-message-string err)
                claude-agent-python-command
                agent-dir))))))

(defun claude-agent--process-sentinel (proc event)
  "Handle process PROC state change EVENT."
  (when (memq (process-status proc) '(exit signal))
    (let ((buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (claude-agent--set-thinking nil)
          (claude-agent--append-to-log
           (format "\n[Process %s]\n" (string-trim event))
           'claude-agent-session-face)
          ;; Clean up MCP config file
          (when (and claude-agent--mcp-config-file
                     (file-exists-p claude-agent--mcp-config-file))
            (delete-file claude-agent--mcp-config-file)
            (setq claude-agent--mcp-config-file nil)))))))

;;;; User commands

(defun claude-agent--is-busy-p ()
  "Return t if the agent is currently busy (thinking)."
  claude-agent--thinking-status)

(defun claude-agent--render-queue ()
  "Render queued messages by rebuilding the dynamic section."
  (claude-agent--render-dynamic-section))

(defun claude-agent--send-next-queued ()
  "Send the next queued message if any and not busy."
  (when (and claude-agent--message-queue
             (not (claude-agent--is-busy-p))
             claude-agent--process
             (process-live-p claude-agent--process))
    (let ((msg (pop claude-agent--message-queue)))
      ;; Send JSON message to process
      (process-send-string claude-agent--process
                           (concat (json-encode `((type . "message") (text . ,msg))) "\n")))))

(defun claude-agent-send ()
  "Send the current input to Claude, or queue if busy."
  (interactive)
  (claude-agent--in-base-buffer
   (when (and claude-agent--input-start-marker
              claude-agent--process
              (process-live-p claude-agent--process))
     (let* ((input (string-trim (buffer-substring-no-properties
                                  claude-agent--input-start-marker (point-max)))))
       ;; Ignore if empty
       (unless (string-empty-p input)
         ;; Clear the input area first
         (let ((inhibit-read-only t))
           (delete-region claude-agent--input-start-marker (point-max)))
         ;; Add to history
         (push input claude-agent--input-history)
         (setq claude-agent--input-history-index 0)
         ;; If busy, queue the message; otherwise send directly
         (if (claude-agent--is-busy-p)
             (progn
               (push input claude-agent--message-queue)
               (claude-agent--render-dynamic-section)
               (message "Message queued (agent is busy)"))
           ;; Send JSON message to process
           (process-send-string claude-agent--process
                                (concat (json-encode `((type . "message") (text . ,input))) "\n"))
           ;; Re-render dynamic section (input already cleared)
           (claude-agent--render-dynamic-section)))))))

(defun claude-agent-send-or-newline ()
  "Send input if on last line, otherwise insert newline."
  (interactive)
  (if (claude-agent--in-input-area-p)
      (if (save-excursion (end-of-line) (eobp))
          (claude-agent-send)
        (newline))
    (newline)))

(defun claude-agent-interrupt ()
  "Interrupt the current Claude operation."
  (interactive)
  (claude-agent--in-base-buffer
   (when (and claude-agent--process
              (process-live-p claude-agent--process))
     (process-send-string claude-agent--process
                          (concat (json-encode '((type . "interrupt"))) "\n")))))

(defun claude-agent-quit ()
  "Quit the Claude session."
  (interactive)
  (when (yes-or-no-p "Quit Claude session? ")
    (claude-agent--in-base-buffer
     (when (and claude-agent--process
                (process-live-p claude-agent--process))
       (process-send-string claude-agent--process
                            (concat (json-encode '((type . "quit"))) "\n"))))))

(defun claude-agent--send-json (msg)
  "Send MSG as JSON to the agent process.
MSG should be an alist that will be encoded as JSON."
  (claude-agent--in-base-buffer
   (when (and claude-agent--process
              (process-live-p claude-agent--process))
     (process-send-string claude-agent--process
                          (concat (json-encode msg) "\n")))))

(defun claude-agent-previous-input ()
  "Recall previous input from history."
  (interactive)
  (claude-agent--in-base-buffer
   (when (and claude-agent--input-history
              (< claude-agent--input-history-index
                 (length claude-agent--input-history)))
     (let ((inhibit-read-only t))
       (delete-region claude-agent--input-start-marker (point-max))
       (goto-char claude-agent--input-start-marker)
       (insert (nth claude-agent--input-history-index
                    claude-agent--input-history))
       (cl-incf claude-agent--input-history-index)))))

(defun claude-agent-next-input ()
  "Recall next input from history."
  (interactive)
  (claude-agent--in-base-buffer
   (when (> claude-agent--input-history-index 0)
     (cl-decf claude-agent--input-history-index)
     (let ((inhibit-read-only t))
       (delete-region claude-agent--input-start-marker (point-max))
       (goto-char claude-agent--input-start-marker)
       (when (> claude-agent--input-history-index 0)
         (insert (nth (1- claude-agent--input-history-index)
                      claude-agent--input-history)))))))

;;;; MCP status functions

(defun claude-agent-mcp-server-status ()
  "Return list of MCP server statuses for current session.
Each element is an alist with keys: name, status."
  (claude-agent--in-base-buffer
   claude-agent--mcp-server-status))

;;;###autoload
(defun claude-agent-show-mcp-status ()
  "Display MCP server connection status for current Claude session."
  (interactive)
  (claude-agent--in-base-buffer
   (let ((status claude-agent--mcp-server-status))
     (if status
         (let ((msg (mapconcat
                     (lambda (s)
                       (let ((name (cdr (assq 'name s)))
                             (st (cdr (assq 'status s))))
                         (format "%s: %s" name
                                 (if (equal st "connected")
                                     (propertize st 'face 'success)
                                   (propertize st 'face 'error)))))
                     status "\n")))
           (message "MCP Servers:\n%s" msg))
       (message "No MCP status available (send a message first to initialize)")))))

;;;; Session history loading

(defun claude-agent--get-session-file (work-dir session-id)
  "Get the session file path for SESSION-ID in WORK-DIR."
  (let* ((encoded-dir (replace-regexp-in-string
                       "/" "-"
                       (directory-file-name (expand-file-name work-dir))))
         (sessions-dir (expand-file-name encoded-dir "~/.claude/projects/")))
    (expand-file-name (concat session-id ".jsonl") sessions-dir)))

(defun claude-agent--load-session-history (work-dir session-id &optional max-messages)
  "Load conversation history from SESSION-ID in WORK-DIR.
Returns a list of message plists with :role, :content, :timestamp.
Loads at most MAX-MESSAGES (default 50) most recent messages."
  (let* ((file (claude-agent--get-session-file work-dir session-id))
         (max-msgs (or max-messages 50))
         (messages nil))
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (json (condition-case nil
                           (json-read-from-string line)
                         (error nil))))
            (when json
              (let ((type (cdr (assq 'type json)))
                    (msg (cdr (assq 'message json)))
                    (ts (cdr (assq 'timestamp json))))
                (when (and msg (member type '("user" "assistant")))
                  (let* ((role (cdr (assq 'role msg)))
                         (content (cdr (assq 'content msg)))
                         ;; Handle content that's either a string or array
                         (text-content
                          (cond
                           ((stringp content) content)
                           ((vectorp content)
                            ;; Extract text from content array
                            (mapconcat
                             (lambda (item)
                               (when (equal (cdr (assq 'type item)) "text")
                                 (cdr (assq 'text item))))
                             content ""))
                           (t nil))))
                    (when (and role text-content (not (string-empty-p text-content)))
                      (push (list :role role
                                  :content text-content
                                  :timestamp ts)
                            messages)))))))
          (forward-line 1)))
      ;; Return most recent messages, in chronological order
      (let ((recent (seq-take (nreverse messages) max-msgs)))
        (nreverse recent)))))

(defun claude-agent--format-history-message (msg)
  "Format a history message MSG for display in the buffer."
  (let* ((role (plist-get msg :role))
         (content (plist-get msg :content))
         (timestamp (plist-get msg :timestamp))
         ;; Truncate very long messages for history display
         (max-len 500)
         (truncated (if (> (length content) max-len)
                        (concat (substring content 0 max-len) "\n[...truncated...]")
                      content)))
    (concat
     (if (equal role "user")
         (propertize "┌─ You" 'face 'claude-agent-user-header-face)
       (propertize "┌─ Claude" 'face 'claude-agent-assistant-header-face))
     (when timestamp
       (propertize (format " (%s)" (format-time-string "%m-%d %H:%M" (date-to-time timestamp)))
                   'face 'claude-agent-session-face))
     "\n"
     (propertize truncated
                 'face (if (equal role "user")
                          'claude-agent-user-face
                        'claude-agent-assistant-face))
     "\n\n")))

(defun claude-agent--insert-history-header ()
  "Insert the history section header."
  (let ((start (point)))
    (insert "─── Previous Conversation History ───────────────────────────\n\n")
    (claude-agent--apply-face start (point) 'claude-agent-session-face)))

(defun claude-agent--insert-history-footer ()
  "Insert the history section footer."
  (let ((start (point)))
    (insert "─── Resuming Session ────────────────────────────────────────\n\n")
    (claude-agent--apply-face start (point) 'claude-agent-session-face)))

(defun claude-agent--display-session-history (work-dir session-id)
  "Display conversation history from SESSION-ID in the current buffer."
  (let ((history (claude-agent--load-session-history work-dir session-id 20)))
    (when history
      (claude-agent--insert-history-header)
      (dolist (msg history)
        (let ((formatted (claude-agent--format-history-message msg)))
          (claude-agent--append-to-log formatted nil nil)))
      (claude-agent--insert-history-footer))))

;;;; Entry point

;;;###autoload
(defun claude-agent-run (work-dir &optional resume-session continue-session slug model)
  "Start a Claude agent session for WORK-DIR.
Optional RESUME-SESSION is a session ID to resume.
Optional CONTINUE-SESSION, if non-nil, continues the most recent session.
Optional SLUG is a suffix for the buffer name (e.g., *claude:project:slug*).
Optional MODEL is the model to use (e.g., 'sonnet', 'opus', 'haiku')."
  (interactive
   (list (read-directory-name "Project directory: "
                              (or (vc-git-root default-directory)
                                  default-directory))))
  (let* ((expanded-dir (expand-file-name work-dir))
         (short-name (file-name-nondirectory
                      (directory-file-name expanded-dir)))
         (buf-name (if slug
                       (format "*claude:%s:%s*" short-name slug)
                     (format "*claude:%s*" short-name)))
         (buf (get-buffer-create buf-name)))

    ;; Set up buffer
    (with-current-buffer buf
      (claude-agent-mode)
      (claude-agent--init-buffer short-name)
      (setq claude-agent--parse-state nil
            claude-agent--pending-output ""
            claude-agent--session-info nil
            claude-agent--has-conversation nil
            claude-agent--work-dir expanded-dir
            default-directory expanded-dir)

      ;; Display history if resuming a specific session
      (when resume-session
        (claude-agent--display-session-history expanded-dir resume-session)))

    ;; Start process with optional resume/continue/model
    (let ((proc (claude-agent--start-process expanded-dir buf resume-session continue-session model)))
      (with-current-buffer buf
        (setq claude-agent--process proc)))

    ;; Display buffer
    (pop-to-buffer buf)
    buf))

;;;; Transient Menu

(defvar claude-agent--available-models
  '(("sonnet" . "claude-sonnet-4-20250514")
    ("opus" . "claude-opus-4-20250514")
    ("haiku" . "claude-haiku-3-5-20241022"))
  "Available Claude models as (alias . full-name) pairs.")

(defun claude-agent--current-model ()
  "Get the current model from session info."
  (plist-get claude-agent--session-info :model))

(defun claude-agent--format-model-for-display (model-string)
  "Format MODEL-STRING for display, extracting key info."
  (cond
   ((string-match "sonnet" model-string) "Sonnet")
   ((string-match "opus" model-string) "Opus")
   ((string-match "haiku" model-string) "Haiku")
   (t model-string)))

(defun claude-agent-set-model (model)
  "Change the model for the current session to MODEL.
MODEL should be an alias like 'sonnet', 'opus', or 'haiku'.
This restarts the session with the new model while preserving the conversation."
  (interactive
   (list (completing-read "Model: "
                          (mapcar #'car claude-agent--available-models)
                          nil t)))
  (if (and claude-agent--process (process-live-p claude-agent--process))
      (let ((session-id (plist-get claude-agent--session-info :session-id))
            (work-dir claude-agent--work-dir))
        (if session-id
            (progn
              ;; Kill current process
              (delete-process claude-agent--process)
              (setq claude-agent--process nil)
              ;; Clear thinking state
              (claude-agent--set-thinking nil)
              ;; Notify user
              (claude-agent--append-to-log
               (format "\n🔄 Switching to %s model...\n" model)
               'claude-agent-session-face)
              ;; Start new process with same session ID but new model
              (let ((proc (claude-agent--start-process
                           work-dir (current-buffer) session-id nil model)))
                (setq claude-agent--process proc))
              (message "Restarting session with %s model..." model))
          (message "No session ID available - cannot switch model")))
    (message "No active Claude session")))

(defun claude-agent-mcp-list ()
  "List configured MCP servers."
  (interactive)
  (let ((output (shell-command-to-string "claude mcp list 2>/dev/null")))
    (if (string-match-p "No MCP servers" output)
        (message "No MCP servers configured")
      (with-current-buffer (get-buffer-create "*Claude MCP Servers*")
        (read-only-mode -1)
        (erase-buffer)
        (insert "MCP Servers\n")
        (insert "===========\n\n")
        (insert output)
        (read-only-mode 1)
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

(defun claude-agent-mcp-add ()
  "Add an MCP server interactively."
  (interactive)
  (let* ((name (read-string "Server name: "))
         (type (completing-read "Type: " '("stdio" "sse") nil t))
         (command-or-url (read-string (if (equal type "stdio")
                                          "Command: "
                                        "URL: "))))
    (if (equal type "stdio")
        (let ((args (read-string "Arguments (space-separated, optional): ")))
          (shell-command (format "claude mcp add %s %s %s"
                                 (shell-quote-argument name)
                                 (shell-quote-argument command-or-url)
                                 args)))
      (shell-command (format "claude mcp add --transport sse %s %s"
                             (shell-quote-argument name)
                             (shell-quote-argument command-or-url))))
    (message "Added MCP server: %s" name)))

(defun claude-agent-mcp-remove ()
  "Remove an MCP server."
  (interactive)
  (let* ((output (shell-command-to-string "claude mcp list --json 2>/dev/null"))
         (servers (ignore-errors (json-read-from-string output)))
         (names (mapcar (lambda (s) (cdr (assq 'name s))) servers)))
    (if names
        (let ((name (completing-read "Remove server: " names nil t)))
          (shell-command (format "claude mcp remove %s" (shell-quote-argument name)))
          (message "Removed MCP server: %s" name))
      (message "No MCP servers to remove"))))

(defun claude-agent-compact ()
  "Compact the conversation history.
Sends /compact as a message to Claude."
  (interactive)
  (if (and claude-agent--process (process-live-p claude-agent--process))
      (progn
        (claude-agent--send-json '((type . "message") (text . "/compact")))
        (message "Compacting conversation..."))
    (message "No active Claude session")))

(defun claude-agent-clear ()
  "Clear the conversation history and start fresh.
Sends /clear as a message to Claude."
  (interactive)
  (if (and claude-agent--process (process-live-p claude-agent--process))
      (when (yes-or-no-p "Clear conversation history? ")
        (claude-agent--send-json '((type . "message") (text . "/clear")))
        (message "Clearing conversation..."))
    (message "No active Claude session")))

(defun claude-agent-restart ()
  "Restart the Claude session, continuing the same conversation.
Kills the current process and starts a new one with --continue.
This reloads the MCP server and Python agent while preserving the session."
  (interactive)
  (claude-agent--in-base-buffer
   (unless claude-agent--work-dir
     (error "No work directory set for this session"))
   (let ((work-dir claude-agent--work-dir)
         (buf (current-buffer)))
     ;; Kill existing process
     (when (and claude-agent--process (process-live-p claude-agent--process))
       (delete-process claude-agent--process))
     ;; Clean up MCP config file if it exists
     (when (and claude-agent--mcp-config-file
                (file-exists-p claude-agent--mcp-config-file))
       (delete-file claude-agent--mcp-config-file))
     ;; Reset state but keep conversation markers
     (setq claude-agent--process nil
           claude-agent--mcp-config-file nil
           claude-agent--thinking-status nil
           claude-agent--progress-indicators nil)
     ;; Append restart message to log
     (claude-agent--append-to-log
      "\n⟳ Restarting session...\n"
      'claude-agent-session-face)
     ;; Start new process with --continue to resume the session
     (let ((proc (claude-agent--start-process work-dir buf nil t)))
       (setq claude-agent--process proc))
     (claude-agent--render-dynamic-section)
     ;; Send a message to the agent after a short delay to let it initialize
     (run-with-timer
      2 nil
      (lambda (buffer)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (and claude-agent--process
                       (process-live-p claude-agent--process))
              (process-send-string
               claude-agent--process
               (concat (json-encode
                        '((type . "message")
                          (text . "Session restarted. MCP server reloaded with any code changes. Please continue.")))
                       "\n"))))))
      buf)
     (message "Session restarted, MCP server reloaded."))))



(defun claude-agent-show-cost ()
  "Show token usage and cost for current session."
  (interactive)
  (let ((cost (plist-get claude-agent--session-info :cost))
        (input claude-agent--input-tokens)
        (output claude-agent--output-tokens))
    (message "Session cost: $%.4f | Last turn: %s in / %s out tokens"
             (or cost 0)
             (or input 0)
             (or output 0))))

;;;; Progress indicator management

(defun claude-agent-toggle-progress ()
  "Toggle visibility of progress indicators."
  (interactive)
  (setq claude-agent--progress-visible (not claude-agent--progress-visible))
  (claude-agent--render-dynamic-section)
  (message "Progress indicators %s" (if claude-agent--progress-visible "shown" "hidden")))

(defun claude-agent-toggle-todos ()
  "Toggle visibility of todo list."
  (interactive)
  (setq claude-agent--todos-visible (not claude-agent--todos-visible))
  (claude-agent--render-dynamic-section)
  (message "Todo list %s" (if claude-agent--todos-visible "shown" "hidden")))

(defun claude-agent-progress-start (label &optional id percent)
  "Start a progress indicator with LABEL at PERCENT (default 0).
Returns the progress ID. Optional ID allows specifying a custom identifier."
  (unless claude-agent--progress-indicators
    (setq claude-agent--progress-indicators (make-hash-table :test 'equal)))
  (let ((progress-id (or id (format "progress-%s" (format-time-string "%s%N"))))
        (pct (or percent 0)))
    (puthash progress-id
             (list :label label
                   :percent (if (numberp pct) pct (string-to-number pct))
                   :start-time (current-time))
             claude-agent--progress-indicators)
    (claude-agent--render-dynamic-section)
    progress-id))

(defun claude-agent-progress-update (id &optional label percent)
  "Update progress indicator ID.
LABEL updates the text label (nil keeps current).
PERCENT sets progress 0-100 (nil keeps current)."
  (when (and claude-agent--progress-indicators
             (gethash id claude-agent--progress-indicators))
    (let ((info (gethash id claude-agent--progress-indicators)))
      (when label
        (plist-put info :label label))
      (when percent
        (plist-put info :percent (if (numberp percent) percent (string-to-number percent))))
      (puthash id info claude-agent--progress-indicators))
    (claude-agent--render-dynamic-section))
  id)

(defun claude-agent-progress-stop (id &optional final-message)
  "Stop progress indicator ID.
Optional FINAL-MESSAGE is displayed briefly in the echo area."
  (when (and claude-agent--progress-indicators
             (gethash id claude-agent--progress-indicators))
    (remhash id claude-agent--progress-indicators)
    (claude-agent--render-dynamic-section)
    (when final-message
      (message "✓ %s" final-message)))
  "stopped")

(defun claude-agent--model-description ()
  "Return a description of the current model for transient."
  (let ((model (claude-agent--current-model)))
    (if model
        (format "Current: %s" (claude-agent--format-model-for-display model))
      "No model set")))

(defun claude-agent--session-description ()
  "Return session info description for transient."
  (let ((session-id (plist-get claude-agent--session-info :session-id)))
    (if session-id
        (format "Session: %s" (substring session-id 0 (min 8 (length session-id))))
      "No session")))

(provide 'claude-agent)
;;; claude-agent.el ends here
