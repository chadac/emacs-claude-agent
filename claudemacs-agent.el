;;; claudemacs-agent.el --- Claude interaction buffer -*- lexical-binding: t; -*-

;; This file is part of claudemacs.
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

;;;; Customization

(defgroup claudemacs-agent nil
  "Claude interaction buffer."
  :group 'claudemacs)

(defcustom claudemacs-agent-python-command "uv"
  "Command to run Python for the agent wrapper."
  :type 'string
  :group 'claudemacs-agent)

;;;; Faces

(defface claudemacs-agent-header-face
  '((t :foreground "#56b6c2" :slant italic))
  "Face for the header section."
  :group 'claudemacs-agent)

(defface claudemacs-agent-user-header-face
  '((t :foreground "#61afef" :weight bold))
  "Face for user message headers."
  :group 'claudemacs-agent)

(defface claudemacs-agent-user-face
  '((t :foreground "#c8ccd4"))  ; Slightly off-white (lighter than default)
  "Face for user message text."
  :group 'claudemacs-agent)

(defface claudemacs-agent-assistant-header-face
  '((t :foreground "#c678dd" :weight bold))
  "Face for assistant message headers."
  :group 'claudemacs-agent)

(defface claudemacs-agent-assistant-face
  '((t :foreground "#e5e5e5"))
  "Face for assistant message text."
  :group 'claudemacs-agent)

(defface claudemacs-agent-tool-face
  '((t :foreground "#e5c07b" :slant italic))
  "Face for tool call indicators."
  :group 'claudemacs-agent)

(defface claudemacs-agent-status-face
  '((t :foreground "#56b6c2" :slant italic))
  "Face for status info section (model, cost, session)."
  :group 'claudemacs-agent)

(defface claudemacs-agent-thinking-face
  '((t :foreground "#98c379" :weight bold))
  "Face for thinking indicator."
  :group 'claudemacs-agent)

(defface claudemacs-agent-error-face
  '((t :foreground "#e06c75" :weight bold))
  "Face for error messages."
  :group 'claudemacs-agent)

(defface claudemacs-agent-session-face
  '((t :foreground "#56b6c2" :slant italic))
  "Face for session info messages."
  :group 'claudemacs-agent)

(defface claudemacs-agent-input-header-face
  '((t :foreground "#5c6370" :weight bold))
  "Face for the input area header."
  :group 'claudemacs-agent)


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

(defvar-local claudemacs-agent--process nil
  "The agent process for this session.")

(defvar-local claudemacs-agent--static-end-marker nil
  "Marker for end of static section (start of dynamic section).
Everything before this is completed content that never changes.")

(defvar-local claudemacs-agent--input-start-marker nil
  "Marker for start of input section (where user types).
Set fresh after each dynamic section rebuild.")

;;;; Buffer-local variables - State

(defvar-local claudemacs-agent--parse-state nil
  "Current parsing state: nil, user, assistant, tool, error, session.")

(defvar-local claudemacs-agent--pending-output ""
  "Buffer for incomplete lines from process output.")

(defvar-local claudemacs-agent--session-info nil
  "Plist with session info: :model :session-id :cost.")

(defvar-local claudemacs-agent--input-history nil
  "History of inputs sent to Claude.")

(defvar-local claudemacs-agent--input-history-index 0
  "Current position in input history.")

;;;; Buffer-local variables - Thinking status

(defconst claudemacs-agent--spinner-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames for the spinner animation.")



(defvar-local claudemacs-agent--spinner-index 0
  "Current index in spinner frames.")

(defvar-local claudemacs-agent--spinner-timer nil
  "Timer for spinner animation.")

(defvar-local claudemacs-agent--thinking-start-time nil
  "Time when thinking started, for elapsed time display.")

(defvar-local claudemacs-agent--thinking-status nil
  "Current thinking status text, or nil if not thinking.")

(defvar-local claudemacs-agent--input-tokens 0
  "Input token count for current turn.")

(defvar-local claudemacs-agent--output-tokens 0
  "Output token count for current turn.")

(defvar-local claudemacs-agent--has-conversation nil
  "Non-nil if conversation has started (first message sent).")

(defvar-local claudemacs-agent--placeholder-overlay nil
  "Overlay for the placeholder text in empty input area.")

(defconst claudemacs-agent--placeholder-text "Enter your message... (C-c C-c to send)"
  "Placeholder text shown when input area is empty.")

(defface claudemacs-agent-placeholder-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for placeholder text in empty input area."
  :group 'claudemacs-agent)

;;;; Buffer-local variables - Message queue

(defvar-local claudemacs-agent--message-queue nil
  "List of messages queued while agent is busy. Each is a string.")


(defface claudemacs-agent-queued-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for queued messages (grayed out)."
  :group 'claudemacs-agent)

(defface claudemacs-agent-queued-header-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for queued message headers."
  :group 'claudemacs-agent)

;;;; Mode definition

(defvar claudemacs-agent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claudemacs-agent-send)
    (define-key map (kbd "C-<return>") #'claudemacs-agent-send)
    (define-key map (kbd "C-c C-k") #'claudemacs-agent-interrupt)
    (define-key map (kbd "C-c C-q") #'claudemacs-agent-quit)
    (define-key map (kbd "M-p") #'claudemacs-agent-previous-input)
    (define-key map (kbd "M-n") #'claudemacs-agent-next-input)
    map)
  "Keymap for `claudemacs-agent-mode'.")

(define-derived-mode claudemacs-agent-mode fundamental-mode "Claude"
  "Major mode for Claude interaction buffer.
Uses org-mode fontification without org-mode keybindings."
  :group 'claudemacs-agent
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local buffer-read-only nil)
  (visual-line-mode 1)
  ;; Set up org-mode fontification without org-mode keybindings
  ;; This calls org's internal function to populate font-lock-keywords
  (org-set-font-lock-defaults)
  (font-lock-mode 1)
  ;; Ensure our keybindings are set (defvar doesn't reinit on re-eval)
  (use-local-map claudemacs-agent-mode-map)
  ;; Re-define keys to ensure they're set
  (local-set-key (kbd "C-c C-c") #'claudemacs-agent-send)
  (local-set-key (kbd "C-<return>") #'claudemacs-agent-send)
  (local-set-key (kbd "C-c C-k") #'claudemacs-agent-interrupt)
  (local-set-key (kbd "C-c C-q") #'claudemacs-agent-quit)
  (local-set-key (kbd "M-p") #'claudemacs-agent-previous-input)
  (local-set-key (kbd "M-n") #'claudemacs-agent-next-input)
  ;; Set up placeholder update hook
  (add-hook 'post-command-hook #'claudemacs-agent--post-command-hook nil t))

;;;; Helper functions

(defun claudemacs-agent--in-input-area-p ()
  "Return t if point is in the input area."
  (and claudemacs-agent--input-start-marker
       (>= (point) claudemacs-agent--input-start-marker)))

(defmacro claudemacs-agent--in-base-buffer (&rest body)
  "Execute BODY in the base buffer (for polymode compatibility)."
  `(let ((base (or (buffer-base-buffer) (current-buffer))))
     (with-current-buffer base
       ,@body)))

;;;; Placeholder management

(defun claudemacs-agent--input-empty-p ()
  "Return t if the input area is empty (only whitespace)."
  (and claudemacs-agent--input-start-marker
       ;; string-blank-p returns match position (0) for empty, so convert to t
       (not (null (string-blank-p (buffer-substring-no-properties
                                   claudemacs-agent--input-start-marker (point-max)))))))

(defun claudemacs-agent--update-placeholder ()
  "Show or hide placeholder based on input area content."
  (when (and claudemacs-agent--input-start-marker
             (marker-position claudemacs-agent--input-start-marker))
    (if (claudemacs-agent--input-empty-p)
        ;; Show placeholder at current input-start position
        (let ((pos (marker-position claudemacs-agent--input-start-marker)))
          ;; Move existing overlay or create new one
          (if (and claudemacs-agent--placeholder-overlay
                   (overlay-buffer claudemacs-agent--placeholder-overlay))
              ;; Move to new position
              (move-overlay claudemacs-agent--placeholder-overlay pos pos)
            ;; Create new overlay
            (setq claudemacs-agent--placeholder-overlay (make-overlay pos pos))
            (overlay-put claudemacs-agent--placeholder-overlay 'before-string
                         (propertize claudemacs-agent--placeholder-text
                                     'face 'claudemacs-agent-placeholder-face
                                     'cursor t))
            (overlay-put claudemacs-agent--placeholder-overlay 'evaporate nil)))
      ;; Hide placeholder
      (when (and claudemacs-agent--placeholder-overlay
                 (overlay-buffer claudemacs-agent--placeholder-overlay))
        (delete-overlay claudemacs-agent--placeholder-overlay)
        (setq claudemacs-agent--placeholder-overlay nil)))))

(defun claudemacs-agent--post-command-hook ()
  "Hook run after each command to update placeholder visibility.
Also moves point to prompt marker when input area is empty."
  (claudemacs-agent--update-placeholder)
  ;; If input is empty and we're in the input area, keep cursor at prompt
  (when (and (claudemacs-agent--input-empty-p)
             (claudemacs-agent--in-input-area-p)
             claudemacs-agent--input-start-marker)
    (goto-char claudemacs-agent--input-start-marker)))

;;;; Section management
;;
;; Two-zone architecture:
;; - Static section: Append-only (header + conversation log)
;; - Dynamic section: Re-rendered from state (status bar + input area)

(defun claudemacs-agent--init-buffer (session-name)
  "Initialize buffer with section structure for SESSION-NAME."
  (let ((inhibit-read-only t))
    (erase-buffer)

    ;; === HEADER (part of static section) ===
    (let ((start (point)))
      (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
      (insert (format " Claude Session: %s\n" session-name))
      (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
      (claudemacs-agent--apply-face start (point) 'claudemacs-agent-header-face))

    ;; === STATIC END MARKER ===
    ;; Everything before this is committed content that never changes
    (setq claudemacs-agent--static-end-marker (point-marker))
    (set-marker-insertion-type claudemacs-agent--static-end-marker nil)

    ;; === INPUT START MARKER ===
    ;; Set initially at same position, will be updated by rebuild
    (setq claudemacs-agent--input-start-marker (point-marker))
    (set-marker-insertion-type claudemacs-agent--input-start-marker nil)

    ;; Show placeholder and position cursor
    (claudemacs-agent--update-placeholder)
    (goto-char claudemacs-agent--input-start-marker)

    ;; Make everything before input read-only
    (claudemacs-agent--update-read-only)))

(defun claudemacs-agent--apply-face (start end face)
  "Apply FACE to region from START to END using overlay."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'priority 100)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'claudemacs-agent-styled t))
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
            (overlay-put ov 'claudemacs-agent-styled t)))))))

(defun claudemacs-agent--update-read-only ()
  "Update read-only text property to cover everything before prompt marker."
  ;; Use text properties for read-only (overlays don't enforce read-only)
  (when (and claudemacs-agent--input-start-marker
             (> (marker-position claudemacs-agent--input-start-marker) (point-min)))
    ;; Remove read-only from entire buffer first (property list needs property names only)
    (remove-list-of-text-properties (point-min) (point-max) '(read-only rear-nonsticky))
    ;; Apply read-only to everything before prompt, with rear-nonsticky
    ;; so text inserted at the boundary is NOT read-only
    (add-text-properties (point-min) claudemacs-agent--input-start-marker
                         '(read-only t rear-nonsticky (read-only)))))

;;;; Dynamic section management
;;
;; Two-zone architecture:
;; - Static section: Append-only log content (header + conversation)
;; - Dynamic section: Re-rendered from state (status bar + input area)
;;
;; `append-to-static` appends to the static section directly.
;; `render-dynamic-section` clears and re-renders dynamic section from state.

(defun claudemacs-agent--get-input-text ()
  "Get the current text in the input area."
  (if (and claudemacs-agent--input-start-marker
           (marker-position claudemacs-agent--input-start-marker))
      (buffer-substring-no-properties
       claudemacs-agent--input-start-marker (point-max))
    ""))

(defun claudemacs-agent--append-to-static (text)
  "Append TEXT to the static section and re-render dynamic section."
  ;; Save input and cursor BEFORE any modifications
  (let* ((saved-input (claudemacs-agent--get-input-text))
         (cursor-offset (when (and claudemacs-agent--input-start-marker
                                   (marker-position claudemacs-agent--input-start-marker)
                                   (>= (point) claudemacs-agent--input-start-marker))
                          (- (point) claudemacs-agent--input-start-marker)))
         (inhibit-read-only t))
    ;; Delete everything from static-end to end
    (delete-region claudemacs-agent--static-end-marker (point-max))
    ;; Insert new static content
    (goto-char claudemacs-agent--static-end-marker)
    (insert text)
    (set-marker claudemacs-agent--static-end-marker (point))
    ;; Insert status bar
    (when claudemacs-agent--has-conversation
      (claudemacs-agent--insert-status-bar))
    ;; Set input marker and restore input
    (setq claudemacs-agent--input-start-marker (point-marker))
    (insert saved-input)
    ;; Finalize
    (claudemacs-agent--update-read-only)
    (claudemacs-agent--update-placeholder)
    ;; Restore cursor
    (goto-char (if (and cursor-offset (>= cursor-offset 0))
                   (min (+ claudemacs-agent--input-start-marker cursor-offset) (point-max))
                 claudemacs-agent--input-start-marker))))

(defun claudemacs-agent--render-dynamic-section ()
  "Render the dynamic section (status bar + input area).
Clears everything after static-end-marker and re-renders from state.
Preserves any text in the input area and cursor position."
  (let* ((inhibit-read-only t)
         ;; Save cursor position relative to input-start (offset into input text)
         (cursor-offset (when (and claudemacs-agent--input-start-marker
                                   (marker-position claudemacs-agent--input-start-marker)
                                   (>= (point) claudemacs-agent--input-start-marker))
                          (- (point) claudemacs-agent--input-start-marker)))
         (input-to-restore (claudemacs-agent--get-input-text)))
    ;; Clear overlays in dynamic section
    (when (and claudemacs-agent--static-end-marker
               (marker-position claudemacs-agent--static-end-marker))
      (dolist (ov (overlays-in claudemacs-agent--static-end-marker (point-max)))
        (when (overlay-get ov 'claudemacs-agent-styled)
          (delete-overlay ov)))
      ;; Delete everything from static-end to end of buffer
      (delete-region claudemacs-agent--static-end-marker (point-max)))

    ;; Position at start of dynamic section
    (goto-char (or claudemacs-agent--static-end-marker (point-max)))

    ;; === INSERT STATUS BAR ===
    (when claudemacs-agent--has-conversation
      (claudemacs-agent--insert-status-bar))

    ;; === SET INPUT MARKER AND RESTORE INPUT ===
    (setq claudemacs-agent--input-start-marker (point-marker))
    (set-marker-insertion-type claudemacs-agent--input-start-marker nil)
    (unless (string-empty-p input-to-restore)
      (insert input-to-restore))

    ;; Update read-only and placeholder
    (claudemacs-agent--update-read-only)
    (claudemacs-agent--update-placeholder)

    ;; Restore cursor position within input area
    (if (and cursor-offset (>= cursor-offset 0))
        ;; Restore to saved position (clamped to input length)
        (goto-char (min (+ claudemacs-agent--input-start-marker cursor-offset)
                        (point-max)))
      ;; Otherwise position at start of input
      (goto-char claudemacs-agent--input-start-marker))))

;;;; Status bar rendering

(defun claudemacs-agent--format-elapsed-time (start-time)
  "Format elapsed time since START-TIME as Xm Ys."
  (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
         (minutes (floor (/ elapsed 60)))
         (seconds (floor (mod elapsed 60))))
    (if (> minutes 0)
        (format "%dm%ds" minutes seconds)
      (format "%ds" seconds))))

(defun claudemacs-agent--insert-status-bar ()
  "Insert the status bar content at point.
Called by `render-dynamic-section'. Assumes point is positioned correctly."
  ;; === Thinking indicator (if active) ===
  (when claudemacs-agent--thinking-status
    (let ((start (point))
          (spinner (nth claudemacs-agent--spinner-index
                        claudemacs-agent--spinner-frames))
          (elapsed (if claudemacs-agent--thinking-start-time
                       (claudemacs-agent--format-elapsed-time
                        claudemacs-agent--thinking-start-time)
                     "0s"))
          (tokens (format "(+%d/-%d)"
                          claudemacs-agent--input-tokens
                          claudemacs-agent--output-tokens)))
      (insert (format "\n%s %s %s %s (C-c C-k to interrupt)\n"
                      spinner
                      claudemacs-agent--thinking-status
                      elapsed
                      tokens))
      (claudemacs-agent--apply-face start (point) 'claudemacs-agent-thinking-face)))

  ;; === Queued messages (if any) ===
  (when claudemacs-agent--message-queue
    (dolist (msg (reverse claudemacs-agent--message-queue))
      (let ((msg-start (point)))
        (insert "\n┄┄┄ Queued ┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄\n")
        (claudemacs-agent--apply-face msg-start (point) 'claudemacs-agent-queued-header-face)
        (setq msg-start (point))
        (let ((lines (split-string msg "\n")))
          (dolist (line lines)
            (insert "  " line "\n")))
        (claudemacs-agent--apply-face msg-start (point) 'claudemacs-agent-queued-face))))

  ;; === Status info line ===
  (let* ((model (or (plist-get claudemacs-agent--session-info :model) "..."))
         (cost (or (plist-get claudemacs-agent--session-info :cost) 0))
         (session-id (or (plist-get claudemacs-agent--session-info :session-id) "..."))
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
    (claudemacs-agent--apply-face start (point) 'claudemacs-agent-header-face)))

(defun claudemacs-agent--spinner-tick ()
  "Advance spinner and rebuild dynamic section."
  (when claudemacs-agent--thinking-status
    (setq claudemacs-agent--spinner-index
          (mod (1+ claudemacs-agent--spinner-index)
               (length claudemacs-agent--spinner-frames)))
    (claudemacs-agent--render-dynamic-section)))

(defun claudemacs-agent--set-thinking (status)
  "Set thinking STATUS, or clear if nil."
  ;; Cancel existing timer
  (when claudemacs-agent--spinner-timer
    (cancel-timer claudemacs-agent--spinner-timer)
    (setq claudemacs-agent--spinner-timer nil))

  (setq claudemacs-agent--thinking-status status)

  (if status
      (progn
        ;; Start timing if not already
        (unless claudemacs-agent--thinking-start-time
          (setq claudemacs-agent--thinking-start-time (current-time)))
        ;; Start spinner timer
        (setq claudemacs-agent--spinner-timer
              (run-with-timer 0.1 0.1 #'claudemacs-agent--spinner-tick)))
    ;; Clear timing when done
    (setq claudemacs-agent--thinking-start-time nil))

  ;; Rebuild dynamic section (handles cursor positioning)
  (claudemacs-agent--render-dynamic-section))

;;;; Content helpers

(defun claudemacs-agent--append-to-log (text &optional _face _virtual-indent)
  "Append TEXT to the static section (conversation log).
FACE and VIRTUAL-INDENT are currently ignored (org fontification handles styling)."
  (claudemacs-agent--append-to-static text))

;;;; Process filter - parsing markers

(defun claudemacs-agent--process-filter (proc output)
  "Process filter for agent PROC handling OUTPUT."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (claudemacs-agent--handle-output output)))))

(defun claudemacs-agent--handle-output (output)
  "Handle OUTPUT from the agent process, parsing markers."
  (setq claudemacs-agent--pending-output
        (concat claudemacs-agent--pending-output output))

  ;; Process complete lines
  (while (string-match "\n" claudemacs-agent--pending-output)
    (let ((line (substring claudemacs-agent--pending-output 0 (match-beginning 0))))
      (setq claudemacs-agent--pending-output
            (substring claudemacs-agent--pending-output (match-end 0)))
      (claudemacs-agent--process-line line)))

  ;; Also check if pending output is a complete marker without trailing newline
  (when (and (not (string-empty-p claudemacs-agent--pending-output))
             (string-match "^\\[/?[A-Z_]+\\]$" claudemacs-agent--pending-output))
    (claudemacs-agent--process-line claudemacs-agent--pending-output)
    (setq claudemacs-agent--pending-output "")))

(defun claudemacs-agent--process-line (line)
  "Process a single LINE of output, handling markers."
  (cond
   ;; Ready marker - clear thinking, send queued messages
   ((string= line "[READY]")
    (claudemacs-agent--set-thinking nil)
    ;; If there are queued messages, send the next one
    (when claudemacs-agent--message-queue
      (claudemacs-agent--send-next-queued)))

   ;; Thinking marker - show thinking indicator
   ((string= line "[THINKING]")
    (setq claudemacs-agent--input-tokens 0
          claudemacs-agent--output-tokens 0
          claudemacs-agent--thinking-start-time (current-time))
    (claudemacs-agent--set-thinking "Thinking..."))

   ;; Progress marker - update token counts
   ((string-match "^\\[PROGRESS \\(.*\\)\\]$" line)
    (let* ((json-str (match-string 1 line))
           (data (ignore-errors (json-read-from-string json-str))))
      (when data
        (when-let ((input (cdr (assq 'input_tokens data))))
          (setq claudemacs-agent--input-tokens input))
        (when-let ((output (cdr (assq 'output_tokens data))))
          (setq claudemacs-agent--output-tokens output)))))

   ;; Result marker - update cost
   ((string-match "^\\[RESULT \\(.*\\)\\]$" line)
    (let* ((json-str (match-string 1 line))
           (data (ignore-errors (json-read-from-string json-str))))
      (when data
        (let ((cost (cdr (assq 'cost_usd data))))
          (when cost
            (setq claudemacs-agent--session-info
                  (plist-put claudemacs-agent--session-info :cost cost))))
        (claudemacs-agent--render-dynamic-section))))

   ;; Session info marker - update model/session-id
   ((string-match "^\\[SESSION_INFO \\(.*\\)\\]$" line)
    (let* ((json-str (match-string 1 line))
           (data (ignore-errors (json-read-from-string json-str))))
      (when data
        (when-let ((model (cdr (assq 'model data))))
          (setq claudemacs-agent--session-info
                (plist-put claudemacs-agent--session-info :model model)))
        (when-let ((session-id (cdr (assq 'session_id data))))
          (setq claudemacs-agent--session-info
                (plist-put claudemacs-agent--session-info :session-id session-id)))
        (claudemacs-agent--render-dynamic-section))))

   ;; Permission request marker - show permission UI
   ((string-match "^\\[PERMISSION_REQUEST \\(.*\\)\\]$" line)
    (let* ((json-str (match-string 1 line))
           (data (ignore-errors (json-read-from-string json-str))))
      (when data
        (claudemacs-agent--set-thinking "Awaiting permission...")
        (claudemacs-agent--show-permission-prompt data))))

   ;; User message start
   ((string= line "[USER]")
    (setq claudemacs-agent--parse-state 'user)
    ;; Mark conversation as started on first user message
    (setq claudemacs-agent--has-conversation t)
    (claudemacs-agent--append-to-log
     "\n━━━ You ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"))

   ;; User message end
   ((string= line "[/USER]")
    (setq claudemacs-agent--parse-state nil))

   ;; Assistant message start
   ((string= line "[ASSISTANT]")
    (setq claudemacs-agent--parse-state 'assistant)
    (claudemacs-agent--append-to-log
     "\n━━━ Claude ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"))

   ;; Assistant message end
   ((string= line "[/ASSISTANT]")
    (setq claudemacs-agent--parse-state nil))

   ;; Tool start - format as org src block for Bash, function-style for others
   ((string-match "^\\[TOOL \\(.+\\)\\]$" line)
    (let* ((tool-info (match-string 1 line))
           (tool-name tool-info)
           (tool-args ""))
      ;; Parse tool name and args if JSON provided
      (when (string-match "^\\([^ ]+\\) \\(.*\\)$" tool-info)
        (setq tool-name (match-string 1 tool-info)
              tool-args (match-string 2 tool-info)))
      (setq claudemacs-agent--parse-state 'tool)
      (claudemacs-agent--set-thinking (format "Running: %s" tool-name))
      ;; Format based on tool type
      (if (string= tool-name "Bash")
          ;; Bash commands get org src block formatting
          (claudemacs-agent--append-to-log
           (format "\n#+begin_src bash\n%s\n#+end_src\n" tool-args)
           nil)  ; Let org-mode handle fontification
        ;; Other tools get function-style display
        (claudemacs-agent--append-to-log
         (format "\n⚙ %s(%s)\n" tool-name tool-args)
         'claudemacs-agent-tool-face))))

   ;; Tool result start - format as org example block
   ((string= line "[TOOL_RESULT]")
    (setq claudemacs-agent--parse-state 'tool-result)
    (claudemacs-agent--append-to-log "#+begin_example\n" nil))

   ;; Tool result end
   ((string= line "[/TOOL_RESULT]")
    (claudemacs-agent--append-to-log "#+end_example\n" nil)
    (setq claudemacs-agent--parse-state 'tool))

   ;; Tool end
   ((string= line "[/TOOL]")
    (setq claudemacs-agent--parse-state nil)
    (claudemacs-agent--set-thinking "Thinking..."))

   ;; Session info start (legacy)
   ((string= line "[SESSION]")
    (setq claudemacs-agent--parse-state 'session))

   ;; Session info end (legacy)
   ((string= line "[/SESSION]")
    (setq claudemacs-agent--parse-state nil))

   ;; Error start
   ((string= line "[ERROR]")
    (setq claudemacs-agent--parse-state 'error)
    (claudemacs-agent--append-to-log "\n⚠ Error: " 'claudemacs-agent-error-face))

   ;; Error end
   ((string= line "[/ERROR]")
    (setq claudemacs-agent--parse-state nil)
    (claudemacs-agent--append-to-log "\n" nil))

   ;; Regular content line
   (t
    ;; Skip session content - it's redundant with our header/status sections
    (unless (eq claudemacs-agent--parse-state 'session)
      (let* ((face (pcase claudemacs-agent--parse-state
                     ('user 'claudemacs-agent-user-face)
                     ('assistant nil)  ; Let org fontification handle Claude's output
                     ('tool 'claudemacs-agent-tool-face)
                     ('error 'claudemacs-agent-error-face)
                     (_ nil)))
             ;; Use virtual indent (line-prefix) for user/assistant - doesn't break org
             (virtual-indent (pcase claudemacs-agent--parse-state
                               ('user "  ")
                               ('assistant "  ")
                               (_ nil))))
        (claudemacs-agent--append-to-log (concat line "\n") face virtual-indent))))))

;;;; Permission prompt UI

(defvar-local claudemacs-agent--permission-data nil
  "Current permission request data.")

(defvar-local claudemacs-agent--permission-selection 0
  "Currently selected option in permission prompt (0-3).")

(defface claudemacs-agent-permission-box-face
  '((t :foreground "#e5c07b" :background "#3e4451" :box (:line-width 1 :color "#5c6370")))
  "Face for permission dialog box."
  :group 'claudemacs-agent)

(defface claudemacs-agent-permission-selected-face
  '((t :foreground "#282c34" :background "#61afef" :weight bold))
  "Face for selected option in permission dialog."
  :group 'claudemacs-agent)

(defface claudemacs-agent-permission-option-face
  '((t :foreground "#abb2bf"))
  "Face for unselected options in permission dialog."
  :group 'claudemacs-agent)

(defun claudemacs-agent--format-tool-input (tool-name tool-input)
  "Format TOOL-INPUT for display based on TOOL-NAME."
  (cond
   ((string= tool-name "Read")
    (cdr (assq 'file_path tool-input)))
   ((string= tool-name "Write")
    (cdr (assq 'file_path tool-input)))
   ((string= tool-name "Edit")
    (cdr (assq 'file_path tool-input)))
   ((string= tool-name "Bash")
    (let ((cmd (cdr (assq 'command tool-input))))
      (if (> (length cmd) 50)
          (concat (substring cmd 0 47) "...")
        cmd)))
   (t (format "%s" tool-input))))

(defun claudemacs-agent--generate-permission-pattern (tool-name tool-input scope)
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

(defvar-local claudemacs-agent--permission-overlay-specs nil
  "List of (start end face) specs for permission dialog overlays.")

(defun claudemacs-agent--apply-permission-overlays ()
  "Apply permission overlays in the current buffer using saved specs."
  (when claudemacs-agent--permission-overlay-specs
    ;; Remove existing permission overlays in this buffer
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'claudemacs-permission-face)
        (delete-overlay ov)))
    ;; Apply new overlays
    (dolist (spec claudemacs-agent--permission-overlay-specs)
      (let ((ov (make-overlay (nth 0 spec) (nth 1 spec))))
        (overlay-put ov 'face (nth 2 spec))
        (overlay-put ov 'priority 1000)
        (overlay-put ov 'evaporate nil)
        (overlay-put ov 'claudemacs-permission-face t)))))

(defun claudemacs-agent--render-permission-dialog ()
  "Render the permission dialog with current selection state."
  (when (and claudemacs-agent--permission-data
             claudemacs-agent--input-start-marker
             claudemacs-agent--input-start-marker)
    (let* ((tool-name (cdr (assq 'tool_name claudemacs-agent--permission-data)))
           (tool-input (cdr (assq 'tool_input claudemacs-agent--permission-data)))
           (input-str (claudemacs-agent--format-tool-input tool-name tool-input))
           (sel claudemacs-agent--permission-selection)
           (inhibit-read-only t)
           (options '("Allow once" "Allow for this session" "Always allow" "Deny"))
           (overlay-specs nil))
      ;; Remove existing permission overlays
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'claudemacs-permission-face)
          (delete-overlay ov)))
      ;; Clear from input-start to end of buffer and replace with dialog
      (save-excursion
        (delete-region claudemacs-agent--input-start-marker (point-max))
        (goto-char claudemacs-agent--input-start-marker)
        ;; Helper to insert and record overlay spec
        (cl-flet ((insert-styled (text face)
                    (let ((start (point)))
                      (insert text)
                      (push (list start (point) face) overlay-specs))))
          ;; Header
          (insert-styled "── Permission Request " 'claudemacs-agent-input-header-face)
          (insert-styled (make-string 40 ?─) 'claudemacs-agent-input-header-face)
          (insert "\n")
          ;; Tool info - now function-style
          (insert-styled " Claude wants to run:\n" 'claudemacs-agent-session-face)
          (insert-styled (format " %s(%s)\n\n" tool-name input-str) 'claudemacs-agent-tool-face)
          ;; Options
          (dotimes (i 4)
            (let* ((selected (= i sel))
                   (checkbox (if selected "[X]" "[ ]"))
                   (label (nth i options))
                   (face (if selected
                             'claudemacs-agent-permission-selected-face
                           'claudemacs-agent-permission-option-face)))
              (insert-styled (format " %d. %s %s\n" (1+ i) checkbox label) face)))
          ;; Footer
          (insert-styled (make-string 62 ?─) 'claudemacs-agent-input-header-face)
          (insert "\n")))
      ;; Save overlay specs
      (setq claudemacs-agent--permission-overlay-specs (nreverse overlay-specs))
      ;; Apply overlays in this buffer
      (claudemacs-agent--apply-permission-overlays)
      ;; Apply overlays in all indirect buffers too
      (let ((base (current-buffer)))
        (dolist (buf (buffer-list))
          (when (and (buffer-live-p buf)
                     (eq (buffer-base-buffer buf) base))
            (with-current-buffer buf
              (setq claudemacs-agent--permission-overlay-specs
                    (buffer-local-value 'claudemacs-agent--permission-overlay-specs base))
              (claudemacs-agent--apply-permission-overlays)))))
      ;; Update prompt marker to end
      (set-marker claudemacs-agent--input-start-marker (point-max)))))

(defun claudemacs-agent--show-permission-prompt (data)
  "Show permission prompt for DATA in the input area."
  (setq claudemacs-agent--permission-data data)
  (setq claudemacs-agent--permission-selection 0)
  ;; Render the dialog
  (claudemacs-agent--render-permission-dialog)
  ;; Set up keyboard navigation
  (claudemacs-agent--setup-permission-keymap))

(defun claudemacs-agent--permission-select-next ()
  "Move selection down in permission dialog."
  (interactive)
  (claudemacs-agent--in-base-buffer
   (when claudemacs-agent--permission-data
     (setq claudemacs-agent--permission-selection
           (mod (1+ claudemacs-agent--permission-selection) 4))
     (claudemacs-agent--render-permission-dialog))))

(defun claudemacs-agent--permission-select-prev ()
  "Move selection up in permission dialog."
  (interactive)
  (claudemacs-agent--in-base-buffer
   (when claudemacs-agent--permission-data
     (setq claudemacs-agent--permission-selection
           (mod (1- claudemacs-agent--permission-selection) 4))
     (claudemacs-agent--render-permission-dialog))))

(defun claudemacs-agent--permission-confirm ()
  "Confirm the current selection in permission dialog."
  (interactive)
  (claudemacs-agent--in-base-buffer
   (when claudemacs-agent--permission-data
     (pcase claudemacs-agent--permission-selection
       (0 (claudemacs-agent--send-permission-response "allow_once"))
       (1 (claudemacs-agent--send-permission-response "allow_session"))
       (2 (claudemacs-agent--send-permission-response "allow_always"))
       (3 (claudemacs-agent--send-permission-response "deny"))))))

;; Minor mode for permission dialog - takes precedence over evil-mode
(defvar claudemacs-agent-permission-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Number keys for direct selection
    (define-key map (kbd "1") #'claudemacs-agent-permit-once)
    (define-key map (kbd "2") #'claudemacs-agent-permit-session)
    (define-key map (kbd "3") #'claudemacs-agent-permit-always)
    (define-key map (kbd "4") #'claudemacs-agent-deny)
    ;; Arrow keys for navigation
    (define-key map (kbd "<up>") #'claudemacs-agent--permission-select-prev)
    (define-key map (kbd "<down>") #'claudemacs-agent--permission-select-next)
    (define-key map (kbd "C-p") #'claudemacs-agent--permission-select-prev)
    (define-key map (kbd "C-n") #'claudemacs-agent--permission-select-next)
    (define-key map (kbd "k") #'claudemacs-agent--permission-select-prev)
    (define-key map (kbd "j") #'claudemacs-agent--permission-select-next)
    ;; Confirm selection
    (define-key map (kbd "RET") #'claudemacs-agent--permission-confirm)
    (define-key map (kbd "SPC") #'claudemacs-agent--permission-confirm)
    ;; Quick keys
    (define-key map (kbd "y") #'claudemacs-agent-permit-once)
    (define-key map (kbd "n") #'claudemacs-agent-deny)
    (define-key map (kbd "a") #'claudemacs-agent-permit-always)
    (define-key map (kbd "q") #'claudemacs-agent-deny)
    ;; Escape to deny (helpful for evil users)
    (define-key map (kbd "<escape>") #'claudemacs-agent-deny)
    map)
  "Keymap for permission dialog mode.")

(define-minor-mode claudemacs-agent-permission-mode
  "Minor mode for permission dialog interaction.
Takes precedence over evil-mode keybindings."
  :lighter " Permit"
  :keymap claudemacs-agent-permission-mode-map
  (if claudemacs-agent-permission-mode
      (progn
        ;; Switch to emacs state to use our keymap directly
        (when (bound-and-true-p evil-local-mode)
          (evil-emacs-state))
        (message "Permission: j/k to select, RET to confirm, 1-4 for direct choice"))
    ;; When disabling, return to normal state
    (when (bound-and-true-p evil-local-mode)
      (evil-normal-state))))

(defun claudemacs-agent--setup-permission-keymap ()
  "Set up keymap for permission prompt interaction."
  ;; Enable permission mode in the base buffer
  (claudemacs-agent-permission-mode 1)
  ;; For polymode: also enable in all indirect buffers sharing this base
  (let ((base (or (buffer-base-buffer) (current-buffer))))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (eq (buffer-base-buffer buf) base))
        (with-current-buffer buf
          (claudemacs-agent-permission-mode 1))))))

(defun claudemacs-agent--send-permission-response (action)
  "Send permission response with ACTION to the agent process."
  (claudemacs-agent--in-base-buffer
   (when claudemacs-agent--permission-data
     (let* ((tool-name (cdr (assq 'tool_name claudemacs-agent--permission-data)))
            (tool-input (cdr (assq 'tool_input claudemacs-agent--permission-data)))
            (scope (pcase action
                     ("allow_once" 'once)
                     ("allow_session" 'session)
                     ("allow_always" 'always)
                     (_ nil)))
            (pattern (when scope
                       (claudemacs-agent--generate-permission-pattern
                        tool-name tool-input scope)))
            (response (json-encode `((action . ,action)
                                     (pattern . ,pattern)))))
       ;; Clear permission state and disable minor mode in all related buffers
       (setq claudemacs-agent--permission-data nil)
       (setq claudemacs-agent--permission-overlay-specs nil)
       (claudemacs-agent-permission-mode -1)
       ;; For polymode: also disable in all indirect buffers sharing this base
       (let ((base (current-buffer)))
         (dolist (buf (buffer-list))
           (when (and (buffer-live-p buf)
                      (eq (buffer-base-buffer buf) base))
             (with-current-buffer buf
               (claudemacs-agent-permission-mode -1)
               ;; Clear permission overlays in indirect buffers too
               (dolist (ov (overlays-in (point-min) (point-max)))
                 (when (overlay-get ov 'claudemacs-permission-face)
                   (delete-overlay ov)))))))
       ;; Clear permission overlays in base buffer
       (dolist (ov (overlays-in (point-min) (point-max)))
         (when (overlay-get ov 'claudemacs-permission-face)
           (delete-overlay ov)))
       ;; Show thinking status (this rebuilds dynamic section)
       (claudemacs-agent--set-thinking "Processing...")
       ;; Send response to process
       (when (and claudemacs-agent--process
                  (process-live-p claudemacs-agent--process))
         (process-send-string claudemacs-agent--process
                              (format "/permit %s\n" response)))))))

(defun claudemacs-agent-permit-once ()
  "Allow the tool to run once."
  (interactive)
  (claudemacs-agent--send-permission-response "allow_once"))

(defun claudemacs-agent-permit-session ()
  "Allow the tool pattern for this session."
  (interactive)
  (claudemacs-agent--send-permission-response "allow_session"))

(defun claudemacs-agent-permit-always ()
  "Always allow this tool pattern (saves to settings)."
  (interactive)
  (claudemacs-agent--send-permission-response "allow_always"))

(defun claudemacs-agent-deny ()
  "Deny the permission request."
  (interactive)
  (claudemacs-agent--send-permission-response "deny"))

;;;; Process management

(defun claudemacs-agent--get-agent-dir ()
  "Get the directory containing the Python agent."
  (let ((this-file (or load-file-name
                       buffer-file-name
                       (locate-library "claudemacs-agent")
                       (symbol-file 'claudemacs-agent-run 'defun))))
    (when this-file
      (expand-file-name "claude_emacs_agent"
                        (file-name-directory this-file)))))

(defun claudemacs-agent--start-process (work-dir buffer)
  "Start the Python agent process for WORK-DIR with BUFFER."
  (let* ((agent-dir (claudemacs-agent--get-agent-dir))
         (log-file (expand-file-name "claude-agent.log" work-dir))
         (args (list "run" "--directory" agent-dir
                     "python" "-u" "-m" "claude_emacs_agent"  ; -u for unbuffered
                     "--work-dir" work-dir
                     "--log-file" log-file))
         (process-connection-type t)  ; Use PTY for line-buffered output
         (process-environment (cons "PYTHONUNBUFFERED=1" process-environment))
         (proc (apply #'start-process
                      "claude-agent"
                      buffer
                      claudemacs-agent-python-command
                      args)))
    (set-process-coding-system proc 'utf-8 'utf-8)
    (set-process-filter proc #'claudemacs-agent--process-filter)
    (set-process-sentinel proc #'claudemacs-agent--process-sentinel)
    proc))

(defun claudemacs-agent--process-sentinel (proc event)
  "Handle process PROC state change EVENT."
  (when (memq (process-status proc) '(exit signal))
    (let ((buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (claudemacs-agent--set-thinking nil)
          (claudemacs-agent--append-to-log
           (format "\n[Process %s]\n" (string-trim event))
           'claudemacs-agent-session-face))))))

;;;; User commands

(defun claudemacs-agent--is-busy-p ()
  "Return t if the agent is currently busy (thinking)."
  claudemacs-agent--thinking-status)

(defun claudemacs-agent--render-queue ()
  "Render queued messages by rebuilding the dynamic section."
  (claudemacs-agent--render-dynamic-section))

(defun claudemacs-agent--send-next-queued ()
  "Send the next queued message if any and not busy."
  (when (and claudemacs-agent--message-queue
             (not (claudemacs-agent--is-busy-p))
             claudemacs-agent--process
             (process-live-p claudemacs-agent--process))
    (let ((msg (pop claudemacs-agent--message-queue)))
      ;; Send to process
      (process-send-string claudemacs-agent--process
                           (concat "[INPUT]\n" msg "\n[/INPUT]\n")))))

(defun claudemacs-agent-send ()
  "Send the current input to Claude, or queue if busy."
  (interactive)
  (claudemacs-agent--in-base-buffer
   (when (and claudemacs-agent--input-start-marker
              claudemacs-agent--process
              (process-live-p claudemacs-agent--process))
     (let* ((input (string-trim (buffer-substring-no-properties
                                  claudemacs-agent--input-start-marker (point-max)))))
       ;; Ignore if empty
       (unless (string-empty-p input)
         ;; Clear the input area first
         (let ((inhibit-read-only t))
           (delete-region claudemacs-agent--input-start-marker (point-max)))
         ;; Add to history
         (push input claudemacs-agent--input-history)
         (setq claudemacs-agent--input-history-index 0)
         ;; If busy, queue the message; otherwise send directly
         (if (claudemacs-agent--is-busy-p)
             (progn
               (push input claudemacs-agent--message-queue)
               (claudemacs-agent--render-dynamic-section)
               (message "Message queued (agent is busy)"))
           ;; Send to process - wrap in [INPUT]...[/INPUT] for multi-line support
           (process-send-string claudemacs-agent--process
                                (concat "[INPUT]\n" input "\n[/INPUT]\n"))
           ;; Re-render dynamic section (input already cleared)
           (claudemacs-agent--render-dynamic-section)))))))

(defun claudemacs-agent-send-or-newline ()
  "Send input if on last line, otherwise insert newline."
  (interactive)
  (if (claudemacs-agent--in-input-area-p)
      (if (save-excursion (end-of-line) (eobp))
          (claudemacs-agent-send)
        (newline))
    (newline)))

(defun claudemacs-agent-interrupt ()
  "Interrupt the current Claude operation."
  (interactive)
  (claudemacs-agent--in-base-buffer
   (when (and claudemacs-agent--process
              (process-live-p claudemacs-agent--process))
     (process-send-string claudemacs-agent--process "/interrupt\n"))))

(defun claudemacs-agent-quit ()
  "Quit the Claude session."
  (interactive)
  (when (yes-or-no-p "Quit Claude session? ")
    (claudemacs-agent--in-base-buffer
     (when (and claudemacs-agent--process
                (process-live-p claudemacs-agent--process))
       (process-send-string claudemacs-agent--process "/quit\n")))))

(defun claudemacs-agent-previous-input ()
  "Recall previous input from history."
  (interactive)
  (claudemacs-agent--in-base-buffer
   (when (and claudemacs-agent--input-history
              (< claudemacs-agent--input-history-index
                 (length claudemacs-agent--input-history)))
     (let ((inhibit-read-only t))
       (delete-region claudemacs-agent--input-start-marker (point-max))
       (goto-char claudemacs-agent--input-start-marker)
       (insert (nth claudemacs-agent--input-history-index
                    claudemacs-agent--input-history))
       (cl-incf claudemacs-agent--input-history-index)))))

(defun claudemacs-agent-next-input ()
  "Recall next input from history."
  (interactive)
  (claudemacs-agent--in-base-buffer
   (when (> claudemacs-agent--input-history-index 0)
     (cl-decf claudemacs-agent--input-history-index)
     (let ((inhibit-read-only t))
       (delete-region claudemacs-agent--input-start-marker (point-max))
       (goto-char claudemacs-agent--input-start-marker)
       (when (> claudemacs-agent--input-history-index 0)
         (insert (nth (1- claudemacs-agent--input-history-index)
                      claudemacs-agent--input-history)))))))

;;;; Entry point

;;;###autoload
(defun claudemacs-agent-run (work-dir)
  "Start a Claude agent session for WORK-DIR."
  (interactive
   (list (read-directory-name "Project directory: "
                              (or (vc-git-root default-directory)
                                  default-directory))))
  (let* ((expanded-dir (expand-file-name work-dir))
         (short-name (file-name-nondirectory
                      (directory-file-name expanded-dir)))
         (buf-name (format "*claude:%s*" short-name))
         (buf (get-buffer-create buf-name)))

    ;; Set up buffer
    (with-current-buffer buf
      (claudemacs-agent-mode)
      (claudemacs-agent--init-buffer short-name)
      (setq claudemacs-agent--parse-state nil
            claudemacs-agent--pending-output ""
            claudemacs-agent--session-info nil
            claudemacs-agent--has-conversation nil))

    ;; Start process
    (let ((proc (claudemacs-agent--start-process expanded-dir buf)))
      (with-current-buffer buf
        (setq claudemacs-agent--process proc)))

    ;; Display buffer
    (pop-to-buffer buf)
    buf))

(provide 'claudemacs-agent)
;;; claudemacs-agent.el ends here
