;;; claudemacs-agent.el --- Claude interaction buffer -*- lexical-binding: t; -*-

;; This file is part of claudemacs.
;; Package-Requires: ((emacs "28.1") (polymode "0.2.2"))

;;; Commentary:

;; This module provides a single-buffer interface for interacting with Claude.
;; The buffer has two regions:
;; - Output region (top): Read-only, nicely formatted conversation
;; - Input region (bottom): Editable area for composing messages
;;
;; The Python agent outputs structured markers which are parsed and formatted.

;;; Code:

(require 'ansi-color)
(require 'org)
(require 'polymode)

;;;; Customization

(defgroup claudemacs-agent nil
  "Claude interaction buffer."
  :group 'claudemacs)

(defcustom claudemacs-agent-python-command "uv"
  "Command to run Python for the agent wrapper."
  :type 'string
  :group 'claudemacs-agent)

;;;; Faces

(defface claudemacs-agent-user-header-face
  '((t :foreground "#61afef" :weight bold))
  "Face for user message headers."
  :group 'claudemacs-agent)

(defface claudemacs-agent-user-face
  '((t :foreground "#abb2bf"))
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
  '((t :foreground "#98c379" :slant italic))
  "Face for status messages."
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


;;;; Buffer-local variables

(defvar-local claudemacs-agent--process nil
  "The agent process for this session.")

(defvar-local claudemacs-agent--parse-state nil
  "Current parsing state: nil, user, assistant, tool, error, session.")

(defvar-local claudemacs-agent--pending-output ""
  "Buffer for incomplete lines from process output.")

(defvar-local claudemacs-agent--input-marker nil
  "Marker for the start of the input area.")

(defvar-local claudemacs-agent--prompt-marker nil
  "Marker for after the prompt (where user types).")

(defvar-local claudemacs-agent--assistant-start nil
  "Position where current assistant message started.")

(defvar-local claudemacs-agent--status-overlay nil
  "Overlay for the status line.")

;;;; Mode definition

(defvar claudemacs-agent-base-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claudemacs-agent-send)
    (define-key map (kbd "C-<return>") #'claudemacs-agent-send)
    (define-key map (kbd "C-c C-k") #'claudemacs-agent-interrupt)
    (define-key map (kbd "C-c C-q") #'claudemacs-agent-quit)
    (define-key map (kbd "M-p") #'claudemacs-agent-previous-input)
    (define-key map (kbd "M-n") #'claudemacs-agent-next-input)
    map)
  "Keymap for `claudemacs-agent-base-mode'.")

(define-derived-mode claudemacs-agent-base-mode fundamental-mode "Claude-Base"
  "Base major mode for Claude interaction buffer."
  :group 'claudemacs-agent
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local buffer-read-only nil)
  (visual-line-mode 1)
  ;; Input history
  (setq-local claudemacs-agent--input-history nil)
  (setq-local claudemacs-agent--input-history-index 0))

;; Polymode setup: org-mode for Claude's output regions
(define-hostmode claudemacs-agent-hostmode
  :mode 'claudemacs-agent-base-mode)

(define-innermode claudemacs-agent-org-innermode
  :mode 'org-mode
  :head-matcher "^━━━ Claude ━+\n"
  :tail-matcher "^\n─── Input ─+"
  :head-mode 'host
  :tail-mode 'host)

(define-polymode claudemacs-agent-mode
  :hostmode 'claudemacs-agent-hostmode
  :innermodes '(claudemacs-agent-org-innermode))

;; Ensure our keybindings work in polymode
(with-eval-after-load 'polymode
  (define-key polymode-mode-map (kbd "C-c C-c") nil))

(defvar-local claudemacs-agent--input-history nil
  "History of inputs sent to Claude.")

(defvar-local claudemacs-agent--input-history-index 0
  "Current position in input history.")

;;;; Helper functions

(defun claudemacs-agent--in-input-area-p ()
  "Return t if point is in the input area."
  (and claudemacs-agent--prompt-marker
       (>= (point) claudemacs-agent--prompt-marker)))

(defconst claudemacs-agent--spinner-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames for the spinner animation.")

(defvar-local claudemacs-agent--spinner-index 0
  "Current index in spinner frames.")

(defvar-local claudemacs-agent--spinner-timer nil
  "Timer for spinner animation.")

(defvar-local claudemacs-agent--current-status nil
  "Current status text (without spinner).")

(defvar-local claudemacs-agent--token-count 0
  "Running count of output tokens.")

(defun claudemacs-agent--spinner-tick ()
  "Advance spinner and update display."
  (when claudemacs-agent--current-status
    (setq claudemacs-agent--spinner-index
          (mod (1+ claudemacs-agent--spinner-index)
               (length claudemacs-agent--spinner-frames)))
    (claudemacs-agent--update-status-display)))

(defun claudemacs-agent--update-status-display ()
  "Update the status overlay with current spinner frame and status."
  (when claudemacs-agent--status-overlay
    (let* ((spinner (nth claudemacs-agent--spinner-index
                         claudemacs-agent--spinner-frames))
           (tokens (if (> claudemacs-agent--token-count 0)
                       (format " (%d tokens)" claudemacs-agent--token-count)
                     ""))
           (text (format "%s %s%s\n" spinner claudemacs-agent--current-status tokens)))
      (overlay-put claudemacs-agent--status-overlay 'before-string
                   (propertize text 'face 'claudemacs-agent-status-face)))))

(defun claudemacs-agent--set-status (status)
  "Set the status line to STATUS, or clear if nil."
  ;; Cancel existing timer
  (when claudemacs-agent--spinner-timer
    (cancel-timer claudemacs-agent--spinner-timer)
    (setq claudemacs-agent--spinner-timer nil))
  ;; Remove existing overlay
  (when claudemacs-agent--status-overlay
    (delete-overlay claudemacs-agent--status-overlay)
    (setq claudemacs-agent--status-overlay nil))
  ;; Set new status
  (setq claudemacs-agent--current-status status)
  (when status
    (let ((inhibit-read-only t))
      (save-excursion
        ;; Status goes right before the input separator
        (when claudemacs-agent--input-marker
          (goto-char claudemacs-agent--input-marker)
          (let ((ov (make-overlay (point) (point))))
            (overlay-put ov 'claudemacs-agent-status t)
            (setq claudemacs-agent--status-overlay ov)
            (claudemacs-agent--update-status-display)
            ;; Start spinner timer
            (setq claudemacs-agent--spinner-timer
                  (run-with-timer 0.1 0.1 #'claudemacs-agent--spinner-tick))))))))

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
  ;; This handles cases where the process output is buffered differently
  (when (and (not (string-empty-p claudemacs-agent--pending-output))
             (string-match "^\\[/?[A-Z]+\\]$" claudemacs-agent--pending-output))
    (claudemacs-agent--process-line claudemacs-agent--pending-output)
    (setq claudemacs-agent--pending-output "")))

(defun claudemacs-agent--process-line (line)
  "Process a single LINE of output, handling markers."
  (cond
   ;; Ready marker - recreate input area and clear status
   ((string= line "[READY]")
    (claudemacs-agent--set-status nil)
    (claudemacs-agent--setup-input-area))

   ;; Thinking marker - show status indicator and reset token count
   ((string= line "[THINKING]")
    (setq claudemacs-agent--token-count 0)
    (claudemacs-agent--set-status "Thinking..."))

   ;; Progress marker - update token count
   ((string-match "^\\[PROGRESS \\(.*\\)\\]$" line)
    (let* ((json-str (match-string 1 line))
           (data (ignore-errors (json-read-from-string json-str))))
      (when data
        (let ((output-tokens (cdr (assq 'output_tokens data))))
          (when output-tokens
            (setq claudemacs-agent--token-count output-tokens)
            (claudemacs-agent--update-status-display))))))

   ;; Result marker - show final stats briefly
   ((string-match "^\\[RESULT \\(.*\\)\\]$" line)
    (let* ((json-str (match-string 1 line))
           (data (ignore-errors (json-read-from-string json-str))))
      (when data
        (let ((cost (cdr (assq 'cost_usd data)))
              (duration (cdr (assq 'duration_ms data)))
              (turns (cdr (assq 'num_turns data))))
          (message "Claude: $%.4f | %dms | %d turns"
                   (or cost 0) (or duration 0) (or turns 0))))))

   ;; User message start
   ((string= line "[USER]")
    (setq claudemacs-agent--parse-state 'user)
    (claudemacs-agent--append-output
     "\n━━━ You ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
     'claudemacs-agent-user-header-face))

   ;; User message end
   ((string= line "[/USER]")
    (setq claudemacs-agent--parse-state nil))

   ;; Assistant message start
   ((string= line "[ASSISTANT]")
    (setq claudemacs-agent--parse-state 'assistant)
    (claudemacs-agent--append-output
     "\n━━━ Claude ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
     'claudemacs-agent-assistant-header-face)
    ;; Mark where assistant content starts (after the header)
    (setq claudemacs-agent--assistant-start
          (if claudemacs-agent--input-marker
              (marker-position claudemacs-agent--input-marker)
            (point-max))))

   ;; Assistant message end
   ((string= line "[/ASSISTANT]")
    (setq claudemacs-agent--parse-state nil))

   ;; Tool start
   ((string-match "^\\[TOOL \\(.+\\)\\]$" line)
    (let ((tool-name (match-string 1 line)))
      (setq claudemacs-agent--parse-state 'tool)
      (claudemacs-agent--set-status (format "Running: %s" tool-name))
      (claudemacs-agent--append-output
       (format "\n⚙ %s\n" tool-name)
       'claudemacs-agent-tool-face)))

   ;; Tool end
   ((string= line "[/TOOL]")
    (setq claudemacs-agent--parse-state nil)
    (claudemacs-agent--set-status "Thinking..."))

   ;; Session info start
   ((string= line "[SESSION]")
    (setq claudemacs-agent--parse-state 'session))

   ;; Session info end
   ((string= line "[/SESSION]")
    (setq claudemacs-agent--parse-state nil))

   ;; Error start
   ((string= line "[ERROR]")
    (setq claudemacs-agent--parse-state 'error)
    (claudemacs-agent--append-output
     "\n⚠ Error: "
     'claudemacs-agent-error-face))

   ;; Error end
   ((string= line "[/ERROR]")
    (setq claudemacs-agent--parse-state nil)
    (claudemacs-agent--append-output "\n" nil))

   ;; Regular content line
   (t
    (let ((face (pcase claudemacs-agent--parse-state
                  ('user 'claudemacs-agent-user-face)
                  ('assistant nil)  ; Let org fontification handle Claude's output
                  ('tool 'claudemacs-agent-tool-face)
                  ('error 'claudemacs-agent-error-face)
                  ('session 'claudemacs-agent-session-face)
                  (_ nil))))
      (claudemacs-agent--append-output (concat line "\n") face)))))

(defun claudemacs-agent--append-output (text &optional face)
  "Append TEXT to the output area (before input) with optional FACE."
  (let ((inhibit-read-only t))
    (save-excursion
      ;; Insert before the input area
      (if claudemacs-agent--input-marker
          (goto-char claudemacs-agent--input-marker)
        (goto-char (point-max)))
      (let ((start (point))
            (text-to-insert (if face (propertize text 'face face) text)))
        (insert text-to-insert)
        (let ((end (point)))
          ;; Update markers to stay at correct positions
          (when claudemacs-agent--input-marker
            (set-marker claudemacs-agent--input-marker end))
          (when claudemacs-agent--prompt-marker
            (set-marker claudemacs-agent--prompt-marker
                        (+ claudemacs-agent--prompt-marker (- end start))))
          ;; Update read-only overlay to cover new text
          (dolist (ov (overlays-in (point-min) end))
            (when (overlay-get ov 'claudemacs-agent-output)
              (move-overlay ov (point-min) claudemacs-agent--prompt-marker))))))))


(defun claudemacs-agent--setup-input-area ()
  "Set up or refresh the input area at the bottom of the buffer."
  (let ((inhibit-read-only t)
        (saved-input ""))
    ;; Save any existing input
    (when (and claudemacs-agent--prompt-marker
               (marker-position claudemacs-agent--prompt-marker))
      (setq saved-input (buffer-substring-no-properties
                         claudemacs-agent--prompt-marker (point-max))))
    ;; Remove old input area if it exists
    (when (and claudemacs-agent--input-marker
               (marker-position claudemacs-agent--input-marker))
      (delete-region claudemacs-agent--input-marker (point-max)))

    ;; Create input area
    (goto-char (point-max))

    ;; Set input marker at start of input area
    (setq claudemacs-agent--input-marker (point-marker))
    (set-marker-insertion-type claudemacs-agent--input-marker nil)

    ;; Insert separator
    (let ((sep-start (point)))
      (insert "\n─── Input ")
      (insert (make-string 47 ?─))
      (insert "\n")
      (put-text-property sep-start (point) 'face 'claudemacs-agent-input-header-face))

    ;; Set prompt marker where user types
    (setq claudemacs-agent--prompt-marker (point-marker))
    (set-marker-insertion-type claudemacs-agent--prompt-marker nil)

    ;; Restore any saved input
    (insert saved-input)

    ;; Delete any existing output overlay
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'claudemacs-agent-output)
        (delete-overlay ov)))

    ;; Make everything BEFORE the prompt marker read-only using an overlay
    ;; The overlay ends right before where the user types
    (when (> (marker-position claudemacs-agent--prompt-marker) (point-min))
      (let ((ov (make-overlay (point-min) claudemacs-agent--prompt-marker nil t nil)))
        ;; front-advance=t means overlay won't extend when inserting at start
        ;; rear-advance=nil (default) means overlay won't extend when inserting at end
        (overlay-put ov 'read-only t)
        (overlay-put ov 'claudemacs-agent-output t)))

    ;; Move point to input area
    (goto-char claudemacs-agent--prompt-marker)

    ;; Scroll to show input area
    (let ((win (get-buffer-window (current-buffer))))
      (when win
        (with-selected-window win
          (goto-char (point-max))
          (recenter -2))))))

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
                     "python" "-m" "claude_emacs_agent"
                     "--work-dir" work-dir
                     "--log-file" log-file))
         (process-connection-type nil)  ; Use pipes for better buffering
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
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (propertize (format "\n[Process %s]\n" (string-trim event))
                                'face 'claudemacs-agent-session-face))))))))

;;;; User commands

(defun claudemacs-agent-send ()
  "Send the current input to Claude."
  (interactive)
  (when (and claudemacs-agent--prompt-marker
             claudemacs-agent--process
             (process-live-p claudemacs-agent--process))
    (let ((input (string-trim
                  (buffer-substring-no-properties
                   claudemacs-agent--prompt-marker (point-max)))))
      (unless (string-empty-p input)
        ;; Add to history
        (push input claudemacs-agent--input-history)
        (setq claudemacs-agent--input-history-index 0)
        ;; Clear input area
        (let ((inhibit-read-only t))
          (delete-region claudemacs-agent--prompt-marker (point-max)))
        ;; Send to process
        (process-send-string claudemacs-agent--process (concat input "\n"))))))

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
  (when (and claudemacs-agent--process
             (process-live-p claudemacs-agent--process))
    (process-send-string claudemacs-agent--process "/interrupt\n")))

(defun claudemacs-agent-quit ()
  "Quit the Claude session."
  (interactive)
  (when (yes-or-no-p "Quit Claude session? ")
    (when (and claudemacs-agent--process
               (process-live-p claudemacs-agent--process))
      (process-send-string claudemacs-agent--process "/quit\n"))))

(defun claudemacs-agent-previous-input ()
  "Recall previous input from history."
  (interactive)
  (when (and claudemacs-agent--input-history
             (< claudemacs-agent--input-history-index
                (length claudemacs-agent--input-history)))
    (let ((inhibit-read-only t))
      (delete-region claudemacs-agent--prompt-marker (point-max))
      (goto-char claudemacs-agent--prompt-marker)
      (insert (nth claudemacs-agent--input-history-index
                   claudemacs-agent--input-history))
      (cl-incf claudemacs-agent--input-history-index))))

(defun claudemacs-agent-next-input ()
  "Recall next input from history."
  (interactive)
  (when (> claudemacs-agent--input-history-index 0)
    (cl-decf claudemacs-agent--input-history-index)
    (let ((inhibit-read-only t))
      (delete-region claudemacs-agent--prompt-marker (point-max))
      (goto-char claudemacs-agent--prompt-marker)
      (when (> claudemacs-agent--input-history-index 0)
        (insert (nth (1- claudemacs-agent--input-history-index)
                     claudemacs-agent--input-history))))))

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
      (let ((inhibit-read-only t))
        (erase-buffer))
      (claudemacs-agent-mode)
      (setq claudemacs-agent--parse-state nil
            claudemacs-agent--pending-output ""
            claudemacs-agent--input-marker nil
            claudemacs-agent--prompt-marker nil))

    ;; Start process
    (let ((proc (claudemacs-agent--start-process expanded-dir buf)))
      (with-current-buffer buf
        (setq claudemacs-agent--process proc)))

    ;; Display buffer
    (pop-to-buffer buf)
    buf))

(provide 'claudemacs-agent)
;;; claudemacs-agent.el ends here
