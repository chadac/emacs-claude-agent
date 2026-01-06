;;; claude-sessions.el --- Session monitoring for Claude -*- lexical-binding: t; -*-

;; This file is part of Claude.

;;; Commentary:

;; This module provides a session monitoring buffer for Claude, allowing:
;; - View all running Claude sessions in a table
;; - See session status (alive, thinking, waiting for input)
;; - Open/switch to sessions with RET
;; - Kill or restart sessions from the list
;; - Automatic refresh every 2 seconds when buffer is active

;;; Code:

(require 'tabulated-list)
(require 'cl-lib)

;;;; Customization

(defgroup claude-sessions nil
  "Session monitoring for Claude."
  :group 'claude-agent)

(defcustom claude-sessions-refresh-interval 2.0
  "Interval in seconds between automatic refreshes of the sessions buffer."
  :type 'number
  :group 'claude-agent-sessions)

(defcustom claude-sessions-buffer-name "*claude-sessions*"
  "Name of the sessions monitoring buffer."
  :type 'string
  :group 'claude-agent-sessions)

;;;; Faces

(defface claude-sessions-status-ready
  '((t :foreground "#00ff00" :weight bold))
  "Face for sessions that are ready for input."
  :group 'claude-agent-sessions)

(defface claude-sessions-status-thinking
  '((t :foreground "#ffaa00" :weight bold))
  "Face for sessions that are thinking/processing."
  :group 'claude-agent-sessions)

(defface claude-sessions-status-typing
  '((t :foreground "#66ccff" :weight bold))
  "Face for sessions where user is typing."
  :group 'claude-agent-sessions)

(defface claude-sessions-status-waiting
  '((t :foreground "#ff66ff" :weight bold))
  "Face for sessions waiting for user input."
  :group 'claude-agent-sessions)

(defface claude-sessions-status-dead
  '((t :foreground "#ff0000" :weight bold))
  "Face for sessions that are dead/not running."
  :group 'claude-agent-sessions)

(defface claude-sessions-project
  '((t :foreground "#88aaff"))
  "Face for project directory column."
  :group 'claude-agent-sessions)

(defface claude-sessions-buffer
  '((t :foreground "#aaaaff"))
  "Face for buffer name column."
  :group 'claude-agent-sessions)

(defface claude-sessions-label
  '((t :foreground "#aaffaa"))
  "Face for session label column."
  :group 'claude-agent-sessions)

;;;; Internal Variables

(defvar claude-sessions--refresh-timer nil
  "Timer for automatic refresh of the sessions buffer.")

;;;; Status Detection Functions

(defun claude-sessions--get-session-status (buffer)
  "Get the status of a Claude session BUFFER.
Returns one of: `ready', `thinking', `waiting', or `dead'.
Supports both the new agent architecture (claude-agent--process)
and the old eat-based architecture."
  (unless (buffer-live-p buffer)
    (cl-return-from claude-sessions--get-session-status 'dead))

  (with-current-buffer buffer
    ;; Check for new agent architecture first
    (cond
     ;; New agent architecture: claude-agent--process
     ((and (boundp 'claude-agent--process) claude-agent--process)
      (if (not (process-live-p claude-agent--process))
          'dead
        ;; Process is running, check status
        (cond
         ;; Waiting for permission: input-mode is 'text-with-permission
         ((and (boundp 'claude-agent--input-mode)
               (eq claude-agent--input-mode 'text-with-permission))
          'waiting)
         ;; Thinking: claude-agent--thinking-status is set
         ((and (boundp 'claude-agent--thinking-status)
               claude-agent--thinking-status)
          'thinking)
         ;; Compacting: conversation being summarized
         ((and (boundp 'claude-agent--compacting)
               claude-agent--compacting)
          'thinking)
         ;; Ready: Not thinking, process alive
         (t 'ready))))

     ;; Old eat-based architecture
     ((and (boundp 'eat-terminal) eat-terminal)
      (let* ((process (eat-term-parameter eat-terminal 'eat--process)))
        (if (not (and process (memq (process-status process) '(run open listen connect))))
            'dead
          ;; Process is running, check status
          ;; Look at the last ~1000 chars which should contain the prompt area
          (let* ((tail-start (max (point-min) (- (point-max) 1000)))
                 (tail-content (buffer-substring-no-properties tail-start (point-max))))
            (cond
             ;; Thinking: Claude is actively processing
             ((string-match-p "esc to interrupt" tail-content)
              'thinking)
             ;; Typing: Has "> " prompt with text after it (user is composing)
             ((string-match-p "─\n>..+.\n─" tail-content)
              'typing)
             ;; Ready: Has empty "> " prompt between horizontal lines
             ((string-match-p "─\n>..\n─" tail-content)
              'ready)
             ;; Otherwise waiting for user input (edit approval, multi-select, etc.)
             (t 'waiting))))))

     ;; No recognized architecture - dead
     (t 'dead))))

(defun claude-sessions--format-status (status)
  "Format STATUS symbol into a display string with appropriate face."
  (pcase status
    ('ready (propertize "Ready" 'face 'claude-sessions-status-ready))
    ('thinking (propertize "Thinking" 'face 'claude-sessions-status-thinking))
    ('typing (propertize "Typing" 'face 'claude-sessions-status-typing))
    ('waiting (propertize "Waiting" 'face 'claude-sessions-status-waiting))
    ('dead (propertize "Dead" 'face 'claude-sessions-status-dead))
    (_ (propertize "Unknown" 'face 'font-lock-comment-face))))

(defun claude-sessions--parse-buffer-name (buffer-name)
  "Parse Claude buffer name into components.
Returns plist with :directory, :label, and :project."
  (when (string-match "^\\*claude:\\([^:*]+\\)\\(?::\\([^*]+\\)\\)?\\*$" buffer-name)
    (let* ((directory (match-string 1 buffer-name))
           (label (match-string 2 buffer-name))
           (project (file-name-nondirectory (directory-file-name directory))))
      (list :directory directory
            :label (or label "main")
            :project project))))

(defun claude-sessions--get-all-sessions ()
  "Get list of all Claude session buffers with their info.
Returns a list of plists with session information."
  (let (sessions)
    (dolist (buffer (buffer-list))
      (let ((name (buffer-name buffer)))
        (when (string-match-p "^\\*claude:" name)
          (let* ((parsed (claude-sessions--parse-buffer-name name))
                 (status (claude-sessions--get-session-status buffer)))
            (when parsed
              (push (append parsed
                           (list :buffer-name name
                                 :buffer buffer
                                 :status status))
                    sessions))))))
    (nreverse sessions)))

;;;; Tabulated List Mode Implementation

(defun claude-sessions--get-entries ()
  "Get tabulated list entries for all Claude sessions."
  (mapcar
   (lambda (session)
     (let* ((buffer-name (plist-get session :buffer-name))
            (project (plist-get session :project))
            (directory (plist-get session :directory))
            (label (plist-get session :label))
            (status (plist-get session :status)))
       (list buffer-name
             (vector
              (propertize project 'face 'claude-sessions-project)
              (propertize buffer-name 'face 'claude-sessions-buffer)
              (propertize label 'face 'claude-sessions-label)
              (claude-sessions--format-status status)
              directory))))
   (claude-sessions--get-all-sessions)))

(defun claude-sessions--get-marked-ids ()
  "Get list of buffer IDs that are marked with D."
  (let (marked)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (eq (char-after) ?D)
          (push (tabulated-list-get-id) marked))
        (forward-line 1)))
    marked))

(defun claude-sessions--restore-marks (marked-ids)
  "Restore D marks on entries with IDs in MARKED-IDS."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (member (tabulated-list-get-id) marked-ids)
        (tabulated-list-put-tag "D"))
      (forward-line 1))))

(defun claude-sessions-refresh ()
  "Refresh the sessions buffer content, preserving marks."
  (interactive)
  (when-let ((buffer (get-buffer claude-sessions-buffer-name)))
    (with-current-buffer buffer
      (let ((pos (point))
            (marked (claude-sessions--get-marked-ids)))
        (tabulated-list-revert)
        (claude-sessions--restore-marks marked)
        (goto-char (min pos (point-max)))))))

(defun claude-sessions--start-auto-refresh ()
  "Start the auto-refresh timer for the sessions buffer."
  (claude-sessions--stop-auto-refresh)
  (setq claude-sessions--refresh-timer
        (run-with-timer claude-sessions-refresh-interval
                        claude-sessions-refresh-interval
                        #'claude-sessions--auto-refresh)))

(defun claude-sessions--stop-auto-refresh ()
  "Stop the auto-refresh timer."
  (when claude-sessions--refresh-timer
    (cancel-timer claude-sessions--refresh-timer)
    (setq claude-sessions--refresh-timer nil)))

(defun claude-sessions--auto-refresh ()
  "Auto-refresh callback that only refreshes if buffer is visible."
  (let ((buffer (get-buffer claude-sessions-buffer-name)))
    (if (and buffer (get-buffer-window buffer 'visible))
        (claude-sessions-refresh)
      ;; Buffer not visible, stop the timer
      (claude-sessions--stop-auto-refresh))))

;;;; Interactive Commands

(defun claude-sessions-open-session ()
  "Open the session at point."
  (interactive)
  (when-let ((buffer-name (tabulated-list-get-id)))
    (if-let ((buffer (get-buffer buffer-name)))
        (progn
          (display-buffer buffer)
          (select-window (get-buffer-window buffer)))
      (message "Buffer %s no longer exists" buffer-name))))

(defun claude-sessions--kill-session-process (buffer)
  "Kill the process associated with session BUFFER.
Handles both new agent and old eat-based architectures."
  (with-current-buffer buffer
    (cond
     ;; New agent architecture
     ((and (boundp 'claude-agent--process) claude-agent--process)
      (when (process-live-p claude-agent--process)
        (delete-process claude-agent--process)))
     ;; Old eat-based architecture
     ((and (boundp 'eat-terminal) eat-terminal)
      (let ((process (eat-term-parameter eat-terminal 'eat--process)))
        (when (and process (process-live-p process))
          (kill-process process)))))))

(defun claude-sessions-kill-session ()
  "Kill the session at point."
  (interactive)
  (when-let ((buffer-name (tabulated-list-get-id)))
    (if-let ((buffer (get-buffer buffer-name)))
        (when (yes-or-no-p (format "Kill session %s? " buffer-name))
          (claude-sessions--kill-session-process buffer)
          (kill-buffer buffer)
          (claude-sessions-refresh)
          (message "Killed session %s" buffer-name))
      (message "Buffer %s no longer exists" buffer-name))))

(defun claude-sessions-restart-session ()
  "Restart the session at point."
  (interactive)
  (when-let ((buffer-name (tabulated-list-get-id)))
    (if-let ((buffer (get-buffer buffer-name)))
        (when (yes-or-no-p (format "Restart session %s? " buffer-name))
          (require 'claude)
          ;; Get work-dir before killing buffer - check both architectures
          (let ((work-dir (with-current-buffer buffer
                           (or (and (boundp 'claude-agent--work-dir) claude-agent--work-dir)
                               (and (boundp 'claude--cwd) claude--cwd)
                               (when (string-match "^\\*claude:\\([^:*]+\\)" buffer-name)
                                 (match-string 1 buffer-name))))))
            ;; Use claude-restart to handle the restart properly
            (claude-restart work-dir buffer-name)
            (message "Restarting session %s..." buffer-name)))
      (message "Buffer %s no longer exists" buffer-name))))

(defun claude-sessions-mark-for-kill ()
  "Mark the session at point for killing."
  (interactive)
  (tabulated-list-put-tag "D" t))

(defun claude-sessions-unmark ()
  "Remove mark from the session at point."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun claude-sessions-execute-marks ()
  "Execute the marked operations."
  (interactive)
  (let ((kill-list '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (eq (char-after) ?D)
          (push (tabulated-list-get-id) kill-list))
        (forward-line 1)))
    (when kill-list
      (when (yes-or-no-p (format "Kill %d marked session(s)? " (length kill-list)))
        (dolist (buffer-name kill-list)
          (when-let ((buffer (get-buffer buffer-name)))
            (claude-sessions--kill-session-process buffer)
            (kill-buffer buffer)))
        (claude-sessions-refresh)
        (message "Killed %d session(s)" (length kill-list))))))

(defun claude-sessions-display-buffer ()
  "Display the session at point in another window without selecting it."
  (interactive)
  (when-let ((buffer-name (tabulated-list-get-id)))
    (if-let ((buffer (get-buffer buffer-name)))
        (display-buffer buffer)
      (message "Buffer %s no longer exists" buffer-name))))

;;;; Mode Definition

(defvar claude-sessions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-sessions-open-session)
    (define-key map (kbd "o") #'claude-sessions-display-buffer)
    (define-key map (kbd "R") #'claude-sessions-restart-session)
    (define-key map (kbd "g") #'claude-sessions-refresh)
    (define-key map (kbd "d") #'claude-sessions-mark-for-kill)
    (define-key map (kbd "u") #'claude-sessions-unmark)
    (define-key map (kbd "x") #'claude-sessions-execute-marks)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `claude-sessions-mode'.")

;; Evil mode support
(with-eval-after-load 'evil
  (evil-define-key 'normal claude-sessions-mode-map
    (kbd "RET") #'claude-sessions-open-session
    (kbd "o") #'claude-sessions-display-buffer
    (kbd "R") #'claude-sessions-restart-session
    (kbd "gr") #'claude-sessions-refresh
    (kbd "d") #'claude-sessions-mark-for-kill
    (kbd "u") #'claude-sessions-unmark
    (kbd "x") #'claude-sessions-execute-marks
    (kbd "q") #'quit-window)
  (evil-define-key 'motion claude-sessions-mode-map
    (kbd "RET") #'claude-sessions-open-session
    (kbd "o") #'claude-sessions-display-buffer
    (kbd "q") #'quit-window))

(define-derived-mode claude-sessions-mode tabulated-list-mode "Claudemacs-Sessions"
  "Major mode for viewing and managing Claude sessions.

\\{claude-sessions-mode-map}"
  (setq tabulated-list-format
        [("Project" 20 t)
         ("Buffer" 50 t)
         ("Label" 15 t)
         ("Status" 10 t)
         ("Directory" 40 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Project" . nil))
  (setq tabulated-list-entries #'claude-sessions--get-entries)
  (tabulated-list-init-header)
  ;; Start auto-refresh when entering mode
  (add-hook 'kill-buffer-hook #'claude-sessions--stop-auto-refresh nil t)
  ;; Start/stop timer based on window visibility
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (if (get-buffer-window (current-buffer) 'visible)
                  (claude-sessions--start-auto-refresh)
                (claude-sessions--stop-auto-refresh)))
            nil t))

;;;; Entry Point

;;;###autoload
(defun claude-list-sessions ()
  "Display a buffer listing all Claude sessions.
The buffer updates automatically every 2 seconds while visible."
  (interactive)
  (let ((buffer (get-buffer-create claude-sessions-buffer-name)))
    (with-current-buffer buffer
      (claude-sessions-mode)
      (tabulated-list-print))
    (display-buffer buffer)
    (select-window (get-buffer-window buffer))
    ;; Start auto-refresh
    (claude-sessions--start-auto-refresh)))

(provide 'claude-sessions)
;;; claude-sessions.el ends here
