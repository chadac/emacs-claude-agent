;;; claude-pair.el --- Pair programming features for Claude -*- lexical-binding: t; -*-

;; This file is part of Claude Agent.

;;; Commentary:

;; Pair programming features that allow the user to communicate with
;; Claude through inline comments and quick actions.
;;
;; Features:
;; - Claude Comments: Add CLAUDE: prefix to comments, send with C-x c c
;; - (Future) Background Watcher: Agent observes file changes
;; - (Future) Point Actions: Quick actions at current point

;;; Code:

(require 'project)

(defgroup claude-pair nil
  "Pair programming features for Claude."
  :group 'claude-agent
  :prefix "claude-pair-")

(defcustom claude-pair-comment-prefix "CLAUDE:"
  "Prefix that identifies comments meant for Claude."
  :type 'string
  :group 'claude-pair)

(defcustom claude-pair-context-lines-before 2
  "Number of lines of context to include before each comment."
  :type 'integer
  :group 'claude-pair)

(defcustom claude-pair-context-lines-after 5
  "Number of lines of context to include after each comment."
  :type 'integer
  :group 'claude-pair)

;;;; Claude Comments

(defun claude-pair--comment-regex ()
  "Build regex to match CLAUDE: comments in current buffer.
Handles various comment syntaxes."
  (let* ((prefix (regexp-quote claude-pair-comment-prefix))
         ;; Match common comment patterns
         (patterns
          (list
           ;; // CLAUDE: (C, JS, etc.)
           (concat "//\\s-*" prefix "\\s-*\\(.*\\)$")
           ;; # CLAUDE: (Python, Shell, etc.)
           (concat "#\\s-*" prefix "\\s-*\\(.*\\)$")
           ;; ; CLAUDE: (Lisp, elisp)
           (concat ";+\\s-*" prefix "\\s-*\\(.*\\)$")
           ;; -- CLAUDE: (SQL, Haskell)
           (concat "--\\s-*" prefix "\\s-*\\(.*\\)$")
           ;; /* CLAUDE: */ (C block comments)
           (concat "/\\*\\s-*" prefix "\\s-*\\(.*?\\)\\s-*\\*/")
           ;; <!-- CLAUDE: --> (HTML/XML)
           (concat "<!--\\s-*" prefix "\\s-*\\(.*?\\)\\s-*-->")
           ;; % CLAUDE: (LaTeX)
           (concat "%\\s-*" prefix "\\s-*\\(.*\\)$"))))
    (mapconcat #'identity patterns "\\|")))

(defun claude-pair--extract-comment-text (line)
  "Extract the comment text after CLAUDE: prefix from LINE."
  (when (string-match (concat (regexp-quote claude-pair-comment-prefix)
                              "\\s-*\\(.*?\\)\\s-*\\(?:\\*/\\|-->\\)?\\s-*$")
                      line)
    (string-trim (match-string 1 line))))

(defun claude-pair--get-context (buffer line-num)
  "Get context around LINE-NUM in BUFFER.
Returns a plist with :before, :line, and :after."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line-num))
      (let* ((start-line (max 1 (- line-num claude-pair-context-lines-before)))
             (end-line (+ line-num claude-pair-context-lines-after))
             (current-line (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))
             before-lines after-lines)
        ;; Get lines before
        (goto-char (point-min))
        (forward-line (1- start-line))
        (dotimes (_ (- line-num start-line))
          (push (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))
                before-lines)
          (forward-line 1))
        (setq before-lines (nreverse before-lines))
        ;; Get lines after
        (forward-line 1)  ; Skip current line
        (let ((remaining (- end-line line-num)))
          (dotimes (_ remaining)
            (unless (eobp)
              (push (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))
                    after-lines)
              (forward-line 1))))
        (setq after-lines (nreverse after-lines))
        (list :before before-lines
              :line current-line
              :after after-lines)))))

(defun claude-pair--find-comments-in-buffer (&optional buffer)
  "Find all CLAUDE: comments in BUFFER (defaults to current buffer).
Returns a list of plists with :file, :line, :comment, :context."
  (let ((buffer (or buffer (current-buffer)))
        (results '()))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((regex (claude-pair--comment-regex)))
          (while (re-search-forward regex nil t)
            (let* ((line-num (line-number-at-pos))
                   (line-text (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))
                   (comment-text (claude-pair--extract-comment-text line-text))
                   (context (claude-pair--get-context buffer line-num)))
              (when comment-text
                (push (list :file (buffer-file-name buffer)
                            :line line-num
                            :comment comment-text
                            :context context)
                      results)))))))
    (nreverse results)))

(defun claude-pair--find-comments-in-project ()
  "Find all CLAUDE: comments in the current project.
Returns a list of plists with :file, :line, :comment, :context."
  (let* ((project (project-current t))
         (root (project-root project))
         (files (project-files project))
         (results '()))
    (dolist (file files)
      ;; Only check text files we can reasonably parse
      (when (and (file-readable-p file)
                 (not (string-match-p "\\.\\(png\\|jpg\\|gif\\|ico\\|woff\\|ttf\\|eot\\|pdf\\|zip\\|tar\\|gz\\)$" file)))
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents file)
              (let ((comments (claude-pair--find-comments-in-buffer (current-buffer))))
                ;; Fix file paths since temp buffer has no file-name
                (dolist (c comments)
                  (plist-put c :file file))
                (setq results (append results comments))))
          (error nil))))  ; Skip files we can't read
    results))

(defun claude-pair--format-comments-for-agent (comments)
  "Format COMMENTS list into a message for the Claude agent."
  (if (null comments)
      "No CLAUDE: comments found."
    (let ((parts '()))
      (push (format "Found %d CLAUDE: comment(s) to address:\n" (length comments)) parts)
      (dolist (c comments)
        (let* ((file (plist-get c :file))
               (line (plist-get c :line))
               (comment (plist-get c :comment))
               (context (plist-get c :context))
               (before (plist-get context :before))
               (current (plist-get context :line))
               (after (plist-get context :after)))
          (push (format "\n## %s:%d\n" file line) parts)
          (push (format "**Request:** %s\n" comment) parts)
          (push "\n```\n" parts)
          ;; Add line numbers to context
          (let ((ctx-start (- line (length before))))
            (dolist (l before)
              (push (format "%4d: %s\n" ctx-start l) parts)
              (setq ctx-start (1+ ctx-start)))
            (push (format "%4d: %s  <-- CLAUDE comment here\n" line current) parts)
            (let ((after-line (1+ line)))
              (dolist (l after)
                (push (format "%4d: %s\n" after-line l) parts)
                (setq after-line (1+ after-line)))))
          (push "```\n" parts)))
      (push "\nPlease address each comment and remove the CLAUDE: comments when done." parts)
      (apply #'concat (nreverse parts)))))

(defun claude-pair-send-comments (&optional project-wide)
  "Send CLAUDE: comments to the agent.
With prefix arg PROJECT-WIDE, scan entire project instead of current buffer."
  (interactive "P")
  (let* ((comments (if project-wide
                       (progn
                         (message "Scanning project for CLAUDE: comments...")
                         (claude-pair--find-comments-in-project))
                     (claude-pair--find-comments-in-buffer)))
         (message-text (claude-pair--format-comments-for-agent comments)))
    (if (null comments)
        (message "No CLAUDE: comments found.")
      ;; Send to Claude agent
      (claude-pair--send-to-agent message-text)
      (message "Sent %d CLAUDE: comment(s) to agent." (length comments)))))

(defun claude-pair--get-claude-buffers ()
  "Get list of all active Claude agent buffers.
Returns list of (buffer-name . buffer) pairs."
  (let ((buffers nil))
    (dolist (buf (buffer-list))
      (when (and (string-match-p "^\\*claude" (buffer-name buf))
                 (with-current-buffer buf
                   (and (boundp 'claude-agent--process)
                        claude-agent--process
                        (process-live-p claude-agent--process))))
        (push (cons (buffer-name buf) buf) buffers)))
    (nreverse buffers)))

(defun claude-pair--get-org-project-root ()
  "Get PROJECT_ROOT property from current org buffer, if any."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^:PROJECT_ROOT:\\s-*\\(.+\\)$" nil t)
        (let ((root (string-trim (match-string 1))))
          (when (and root (not (string-empty-p root)))
            (expand-file-name root)))))))

(defun claude-pair--find-agent-for-dir (dir claude-buffers)
  "Find a Claude agent buffer whose work-dir matches or contains DIR.
Returns the buffer or nil."
  (let ((expanded-dir (expand-file-name dir)))
    (cdr (cl-find-if
          (lambda (pair)
            (with-current-buffer (cdr pair)
              (and (boundp 'claude-agent--work-dir)
                   claude-agent--work-dir
                   (let ((work-dir (expand-file-name claude-agent--work-dir)))
                     (or (string= work-dir expanded-dir)
                         ;; Also match if file is inside agent's work-dir
                         (string-prefix-p (file-name-as-directory work-dir)
                                          expanded-dir))))))
          claude-buffers))))

(defun claude-pair--get-current-project-root ()
  "Get the project root for the current context.
First checks for org PROJECT_ROOT property, then falls back to project.el."
  (or (claude-pair--get-org-project-root)
      (when-let ((project (project-current)))
        (expand-file-name (project-root project)))))

(defun claude-pair--select-agent-buffer ()
  "Select a Claude agent buffer for the current project.
If a matching agent exists, returns it.
If no matching agent exists, starts a new one.
Returns the selected buffer."
  (let* ((claude-buffers (claude-pair--get-claude-buffers))
         (root (claude-pair--get-current-project-root))
         (matching-buffer (when root
                            (claude-pair--find-agent-for-dir root claude-buffers))))
    (cond
     ;; Found a matching agent for current project
     (matching-buffer matching-buffer)
     ;; No matching agent - start a new one if we have a project root
     (root
      (require 'claude-agent)
      (message "Starting Claude agent for %s..." root)
      (claude-agent-run root))
     ;; No project root - fall back to prompting or error
     ((= (length claude-buffers) 1)
      (cdar claude-buffers))
     ((> (length claude-buffers) 1)
      (let ((choice (completing-read "Select Claude agent: "
                                     (mapcar #'car claude-buffers)
                                     nil t)))
        (cdr (assoc choice claude-buffers))))
     (t nil))))

(defun claude-pair--wait-for-process (buffer &optional timeout)
  "Wait for Claude agent process in BUFFER to be ready.
TIMEOUT is max seconds to wait (default 5).  Returns t if ready, nil if timeout."
  (let ((deadline (+ (float-time) (or timeout 5))))
    (while (and (< (float-time) deadline)
                (not (with-current-buffer buffer
                       (and (boundp 'claude-agent--process)
                            claude-agent--process
                            (process-live-p claude-agent--process)))))
      (sit-for 0.1))
    (with-current-buffer buffer
      (and (boundp 'claude-agent--process)
           claude-agent--process
           (process-live-p claude-agent--process)))))

(defun claude-pair--send-to-agent (text)
  "Send TEXT to the Claude agent for the current project.
Starts a new agent if one doesn't exist for this project."
  (let ((claude-buffer (claude-pair--select-agent-buffer)))
    (if claude-buffer
        (progn
          ;; Wait for agent process to be ready
          (unless (claude-pair--wait-for-process claude-buffer)
            (error "Timed out waiting for Claude agent to start"))
          (with-current-buffer claude-buffer
            (process-send-string
             claude-agent--process
             (concat (json-encode `((type . "message") (text . ,text))) "\n"))))
      (error "No project found.  Open a file in a project first"))))

;;;; Point-based Quick Actions

(defcustom claude-pair-point-context-lines-before 2
  "Number of lines of context to include before point for quick actions."
  :type 'integer
  :group 'claude-pair)

(defcustom claude-pair-point-context-lines-after 5
  "Number of lines of context to include after point for quick actions."
  :type 'integer
  :group 'claude-pair)

(defun claude-pair--get-enclosing-defun ()
  "Get the enclosing function/defun around point.
Returns a plist with :name, :start-line, :end-line, :content or nil."
  (save-excursion
    (condition-case nil
        (let ((start (progn (beginning-of-defun) (point)))
              (start-line (line-number-at-pos))
              (name nil))
          ;; Try to get defun name from various syntaxes
          (cond
           ;; Elisp defun/defmacro/etc
           ((looking-at "^(def\\w+\\s-+\\([a-zA-Z0-9_-]+\\)")
            (setq name (match-string-no-properties 1)))
           ;; Python def/class
           ((looking-at "^\\s-*\\(def\\|class\\|async def\\)\\s-+\\([a-zA-Z0-9_]+\\)")
            (setq name (match-string-no-properties 2)))
           ;; JavaScript/TypeScript function
           ((looking-at "^\\s-*\\(?:async\\s-+\\)?\\(?:function\\|const\\|let\\|var\\)\\s-+\\([a-zA-Z0-9_]+\\)")
            (setq name (match-string-no-properties 1)))
           ;; Go func
           ((looking-at "^func\\s-+\\(?:(.*?)\\s-+\\)?\\([a-zA-Z0-9_]+\\)")
            (setq name (match-string-no-properties 1)))
           ;; Rust fn
           ((looking-at "^\\s-*\\(?:pub\\s-+\\)?\\(?:async\\s-+\\)?fn\\s-+\\([a-zA-Z0-9_]+\\)")
            (setq name (match-string-no-properties 1))))
          (end-of-defun)
          (let ((end (point))
                (end-line (line-number-at-pos)))
            (list :name (or name "<anonymous>")
                  :start-line start-line
                  :end-line end-line
                  :content (buffer-substring-no-properties start end))))
      (error nil))))

(defun claude-pair--get-point-context ()
  "Get context around the current point or region.
Returns a plist with location info and code context."
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (line (line-number-at-pos))
         (column (current-column))
         (has-region (use-region-p))
         (region-start (when has-region (region-beginning)))
         (region-end (when has-region (region-end)))
         (region-start-line (when has-region (line-number-at-pos region-start)))
         (region-end-line (when has-region (line-number-at-pos region-end)))
         (region-text (when has-region
                        (buffer-substring-no-properties region-start region-end)))
         (enclosing-defun (unless has-region (claude-pair--get-enclosing-defun)))
         (context-lines nil))
    ;; Get surrounding context if no region selected
    (unless has-region
      (save-excursion
        (let ((start-line (max 1 (- line claude-pair-point-context-lines-before)))
              (end-line (+ line claude-pair-point-context-lines-after)))
          (goto-char (point-min))
          (forward-line (1- start-line))
          (let ((ctx-line start-line))
            (while (and (<= ctx-line end-line) (not (eobp)))
              (push (cons ctx-line
                          (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))
                    context-lines)
              (setq ctx-line (1+ ctx-line))
              (forward-line 1)))
          (setq context-lines (nreverse context-lines)))))
    (list :file file
          :line line
          :column column
          :has-region has-region
          :region-start-line region-start-line
          :region-end-line region-end-line
          :region-text region-text
          :enclosing-defun enclosing-defun
          :context-lines context-lines)))

(defun claude-pair--format-point-action (context message action-type)
  "Format a point action request with CONTEXT, MESSAGE, and ACTION-TYPE.
Uses org-mode formatting for display in Emacs."
  (let* ((file (plist-get context :file))
         (line (plist-get context :line))
         (column (plist-get context :column))
         (has-region (plist-get context :has-region))
         (region-start (plist-get context :region-start-line))
         (region-end (plist-get context :region-end-line))
         (region-text (plist-get context :region-text))
         (defun-info (plist-get context :enclosing-defun))
         (context-lines (plist-get context :context-lines))
         (is-file-buffer (buffer-file-name))
         (parts '()))
    ;; Header with action type
    (push (format "** Quick Action: %s\n\n" action-type) parts)
    ;; Location info
    (if is-file-buffer
        (push (format "*File:* =%s=\n" file) parts)
      (push (format "*Buffer:* =%s=\n" file) parts))
    (unless has-region
      (push (format "*Position:* Line %d, Column %d\n" line column) parts))
    ;; Code context
    (if has-region
        ;; Show selected region with line numbers
        (progn
          (push "\n*Selected code:*\n" parts)
          (let ((lines (split-string region-text "\n"))
                (line-num region-start))
            (dolist (line-text lines)
              (push (format "%4d→%s\n" line-num line-text) parts)
              (cl-incf line-num))))
      ;; Show surrounding context with defun info
      (when defun-info
        (push (format "\n*Inside function:* =%s= (lines %d-%d)\n\n"
                      (plist-get defun-info :name)
                      (plist-get defun-info :start-line)
                      (plist-get defun-info :end-line))
              parts))
      (dolist (ctx context-lines)
        (let ((ctx-line (car ctx))
              (ctx-text (cdr ctx)))
          (if (= ctx-line line)
              (push (format "%4d→%s  <-- cursor here\n" ctx-line ctx-text) parts)
            (push (format "%4d→%s\n" ctx-line ctx-text) parts)))))
    ;; User request at the end
    (push (format "\n*Request:* %s\n" message) parts)
    (apply #'concat (nreverse parts))))

(defun claude-pair-point-action (message)
  "Send a quick action request at the current point with MESSAGE."
  (interactive "sAction request: ")
  (let* ((context (claude-pair--get-point-context))
         (formatted (claude-pair--format-point-action context message "General Action")))
    (claude-pair--send-to-agent formatted)
    (message "Sent quick action to agent.")))

(defun claude-pair-point-action-test ()
  "Request Claude to write a test for the code at point."
  (interactive)
  (let* ((context (claude-pair--get-point-context))
         (defun-info (plist-get context :enclosing-defun))
         (message (if defun-info
                      (format "Write a test for the function `%s`"
                              (plist-get defun-info :name))
                    "Write a test for this code"))
         (formatted (claude-pair--format-point-action context message "Write Test")))
    (claude-pair--send-to-agent formatted)
    (message "Sent test request to agent.")))

(defun claude-pair-point-action-doc ()
  "Request Claude to add documentation at point."
  (interactive)
  (let* ((context (claude-pair--get-point-context))
         (defun-info (plist-get context :enclosing-defun))
         (message (if defun-info
                      (format "Add documentation/docstring for the function `%s`"
                              (plist-get defun-info :name))
                    "Add documentation for this code"))
         (formatted (claude-pair--format-point-action context message "Add Documentation")))
    (claude-pair--send-to-agent formatted)
    (message "Sent documentation request to agent.")))

(defun claude-pair-point-action-fix ()
  "Request Claude to fix an issue at point."
  (interactive)
  (let* ((context (claude-pair--get-point-context))
         (formatted (claude-pair--format-point-action context "Fix this code - there's likely a bug or issue here" "Fix Issue")))
    (claude-pair--send-to-agent formatted)
    (message "Sent fix request to agent.")))

;;;; Keybindings

;; Forward declarations
(declare-function claude-menu "claude-transient")
(declare-function claude-agent-run "claude-agent")
(defvar claude-agent-mode)

(defvar claude-pair-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Main transient menu (dispatches to pair or agent menu based on context)
    (define-key map (kbd "C-c c") #'claude-menu)
    map)
  "Keymap for claude-pair-mode.")

;;;###autoload
(define-minor-mode claude-pair-mode
  "Minor mode for pair programming with Claude.
\\{claude-pair-mode-map}"
  :lighter " Claude-Pair"
  :keymap claude-pair-mode-map)

;;;###autoload
(define-globalized-minor-mode global-claude-pair-mode
  claude-pair-mode
  claude-pair--turn-on-maybe
  :group 'claude-pair)

(defun claude-pair--turn-on-maybe ()
  "Turn on `claude-pair-mode' unless in a Claude agent buffer."
  (unless (derived-mode-p 'claude-agent-mode)
    (claude-pair-mode 1)))

(provide 'claude-pair)
;;; claude-pair.el ends here
