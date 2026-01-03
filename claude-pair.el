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

(defun claude-pair--send-to-agent (text)
  "Send TEXT to the Claude agent in the current project."
  ;; Find the claude buffer for this project
  (let* ((project (project-current))
         (root (when project (project-root project)))
         (claude-buffer (when root
                          (cl-find-if
                           (lambda (buf)
                             (and (string-match-p "^\\*claude" (buffer-name buf))
                                  (with-current-buffer buf
                                    (and (boundp 'claude-agent--work-dir)
                                         claude-agent--work-dir
                                         (string-prefix-p
                                          (expand-file-name claude-agent--work-dir)
                                          (expand-file-name root))))))
                           (buffer-list)))))
    (if claude-buffer
        (with-current-buffer claude-buffer
          (when (and (boundp 'claude-agent--process)
                     claude-agent--process
                     (process-live-p claude-agent--process))
            (process-send-string
             claude-agent--process
             (concat (json-encode `((type . "message") (text . ,text))) "\n"))))
      (error "No Claude agent found for this project"))))

;;;; Keybindings

(defvar claude-pair-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x c c") #'claude-pair-send-comments)
    map)
  "Keymap for claude-pair-mode.")

;;;###autoload
(define-minor-mode claude-pair-mode
  "Minor mode for pair programming with Claude.
\\{claude-pair-mode-map}"
  :lighter " Claude-Pair"
  :keymap claude-pair-mode-map
  :global t)

(provide 'claude-pair)
;;; claude-pair.el ends here
