;;; claude-mcp-magit.el --- Magit section querying for Claude -*- lexical-binding: t; -*-

;; This file is part of Claude.

;;; Commentary:

;; This module provides a clean API for querying magit-section buffers.
;; Magit sections are used by various Emacs tools (not just magit itself)
;; to display structured, collapsible content.
;;
;; Main functions:
;; - claude-mcp-magit-section-query-find: Find sections by criteria
;; - claude-mcp-magit-section-query-content: Extract section content
;; - claude-mcp-magit-section-query-get: Get section metadata
;; - claude-mcp-magit-section-query-children: Get child sections

;;; Code:

(require 'magit-section nil t)

(defun claude-mcp-magit-section-query--strip-indent (text)
  "Remove common leading whitespace from TEXT.
Designed to be called via emacsclient by Claude AI."
  (let* ((lines (split-string text "\n"))
         (non-empty-lines (seq-filter (lambda (line) (not (string-empty-p (string-trim-left line)))) lines))
         (indents (mapcar (lambda (line) (length (replace-regexp-in-string "^\\( *\\).*" "\\1" line))) non-empty-lines))
         (min-indent (if indents (apply 'min indents) 0)))
    (mapconcat (lambda (line)
                 (if (> (length line) min-indent)
                     (substring line min-indent)
                   line))
               lines "\n")))

(defun claude-mcp-magit-section-query--walk (section fn)
  "Walk SECTION tree applying FN to each section.
FN should accept a section and return non-nil to continue walking.
Designed to be called via emacsclient by Claude AI."
  (when (funcall fn section)
    (dolist (child (eieio-oref section 'children))
      (claude-mcp-magit-section-query--walk child fn))))

(defun claude-mcp-magit-section-query--matches-p (section criteria)
  "Check if SECTION matches CRITERIA (a plist).
Supported criteria: :type, :heading (regex), :hidden, :value
Designed to be called via emacsclient by Claude AI."
  (let ((matches t))
    (when (plist-member criteria :type)
      (let ((expected-type (plist-get criteria :type)))
        (unless (eq (eieio-oref section 'type) expected-type)
          (setq matches nil))))
    (when (and matches (plist-member criteria :heading))
      (let ((heading-pattern (plist-get criteria :heading))
            (content (eieio-oref section 'content)))
        ;; Match against the content text if available
        (unless (and content (string-match-p heading-pattern content))
          (setq matches nil))))
    (when (and matches (plist-member criteria :hidden))
      (let ((expected-hidden (plist-get criteria :hidden))
            (hidden (eieio-oref section 'hidden)))
        (unless (eq hidden expected-hidden)
          (setq matches nil))))
    (when (and matches (plist-member criteria :value))
      (let ((expected-value (plist-get criteria :value)))
        (unless (equal (eieio-oref section 'value) expected-value)
          (setq matches nil))))
    matches))

(defun claude-mcp-magit-section-query-content (buffer-name section-position &optional strip-indent)
  "Extract content of magit section at SECTION-POSITION in BUFFER-NAME as string.
SECTION-POSITION should be a buffer position (integer) within the section.
If STRIP-INDENT is non-nil, remove common leading whitespace.
Designed to be called via emacsclient by Claude AI."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))
  (with-current-buffer buffer-name
    (save-excursion
      (goto-char section-position)
      (let ((section (magit-current-section)))
        (unless section
          (error "No magit section found at position %d" section-position))
        (let* ((start (marker-position (eieio-oref section 'start)))
               (end (marker-position (eieio-oref section 'end)))
               (content (buffer-substring-no-properties start end)))
          (if strip-indent
              (claude-mcp-magit-section-query--strip-indent content)
            content))))))

(defun claude-mcp-magit-section-query-find (buffer-name &rest criteria)
  "Find magit sections in BUFFER-NAME matching CRITERIA.
CRITERIA is a plist that can include:
  :type TYPE - section type symbol
  :heading REGEX - regex to match heading
  :hidden BOOL - whether section is hidden
  :value VALUE - section value

Returns list of positions (integers) for matching sections.
Designed to be called via emacsclient by Claude AI."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))
  (with-current-buffer buffer-name
    (let ((results '())
          (root (magit-current-section)))
      ;; Get the root section
      (save-excursion
        (goto-char (point-min))
        (setq root (magit-current-section))
        (when root
          (claude-mcp-magit-section-query--walk
           root
           (lambda (section)
             (when (claude-mcp-magit-section-query--matches-p section criteria)
               (push (marker-position (eieio-oref section 'start)) results))
             t))))
      (nreverse results))))

(defun claude-mcp-magit-section-query-children (buffer-name section-position &rest criteria)
  "Get child sections of section at SECTION-POSITION in BUFFER-NAME matching CRITERIA.
SECTION-POSITION should be a buffer position (integer) within the parent section.
CRITERIA is a plist (same format as magit-section-query-find).
Returns list of positions (integers) for matching child sections.
Designed to be called via emacsclient by Claude AI."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))
  (with-current-buffer buffer-name
    (save-excursion
      (goto-char section-position)
      (let* ((section (magit-current-section))
             (results '()))
        (unless section
          (error "No magit section found at position %d" section-position))
        (dolist (child (eieio-oref section 'children))
          (when (claude-mcp-magit-section-query--matches-p child criteria)
            (push (marker-position (eieio-oref child 'start)) results)))
        (nreverse results)))))

(defun claude-mcp-magit-section-query-get (buffer-name section-position &optional include-content)
  "Get metadata for magit section at SECTION-POSITION in BUFFER-NAME.
SECTION-POSITION should be a buffer position (integer) within the section.
Returns a list with metadata: (type heading hidden start end [content])
If INCLUDE-CONTENT is non-nil, includes section content as last element.
Designed to be called via emacsclient by Claude AI."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))
  (with-current-buffer buffer-name
    (save-excursion
      (goto-char section-position)
      (let ((section (magit-current-section)))
        (unless section
          (error "No magit section found at position %d" section-position))
        (let* ((start-pos (marker-position (eieio-oref section 'start)))
               (end-pos (marker-position (eieio-oref section 'end)))
               (result (list
                        (eieio-oref section 'type)
                        (eieio-oref section 'content)  ; Use content as heading
                        (eieio-oref section 'hidden)
                        start-pos
                        end-pos)))
          (when include-content
            (setq result (append result (list (buffer-substring-no-properties start-pos end-pos)))))
          result)))))

;;;; Git Operations for Claude Agent
;;
;; These functions provide a commit workflow where the agent can:
;; 1. Query git status
;; 2. Stage/unstage files
;; 3. Propose commits for user approval

;;; Worktree-aware helpers
;;
;; These helpers avoid using magit functions that could:
;; 1. Return the main repo instead of the worktree root (magit-toplevel issue)
;; 2. Create/pollute magit buffers (magit-call-git uses magit-process-buffer)
;; 3. Fail if magit hasn't been initialized for the project

(defun claude-mcp-magit--git-toplevel (&optional directory)
  "Get the git working tree root for DIRECTORY using git directly.
Unlike `magit-toplevel', this correctly returns the worktree root
rather than the main repository root when in a git worktree.
Returns nil if not in a git repository."
  (let ((default-directory (or directory default-directory)))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "rev-parse" "--show-toplevel"))
        (string-trim (buffer-string))))))

(defun claude-mcp-magit--call-git (&rest args)
  "Call git with ARGS synchronously without creating magit buffers.
Returns a cons of (exit-code . output-string)."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process "git" nil t nil args)))
      (cons exit-code (buffer-string)))))

(defun claude-mcp-magit--git-output (&rest args)
  "Call git with ARGS and return trimmed output.
Signals an error if git command fails."
  (let ((result (apply #'claude-mcp-magit--call-git args)))
    (unless (zerop (car result))
      (error "Git command failed: git %s\n%s" 
             (string-join args " ") 
             (cdr result)))
    (string-trim (cdr result))))

(defun claude-mcp-magit--git-lines (&rest args)
  "Call git with ARGS and return output as a list of lines.
Empty lines are excluded."
  (let ((output (apply #'claude-mcp-magit--git-output args)))
    (if (string-empty-p output)
        '()
      (split-string output "\n" t))))

(defun claude-mcp-magit-status (&optional directory)
  "Get current git status for DIRECTORY (or claude-session-cwd).
Returns an alist with :staged, :unstaged, :untracked, and :branch keys.
Does not open or switch to any buffers.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    ;; Use git directly to avoid opening magit buffers
    (let ((branch (string-trim 
                   (or (ignore-errors 
                         (claude-mcp-magit--git-output "symbolic-ref" "--short" "HEAD"))
                       ;; Detached HEAD - get short commit hash
                       (claude-mcp-magit--git-output "rev-parse" "--short" "HEAD"))))
          ;; Get staged files
          (staged (claude-mcp-magit--git-lines "diff" "--cached" "--name-only"))
          ;; Get unstaged (modified) files
          (unstaged (claude-mcp-magit--git-lines "diff" "--name-only"))
          ;; Get untracked files
          (untracked (claude-mcp-magit--git-lines "ls-files" "--others" "--exclude-standard")))
      ;; Return as JSON-friendly alist
      `((branch . ,branch)
        (staged . ,staged)
        (unstaged . ,unstaged)
        (untracked . ,untracked)))))

(defun claude-mcp-magit-stage (files &optional directory)
  "Stage FILES (a list of file paths) for commit.
DIRECTORY defaults to claude-session-cwd.
Does not open or switch to any buffers.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((files-list (if (listp files) files (list files))))
      ;; Use git directly to avoid opening magit buffers
      (dolist (file files-list)
        (claude-mcp-magit--git-output "add" "--" file))
      (format "Staged %d file(s): %s" 
              (length files-list)
              (string-join files-list ", ")))))

(defun claude-mcp-magit-unstage (files &optional directory)
  "Unstage FILES (a list of file paths).
DIRECTORY defaults to claude-session-cwd.
Does not open or switch to any buffers.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((files-list (if (listp files) files (list files))))
      ;; Use git directly to avoid opening magit buffers
      (dolist (file files-list)
        (claude-mcp-magit--git-output "reset" "HEAD" "--" file))
      (format "Unstaged %d file(s): %s" 
              (length files-list)
              (string-join files-list ", ")))))

(defun claude-mcp-magit-diff (&optional file directory staged)
  "Get diff for FILE (or all changes if nil).
If STAGED is non-nil, show staged diff.  Otherwise show unstaged diff.
DIRECTORY defaults to claude-session-cwd.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    ;; Build args list for git diff
    (let ((args (if staged '("diff" "--cached") '("diff"))))
      (when file
        (setq args (append args (list "--" file))))
      (apply #'claude-mcp-magit--git-output args))))

(defun claude-mcp-magit-log (&optional count directory)
  "Get recent git log entries.
COUNT defaults to 5.  DIRECTORY defaults to claude-session-cwd.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir))
         (n (or count 5)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (claude-mcp-magit--git-output "log" (format "-%d" n) "--oneline" "--no-decorate")))

(defvar claude-mcp-magit--pending-commit nil
  "Pending commit proposal: (directory message files).")

(defvar claude-mcp-magit--pending-message nil
  "Pending commit message to insert into COMMIT_EDITMSG.")

(defun claude-mcp-magit--insert-pending-message ()
  "Insert pending commit message and remove self from hook."
  (when claude-mcp-magit--pending-message
    (goto-char (point-min))
    (insert claude-mcp-magit--pending-message)
    (setq claude-mcp-magit--pending-message nil)
    (remove-hook 'git-commit-setup-hook #'claude-mcp-magit--insert-pending-message)
    ;; Save the buffer so the commit message isn't empty
    (save-buffer)))

(defun claude-mcp-magit-commit-propose (message &optional directory)
  "Propose a commit with MESSAGE for user approval.
This stages the proposal but does not commit.  User must approve.
DIRECTORY defaults to claude-session-cwd.
Returns instructions for the user.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((staged-files (claude-mcp-magit--git-lines "diff" "--cached" "--name-only")))
      (unless staged-files
        (error "No files staged for commit"))
      ;; Store the pending commit
      (setq claude-mcp-magit--pending-commit
            (list default-directory message staged-files))
      ;; Return info about what's proposed
      `((status . "pending_approval")
        (message . ,message)
        (files . ,staged-files)
        (instructions . "Commit proposed. User should review and approve with claude-mcp-magit-commit-approve or reject with claude-mcp-magit-commit-reject.")))))

(defun claude-mcp-magit-commit-approve ()
  "Approve the pending commit and open magit commit buffer for final review.
This populates COMMIT_EDITMSG with the proposed message for editing."
  (interactive)
  (unless claude-mcp-magit--pending-commit
    (error "No pending commit to approve"))
  (let* ((info claude-mcp-magit--pending-commit)
         (directory (nth 0 info))
         (proposed-message (nth 1 info))
         (files (nth 2 info))
         (default-directory directory))
    ;; Verify files are still staged
    (let ((currently-staged (claude-mcp-magit--git-lines "diff" "--cached" "--name-only")))
      (unless (equal (sort (copy-sequence files) #'string<)
                     (sort (copy-sequence currently-staged) #'string<))
        (error "Staged files have changed since proposal.  Please re-stage and propose again")))
    ;; Clear pending commit
    (setq claude-mcp-magit--pending-commit nil)
    ;; Store message and add hook (hook removes itself after running)
    (setq claude-mcp-magit--pending-message proposed-message)
    (add-hook 'git-commit-setup-hook #'claude-mcp-magit--insert-pending-message 90)
    ;; Open the commit buffer for review
    (magit-commit-create)))

(defun claude-mcp-magit-commit-status ()
  "Check if there's a pending commit proposal.
Returns the proposal details or nil."
  (when claude-mcp-magit--pending-commit
    (let* ((info claude-mcp-magit--pending-commit)
           (directory (nth 0 info))
           (message (nth 1 info))
           (files (nth 2 info)))
      `((status . "pending")
        (directory . ,directory)
        (message . ,message)
        (files . ,files)))))

(provide 'claude-mcp-magit)
;;; claude-mcp-magit.el ends here
