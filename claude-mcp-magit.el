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
(require 'claude-mcp-registry)

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
        (file-name-as-directory (string-trim (buffer-string)))))))

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

(defconst claude-mcp-magit--proposed-commit-file "CLAUDE_PROPOSED_COMMIT_MSG"
  "Filename within .git dir for storing proposed commit messages.")

(defun claude-mcp-magit--git-dir (&optional directory)
  "Get the .git directory for DIRECTORY (works for worktrees too).
Unlike `magit-gitdir', this works without magit initialization and
correctly returns the worktree-specific git dir."
  (let ((default-directory (or directory default-directory)))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "rev-parse" "--git-dir"))
        (file-name-as-directory (file-truename (string-trim (buffer-string))))))))

(defun claude-mcp-magit--proposed-commit-path (&optional directory)
  "Return path to the proposed commit file for DIRECTORY's git repo.
Each worktree has its own .git dir, so proposals never collide."
  (let ((git-dir (claude-mcp-magit--git-dir directory)))
    (when git-dir
      (expand-file-name claude-mcp-magit--proposed-commit-file git-dir))))

(defun claude-mcp-magit--write-proposed-commit (message &optional directory)
  "Write proposed commit MESSAGE to the git dir for DIRECTORY."
  (let ((path (claude-mcp-magit--proposed-commit-path directory)))
    (unless path
      (error "Cannot determine .git directory"))
    (with-temp-file path
      (insert message))
    path))

(defun claude-mcp-magit--read-proposed-commit (&optional directory)
  "Read the proposed commit message for DIRECTORY, or nil if none."
  (let ((path (claude-mcp-magit--proposed-commit-path directory)))
    (when (and path (file-exists-p path))
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string)))))

(defun claude-mcp-magit--clear-proposed-commit (&optional directory)
  "Remove the proposed commit file for DIRECTORY."
  (let ((path (claude-mcp-magit--proposed-commit-path directory)))
    (when (and path (file-exists-p path))
      (delete-file path))))

(defun claude-mcp-magit-commit-propose (message &optional directory)
  "Propose a commit with MESSAGE for user approval.
Writes the proposed message to .git/CLAUDE_PROPOSED_COMMIT_MSG so it
persists across sessions and doesn't collide with other worktrees.
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
      ;; Write proposed commit message to .git/CLAUDE_PROPOSED_COMMIT_MSG
      (claude-mcp-magit--write-proposed-commit message)
      ;; Return info about what's proposed
      `((status . "pending_approval")
        (message . ,message)
        (files . ,staged-files)
        (instructions . "Commit proposed. User should review and approve with claude-mcp-magit-commit-approve or reject with claude-mcp-magit-commit-reject.")))))

(defun claude-mcp-magit--make-commit-message-hook (message &optional directory)
  "Return a one-shot hook function that inserts MESSAGE into COMMIT_EDITMSG.
The hook is scoped to the git-dir of DIRECTORY (defaults to
`default-directory') so it won't fire for commits in other worktrees.
The hook removes itself from `git-commit-setup-hook' after it fires."
  (let* ((target-git-dir (claude-mcp-magit--git-dir directory))
         (hook-fn nil))
    (setq hook-fn
          (lambda ()
            ;; Only act if we're committing in the right repo
            (when (string= (claude-mcp-magit--git-dir) target-git-dir)
              (goto-char (point-min))
              (insert message)
              (save-buffer)
              ;; Remove ourselves from the hook (one-shot)
              (remove-hook 'git-commit-setup-hook hook-fn))))
    hook-fn))

(defun claude-mcp-magit-commit-approve (&optional directory)
  "Approve the pending commit and open magit commit buffer for final review.
Reads the proposed message from .git/CLAUDE_PROPOSED_COMMIT_MSG and
populates COMMIT_EDITMSG with it for editing.  The file is NOT deleted
until the commit succeeds (handled by `claude-mcp-magit--post-commit-cleanup').
DIRECTORY defaults to `default-directory'."
  (interactive)
  (let* ((default-directory (or (claude-mcp-magit--git-toplevel directory)
                                (claude-mcp-magit--git-toplevel)
                                default-directory))
         (proposed-message (claude-mcp-magit--read-proposed-commit)))
    (unless proposed-message
      (error "No pending commit to approve (no CLAUDE_PROPOSED_COMMIT_MSG found)"))
    (let ((currently-staged (claude-mcp-magit--git-lines "diff" "--cached" "--name-only")))
      (unless currently-staged
        (error "No files are currently staged")))
    ;; Add a one-shot hook that inserts the proposed message into COMMIT_EDITMSG
    (add-hook 'git-commit-setup-hook
              (claude-mcp-magit--make-commit-message-hook proposed-message)
              90)
    ;; Open magit and start commit
    (magit-status)
    (magit-commit-create)))

(defun claude-mcp-magit-commit-prefill (message &optional directory)
  "Set up a one-shot hook to pre-fill MESSAGE into the next commit in DIRECTORY.
This is for programmatic use (e.g. the merge workflow) where the caller
opens the magit commit editor itself.  Unlike `commit-approve', this does
not read from .git/CLAUDE_PROPOSED_COMMIT_MSG and does not open magit."
  (add-hook 'git-commit-setup-hook
            (claude-mcp-magit--make-commit-message-hook message directory)
            90))

(defun claude-mcp-magit-has-proposed-commit-p (&optional directory)
  "Return non-nil if DIRECTORY has a pending proposed commit.
DIRECTORY defaults to `default-directory'."
  (let ((path (claude-mcp-magit--proposed-commit-path
               (or directory default-directory))))
    (and path (file-exists-p path))))

(defun claude-mcp-magit-commit-status ()
  "Check if there's a pending commit proposal.
Reads from .git/CLAUDE_PROPOSED_COMMIT_MSG in the current directory.
Returns the proposal details or nil."
  (let* ((default-directory (or claude-session-cwd default-directory))
         (toplevel (claude-mcp-magit--git-toplevel))
         (default-directory (or toplevel default-directory))
         (proposed-message (claude-mcp-magit--read-proposed-commit)))
    (when proposed-message
      (let ((staged-files (ignore-errors
                            (claude-mcp-magit--git-lines "diff" "--cached" "--name-only"))))
        `((status . "pending")
          (directory . ,default-directory)
          (message . ,proposed-message)
          (files . ,(or staged-files '())))))))

;; Global cleanup hook: remove CLAUDE_PROPOSED_COMMIT_MSG after successful commit
(defun claude-mcp-magit--post-commit-cleanup ()
  "Remove the proposed commit file after a successful commit.
Added to `git-commit-post-finish-hook'.
`default-directory' is set to the git working tree by `with-editor'."
  (claude-mcp-magit--clear-proposed-commit))

(add-hook 'git-commit-post-finish-hook #'claude-mcp-magit--post-commit-cleanup)

;;;; Direct Commit Operations
;;
;; These operations create commits directly without the propose/approve workflow.
;; Useful for automated workflows where user approval is not required.

(defun claude-mcp-magit-commit (message &optional directory no-gpg-sign)
  "Create a commit with MESSAGE.
If NO-GPG-SIGN is non-nil (default t), skip GPG signing.
DIRECTORY defaults to claude-session-cwd.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir))
         (no-sign (if (eq no-gpg-sign nil) nil t)))  ; default to t
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((staged (claude-mcp-magit--git-lines "diff" "--cached" "--name-only")))
      (unless staged
        (error "No files staged for commit"))
      (let ((args (list "commit" "-m" message)))
        (when no-sign
          (push "--no-gpg-sign" args))
        (apply #'claude-mcp-magit--git-output (nreverse args)))
      `((status . "committed")
        (message . ,message)
        (files . ,staged)))))

(defun claude-mcp-magit-amend (message &optional directory force no-gpg-sign)
  "Amend HEAD commit with MESSAGE.
Fails if HEAD has been pushed unless FORCE is non-nil.
If NO-GPG-SIGN is non-nil (default t), skip GPG signing.
DIRECTORY defaults to claude-session-cwd.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir))
         (no-sign (if (eq no-gpg-sign nil) nil t)))  ; default to t
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    ;; Safety check: is HEAD pushed?
    (unless force
      (let* ((branch (ignore-errors
                       (string-trim (claude-mcp-magit--git-output "symbolic-ref" "--short" "HEAD"))))
             (upstream (when branch
                         (ignore-errors
                           (string-trim (claude-mcp-magit--git-output 
                            "rev-parse" "--abbrev-ref" (concat branch "@{upstream}"))))))
             (head-pushed (when upstream
                            (zerop (car (claude-mcp-magit--call-git
                                         "merge-base" "--is-ancestor" "HEAD" upstream))))))
        (when head-pushed
          (error "HEAD commit has been pushed to %s. Use force=true to amend anyway" upstream))))
    (let ((args (list "commit" "--amend" "-m" message)))
      (when no-sign
        (push "--no-gpg-sign" args))
      (apply #'claude-mcp-magit--git-output (nreverse args)))
    `((status . "amended")
      (message . ,message))))

(defun claude-mcp-magit-rebase (onto &optional directory autosquash)
  "Rebase current branch onto ONTO.
If AUTOSQUASH is non-nil, automatically apply fixup/squash commits.
DIRECTORY defaults to claude-session-cwd.
Non-interactive only. Auto-aborts on conflicts.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((args (list "rebase")))
      (when autosquash
        (push "--autosquash" args))
      (push onto args)
      (let ((result (apply #'claude-mcp-magit--call-git (nreverse args))))
        (if (zerop (car result))
            `((status . "rebased")
              (onto . ,onto)
              (output . ,(string-trim (cdr result))))
          ;; Rebase failed - likely conflicts
          (claude-mcp-magit--git-output "rebase" "--abort")  ; Clean up
          (error "Rebase failed (aborted automatically):\n%s" (cdr result)))))))

(defun claude-mcp-magit-ignore (file &optional directory unignore)
  "Mark FILE as assumed-unchanged so git ignores local modifications.
If UNIGNORE is non-nil, remove the assume-unchanged flag instead.
DIRECTORY defaults to claude-session-cwd.
Useful for temporarily ignoring changes to tracked files.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((flag (if unignore "--no-assume-unchanged" "--assume-unchanged")))
      (claude-mcp-magit--git-output "update-index" flag "--" file)
      `((status . ,(if unignore "unignored" "ignored"))
        (file . ,file)))))

;;;; Additional Git Operations
;;
;; Extended git operations for more comprehensive repository management.

(defun claude-mcp-magit-branch (&optional directory)
  "Get branch information for DIRECTORY (or claude-session-cwd).
Returns an alist with :current (current branch name) and :branches (list of all branches).
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((current (string-trim
                    (or (ignore-errors
                          (claude-mcp-magit--git-output "symbolic-ref" "--short" "HEAD"))
                        ;; Detached HEAD
                        (concat "HEAD detached at "
                                (claude-mcp-magit--git-output "rev-parse" "--short" "HEAD")))))
          (branches (claude-mcp-magit--git-lines "branch" "--format=%(refname:short)")))
      `((current . ,current)
        (branches . ,branches)))))

(defun claude-mcp-magit-show (revision &optional file directory)
  "Show content of REVISION (commit hash, branch, tag, etc).
If FILE is provided, show only that file's content at REVISION.
DIRECTORY defaults to claude-session-cwd.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (if file
        (claude-mcp-magit--git-output "show" (format "%s:%s" revision file))
      (claude-mcp-magit--git-output "show" "--stat" "--patch" revision))))

(defun claude-mcp-magit-blame (file &optional directory revision)
  "Get git blame output for FILE.
DIRECTORY defaults to claude-session-cwd.
REVISION optionally specifies a commit to blame from (default: HEAD).
Returns blame output with commit hash, author, date, and line content.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((args (list "blame" "--date=short")))
      (when revision
        (setq args (append args (list revision))))
      (setq args (append args (list "--" file)))
      (apply #'claude-mcp-magit--git-output args))))

(defun claude-mcp-magit-stash-list (&optional directory)
  "List all stashes for DIRECTORY (or claude-session-cwd).
Returns a list of stash entries with index, message, and branch.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (claude-mcp-magit--git-output "stash" "list")))

(defun claude-mcp-magit-stash-push (&optional message directory include-untracked)
  "Stash current changes with optional MESSAGE.
DIRECTORY defaults to claude-session-cwd.
If INCLUDE-UNTRACKED is non-nil, also stash untracked files.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((args (list "stash" "push")))
      (when include-untracked
        (setq args (append args (list "--include-untracked"))))
      (when message
        (setq args (append args (list "-m" message))))
      (apply #'claude-mcp-magit--git-output args))))

(defun claude-mcp-magit-stash-pop (&optional stash-ref directory)
  "Pop a stash, applying it to the working directory.
STASH-REF defaults to stash@{0} (most recent).
DIRECTORY defaults to claude-session-cwd.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir))
         (ref (or stash-ref "stash@{0}")))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (claude-mcp-magit--git-output "stash" "pop" ref)))

(defun claude-mcp-magit-file-log (file &optional count directory)
  "Get commit history for FILE.
COUNT defaults to 10.  DIRECTORY defaults to claude-session-cwd.
Returns a detailed log with commit hash, author, date, and message.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir))
         (n (or count 10)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (claude-mcp-magit--git-output
     "log" (format "-%d" n)
     "--format=%h %ad %an: %s"
     "--date=short"
     "--follow"
     "--" file)))

(defun claude-mcp-magit-checkout-file (file &optional revision directory)
  "Checkout FILE from REVISION (default: HEAD), discarding local changes.
DIRECTORY defaults to claude-session-cwd.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir))
         (rev (or revision "HEAD")))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (claude-mcp-magit--git-output "checkout" rev "--" file)
    (format "Checked out %s from %s" file rev)))

(defun claude-mcp-magit-remote (&optional directory)
  "Get remote information for DIRECTORY (or claude-session-cwd).
Returns an alist with remote names and their URLs.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((remotes (claude-mcp-magit--git-lines "remote")))
      (mapcar (lambda (remote)
                (let ((url (string-trim
                            (claude-mcp-magit--git-output "remote" "get-url" remote))))
                  (cons remote url)))
              remotes))))

(defun claude-mcp-magit-tags (&optional directory)
  "List all tags for DIRECTORY (or claude-session-cwd).
Returns a list of tag names, most recent first.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (claude-mcp-magit--git-lines "tag" "--sort=-creatordate")))

(defun claude-mcp-magit-rev-parse (revision &optional directory)
  "Resolve REVISION to a full commit hash.
DIRECTORY defaults to claude-session-cwd.
Useful for resolving branch names, tags, HEAD~N, etc. to actual commits.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-magit--git-toplevel start-dir) start-dir)))
    (unless (claude-mcp-magit--git-toplevel)
      (error "Not in a git repository: %s" start-dir))
    (claude-mcp-magit--git-output "rev-parse" revision)))

;;;; MCP Tool Definitions
;;
;; Tool definitions for the MCP (Model Context Protocol) interface.

(claude-mcp-deftool magit-status
  "Get current git status including staged, unstaged, and untracked files. Returns branch name and file lists."
  :function #'claude-mcp-magit-status
  :safe t
  :args ((directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-stage
  "Stage files for commit. Takes a list of file paths relative to the repository root."
  :function #'claude-mcp-magit-stage
  :safe nil
  :args ((files array :required "Array of file paths to stage")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-unstage
  "Unstage files (remove from staging area). Takes a list of file paths."
  :function #'claude-mcp-magit-unstage
  :safe nil
  :args ((files array :required "Array of file paths to unstage")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-diff
  "Get git diff output. Can get diff for a specific file or all changes. Use staged=true for staged changes."
  :function #'claude-mcp-magit-diff
  :safe t
  :args ((file string "Specific file to diff (default: all files)")
         (directory string "Git repository directory (default: session working directory)")
         (staged boolean "If true, show staged diff; otherwise show unstaged diff")))

(claude-mcp-deftool magit-log
  "Get recent git commit log entries."
  :function #'claude-mcp-magit-log
  :safe t
  :args ((count integer "Number of log entries to return (default: 5)")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-commit-propose
  "Propose a commit for user approval. The user must call magit-commit-approve to actually create the commit. This allows the user to review and sign the commit with their GPG key."
  :function #'claude-mcp-magit-commit-propose
  :safe nil
  :args ((message string :required "The commit message to propose")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-commit-status
  "Check if there's a pending commit proposal awaiting user approval."
  :function #'claude-mcp-magit-commit-status
  :safe t
  :args ())

(claude-mcp-deftool magit-commit
  "Create a git commit with staged changes. Use magit-commit-propose for commits requiring user approval/GPG signing."
  :function #'claude-mcp-magit-commit
  :safe nil
  :args ((message string :required "Commit message")
         (directory string "Git repository directory (default: session working directory)")
         (no_gpg_sign boolean "Skip GPG signing (default: true for agent commits)")))

(claude-mcp-deftool magit-amend
  "Amend the HEAD commit with a new message. Fails if HEAD is already pushed unless force=true."
  :function #'claude-mcp-magit-amend
  :safe nil
  :args ((message string :required "New commit message")
         (directory string "Git repository directory (default: session working directory)")
         (force boolean "Allow amending pushed commits (dangerous, default: false)")
         (no_gpg_sign boolean "Skip GPG signing (default: true for agent commits)")))

(claude-mcp-deftool magit-rebase
  "Rebase current branch onto another branch/commit. Non-interactive only. Auto-aborts on conflicts."
  :function #'claude-mcp-magit-rebase
  :safe nil
  :args ((onto string :required "Branch or commit to rebase onto")
         (directory string "Git repository directory (default: session working directory)")
         (autosquash boolean "Apply fixup!/squash! commits automatically (default: false)")))

(claude-mcp-deftool magit-ignore
  "Ignore local changes to a tracked file using git update-index --assume-unchanged. Use unignore=true to restore tracking."
  :function #'claude-mcp-magit-ignore
  :safe nil
  :args ((file string :required "File path to ignore or unignore")
         (directory string "Git repository directory (default: session working directory)")
         (unignore boolean "If true, restore tracking of the file (default: false)")))

(claude-mcp-deftool magit-branch
  "Get branch information including current branch and list of all branches."
  :function #'claude-mcp-magit-branch
  :safe t
  :args ((directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-show
  "Show content of a commit, or a specific file at a commit. Use this to inspect commit details or historical file contents."
  :function #'claude-mcp-magit-show
  :safe t
  :args ((revision string :required "Commit hash, branch name, tag, or other revision (e.g., 'HEAD~1', 'main', 'v1.0.0')")
         (file string "Optional file path to show only that file's content at the revision")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-blame
  "Get git blame output for a file, showing who last modified each line."
  :function #'claude-mcp-magit-blame
  :safe t
  :args ((file string :required "File path to blame")
         (directory string "Git repository directory (default: session working directory)")
         (revision string "Optional revision to blame from (default: HEAD)")))

(claude-mcp-deftool magit-stash-list
  "List all stashes in the repository."
  :function #'claude-mcp-magit-stash-list
  :safe t
  :args ((directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-stash-push
  "Stash current changes. Useful for temporarily saving work in progress."
  :function #'claude-mcp-magit-stash-push
  :safe nil
  :args ((message string "Optional message to describe the stash")
         (directory string "Git repository directory (default: session working directory)")
         (include-untracked boolean "If true, also stash untracked files")))

(claude-mcp-deftool magit-stash-pop
  "Pop the most recent stash (or specified stash), applying it to the working directory."
  :function #'claude-mcp-magit-stash-pop
  :safe nil
  :args ((stash-ref string "Stash reference (default: stash@{0})")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-file-log
  "Get commit history for a specific file. Useful for understanding file evolution."
  :function #'claude-mcp-magit-file-log
  :safe t
  :args ((file string :required "File path to get history for")
         (count integer "Number of commits to show (default: 10)")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-checkout-file
  "Checkout a file from a specific revision, discarding local changes to that file."
  :function #'claude-mcp-magit-checkout-file
  :safe nil
  :args ((file string :required "File path to checkout")
         (revision string "Revision to checkout from (default: HEAD)")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-remote
  "Get information about configured git remotes (names and URLs)."
  :function #'claude-mcp-magit-remote
  :safe t
  :args ((directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-tags
  "List all tags in the repository, sorted by creation date (most recent first)."
  :function #'claude-mcp-magit-tags
  :safe t
  :args ((directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-rev-parse
  "Resolve a revision to its full commit hash. Useful for resolving branch names, tags, HEAD~N, etc."
  :function #'claude-mcp-magit-rev-parse
  :safe t
  :args ((revision string :required "Revision to resolve (e.g., 'HEAD', 'main~3', 'v1.0.0')")
         (directory string "Git repository directory (default: session working directory)")))

(provide 'claude-mcp-magit)
;;; claude-mcp-magit.el ends here
