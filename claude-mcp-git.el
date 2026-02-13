;;; claude-mcp-git.el --- Git operations for Claude MCP -*- lexical-binding: t; -*-

;; This file is part of Claude.

;;; Commentary:

;; This module provides git operations for Claude agents via MCP tools.
;; All operations call git directly (not via magit) and work correctly
;; in git worktrees.
;;
;; Main features:
;; - Status, stage, unstage, diff, log operations
;; - Commit and amend with multiline message support
;; - Commit proposal workflow for user approval
;; - Branch, stash, blame, and other git operations
;;
;; All functions are worktree-aware and avoid creating magit buffers.

;;; Code:

(require 'claude-mcp-registry)

;; Forward declaration for claude-session-cwd
(defvar claude-session-cwd)

;;;; Worktree-aware helpers
;;
;; These helpers avoid using magit functions that could:
;; 1. Return the main repo instead of the worktree root (magit-toplevel issue)
;; 2. Create/pollute magit buffers (magit-call-git uses magit-process-buffer)
;; 3. Fail if magit hasn't been initialized for the project

(defun claude-mcp-git--toplevel (&optional directory)
  "Get the git working tree root for DIRECTORY using git directly.
Unlike `magit-toplevel', this correctly returns the worktree root
rather than the main repository root when in a git worktree.
Returns nil if not in a git repository."
  (let ((default-directory (or directory default-directory)))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "rev-parse" "--show-toplevel"))
        (file-name-as-directory (string-trim (buffer-string)))))))

(defun claude-mcp-git--call-git (&rest args)
  "Call git with ARGS synchronously without creating magit buffers.
Returns a cons of (exit-code . output-string)."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process "git" nil t nil args)))
      (cons exit-code (buffer-string)))))

(defun claude-mcp-git--output (&rest args)
  "Call git with ARGS and return trimmed output.
Signals an error if git command fails."
  (let ((result (apply #'claude-mcp-git--call-git args)))
    (unless (zerop (car result))
      (error "Git command failed: git %s\n%s"
             (string-join args " ")
             (cdr result)))
    (string-trim (cdr result))))

(defun claude-mcp-git--lines (&rest args)
  "Call git with ARGS and return output as a list of lines.
Empty lines are excluded."
  (let ((output (apply #'claude-mcp-git--output args)))
    (if (string-empty-p output)
        '()
      (split-string output "\n" t))))

(defun claude-mcp-git--commit-with-message (message extra-args)
  "Run git commit with MESSAGE, handling multiline messages correctly.
EXTRA-ARGS is a list of additional arguments to pass to git commit.
Uses a temporary file with -F for multiline messages to avoid shell escaping issues."
  (let ((temp-file (make-temp-file "claude-commit-msg")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert message))
          (apply #'claude-mcp-git--output
                 (append (list "commit" "-F" temp-file) extra-args)))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;;; Basic Git Operations

(defun claude-mcp-git-status (&optional directory)
  "Get current git status for DIRECTORY (or claude-session-cwd).
Returns an alist with :staged, :unstaged, :untracked, and :branch keys.
Does not open or switch to any buffers.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    ;; Use git directly to avoid opening magit buffers
    (let ((branch (string-trim
                   (or (ignore-errors
                         (claude-mcp-git--output "symbolic-ref" "--short" "HEAD"))
                       ;; Detached HEAD - get short commit hash
                       (claude-mcp-git--output "rev-parse" "--short" "HEAD"))))
          ;; Get staged files
          (staged (claude-mcp-git--lines "diff" "--cached" "--name-only"))
          ;; Get unstaged (modified) files
          (unstaged (claude-mcp-git--lines "diff" "--name-only"))
          ;; Get untracked files
          (untracked (claude-mcp-git--lines "ls-files" "--others" "--exclude-standard")))
      ;; Return as JSON-friendly alist
      `((branch . ,branch)
        (staged . ,staged)
        (unstaged . ,unstaged)
        (untracked . ,untracked)))))

(defun claude-mcp-git-stage (files &optional directory)
  "Stage FILES (a list of file paths) for commit.
DIRECTORY defaults to claude-session-cwd.
Does not open or switch to any buffers.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((files-list (if (listp files) files (list files))))
      ;; Use git directly to avoid opening magit buffers
      (dolist (file files-list)
        (claude-mcp-git--output "add" "--" file))
      (format "Staged %d file(s): %s"
              (length files-list)
              (string-join files-list ", ")))))

(defun claude-mcp-git-unstage (files &optional directory)
  "Unstage FILES (a list of file paths).
DIRECTORY defaults to claude-session-cwd.
Does not open or switch to any buffers.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((files-list (if (listp files) files (list files))))
      ;; Use git directly to avoid opening magit buffers
      (dolist (file files-list)
        (claude-mcp-git--output "reset" "HEAD" "--" file))
      (format "Unstaged %d file(s): %s"
              (length files-list)
              (string-join files-list ", ")))))

(defun claude-mcp-git-diff (&optional file directory staged)
  "Get diff for FILE (or all changes if nil).
If STAGED is non-nil, show staged diff.  Otherwise show unstaged diff.
DIRECTORY defaults to claude-session-cwd.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    ;; Build args list for git diff
    (let ((args (if staged '("diff" "--cached") '("diff"))))
      (when file
        (setq args (append args (list "--" file))))
      (apply #'claude-mcp-git--output args))))

(defun claude-mcp-git-log (&optional count directory)
  "Get recent git log entries.
COUNT defaults to 5.  DIRECTORY defaults to claude-session-cwd.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir))
         (n (or count 5)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (claude-mcp-git--output "log" (format "-%d" n) "--oneline" "--no-decorate")))

;;;; Commit Proposal Workflow
;;
;; The proposal workflow allows agents to propose commits that users
;; can review and approve with their own GPG keys.

(defconst claude-mcp-git--proposed-commit-file "CLAUDE_PROPOSED_COMMIT_MSG"
  "Filename within .git dir for storing proposed commit messages.")

(defun claude-mcp-git--git-dir (&optional directory)
  "Get the .git directory for DIRECTORY (works for worktrees too).
Unlike `magit-gitdir', this works without magit initialization and
correctly returns the worktree-specific git dir."
  (let ((default-directory (or directory default-directory)))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "rev-parse" "--git-dir"))
        ;; Use expand-file-name first to handle relative paths like ".git"
        ;; correctly before file-truename resolves symlinks
        (file-name-as-directory
         (file-truename
          (expand-file-name (string-trim (buffer-string)))))))))

(defun claude-mcp-git--proposed-commit-path (&optional directory)
  "Return path to the proposed commit file for DIRECTORY's git repo.
Each worktree has its own .git dir, so proposals never collide."
  (let ((git-dir (claude-mcp-git--git-dir directory)))
    (when git-dir
      (expand-file-name claude-mcp-git--proposed-commit-file git-dir))))

(defun claude-mcp-git--write-proposed-commit (message &optional directory)
  "Write proposed commit MESSAGE to the git dir for DIRECTORY."
  (let ((path (claude-mcp-git--proposed-commit-path directory)))
    (unless path
      (error "Cannot determine .git directory"))
    (with-temp-file path
      (insert message))
    path))

(defun claude-mcp-git--read-proposed-commit (&optional directory)
  "Read the proposed commit message for DIRECTORY, or nil if none."
  (let ((path (claude-mcp-git--proposed-commit-path directory)))
    (when (and path (file-exists-p path))
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string)))))

(defun claude-mcp-git--clear-proposed-commit (&optional directory)
  "Remove the proposed commit file for DIRECTORY."
  (let ((path (claude-mcp-git--proposed-commit-path directory)))
    (when (and path (file-exists-p path))
      (delete-file path))))

(defun claude-mcp-git-commit-propose (message &optional directory)
  "Propose a commit with MESSAGE for user approval.
Writes the proposed message to .git/CLAUDE_PROPOSED_COMMIT_MSG so it
persists across sessions and doesn't collide with other worktrees.
DIRECTORY defaults to claude-session-cwd.
Returns instructions for the user.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((staged-files (claude-mcp-git--lines "diff" "--cached" "--name-only")))
      (unless staged-files
        (error "No files staged for commit"))
      ;; Write proposed commit message to .git/CLAUDE_PROPOSED_COMMIT_MSG
      (claude-mcp-git--write-proposed-commit message)
      ;; Return info about what's proposed
      `((status . "pending_approval")
        (message . ,message)
        (files . ,staged-files)
        (instructions . "Commit proposed. User should review and approve with claude-mcp-git-commit-approve or reject with claude-mcp-git-commit-reject.")))))

(defun claude-mcp-git--make-commit-message-hook (message &optional directory)
  "Return a one-shot hook function that inserts MESSAGE into COMMIT_EDITMSG.
The hook is scoped to the git-dir of DIRECTORY (defaults to
`default-directory') so it won't fire for commits in other worktrees.
The hook removes itself from `git-commit-setup-hook' after it fires."
  (let* ((target-git-dir (claude-mcp-git--git-dir directory))
         (hook-fn nil))
    (setq hook-fn
          (lambda ()
            ;; Only act if we're committing in the right repo
            (when (string= (claude-mcp-git--git-dir) target-git-dir)
              (goto-char (point-min))
              (insert message)
              (save-buffer)
              ;; Remove ourselves from the hook (one-shot)
              (remove-hook 'git-commit-setup-hook hook-fn))))
    hook-fn))

(defun claude-mcp-git-commit-approve (&optional directory)
  "Approve the pending commit and open magit commit buffer for final review.
Reads the proposed message from .git/CLAUDE_PROPOSED_COMMIT_MSG and
populates COMMIT_EDITMSG with it for editing.  The file is NOT deleted
until the commit succeeds (handled by `claude-mcp-git--post-commit-cleanup').
DIRECTORY defaults to `default-directory'."
  (interactive)
  (require 'magit nil t)
  (let* ((default-directory (or (claude-mcp-git--toplevel directory)
                                (claude-mcp-git--toplevel)
                                default-directory))
         (proposed-message (claude-mcp-git--read-proposed-commit)))
    (unless proposed-message
      (error "No pending commit to approve (no CLAUDE_PROPOSED_COMMIT_MSG found)"))
    (let ((currently-staged (claude-mcp-git--lines "diff" "--cached" "--name-only")))
      (unless currently-staged
        (error "No files are currently staged")))
    ;; Add a one-shot hook that inserts the proposed message into COMMIT_EDITMSG
    (add-hook 'git-commit-setup-hook
              (claude-mcp-git--make-commit-message-hook proposed-message)
              90)
    ;; Open magit and start commit
    (magit-status)
    (magit-commit-create)))

(defun claude-mcp-git-commit-prefill (message &optional directory)
  "Set up a one-shot hook to pre-fill MESSAGE into the next commit in DIRECTORY.
This is for programmatic use (e.g. the merge workflow) where the caller
opens the magit commit editor itself.  Unlike `commit-approve', this does
not read from .git/CLAUDE_PROPOSED_COMMIT_MSG and does not open magit."
  (add-hook 'git-commit-setup-hook
            (claude-mcp-git--make-commit-message-hook message directory)
            90))

(defun claude-mcp-git-has-proposed-commit-p (&optional directory)
  "Return non-nil if DIRECTORY has a pending proposed commit.
DIRECTORY defaults to `default-directory'."
  (let ((path (claude-mcp-git--proposed-commit-path
               (or directory default-directory))))
    (and path (file-exists-p path))))

(defun claude-mcp-git-commit-status ()
  "Check if there's a pending commit proposal.
Reads from .git/CLAUDE_PROPOSED_COMMIT_MSG in the current directory.
Returns the proposal details or nil."
  (let* ((default-directory (or claude-session-cwd default-directory))
         (toplevel (claude-mcp-git--toplevel))
         (default-directory (or toplevel default-directory))
         (proposed-message (claude-mcp-git--read-proposed-commit)))
    (when proposed-message
      (let ((staged-files (ignore-errors
                            (claude-mcp-git--lines "diff" "--cached" "--name-only"))))
        `((status . "pending")
          (directory . ,default-directory)
          (message . ,proposed-message)
          (files . ,(or staged-files '())))))))

;; Global cleanup hook: remove CLAUDE_PROPOSED_COMMIT_MSG after successful commit
(defun claude-mcp-git--post-commit-cleanup ()
  "Remove the proposed commit file after a successful commit.
Added to `git-commit-post-finish-hook'.
`default-directory' is set to the git working tree by `with-editor'."
  (claude-mcp-git--clear-proposed-commit))

(with-eval-after-load 'git-commit
  (add-hook 'git-commit-post-finish-hook #'claude-mcp-git--post-commit-cleanup))

;;;; Direct Commit Operations
;;
;; These operations create commits directly without the propose/approve workflow.
;; Useful for automated workflows where user approval is not required.

(defun claude-mcp-git-commit (message &optional directory no-gpg-sign)
  "Create a commit with MESSAGE.
MESSAGE can be multiline - it will be handled correctly.
If NO-GPG-SIGN is nil, skip GPG signing (default behavior for agent commits).
Pass NO-GPG-SIGN as 'sign to enable GPG signing.
DIRECTORY defaults to claude-session-cwd.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir))
         ;; Default to skipping GPG sign unless explicitly set to 'sign
         (no-sign (not (eq no-gpg-sign 'sign))))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((staged (claude-mcp-git--lines "diff" "--cached" "--name-only")))
      (unless staged
        (error "No files staged for commit"))
      (let ((extra-args (when no-sign '("--no-gpg-sign"))))
        (claude-mcp-git--commit-with-message message extra-args))
      `((status . "committed")
        (message . ,message)
        (files . ,staged)))))

(defun claude-mcp-git-amend (message &optional directory force no-gpg-sign)
  "Amend HEAD commit with MESSAGE.
MESSAGE can be multiline - it will be handled correctly.
Fails if HEAD has been pushed unless FORCE is non-nil.
If NO-GPG-SIGN is nil, skip GPG signing (default behavior for agent commits).
Pass NO-GPG-SIGN as 'sign to enable GPG signing.
DIRECTORY defaults to claude-session-cwd.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir))
         ;; Default to skipping GPG sign unless explicitly set to 'sign
         (no-sign (not (eq no-gpg-sign 'sign))))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    ;; Safety check: is HEAD pushed?
    (unless force
      (let* ((branch (ignore-errors
                       (string-trim (claude-mcp-git--output "symbolic-ref" "--short" "HEAD"))))
             (upstream (when branch
                         (ignore-errors
                           (string-trim (claude-mcp-git--output
                            "rev-parse" "--abbrev-ref" (concat branch "@{upstream}"))))))
             (head-pushed (when upstream
                            (zerop (car (claude-mcp-git--call-git
                                         "merge-base" "--is-ancestor" "HEAD" upstream))))))
        (when head-pushed
          (error "HEAD commit has been pushed to %s. Use force=true to amend anyway" upstream))))
    (let ((extra-args (if no-sign '("--amend" "--no-gpg-sign") '("--amend"))))
      (claude-mcp-git--commit-with-message message extra-args))
    `((status . "amended")
      (message . ,message))))

(defun claude-mcp-git-rebase (onto &optional directory autosquash)
  "Rebase current branch onto ONTO.
If AUTOSQUASH is non-nil, automatically apply fixup/squash commits.
DIRECTORY defaults to claude-session-cwd.
Non-interactive only. Auto-aborts on conflicts.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((args (list "rebase")))
      (when autosquash
        (push "--autosquash" args))
      (push onto args)
      (let ((result (apply #'claude-mcp-git--call-git (nreverse args))))
        (if (zerop (car result))
            `((status . "rebased")
              (onto . ,onto)
              (output . ,(string-trim (cdr result))))
          ;; Rebase failed - likely conflicts
          (claude-mcp-git--output "rebase" "--abort")  ; Clean up
          (error "Rebase failed (aborted automatically):\n%s" (cdr result)))))))

(defun claude-mcp-git-ignore (file &optional directory unignore)
  "Mark FILE as assumed-unchanged so git ignores local modifications.
If UNIGNORE is non-nil, remove the assume-unchanged flag instead.
DIRECTORY defaults to claude-session-cwd.
Useful for temporarily ignoring changes to tracked files.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((flag (if unignore "--no-assume-unchanged" "--assume-unchanged")))
      (claude-mcp-git--output "update-index" flag "--" file)
      `((status . ,(if unignore "unignored" "ignored"))
        (file . ,file)))))

;;;; Additional Git Operations
;;
;; Extended git operations for more comprehensive repository management.

(defun claude-mcp-git-branch (&optional directory)
  "Get branch information for DIRECTORY (or claude-session-cwd).
Returns an alist with :current (current branch name) and :branches (list of all branches).
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((current (string-trim
                    (or (ignore-errors
                          (claude-mcp-git--output "symbolic-ref" "--short" "HEAD"))
                        ;; Detached HEAD
                        (concat "HEAD detached at "
                                (claude-mcp-git--output "rev-parse" "--short" "HEAD")))))
          (branches (claude-mcp-git--lines "branch" "--format=%(refname:short)")))
      `((current . ,current)
        (branches . ,branches)))))

(defun claude-mcp-git-show (revision &optional file directory)
  "Show content of REVISION (commit hash, branch, tag, etc).
If FILE is provided, show only that file's content at REVISION.
DIRECTORY defaults to claude-session-cwd.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (if file
        (claude-mcp-git--output "show" (format "%s:%s" revision file))
      (claude-mcp-git--output "show" "--stat" "--patch" revision))))

(defun claude-mcp-git-blame (file &optional directory revision)
  "Get git blame output for FILE.
DIRECTORY defaults to claude-session-cwd.
REVISION optionally specifies a commit to blame from (default: HEAD).
Returns blame output with commit hash, author, date, and line content.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((args (list "blame" "--date=short")))
      (when revision
        (setq args (append args (list revision))))
      (setq args (append args (list "--" file)))
      (apply #'claude-mcp-git--output args))))

(defun claude-mcp-git-stash-list (&optional directory)
  "List all stashes for DIRECTORY (or claude-session-cwd).
Returns a list of stash entries with index, message, and branch.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (claude-mcp-git--output "stash" "list")))

(defun claude-mcp-git-stash-push (&optional message directory include-untracked)
  "Stash current changes with optional MESSAGE.
MESSAGE can be multiline - it will be handled correctly.
DIRECTORY defaults to claude-session-cwd.
If INCLUDE-UNTRACKED is non-nil, also stash untracked files.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((args (list "stash" "push")))
      (when include-untracked
        (setq args (append args (list "--include-untracked"))))
      (when message
        (setq args (append args (list "-m" message))))
      (apply #'claude-mcp-git--output args))))

(defun claude-mcp-git-stash-pop (&optional stash-ref directory)
  "Pop a stash, applying it to the working directory.
STASH-REF defaults to stash@{0} (most recent).
DIRECTORY defaults to claude-session-cwd.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir))
         (ref (or stash-ref "stash@{0}")))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (claude-mcp-git--output "stash" "pop" ref)))

(defun claude-mcp-git-file-log (file &optional count directory)
  "Get commit history for FILE.
COUNT defaults to 10.  DIRECTORY defaults to claude-session-cwd.
Returns a detailed log with commit hash, author, date, and message.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir))
         (n (or count 10)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (claude-mcp-git--output
     "log" (format "-%d" n)
     "--format=%h %ad %an: %s"
     "--date=short"
     "--follow"
     "--" file)))

(defun claude-mcp-git-checkout-file (file &optional revision directory)
  "Checkout FILE from REVISION (default: HEAD), discarding local changes.
DIRECTORY defaults to claude-session-cwd.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir))
         (rev (or revision "HEAD")))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (claude-mcp-git--output "checkout" rev "--" file)
    (format "Checked out %s from %s" file rev)))

(defun claude-mcp-git-remote (&optional directory)
  "Get remote information for DIRECTORY (or claude-session-cwd).
Returns an alist with remote names and their URLs.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let ((remotes (claude-mcp-git--lines "remote")))
      (mapcar (lambda (remote)
                (let ((url (string-trim
                            (claude-mcp-git--output "remote" "get-url" remote))))
                  (cons remote url)))
              remotes))))

(defun claude-mcp-git-tags (&optional directory)
  "List all tags for DIRECTORY (or claude-session-cwd).
Returns a list of tag names, most recent first.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (claude-mcp-git--lines "tag" "--sort=-creatordate")))

(defun claude-mcp-git-rev-parse (revision &optional directory)
  "Resolve REVISION to a full commit hash.
DIRECTORY defaults to claude-session-cwd.
Useful for resolving branch names, tags, HEAD~N, etc. to actual commits.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (claude-mcp-git--output "rev-parse" revision)))

(defun claude-mcp-git-rebase-interactive (onto actions &optional directory)
  "Perform an interactive rebase onto ONTO with specified ACTIONS.
ACTIONS is a list of alists, each with keys:
  - commit: The commit hash (short or full)
  - action: One of \"pick\", \"reword\", \"edit\", \"squash\", \"fixup\", \"drop\"
  - message: (optional) New commit message for \"reword\" action

The actions are applied in order (oldest to newest, as git rebase expects).
DIRECTORY defaults to claude-session-cwd.

Example ACTIONS:
  (((commit . \"abc123\") (action . \"pick\"))
   ((commit . \"def456\") (action . \"squash\"))
   ((commit . \"ghi789\") (action . \"reword\") (message . \"New message\")))

Returns an alist with status and details.
Auto-aborts on conflicts.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    ;; Validate actions
    (unless (and actions (listp actions))
      (error "ACTIONS must be a non-empty list of commit actions"))
    (dolist (action-spec actions)
      (let ((commit (cdr (assoc 'commit action-spec)))
            (action (cdr (assoc 'action action-spec))))
        (unless commit
          (error "Each action must have a 'commit' key"))
        (unless (member action '("pick" "reword" "edit" "squash" "fixup" "drop"))
          (error "Invalid action '%s'. Must be one of: pick, reword, edit, squash, fixup, drop" action))))
    ;; Create the sequence file content
    (let* ((sequence-content
            (mapconcat
             (lambda (action-spec)
               (let ((commit (cdr (assoc 'commit action-spec)))
                     (action (cdr (assoc 'action action-spec))))
                 (format "%s %s" action commit)))
             actions
             "\n"))
           ;; Create temp files for the sequence editor script and reword messages
           (sequence-file (make-temp-file "claude-rebase-sequence-"))
           (reword-messages (make-temp-file "claude-rebase-reword-"))
           (editor-script (make-temp-file "claude-rebase-editor-" nil ".sh")))
      (unwind-protect
          (progn
            ;; Write sequence content
            (with-temp-file sequence-file
              (insert sequence-content))
            ;; Collect reword messages into a file (commit-hash:message format)
            (with-temp-file reword-messages
              (dolist (action-spec actions)
                (when (and (equal (cdr (assoc 'action action-spec)) "reword")
                           (cdr (assoc 'message action-spec)))
                  (insert (format "%s:%s\n"
                                  (cdr (assoc 'commit action-spec))
                                  ;; Base64 encode to handle newlines in messages
                                  (base64-encode-string
                                   (encode-coding-string
                                    (cdr (assoc 'message action-spec))
                                    'utf-8)
                                   t))))))
            ;; Create the editor script that handles both sequence and commit messages
            (with-temp-file editor-script
              (insert "#!/usr/bin/env bash\n")
              (insert "# Claude rebase editor script\n")
              (insert "TARGET_FILE=\"$1\"\n")
              (insert (format "SEQUENCE_FILE='%s'\n" sequence-file))
              (insert (format "REWORD_FILE='%s'\n" reword-messages))
              (insert "\n")
              (insert "# Check if this is the rebase todo file (git-rebase-todo)\n")
              (insert "if [[ \"$TARGET_FILE\" == *\"git-rebase-todo\"* ]]; then\n")
              (insert "  # This is the sequence/todo file - replace with our sequence\n")
              (insert "  cat \"$SEQUENCE_FILE\" > \"$TARGET_FILE\"\n")
              (insert "  exit 0\n")
              (insert "fi\n")
              (insert "\n")
              (insert "# This is a commit message file (COMMIT_EDITMSG for reword/squash)\n")
              (insert "# For reword, we replace with the provided message\n")
              (insert "# For squash, we let git's default message through (don't modify)\n")
              (insert "CURRENT_COMMIT=$(git rev-parse --short HEAD 2>/dev/null || echo '')\n")
              (insert "if [ -n \"$CURRENT_COMMIT\" ] && [ -s \"$REWORD_FILE\" ]; then\n")
              (insert "  # Look for a reword message for this commit\n")
              (insert "  while IFS=: read -r COMMIT MSG; do\n")
              (insert "    # Check if current commit starts with the stored commit hash\n")
              (insert "    if [[ \"$CURRENT_COMMIT\" == \"$COMMIT\"* ]] || [[ \"$COMMIT\" == \"$CURRENT_COMMIT\"* ]]; then\n")
              (insert "      # Decode and write the message\n")
              (insert "      echo \"$MSG\" | base64 -d > \"$TARGET_FILE\"\n")
              (insert "      exit 0\n")
              (insert "    fi\n")
              (insert "  done < \"$REWORD_FILE\"\n")
              (insert "fi\n")
              (insert "# No reword message found - leave file unchanged (for squash/fixup)\n"))
            (set-file-modes editor-script #o755)
            ;; Run the rebase with our custom editor
            (let* ((process-environment
                    (cons (format "GIT_SEQUENCE_EDITOR=%s" editor-script)
                          (cons (format "GIT_EDITOR=%s" editor-script)
                                process-environment)))
                   (result (claude-mcp-git--call-git "rebase" "-i" onto)))
              (if (zerop (car result))
                  `((status . "rebased")
                    (onto . ,onto)
                    (actions . ,(length actions))
                    (output . ,(string-trim (cdr result))))
                ;; Rebase failed - abort and report
                (claude-mcp-git--call-git "rebase" "--abort")
                (error "Interactive rebase failed (aborted automatically):\n%s" (cdr result)))))
        ;; Cleanup temp files
        (when (file-exists-p sequence-file)
          (delete-file sequence-file))
        (when (file-exists-p reword-messages)
          (delete-file reword-messages))
        (when (file-exists-p editor-script)
          (delete-file editor-script))))))

(defun claude-mcp-git-rebase-interactive-get-commits (base &optional directory)
  "Get the list of commits from HEAD back to BASE for interactive rebase planning.
Returns a list of alists with commit info (hash, subject, author, date).
Commits are returned in rebase order (oldest first).
DIRECTORY defaults to claude-session-cwd.
Works correctly in git worktrees."
  (let* ((start-dir (or directory claude-session-cwd default-directory))
         (default-directory (or (claude-mcp-git--toplevel start-dir) start-dir)))
    (unless (claude-mcp-git--toplevel)
      (error "Not in a git repository: %s" start-dir))
    (let* ((range (format "%s..HEAD" base))
           ;; Get commits in reverse order (oldest first, like rebase shows them)
           (output (claude-mcp-git--output
                    "log" "--reverse" "--format=%H|%h|%s|%an|%ad" "--date=short" range))
           (lines (if (string-empty-p output) '() (split-string output "\n" t))))
      (mapcar (lambda (line)
                (let ((parts (split-string line "|")))
                  `((hash . ,(nth 0 parts))
                    (short_hash . ,(nth 1 parts))
                    (subject . ,(nth 2 parts))
                    (author . ,(nth 3 parts))
                    (date . ,(nth 4 parts)))))
              lines))))

;;;; MCP Tool Definitions
;;
;; Tool definitions for the MCP (Model Context Protocol) interface.

(claude-mcp-deftool git-status
  "Get current git status including staged, unstaged, and untracked files. Returns branch name and file lists."
  :function #'claude-mcp-git-status
  :safe t
  :args ((directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool git-stage
  "Stage files for commit. Takes a list of file paths relative to the repository root."
  :function #'claude-mcp-git-stage
  :safe nil
  :args ((files array :required "Array of file paths to stage")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool git-unstage
  "Unstage files (remove from staging area). Takes a list of file paths."
  :function #'claude-mcp-git-unstage
  :safe nil
  :args ((files array :required "Array of file paths to unstage")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool git-diff
  "Get git diff output. Can get diff for a specific file or all changes. Use staged=true for staged changes."
  :function #'claude-mcp-git-diff
  :safe t
  :args ((file string "Specific file to diff (default: all files)")
         (directory string "Git repository directory (default: session working directory)")
         (staged boolean "If true, show staged diff; otherwise show unstaged diff")))

(claude-mcp-deftool git-log
  "Get recent git commit log entries."
  :function #'claude-mcp-git-log
  :safe t
  :args ((count integer "Number of log entries to return (default: 5)")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool git-commit-propose
  "Propose a commit for user approval. The user must call git-commit-approve to actually create the commit. This allows the user to review and sign the commit with their GPG key."
  :function #'claude-mcp-git-commit-propose
  :safe nil
  :args ((message string :required "The commit message to propose")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool git-commit-status
  "Check if there's a pending commit proposal awaiting user approval."
  :function #'claude-mcp-git-commit-status
  :safe t
  :args ())

(claude-mcp-deftool git-commit
  "Create a git commit with staged changes. Supports multiline commit messages. Use git-commit-propose for commits requiring user approval/GPG signing."
  :function #'claude-mcp-git-commit
  :safe nil
  :args ((message string :required "Commit message (can be multiline)")
         (directory string "Git repository directory (default: session working directory)")
         (no_gpg_sign boolean "Skip GPG signing (default: true for agent commits)")))

(claude-mcp-deftool git-amend
  "Amend the HEAD commit with a new message. Supports multiline commit messages. Fails if HEAD is already pushed unless force=true."
  :function #'claude-mcp-git-amend
  :safe nil
  :args ((message string :required "New commit message (can be multiline)")
         (directory string "Git repository directory (default: session working directory)")
         (force boolean "Allow amending pushed commits (dangerous, default: false)")
         (no_gpg_sign boolean "Skip GPG signing (default: true for agent commits)")))

(claude-mcp-deftool git-rebase
  "Rebase current branch onto another branch/commit. Non-interactive only. Auto-aborts on conflicts."
  :function #'claude-mcp-git-rebase
  :safe nil
  :args ((onto string :required "Branch or commit to rebase onto")
         (directory string "Git repository directory (default: session working directory)")
         (autosquash boolean "Apply fixup!/squash! commits automatically (default: false)")))

(claude-mcp-deftool git-rebase-interactive
  "Perform an interactive rebase with specified actions for each commit.
Use git-rebase-interactive-get-commits first to get the list of commits to rebase.
Then specify an action for each commit: pick, reword, edit, squash, fixup, or drop.

Example workflow:
1. Call git-rebase-interactive-get-commits with base='main' to see commits
2. Call git-rebase-interactive with actions specifying what to do with each commit

Actions array format: [{commit: 'abc123', action: 'pick'}, {commit: 'def456', action: 'squash'}, ...]
For reword action, include message: {commit: 'abc', action: 'reword', message: 'New message'}

Auto-aborts on conflicts."
  :function #'claude-mcp-git-rebase-interactive
  :safe nil
  :args ((onto string :required "Branch or commit to rebase onto (e.g., 'main', 'HEAD~3')")
         (actions array :required "Array of action objects. Each has: commit (hash), action (pick/reword/edit/squash/fixup/drop), and optionally message (for reword)")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool git-rebase-interactive-get-commits
  "Get the list of commits that would be included in an interactive rebase.
Returns commits from BASE to HEAD in rebase order (oldest first).
Use this to see what commits you're working with before calling git-rebase-interactive.

Each commit includes: hash (full), short_hash, subject (first line of message), author, date."
  :function #'claude-mcp-git-rebase-interactive-get-commits
  :safe t
  :args ((base string :required "Base commit/branch to rebase from (e.g., 'main', 'HEAD~5')")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool git-ignore
  "Ignore local changes to a tracked file using git update-index --assume-unchanged. Use unignore=true to restore tracking."
  :function #'claude-mcp-git-ignore
  :safe nil
  :args ((file string :required "File path to ignore or unignore")
         (directory string "Git repository directory (default: session working directory)")
         (unignore boolean "If true, restore tracking of the file (default: false)")))

(claude-mcp-deftool git-branch
  "Get branch information including current branch and list of all branches."
  :function #'claude-mcp-git-branch
  :safe t
  :args ((directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool git-show
  "Show content of a commit, or a specific file at a commit. Use this to inspect commit details or historical file contents."
  :function #'claude-mcp-git-show
  :safe t
  :args ((revision string :required "Commit hash, branch name, tag, or other revision (e.g., 'HEAD~1', 'main', 'v1.0.0')")
         (file string "Optional file path to show only that file's content at the revision")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool git-blame
  "Get git blame output for a file, showing who last modified each line."
  :function #'claude-mcp-git-blame
  :safe t
  :args ((file string :required "File path to blame")
         (directory string "Git repository directory (default: session working directory)")
         (revision string "Optional revision to blame from (default: HEAD)")))

(claude-mcp-deftool git-stash-list
  "List all stashes in the repository."
  :function #'claude-mcp-git-stash-list
  :safe t
  :args ((directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool git-stash-push
  "Stash current changes. Useful for temporarily saving work in progress."
  :function #'claude-mcp-git-stash-push
  :safe nil
  :args ((message string "Optional message to describe the stash")
         (directory string "Git repository directory (default: session working directory)")
         (include_untracked boolean "If true, also stash untracked files")))

(claude-mcp-deftool git-stash-pop
  "Pop the most recent stash (or specified stash), applying it to the working directory."
  :function #'claude-mcp-git-stash-pop
  :safe nil
  :args ((stash_ref string "Stash reference (default: stash@{0})")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool git-file-log
  "Get commit history for a specific file. Useful for understanding file evolution."
  :function #'claude-mcp-git-file-log
  :safe t
  :args ((file string :required "File path to get history for")
         (count integer "Number of commits to show (default: 10)")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool git-checkout-file
  "Checkout a file from a specific revision, discarding local changes to that file."
  :function #'claude-mcp-git-checkout-file
  :safe nil
  :args ((file string :required "File path to checkout")
         (revision string "Revision to checkout from (default: HEAD)")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool git-remote
  "Get information about configured git remotes (names and URLs)."
  :function #'claude-mcp-git-remote
  :safe t
  :args ((directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool git-tags
  "List all tags in the repository, sorted by creation date (most recent first)."
  :function #'claude-mcp-git-tags
  :safe t
  :args ((directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool git-rev-parse
  "Resolve a revision to its full commit hash. Useful for resolving branch names, tags, HEAD~N, etc."
  :function #'claude-mcp-git-rev-parse
  :safe t
  :args ((revision string :required "Revision to resolve (e.g., 'HEAD', 'main~3', 'v1.0.0')")
         (directory string "Git repository directory (default: session working directory)")))

(provide 'claude-mcp-git)
;;; claude-mcp-git.el ends here
