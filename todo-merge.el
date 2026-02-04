;;; todo-merge.el --- Per-project merge/approval workflows for TODO worktrees -*- lexical-binding: t; -*-

;; This file is part of Claude Agent / Claudemacs.

;;; Commentary:

;; Provides per-project customizable merge workflows for TODO worktrees.
;; Two built-in workflows:
;;
;; 1. `local-rebase' - For repos managed locally (e.g. claude-agent itself):
;;    - Rebase worktree branch onto latest main
;;    - Open magit-status for review
;;    - Fast-forward merge into main
;;    - Clean up worktree + branch
;;
;; 2. `github-pr' - For GitHub-based repos:
;;    - Fetch upstream and rebase onto default branch
;;    - Push and open magit-status for review
;;    - Create PR via forge or gh CLI
;;    - Clean up worktree + branch after merge
;;
;; Configure per-project via `org-roam-todo-merge-workflows' or
;; `.dir-locals.el' with `org-roam-todo-merge-workflow'.

;;; Code:

(require 'cl-lib)
(require 'claude-mcp-magit)
;; Forward declarations

(declare-function magit-status "magit")
(declare-function magit-commit-create "magit-commit")
(declare-function magit-get "magit-git")
(declare-function magit-get-current-branch "magit-git")
(declare-function forge-create-pullreq "forge-commands")
(declare-function claude-mcp-magit-commit-prefill "claude-mcp-magit")
(declare-function org-roam-todo--set-property "todo")
(declare-function org-roam-todo--kill-claude-session "todo")
(declare-function org-roam-todo--kill-worktree-buffers "todo")
(declare-function org-roam-todo--remove-worktree "todo")
(declare-function org-roam-todo--delete-branch "todo")
(declare-function org-roam-todo--branch-exists-p "todo")
(declare-function org-roam-todo--worktree-exists-p "todo")
(declare-function org-roam-todo-list-refresh-all "todo")
;;;; Customization

(defgroup org-roam-todo-merge nil
  "Per-project merge/approval workflows for TODO worktrees."
  :group 'org-roam-todo)

(defcustom org-roam-todo-merge-workflows nil
  "Alist mapping project names to merge workflow symbols.
Each entry is (PROJECT-NAME . WORKFLOW) where WORKFLOW is one of:
  `local-rebase' - Rebase onto main, review, ff-merge locally
  `github-pr'    - Rebase onto default branch, push, create PR
  A function      - Custom function called with (todo) plist

When nil or no matching entry, defaults to `github-pr'.

Example:
  \\='((\"claude-agent\" . local-rebase)
    (\"my-oss-project\" . github-pr))"
  :type '(alist :key-type string :value-type symbol)
  :group 'org-roam-todo-merge)

(defcustom org-roam-todo-merge-workflow 'github-pr
  "Default merge workflow for the current project.
Can be overridden per-project via `.dir-locals.el'.
See `org-roam-todo-merge-workflows' for possible values."
  :type '(choice (const :tag "Local rebase + ff-merge" local-rebase)
                 (const :tag "GitHub PR via forge/gh" github-pr)
                 function)
  :safe #'symbolp
  :group 'org-roam-todo-merge)

(defcustom org-roam-todo-merge-main-branch nil
  "Main branch name for merge workflows.
When nil, auto-detects from `origin/HEAD' or falls back to \"main\".
Can be set per-project via `.dir-locals.el'."
  :type '(choice (const :tag "Auto-detect" nil) string)
  :safe #'stringp
  :group 'org-roam-todo-merge)

(defcustom org-roam-todo-merge-cleanup-after t
  "Whether to clean up worktree and branch after successful merge.
When non-nil, the worktree is removed and branch deleted after merge."
  :type 'boolean
  :group 'org-roam-todo-merge)

;;;; Utility Functions

(defvar org-roam-todo-merge--pending-continuation nil
  "Continuation function to call after magit commit finishes.
Set by `org-roam-todo-merge--commit-then-continue'.")

(defvar org-roam-todo-merge--pre-commit-head nil
  "HEAD ref before the commit, used to detect when commit completes.")

(defvar org-roam-todo-merge--poll-timer nil
  "Timer used to poll for commit completion.")

(defvar org-roam-todo-merge--poll-attempts 0
  "Number of poll attempts so far.")

(defun org-roam-todo-merge--poll-for-commit ()
  "Check if HEAD has changed since pre-commit.  Called by timer."
  (setq org-roam-todo-merge--poll-attempts
        (1+ org-roam-todo-merge--poll-attempts))
  (let ((current-head (string-trim
                       (shell-command-to-string "git rev-parse HEAD")))
        (max-attempts 60))  ; 30s at 0.5s intervals
    (cond
     ;; Commit detected - HEAD changed
     ((not (equal current-head org-roam-todo-merge--pre-commit-head))
      (when org-roam-todo-merge--poll-timer
        (cancel-timer org-roam-todo-merge--poll-timer)
        (setq org-roam-todo-merge--poll-timer nil))
      (message "[merge] Commit detected, continuing merge workflow...")
      (when org-roam-todo-merge--pending-continuation
        (let ((cont org-roam-todo-merge--pending-continuation))
          (setq org-roam-todo-merge--pending-continuation nil)
          (run-at-time 0.5 nil cont))))
     ;; Timed out
     ((>= org-roam-todo-merge--poll-attempts max-attempts)
      (when org-roam-todo-merge--poll-timer
        (cancel-timer org-roam-todo-merge--poll-timer)
        (setq org-roam-todo-merge--poll-timer nil))
      (message "[merge] Timed out waiting for commit after 30s. Run merge workflow again (m) after committing."))
     ;; Still waiting
     (t
      (when (= 0 (% org-roam-todo-merge--poll-attempts 4))
        (message "[merge] Still waiting for commit to complete (%ds)..."
                 (/ org-roam-todo-merge--poll-attempts 2)))))))

(defun org-roam-todo-merge--wait-for-commit ()
  "Poll for HEAD to change (commit to complete), then run continuation.
This handles GPG signing which can take longer than the 1s timeout
in `git-commit-post-finish-hook'.  Polls every 0.5s for up to 30s."
  (setq org-roam-todo-merge--poll-attempts 0)
  (setq org-roam-todo-merge--poll-timer
        (run-with-timer 0.5 0.5 #'org-roam-todo-merge--poll-for-commit)))

(defun org-roam-todo-merge--post-finish-hook ()
  "Hook run when user finishes the commit message editor.
Uses `with-editor-post-finish-hook' which fires when C-c C-c is pressed,
before the actual git commit process.  We then poll for the commit to
complete (which may involve GPG signing)."
  (remove-hook 'with-editor-post-finish-hook #'org-roam-todo-merge--post-finish-hook)
  (message "[merge] Editor finished, waiting for commit (GPG signing may take a moment)...")
  (org-roam-todo-merge--wait-for-commit))

(defun org-roam-todo-merge--commit-then-continue (worktree-path commit-message continuation)
  "Open magit commit editor in WORKTREE-PATH with COMMIT-MESSAGE pre-filled.
After the user finishes the commit (with GPG signing via `C-c C-c'),
calls CONTINUATION with no arguments to continue the merge workflow.
Handles slow GPG signing by polling for HEAD to change."
  (setq org-roam-todo-merge--pending-continuation continuation)
  ;; Record current HEAD so we can detect when the commit lands
  (let ((default-directory worktree-path))
    (setq org-roam-todo-merge--pre-commit-head
          (string-trim (shell-command-to-string "git rev-parse HEAD"))))
  ;; Use with-editor-post-finish-hook (fires when C-c C-c is pressed)
  ;; rather than git-commit-post-finish-hook (which times out after 1s)
  (add-hook 'with-editor-post-finish-hook #'org-roam-todo-merge--post-finish-hook)
  ;; Pre-fill the commit message using worktree-scoped one-shot hook
  (when commit-message
    (claude-mcp-magit-commit-prefill commit-message worktree-path))
  ;; Open magit-status and start commit
  (let ((default-directory worktree-path))
    (magit-status worktree-path)
    (magit-commit-create)))

(defun org-roam-todo-merge--detect-main-branch (project-root)
  "Detect the main branch name for PROJECT-ROOT.
Checks `org-roam-todo-merge-main-branch', then origin/HEAD, then
falls back to \"main\"."
  (or org-roam-todo-merge-main-branch
      (let ((default-directory project-root))
        ;; Try to get the default branch from origin/HEAD
        (let ((origin-head (string-trim
                            (shell-command-to-string
                             "git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null"))))
          (if (and (not (string-empty-p origin-head))
                   (not (string-match-p "fatal\\|error" origin-head)))
              ;; origin/HEAD is like "origin/main" - strip the "origin/" prefix
              (replace-regexp-in-string "^origin/" "" origin-head)
            "main")))))

(defun org-roam-todo-merge--git-run (directory &rest args)
  "Run git with ARGS in DIRECTORY, returning (exit-code . output)."
  (let ((default-directory directory))
    (with-temp-buffer
      (let ((exit-code (apply #'call-process "git" nil t nil args)))
        (cons exit-code (buffer-string))))))

(defun org-roam-todo-merge--git-run! (directory &rest args)
  "Run git with ARGS in DIRECTORY.  Signal error on failure.
Returns output string on success."
  (let ((result (apply #'org-roam-todo-merge--git-run directory args)))
    (unless (= 0 (car result))
      (error "git %s failed (exit %d): %s"
             (string-join args " ") (car result) (cdr result)))
    (cdr result)))

(defun org-roam-todo-merge--get-workflow (todo)
  "Get the merge workflow for TODO.
Checks `org-roam-todo-merge-workflows' alist first, then
loads .dir-locals.el from the project root to check for
`org-roam-todo-merge-workflow', finally falls back to `github-pr'."
  (let* ((project (plist-get todo :project))
         (project-root (plist-get todo :project-root)))
    (or
     ;; Check the alist first
     (cdr (assoc project org-roam-todo-merge-workflows))
     ;; Check .dir-locals.el in the project root
     (when project-root
       (with-temp-buffer
         (setq default-directory (file-name-as-directory project-root))
         (hack-dir-local-variables-non-file-buffer)
         (when (local-variable-p 'org-roam-todo-merge-workflow)
           (buffer-local-value 'org-roam-todo-merge-workflow (current-buffer)))))
     ;; Default
     'github-pr)))

(defun org-roam-todo-merge--load-project-locals (project-root)
  "Load .dir-locals.el variables from PROJECT-ROOT into current context.
Loads merge-related variables like `org-roam-todo-merge-main-branch'."
  (when project-root
    (with-temp-buffer
      (setq default-directory (file-name-as-directory project-root))
      (hack-dir-local-variables-non-file-buffer)
      (when (local-variable-p 'org-roam-todo-merge-main-branch)
        (setq org-roam-todo-merge-main-branch
              (buffer-local-value 'org-roam-todo-merge-main-branch (current-buffer)))))))

(defun org-roam-todo-merge--cleanup (todo)
  "Clean up worktree and branch for TODO after successful merge.
Kills Claude session, file buffers, removes worktree, deletes branch,
clears properties, and marks TODO as done."
  (let ((file (plist-get todo :file))
        (project-root (plist-get todo :project-root))
        (worktree-path (plist-get todo :worktree-path))
        (branch-name (plist-get todo :worktree-branch)))
    ;; Kill Claude session
    (when worktree-path
      (org-roam-todo--kill-claude-session worktree-path)
      (org-roam-todo--kill-worktree-buffers worktree-path)
      ;; Remove worktree
      (when (org-roam-todo--worktree-exists-p worktree-path)
        (let ((result (org-roam-todo--remove-worktree project-root worktree-path t)))
          (unless (= 0 result)
            (message "Warning: Failed to remove worktree %s" worktree-path)))))
    ;; Delete branch
    (when (and branch-name
               (org-roam-todo--branch-exists-p project-root branch-name))
      (let ((result (org-roam-todo--delete-branch project-root branch-name)))
        (unless (= 0 result)
          ;; Force delete if regular delete fails
          (org-roam-todo--delete-branch project-root branch-name t))))
    ;; Update TODO properties and remove commit message section
    (with-current-buffer (find-file-noselect file)
      (org-roam-todo--set-property "WORKTREE_PATH" nil)
      (org-roam-todo--set-property "WORKTREE_BRANCH" nil)
      (org-roam-todo--set-property "STATUS" "done")
      ;; Remove Commit Message section if present
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\* Commit Message" nil t)
          (beginning-of-line)
          (let ((start (point))
                (end (save-excursion
                       (if (re-search-forward "^\\*\\* " nil t)
                           (match-beginning 0)
                         (point-max)))))
            (delete-region start end))))
      (save-buffer))
    (message "Cleaned up worktree and marked TODO as done")
    ;; Refresh TODO list buffers to reflect the status change
    (org-roam-todo-list-refresh-all)))

;;;; Local Rebase Workflow

(defun org-roam-todo-merge--local-rebase (todo)
  "Run the local-rebase merge workflow for TODO.
Opens the magit commit editor for the user to review, edit, and
GPG-sign the commit.  Then rebases onto latest main and
fast-forward merges into main.

Steps:
1. Open magit commit editor for review/signing (with pre-filled message)
2. Rebase onto latest main (ensures ff-merge will succeed)
3. Open magit-status for review
4. Fast-forward merge into main
5. Clean up worktree + branch, mark done."
  (let* ((project-root (plist-get todo :project-root))
         (worktree-path (plist-get todo :worktree-path))
         (branch-name (plist-get todo :worktree-branch))
         (commit-message (org-roam-todo--read-commit-message (plist-get todo :file))))
    (unless worktree-path
      (user-error "TODO has no worktree path"))
    (unless branch-name
      (user-error "TODO has no worktree branch"))
    (unless (org-roam-todo--worktree-exists-p worktree-path)
      (user-error "Worktree does not exist: %s" worktree-path))

    ;; Load project-specific settings
    (org-roam-todo-merge--load-project-locals project-root)
    (let ((main-branch (org-roam-todo-merge--detect-main-branch project-root)))

      ;; Step 1: If staged changes, open magit commit editor for user review
      (let ((staged (string-trim (cdr (org-roam-todo-merge--git-run
                                       worktree-path "diff" "--cached" "--name-only")))))
        (if (not (string-empty-p staged))
            ;; Has staged changes - open commit editor, continue after commit
            (org-roam-todo-merge--commit-then-continue
             worktree-path commit-message
             (lambda ()
               (org-roam-todo-merge--local-rebase-finish
                todo project-root worktree-path branch-name main-branch)))
          ;; No staged changes - already committed, go straight to merge
          (org-roam-todo-merge--local-rebase-finish
           todo project-root worktree-path branch-name main-branch))))))

(defun org-roam-todo-merge--local-rebase-finish
    (todo project-root worktree-path branch-name main-branch)
  "Finish local-rebase workflow: rebase onto main, review in magit, ff-merge, cleanup.
Rebases the worktree branch onto the latest main before attempting the
fast-forward merge, ensuring the merge will succeed even if main has
moved forward since the agent's initial rebase."
  ;; Step 2: Rebase onto latest main before merge
  (message "Rebasing %s onto %s..." branch-name main-branch)
  (let ((rebase-result (org-roam-todo-merge--git-run
                        worktree-path "rebase" "--autostash" main-branch)))
    (unless (= 0 (car rebase-result))
      ;; Abort the failed rebase
      (org-roam-todo-merge--git-run worktree-path "rebase" "--abort")
      (user-error "Rebase of %s onto %s failed (conflicts).\n%s\nPlease resolve manually or send the agent back to fix conflicts"
                  branch-name main-branch (cdr rebase-result))))
  (message "Rebase successful")

  ;; Step 3: Open magit-status for review
  (message "Opening magit-status for review in worktree...")
  (let ((default-directory worktree-path))
    (magit-status worktree-path))

  ;; Step 4: Ask user to confirm merge after review
  (when (yes-or-no-p (format "Fast-forward merge '%s' into %s? " branch-name main-branch))
    ;; Perform ff-merge in the main repo
    (message "Performing fast-forward merge...")
    (org-roam-todo-merge--git-run! project-root
                                   "merge" "--ff-only" branch-name)
    (message "Successfully merged %s into %s" branch-name main-branch)
    ;; Refresh TODO list to show merge status
    (org-roam-todo-list-refresh-all)

    ;; Step 5: Cleanup
    (when org-roam-todo-merge-cleanup-after
      (when (yes-or-no-p "Clean up worktree and mark TODO as done? ")
        (org-roam-todo-merge--cleanup todo)))))

;;;; GitHub PR Workflow

(defun org-roam-todo-merge--github-pr (todo)
  "Run the GitHub PR merge workflow for TODO.
If there are staged changes, opens the magit commit editor with the
agent's proposed commit message pre-filled.  After the user commits
\(with GPG signing via `C-c C-c'), continues with rebase, push, and PR.

Steps:
1. If staged changes: open magit commit editor for review/signing
2. Fetch origin and rebase onto default branch
3. Force-push with lease
4. Open magit-status for review
5. Create PR via forge or gh CLI."
  (let* ((project-root (plist-get todo :project-root))
         (worktree-path (plist-get todo :worktree-path))
         (branch-name (plist-get todo :worktree-branch))
         (title (plist-get todo :title))
         (commit-message (org-roam-todo--read-commit-message (plist-get todo :file))))
    (unless worktree-path
      (user-error "TODO has no worktree path"))
    (unless branch-name
      (user-error "TODO has no worktree branch"))
    (unless (org-roam-todo--worktree-exists-p worktree-path)
      (user-error "Worktree does not exist: %s" worktree-path))

    ;; Load project-specific settings
    (org-roam-todo-merge--load-project-locals project-root)
    (let ((main-branch (org-roam-todo-merge--detect-main-branch project-root)))

      ;; Step 1: If staged changes, open magit commit editor for user review
      (let ((staged (string-trim (cdr (org-roam-todo-merge--git-run
                                       worktree-path "diff" "--cached" "--name-only")))))
        (if (not (string-empty-p staged))
            ;; Has staged changes - open commit editor, continue after commit
            (org-roam-todo-merge--commit-then-continue
             worktree-path commit-message
             (lambda ()
               (org-roam-todo-merge--github-pr-continue
                todo project-root worktree-path branch-name main-branch
                title commit-message)))
          ;; No staged changes - continue directly
          (org-roam-todo-merge--github-pr-continue
           todo project-root worktree-path branch-name main-branch
           title commit-message))))))

(defun org-roam-todo-merge--github-pr-continue
    (todo project-root worktree-path branch-name main-branch title commit-message)
  "Continue github-pr workflow after commit.
Fetches, rebases, pushes, opens magit for review, and creates PR."
  ;; Step 2: Fetch and rebase
  (message "Fetching origin...")
  (org-roam-todo-merge--git-run! project-root "fetch" "origin")

  (message "Rebasing %s onto origin/%s..." branch-name main-branch)
  (let ((result (org-roam-todo-merge--git-run
                 worktree-path "rebase" (format "origin/%s" main-branch))))
    (unless (= 0 (car result))
      (org-roam-todo-merge--git-run worktree-path "rebase" "--abort")
      (user-error "Rebase failed (conflicts).  Aborted rebase.\n%s\nPlease resolve manually or notify the agent"
                  (cdr result))))

  ;; Step 3: Push
  (message "Pushing %s..." branch-name)
  (org-roam-todo-merge--git-run! worktree-path
                                 "push" "--force-with-lease" "origin" branch-name)

  ;; Step 4: Open magit-status for review
  (message "Opening magit-status for review in worktree...")
  (let ((default-directory worktree-path))
    (magit-status worktree-path))

  ;; Step 5: Create PR
  (when (yes-or-no-p (format "Create PR for '%s' -> %s? " branch-name main-branch))
    (org-roam-todo-merge--create-pr project-root worktree-path
                                    branch-name main-branch
                                    title commit-message)))

(defun org-roam-todo-merge--create-pr (project-root worktree-path
                                        branch-name main-branch
                                        title &optional body)
  "Create a GitHub PR from BRANCH-NAME to MAIN-BRANCH.
Tries forge first, falls back to gh CLI.
PROJECT-ROOT is the main repo path, WORKTREE-PATH is the worktree,
TITLE is the PR title, BODY is the optional PR body (e.g. agent's commit message)."
  (cond
   ;; Try forge if available
   ((and (featurep 'forge)
         (fboundp 'forge-create-pullreq))
    (message "Creating PR via forge...")
    (let ((default-directory project-root))
      (forge-create-pullreq branch-name main-branch))
    (message "PR creation initiated via forge"))

   ;; Fall back to gh CLI
   ((executable-find "gh")
    (message "Creating PR via gh CLI...")
    (let ((default-directory worktree-path))
      (let ((output (shell-command-to-string
                     (format "gh pr create --title %s --base %s --head %s %s 2>&1"
                             (shell-quote-argument (or title branch-name))
                             (shell-quote-argument main-branch)
                             (shell-quote-argument branch-name)
                             (if body
                                 (format "--body %s" (shell-quote-argument body))
                               "--fill")))))
        (if (string-match-p "https://github.com" output)
            (progn
              (message "PR created: %s" (string-trim output))
              ;; Try to open in browser
              (when (yes-or-no-p "Open PR in browser? ")
                (browse-url (string-trim output))))
          (message "gh pr create output: %s" output)))))

   ;; No forge or gh available
   (t
    (message "Neither forge nor gh CLI available.  Push complete - create PR manually.")
    (message "Branch '%s' has been pushed to origin" branch-name))))

;;;; Entry Point

;;;###autoload
(defun org-roam-todo-merge-run (todo)
  "Run the configured merge workflow for TODO plist.
Dispatches to the appropriate workflow based on project configuration."
  (let ((workflow (org-roam-todo-merge--get-workflow todo)))
    (pcase workflow
      ('local-rebase (org-roam-todo-merge--local-rebase todo))
      ('github-pr (org-roam-todo-merge--github-pr todo))
      ((pred functionp) (funcall workflow todo))
      (_ (user-error "Unknown merge workflow: %s" workflow)))))

(provide 'todo-merge)
;;; todo-merge.el ends here
