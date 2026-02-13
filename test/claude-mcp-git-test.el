;;; claude-mcp-git-test.el --- Tests for MCP git operations -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;; Unit tests for git operations in claude-mcp-git.el.
;; Covers: status, stage, unstage, diff, log, commit, amend, and more.
;;
;; These tests create temporary git repositories to test git operations
;; in isolation. Each test gets a fresh repository.
;;
;; Run with:
;;   emacs -batch -l ert -l test/claude-mcp-git-test.el -f ert-run-tests-batch

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load test helper (sets up load path)
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'test-helper)

;; Load only what we need - avoid pulling in claude-mcp which has heavy dependencies
(require 'claude-mcp-registry)
(require 'claude-mcp-git)

;; Define claude-session-cwd for tests (normally defined in claude-mcp.el)
(defvar claude-session-cwd nil
  "The working directory for the current Claude session.")

;;; Test Utilities

(defvar claude-mcp-git-test--temp-dirs nil
  "List of temporary directories to clean up after tests.")

(defun claude-mcp-git-test--create-temp-repo ()
  "Create a temporary git repository and return its path.
The repository is initialized with git init and has initial config set."
  (let* ((temp-dir (make-temp-file "claude-git-test-" t))
         (default-directory temp-dir))
    (push temp-dir claude-mcp-git-test--temp-dirs)
    ;; Initialize git repo
    (call-process "git" nil nil nil "init")
    (call-process "git" nil nil nil "config" "user.email" "test@example.com")
    (call-process "git" nil nil nil "config" "user.name" "Test User")
    ;; Disable GPG signing for tests
    (call-process "git" nil nil nil "config" "commit.gpgsign" "false")
    temp-dir))

(defun claude-mcp-git-test--cleanup-temp-dirs ()
  "Clean up all temporary directories created during tests."
  (dolist (dir claude-mcp-git-test--temp-dirs)
    (when (file-directory-p dir)
      (delete-directory dir t)))
  (setq claude-mcp-git-test--temp-dirs nil))

(defun claude-mcp-git-test--create-file (repo-dir filename content)
  "Create a file FILENAME in REPO-DIR with CONTENT."
  (let ((file-path (expand-file-name filename repo-dir)))
    (make-directory (file-name-directory file-path) t)
    (with-temp-file file-path
      (insert content))
    file-path))

(defun claude-mcp-git-test--commit (repo-dir message)
  "Create a commit in REPO-DIR with MESSAGE."
  (let ((default-directory repo-dir))
    (call-process "git" nil nil nil "commit" "-m" message "--allow-empty")))

(defun claude-mcp-git-test--stage-file (repo-dir filename)
  "Stage FILENAME in REPO-DIR."
  (let ((default-directory repo-dir))
    (call-process "git" nil nil nil "add" filename)))

(defmacro claude-mcp-git-test-with-repo (&rest body)
  "Execute BODY with a temporary git repository.
The variable `repo-dir' is bound to the repository path."
  (declare (indent 0))
  `(let ((repo-dir (claude-mcp-git-test--create-temp-repo))
         (claude-session-cwd nil))  ; Clear session cwd
     (unwind-protect
         (let ((default-directory repo-dir))
           ,@body)
       (claude-mcp-git-test--cleanup-temp-dirs))))

;;; ============================================================
;;; Helper Function Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test--toplevel ()
  "Test git toplevel detection."
  :tags '(:unit :git :helper)
  (claude-mcp-git-test-with-repo
    (should (string= (file-name-as-directory repo-dir)
                     (claude-mcp-git--toplevel repo-dir)))))

(ert-deftest claude-mcp-git-test--toplevel-not-git-repo ()
  "Test toplevel returns nil for non-git directory."
  :tags '(:unit :git :helper)
  (let ((temp-dir (make-temp-file "non-git-" t)))
    (unwind-protect
        (should (null (claude-mcp-git--toplevel temp-dir)))
      (delete-directory temp-dir t))))

(ert-deftest claude-mcp-git-test--call-git ()
  "Test low-level git call."
  :tags '(:unit :git :helper)
  (claude-mcp-git-test-with-repo
    (let ((result (claude-mcp-git--call-git "status")))
      (should (consp result))
      (should (zerop (car result)))
      (should (stringp (cdr result))))))

(ert-deftest claude-mcp-git-test--git-output ()
  "Test git output helper."
  :tags '(:unit :git :helper)
  (claude-mcp-git-test-with-repo
    (let ((output (claude-mcp-git--output "rev-parse" "--git-dir")))
      (should (stringp output))
      (should (string-match-p "\\.git" output)))))

(ert-deftest claude-mcp-git-test--git-output-error ()
  "Test git output helper signals error on failure."
  :tags '(:unit :git :helper)
  (claude-mcp-git-test-with-repo
    (should-error (claude-mcp-git--output "nonexistent-command")
                  :type 'error)))

;;; ============================================================
;;; Status Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-status-empty-repo ()
  "Test status on an empty repository."
  :tags '(:unit :git :status)
  (claude-mcp-git-test-with-repo
    (let ((status (claude-mcp-git-status repo-dir)))
      (should (assoc 'branch status))
      (should (assoc 'staged status))
      (should (assoc 'unstaged status))
      (should (assoc 'untracked status))
      (should (null (cdr (assoc 'staged status))))
      (should (null (cdr (assoc 'unstaged status))))
      (should (null (cdr (assoc 'untracked status)))))))

(ert-deftest claude-mcp-git-test-status-with-untracked ()
  "Test status with untracked files."
  :tags '(:unit :git :status)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--create-file repo-dir "new-file.txt" "content")
    (let ((status (claude-mcp-git-status repo-dir)))
      (should (member "new-file.txt" (cdr (assoc 'untracked status)))))))

(ert-deftest claude-mcp-git-test-status-with-staged ()
  "Test status with staged files."
  :tags '(:unit :git :status)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--create-file repo-dir "staged.txt" "content")
    (claude-mcp-git-test--stage-file repo-dir "staged.txt")
    (let ((status (claude-mcp-git-status repo-dir)))
      (should (member "staged.txt" (cdr (assoc 'staged status)))))))

(ert-deftest claude-mcp-git-test-status-with-unstaged ()
  "Test status with unstaged changes."
  :tags '(:unit :git :status)
  (claude-mcp-git-test-with-repo
    ;; Create initial commit
    (claude-mcp-git-test--create-file repo-dir "file.txt" "initial")
    (claude-mcp-git-test--stage-file repo-dir "file.txt")
    (claude-mcp-git-test--commit repo-dir "Initial commit")
    ;; Modify file
    (claude-mcp-git-test--create-file repo-dir "file.txt" "modified")
    (let ((status (claude-mcp-git-status repo-dir)))
      (should (member "file.txt" (cdr (assoc 'unstaged status)))))))

(ert-deftest claude-mcp-git-test-status-not-git-repo ()
  "Test status fails for non-git directory."
  :tags '(:unit :git :status :error)
  (let ((temp-dir (make-temp-file "non-git-" t)))
    (unwind-protect
        (should-error (claude-mcp-git-status temp-dir)
                      :type 'error)
      (delete-directory temp-dir t))))

;;; ============================================================
;;; Stage/Unstage Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-stage-single-file ()
  "Test staging a single file."
  :tags '(:unit :git :stage)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--create-file repo-dir "to-stage.txt" "content")
    (claude-mcp-git-stage '("to-stage.txt") repo-dir)
    (let ((status (claude-mcp-git-status repo-dir)))
      (should (member "to-stage.txt" (cdr (assoc 'staged status)))))))

(ert-deftest claude-mcp-git-test-stage-multiple-files ()
  "Test staging multiple files."
  :tags '(:unit :git :stage)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--create-file repo-dir "file1.txt" "content1")
    (claude-mcp-git-test--create-file repo-dir "file2.txt" "content2")
    (claude-mcp-git-stage '("file1.txt" "file2.txt") repo-dir)
    (let ((status (claude-mcp-git-status repo-dir)))
      (should (member "file1.txt" (cdr (assoc 'staged status))))
      (should (member "file2.txt" (cdr (assoc 'staged status)))))))

(ert-deftest claude-mcp-git-test-unstage-file ()
  "Test unstaging a file."
  :tags '(:unit :git :unstage)
  (claude-mcp-git-test-with-repo
    ;; Need an initial commit for reset to work
    (claude-mcp-git-test--create-file repo-dir "initial.txt" "initial")
    (claude-mcp-git-test--stage-file repo-dir "initial.txt")
    (claude-mcp-git-test--commit repo-dir "Initial commit")
    ;; Now stage and unstage
    (claude-mcp-git-test--create-file repo-dir "to-unstage.txt" "content")
    (claude-mcp-git-stage '("to-unstage.txt") repo-dir)
    (let ((status (claude-mcp-git-status repo-dir)))
      (should (member "to-unstage.txt" (cdr (assoc 'staged status)))))
    (claude-mcp-git-unstage '("to-unstage.txt") repo-dir)
    (let ((status (claude-mcp-git-status repo-dir)))
      (should-not (member "to-unstage.txt" (cdr (assoc 'staged status))))
      (should (member "to-unstage.txt" (cdr (assoc 'untracked status)))))))

;;; ============================================================
;;; Diff Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-diff-unstaged ()
  "Test getting unstaged diff."
  :tags '(:unit :git :diff)
  (claude-mcp-git-test-with-repo
    ;; Create initial commit
    (claude-mcp-git-test--create-file repo-dir "file.txt" "initial content")
    (claude-mcp-git-test--stage-file repo-dir "file.txt")
    (claude-mcp-git-test--commit repo-dir "Initial commit")
    ;; Modify file
    (claude-mcp-git-test--create-file repo-dir "file.txt" "modified content")
    (let ((diff (claude-mcp-git-diff nil repo-dir nil)))
      (should (stringp diff))
      (should (string-match-p "initial content" diff))
      (should (string-match-p "modified content" diff)))))

(ert-deftest claude-mcp-git-test-diff-staged ()
  "Test getting staged diff."
  :tags '(:unit :git :diff)
  (claude-mcp-git-test-with-repo
    ;; Create initial commit
    (claude-mcp-git-test--create-file repo-dir "file.txt" "initial")
    (claude-mcp-git-test--stage-file repo-dir "file.txt")
    (claude-mcp-git-test--commit repo-dir "Initial commit")
    ;; Modify and stage
    (claude-mcp-git-test--create-file repo-dir "file.txt" "staged changes")
    (claude-mcp-git-test--stage-file repo-dir "file.txt")
    (let ((diff (claude-mcp-git-diff nil repo-dir t)))
      (should (stringp diff))
      (should (string-match-p "staged changes" diff)))))

(ert-deftest claude-mcp-git-test-diff-specific-file ()
  "Test getting diff for a specific file."
  :tags '(:unit :git :diff)
  (claude-mcp-git-test-with-repo
    ;; Create initial commit with two files
    (claude-mcp-git-test--create-file repo-dir "file1.txt" "content1")
    (claude-mcp-git-test--create-file repo-dir "file2.txt" "content2")
    (claude-mcp-git-test--stage-file repo-dir "file1.txt")
    (claude-mcp-git-test--stage-file repo-dir "file2.txt")
    (claude-mcp-git-test--commit repo-dir "Initial commit")
    ;; Modify both
    (claude-mcp-git-test--create-file repo-dir "file1.txt" "modified1")
    (claude-mcp-git-test--create-file repo-dir "file2.txt" "modified2")
    ;; Get diff for just file1
    (let ((diff (claude-mcp-git-diff "file1.txt" repo-dir nil)))
      (should (string-match-p "modified1" diff))
      (should-not (string-match-p "modified2" diff)))))

;;; ============================================================
;;; Log Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-log ()
  "Test getting git log."
  :tags '(:unit :git :log)
  (claude-mcp-git-test-with-repo
    ;; Create some commits
    (claude-mcp-git-test--create-file repo-dir "file.txt" "v1")
    (claude-mcp-git-test--stage-file repo-dir "file.txt")
    (claude-mcp-git-test--commit repo-dir "First commit")
    (claude-mcp-git-test--create-file repo-dir "file.txt" "v2")
    (claude-mcp-git-test--stage-file repo-dir "file.txt")
    (claude-mcp-git-test--commit repo-dir "Second commit")
    (let ((log (claude-mcp-git-log 5 repo-dir)))
      (should (stringp log))
      (should (string-match-p "First commit" log))
      (should (string-match-p "Second commit" log)))))

(ert-deftest claude-mcp-git-test-log-count ()
  "Test log respects count parameter."
  :tags '(:unit :git :log)
  (claude-mcp-git-test-with-repo
    ;; Create 3 commits
    (dotimes (i 3)
      (claude-mcp-git-test--commit repo-dir (format "Commit %d" (1+ i))))
    ;; Request only 2
    (let ((log (claude-mcp-git-log 2 repo-dir)))
      (should (string-match-p "Commit 3" log))
      (should (string-match-p "Commit 2" log))
      (should-not (string-match-p "Commit 1" log)))))

;;; ============================================================
;;; Commit Tests - Multiline Message Support
;;; ============================================================

(ert-deftest claude-mcp-git-test-commit-single-line ()
  "Test creating a commit with single-line message."
  :tags '(:unit :git :commit)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--create-file repo-dir "file.txt" "content")
    (claude-mcp-git-stage '("file.txt") repo-dir)
    (let ((result (claude-mcp-git-commit "Single line message" repo-dir)))
      (should (equal "committed" (cdr (assoc 'status result))))
      (should (equal "Single line message" (cdr (assoc 'message result)))))
    ;; Verify in log
    (let ((log (claude-mcp-git-log 1 repo-dir)))
      (should (string-match-p "Single line message" log)))))

(ert-deftest claude-mcp-git-test-commit-multiline ()
  "Test creating a commit with multiline message."
  :tags '(:unit :git :commit :multiline)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--create-file repo-dir "file.txt" "content")
    (claude-mcp-git-stage '("file.txt") repo-dir)
    (let* ((message "Subject line\n\nBody paragraph 1.\n\nBody paragraph 2.")
           (result (claude-mcp-git-commit message repo-dir)))
      (should (equal "committed" (cdr (assoc 'status result)))))
    ;; Verify commit message was preserved
    (let ((default-directory repo-dir))
      (let ((full-msg (string-trim (shell-command-to-string "git log -1 --format=%B"))))
        (should (string-match-p "Subject line" full-msg))
        (should (string-match-p "Body paragraph 1" full-msg))
        (should (string-match-p "Body paragraph 2" full-msg))))))

(ert-deftest claude-mcp-git-test-commit-with-blank-lines ()
  "Test commit message with blank lines is preserved."
  :tags '(:unit :git :commit :multiline)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--create-file repo-dir "file.txt" "content")
    (claude-mcp-git-stage '("file.txt") repo-dir)
    (let* ((message "Title\n\n\n\nWith multiple blank lines\n\n\nAbove")
           (result (claude-mcp-git-commit message repo-dir)))
      (should (equal "committed" (cdr (assoc 'status result)))))
    ;; Verify blank lines preserved (git may trim trailing, but internal should be kept)
    (let ((default-directory repo-dir))
      (let ((full-msg (shell-command-to-string "git log -1 --format=%B")))
        (should (string-match-p "Title" full-msg))
        (should (string-match-p "With multiple blank lines" full-msg))))))

(ert-deftest claude-mcp-git-test-commit-with-special-chars ()
  "Test commit message with special characters."
  :tags '(:unit :git :commit :multiline)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--create-file repo-dir "file.txt" "content")
    (claude-mcp-git-stage '("file.txt") repo-dir)
    (let* ((message "Message with \"quotes\" and 'apostrophes'\n\nAnd $pecial ch@rs!")
           (result (claude-mcp-git-commit message repo-dir)))
      (should (equal "committed" (cdr (assoc 'status result)))))
    (let ((default-directory repo-dir))
      (let ((full-msg (shell-command-to-string "git log -1 --format=%B")))
        (should (string-match-p "quotes" full-msg))
        (should (string-match-p "apostrophes" full-msg))
        (should (string-match-p "\\$pecial" full-msg))))))

(ert-deftest claude-mcp-git-test-commit-no-staged-error ()
  "Test commit fails when nothing is staged."
  :tags '(:unit :git :commit :error)
  (claude-mcp-git-test-with-repo
    (should-error (claude-mcp-git-commit "Should fail" repo-dir)
                  :type 'error)))

;;; ============================================================
;;; Amend Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-amend-single-line ()
  "Test amending with single-line message."
  :tags '(:unit :git :amend)
  (claude-mcp-git-test-with-repo
    ;; Create initial commit
    (claude-mcp-git-test--create-file repo-dir "file.txt" "content")
    (claude-mcp-git-stage '("file.txt") repo-dir)
    (claude-mcp-git-commit "Original message" repo-dir)
    ;; Amend it
    (let ((result (claude-mcp-git-amend "Amended message" repo-dir nil)))
      (should (equal "amended" (cdr (assoc 'status result)))))
    ;; Verify
    (let ((log (claude-mcp-git-log 1 repo-dir)))
      (should (string-match-p "Amended message" log))
      (should-not (string-match-p "Original message" log)))))

(ert-deftest claude-mcp-git-test-amend-multiline ()
  "Test amending with multiline message."
  :tags '(:unit :git :amend :multiline)
  (claude-mcp-git-test-with-repo
    ;; Create initial commit
    (claude-mcp-git-test--create-file repo-dir "file.txt" "content")
    (claude-mcp-git-stage '("file.txt") repo-dir)
    (claude-mcp-git-commit "Original" repo-dir)
    ;; Amend with multiline
    (let* ((message "New subject\n\nNew body paragraph.")
           (result (claude-mcp-git-amend message repo-dir nil)))
      (should (equal "amended" (cdr (assoc 'status result)))))
    ;; Verify
    (let ((default-directory repo-dir))
      (let ((full-msg (shell-command-to-string "git log -1 --format=%B")))
        (should (string-match-p "New subject" full-msg))
        (should (string-match-p "New body paragraph" full-msg))))))

;;; ============================================================
;;; Commit Propose Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-commit-propose ()
  "Test proposing a commit."
  :tags '(:unit :git :commit-propose)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--create-file repo-dir "file.txt" "content")
    (claude-mcp-git-stage '("file.txt") repo-dir)
    (let ((result (claude-mcp-git-commit-propose "Proposed commit" repo-dir)))
      (should (equal "pending_approval" (cdr (assoc 'status result))))
      (should (equal "Proposed commit" (cdr (assoc 'message result))))
      (should (member "file.txt" (cdr (assoc 'files result)))))))

(ert-deftest claude-mcp-git-test-commit-propose-multiline ()
  "Test proposing a commit with multiline message."
  :tags '(:unit :git :commit-propose :multiline)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--create-file repo-dir "file.txt" "content")
    (claude-mcp-git-stage '("file.txt") repo-dir)
    (let* ((message "Subject\n\nBody text here.")
           (result (claude-mcp-git-commit-propose message repo-dir)))
      (should (equal "pending_approval" (cdr (assoc 'status result))))
      (should (equal message (cdr (assoc 'message result)))))
    ;; Check the proposal file was written
    (let ((proposal (claude-mcp-git--read-proposed-commit repo-dir)))
      (should (string-match-p "Subject" proposal))
      (should (string-match-p "Body text here" proposal)))))

(ert-deftest claude-mcp-git-test-commit-propose-no-staged-error ()
  "Test commit propose fails when nothing is staged."
  :tags '(:unit :git :commit-propose :error)
  (claude-mcp-git-test-with-repo
    (should-error (claude-mcp-git-commit-propose "Should fail" repo-dir)
                  :type 'error)))

(ert-deftest claude-mcp-git-test-commit-status ()
  "Test checking commit proposal status."
  :tags '(:unit :git :commit-status)
  (claude-mcp-git-test-with-repo
    ;; No proposal initially
    (let ((claude-session-cwd repo-dir))
      (should (null (claude-mcp-git-commit-status))))
    ;; Create a proposal
    (claude-mcp-git-test--create-file repo-dir "file.txt" "content")
    (claude-mcp-git-stage '("file.txt") repo-dir)
    (claude-mcp-git-commit-propose "Test proposal" repo-dir)
    ;; Now should have status
    (let ((claude-session-cwd repo-dir))
      (let ((status (claude-mcp-git-commit-status)))
        (should status)
        (should (equal "pending" (cdr (assoc 'status status))))
        (should (equal "Test proposal" (cdr (assoc 'message status))))))))

;;; ============================================================
;;; Branch Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-branch-info ()
  "Test getting branch information."
  :tags '(:unit :git :branch)
  (claude-mcp-git-test-with-repo
    ;; Create initial commit (needed for branch to exist)
    (claude-mcp-git-test--commit repo-dir "Initial")
    (let ((info (claude-mcp-git-branch repo-dir)))
      (should (assoc 'current info))
      (should (assoc 'branches info))
      ;; Default branch should be master or main
      (let ((current (cdr (assoc 'current info))))
        (should (or (string= "master" current)
                    (string= "main" current)))))))

;;; ============================================================
;;; Show Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-show-commit ()
  "Test showing a commit."
  :tags '(:unit :git :show)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--create-file repo-dir "file.txt" "content for show")
    (claude-mcp-git-test--stage-file repo-dir "file.txt")
    (claude-mcp-git-test--commit repo-dir "Commit to show")
    (let ((output (claude-mcp-git-show "HEAD" nil repo-dir)))
      (should (stringp output))
      (should (string-match-p "Commit to show" output))
      (should (string-match-p "file.txt" output)))))

(ert-deftest claude-mcp-git-test-show-file-at-revision ()
  "Test showing a file at a specific revision."
  :tags '(:unit :git :show)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--create-file repo-dir "file.txt" "version 1")
    (claude-mcp-git-test--stage-file repo-dir "file.txt")
    (claude-mcp-git-test--commit repo-dir "V1")
    (claude-mcp-git-test--create-file repo-dir "file.txt" "version 2")
    (claude-mcp-git-test--stage-file repo-dir "file.txt")
    (claude-mcp-git-test--commit repo-dir "V2")
    ;; Show file at HEAD~1
    (let ((output (claude-mcp-git-show "HEAD~1" "file.txt" repo-dir)))
      (should (string-match-p "version 1" output))
      (should-not (string-match-p "version 2" output)))))

;;; ============================================================
;;; Blame Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-blame ()
  "Test git blame."
  :tags '(:unit :git :blame)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--create-file repo-dir "file.txt" "line 1\nline 2\nline 3")
    (claude-mcp-git-test--stage-file repo-dir "file.txt")
    (claude-mcp-git-test--commit repo-dir "Add file")
    (let ((blame (claude-mcp-git-blame "file.txt" repo-dir)))
      (should (stringp blame))
      (should (string-match-p "line 1" blame))
      (should (string-match-p "line 2" blame))
      (should (string-match-p "Test User" blame)))))

;;; ============================================================
;;; Stash Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-stash-push-pop ()
  "Test stashing and popping changes."
  :tags '(:unit :git :stash)
  (claude-mcp-git-test-with-repo
    ;; Need initial commit
    (claude-mcp-git-test--create-file repo-dir "file.txt" "initial")
    (claude-mcp-git-test--stage-file repo-dir "file.txt")
    (claude-mcp-git-test--commit repo-dir "Initial")
    ;; Make changes
    (claude-mcp-git-test--create-file repo-dir "file.txt" "modified")
    ;; Stash
    (claude-mcp-git-stash-push "Test stash" repo-dir)
    ;; File should be back to initial
    (should (string= "initial" 
                     (with-temp-buffer
                       (insert-file-contents (expand-file-name "file.txt" repo-dir))
                       (buffer-string))))
    ;; Pop
    (claude-mcp-git-stash-pop nil repo-dir)
    ;; File should be modified again
    (should (string= "modified"
                     (with-temp-buffer
                       (insert-file-contents (expand-file-name "file.txt" repo-dir))
                       (buffer-string))))))

(ert-deftest claude-mcp-git-test-stash-list ()
  "Test listing stashes."
  :tags '(:unit :git :stash)
  (claude-mcp-git-test-with-repo
    ;; Need initial commit
    (claude-mcp-git-test--create-file repo-dir "file.txt" "initial")
    (claude-mcp-git-test--stage-file repo-dir "file.txt")
    (claude-mcp-git-test--commit repo-dir "Initial")
    ;; Create a stash
    (claude-mcp-git-test--create-file repo-dir "file.txt" "modified")
    (claude-mcp-git-stash-push "My stash message" repo-dir)
    ;; List should show our stash
    (let ((list (claude-mcp-git-stash-list repo-dir)))
      (should (stringp list))
      (should (string-match-p "My stash message" list)))))

;;; ============================================================
;;; File Log Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-file-log ()
  "Test getting file history."
  :tags '(:unit :git :file-log)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--create-file repo-dir "tracked.txt" "v1")
    (claude-mcp-git-test--stage-file repo-dir "tracked.txt")
    (claude-mcp-git-test--commit repo-dir "Add tracked file")
    (claude-mcp-git-test--create-file repo-dir "tracked.txt" "v2")
    (claude-mcp-git-test--stage-file repo-dir "tracked.txt")
    (claude-mcp-git-test--commit repo-dir "Update tracked file")
    (let ((log (claude-mcp-git-file-log "tracked.txt" 10 repo-dir)))
      (should (stringp log))
      (should (string-match-p "Add tracked file" log))
      (should (string-match-p "Update tracked file" log)))))

;;; ============================================================
;;; Remote Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-remote-empty ()
  "Test remote info on repo with no remotes."
  :tags '(:unit :git :remote)
  (claude-mcp-git-test-with-repo
    (let ((remotes (claude-mcp-git-remote repo-dir)))
      (should (null remotes)))))

;;; ============================================================
;;; Tags Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-tags-empty ()
  "Test tags on repo with no tags."
  :tags '(:unit :git :tags)
  (claude-mcp-git-test-with-repo
    (let ((tags (claude-mcp-git-tags repo-dir)))
      (should (null tags)))))

(ert-deftest claude-mcp-git-test-tags-list ()
  "Test listing tags."
  :tags '(:unit :git :tags)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--commit repo-dir "Initial")
    (let ((default-directory repo-dir))
      (call-process "git" nil nil nil "tag" "v1.0.0")
      (call-process "git" nil nil nil "tag" "v2.0.0"))
    (let ((tags (claude-mcp-git-tags repo-dir)))
      (should (member "v1.0.0" tags))
      (should (member "v2.0.0" tags)))))

;;; ============================================================
;;; Rev-parse Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-rev-parse ()
  "Test resolving revisions."
  :tags '(:unit :git :rev-parse)
  (claude-mcp-git-test-with-repo
    (claude-mcp-git-test--commit repo-dir "Initial")
    (let ((hash (claude-mcp-git-rev-parse "HEAD" repo-dir)))
      (should (stringp hash))
      (should (= 40 (length hash)))  ; Full SHA
      (should (string-match-p "^[0-9a-f]+$" hash)))))

;;; ============================================================
;;; Checkout File Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-checkout-file ()
  "Test checking out a file from a revision."
  :tags '(:unit :git :checkout-file)
  (claude-mcp-git-test-with-repo
    ;; Create and commit original
    (claude-mcp-git-test--create-file repo-dir "file.txt" "original")
    (claude-mcp-git-test--stage-file repo-dir "file.txt")
    (claude-mcp-git-test--commit repo-dir "Original")
    ;; Modify locally (not committed)
    (claude-mcp-git-test--create-file repo-dir "file.txt" "local change")
    ;; Checkout from HEAD should restore original
    (claude-mcp-git-checkout-file "file.txt" "HEAD" repo-dir)
    (should (string= "original"
                     (with-temp-buffer
                       (insert-file-contents (expand-file-name "file.txt" repo-dir))
                       (buffer-string))))))

;;; ============================================================
;;; Ignore Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-ignore-unignore ()
  "Test assume-unchanged ignore functionality."
  :tags '(:unit :git :ignore)
  (claude-mcp-git-test-with-repo
    ;; Create and commit a file
    (claude-mcp-git-test--create-file repo-dir "config.txt" "initial")
    (claude-mcp-git-test--stage-file repo-dir "config.txt")
    (claude-mcp-git-test--commit repo-dir "Add config")
    ;; Ignore it
    (let ((result (claude-mcp-git-ignore "config.txt" repo-dir)))
      (should (equal "ignored" (cdr (assoc 'status result)))))
    ;; Unignore it
    (let ((result (claude-mcp-git-ignore "config.txt" repo-dir t)))
      (should (equal "unignored" (cdr (assoc 'status result)))))))

;;; ============================================================
;;; Tool Registration Tests
;;; ============================================================

(ert-deftest claude-mcp-git-test-tools-registered ()
  "Test that git tools are registered."
  :tags '(:unit :git :registration)
  ;; Tool names use underscores in the registry (MCP convention)
  (should (gethash "git_status" claude-mcp-tools))
  (should (gethash "git_stage" claude-mcp-tools))
  (should (gethash "git_unstage" claude-mcp-tools))
  (should (gethash "git_diff" claude-mcp-tools))
  (should (gethash "git_log" claude-mcp-tools))
  (should (gethash "git_commit" claude-mcp-tools))
  (should (gethash "git_amend" claude-mcp-tools))
  (should (gethash "git_commit_propose" claude-mcp-tools))
  (should (gethash "git_commit_status" claude-mcp-tools))
  (should (gethash "git_branch" claude-mcp-tools))
  (should (gethash "git_show" claude-mcp-tools))
  (should (gethash "git_blame" claude-mcp-tools))
  (should (gethash "git_stash_list" claude-mcp-tools))
  (should (gethash "git_stash_push" claude-mcp-tools))
  (should (gethash "git_stash_pop" claude-mcp-tools))
  (should (gethash "git_file_log" claude-mcp-tools))
  (should (gethash "git_checkout_file" claude-mcp-tools))
  (should (gethash "git_remote" claude-mcp-tools))
  (should (gethash "git_tags" claude-mcp-tools))
  (should (gethash "git_rev_parse" claude-mcp-tools))
  (should (gethash "git_rebase" claude-mcp-tools))
  (should (gethash "git_ignore" claude-mcp-tools)))

(provide 'claude-mcp-git-test)
;;; claude-mcp-git-test.el ends here
