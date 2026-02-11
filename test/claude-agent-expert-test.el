;;; claude-agent-expert-test.el --- Tests for expert system -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;; Unit tests for the Ask-the-Expert system in claude-agent-expert.el.
;; Covers: worktree detection, session management, permission checking,
;; KB operations, and MCP tool registration.
;;
;; Run with:
;;   emacs -batch -l ert -l test/claude-agent-expert-test.el -f ert-run-tests-batch

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

;; Set up load path
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;; Load claude-mcp-registry first (needed for tool registration)
(require 'claude-mcp-registry)

;; Load claude-mcp-messaging (needed for send_message)
;; But stub out the parts that need the full claude-agent-repl
(unless (fboundp 'claude-mcp-send-message)
  (defun claude-mcp-send-message (buffer-name message &optional from-buffer)
    "Stub for testing - just return success."
    (format "Message sent to %s" buffer-name)))

;; Now require the module under test
(require 'claude-agent-expert)
;;; Test Utilities

(defvar claude-agent-expert-test--temp-dir nil
  "Temporary directory for test KB files.")

(defmacro claude-agent-expert-test-with-temp-kb (&rest body)
  "Execute BODY with a temporary KB directory."
  (declare (indent 0))
  `(let* ((claude-agent-expert-test--temp-dir (make-temp-file "claude-expert-test-" t))
          (claude-agent-expert-kb-directory claude-agent-expert-test--temp-dir)
          (claude-agent-expert--pending-queries (make-hash-table :test 'equal))
          (claude-agent-expert--sessions (make-hash-table :test 'equal))
          (inhibit-message t))
     (unwind-protect
         (progn ,@body)
       (delete-directory claude-agent-expert-test--temp-dir t))))

;;; ============================================================
;;; Worktree Detection Tests
;;; ============================================================

(ert-deftest claude-agent-expert-test-worktree-main-repo-regular-git ()
  "Test worktree detection with a regular .git directory."
  :tags '(:unit :expert :worktree)
  (claude-agent-expert-test-with-temp-kb
    ;; Create a mock repo with a .git directory (not a worktree)
    (let ((mock-repo (expand-file-name "mock-repo" claude-agent-expert-test--temp-dir)))
      (make-directory mock-repo t)
      (make-directory (expand-file-name ".git" mock-repo) t)
      ;; Should return nil for regular repo
      (should-not (claude-agent-expert--worktree-main-repo mock-repo)))))

(ert-deftest claude-agent-expert-test-worktree-main-repo-worktree ()
  "Test worktree detection with a .git file (worktree)."
  :tags '(:unit :expert :worktree)
  (claude-agent-expert-test-with-temp-kb
    ;; Create a mock worktree with a .git file
    (let* ((main-repo (expand-file-name "main-repo" claude-agent-expert-test--temp-dir))
           (worktree (expand-file-name "worktree" claude-agent-expert-test--temp-dir))
           (main-git-dir (expand-file-name ".git" main-repo)))
      (make-directory main-repo t)
      (make-directory main-git-dir t)
      (make-directory (expand-file-name "worktrees/test-branch" main-git-dir) t)
      (make-directory worktree t)
      ;; Write .git file pointing to main repo's worktrees dir
      (with-temp-file (expand-file-name ".git" worktree)
        (insert (format "gitdir: %s/worktrees/test-branch\n"
                        main-git-dir)))
      ;; Should return the main repo path
      (let ((result (claude-agent-expert--worktree-main-repo worktree)))
        (should result)
        (should (string= (expand-file-name main-repo) result))))))

(ert-deftest claude-agent-expert-test-resolve-project-path-regular ()
  "Test project path resolution for regular directories."
  :tags '(:unit :expert :worktree)
  (claude-agent-expert-test-with-temp-kb
    (let ((test-dir (expand-file-name "test-project" claude-agent-expert-test--temp-dir)))
      (make-directory test-dir t)
      ;; Regular dir should just be expanded
      (should (string= test-dir
                       (claude-agent-expert--resolve-project-path test-dir))))))

;;; ============================================================
;;; Session Detection Tests
;;; ============================================================

(ert-deftest claude-agent-expert-test-is-expert-session-p ()
  "Test expert session buffer name detection."
  :tags '(:unit :expert :session)
  (should (claude-agent-expert--is-expert-session-p "*claude:myproject:expert*"))
  (should (claude-agent-expert--is-expert-session-p "*claude:another-project:expert*"))
  (should-not (claude-agent-expert--is-expert-session-p "*claude:myproject*"))
  (should-not (claude-agent-expert--is-expert-session-p "*claude:myproject:other*"))
  (should-not (claude-agent-expert--is-expert-session-p nil))
  (should-not (claude-agent-expert--is-expert-session-p "")))

(ert-deftest claude-agent-expert-test-get-project-from-buffer ()
  "Test extracting project name from expert buffer name."
  :tags '(:unit :expert :session)
  (should (string= "myproject"
                   (claude-agent-expert--get-project-from-buffer "*claude:myproject:expert*")))
  (should (string= "another-project"
                   (claude-agent-expert--get-project-from-buffer "*claude:another-project:expert*")))
  (should-not (claude-agent-expert--get-project-from-buffer "*claude:myproject*"))
  (should-not (claude-agent-expert--get-project-from-buffer nil)))

(ert-deftest claude-agent-expert-test-buffer-name ()
  "Test buffer name generation for experts."
  :tags '(:unit :expert :session)
  (should (string= "*claude:test-project:expert*"
                   (claude-agent-expert--buffer-name "test-project")))
  (should (string= "*claude:my-app:expert*"
                   (claude-agent-expert--buffer-name "my-app"))))

;;; ============================================================
;;; KB File Tests
;;; ============================================================

(ert-deftest claude-agent-expert-test-kb-file-path ()
  "Test KB file path generation."
  :tags '(:unit :expert :kb)
  (claude-agent-expert-test-with-temp-kb
    (let ((kb-file (claude-agent-expert--kb-file "test-project")))
      (should (string-match-p "test-project\\.org$" kb-file))
      (should (string-prefix-p claude-agent-expert-test--temp-dir kb-file)))))

(ert-deftest claude-agent-expert-test-is-kb-file-p ()
  "Test KB file path checking."
  :tags '(:unit :expert :kb)
  (claude-agent-expert-test-with-temp-kb
    (let ((kb-file (claude-agent-expert--kb-file "myproject")))
      ;; Matching file should return t
      (should (claude-agent-expert--is-kb-file-p kb-file "myproject"))
      ;; Different file should return nil
      (should-not (claude-agent-expert--is-kb-file-p "/some/other/file.el" "myproject"))
      ;; Wrong project should return nil
      (should-not (claude-agent-expert--is-kb-file-p kb-file "other-project"))
      ;; nil inputs should return nil
      (should-not (claude-agent-expert--is-kb-file-p nil "myproject"))
      (should-not (claude-agent-expert--is-kb-file-p kb-file nil)))))

(ert-deftest claude-agent-expert-test-ensure-kb-file ()
  "Test KB file creation with proper structure."
  :tags '(:unit :expert :kb)
  (claude-agent-expert-test-with-temp-kb
    (let ((kb-file (claude-agent-expert--kb-file "test-project")))
      ;; File shouldn't exist yet
      (should-not (file-exists-p kb-file))
      ;; Create it
      (claude-agent-expert--ensure-kb-file kb-file "test-project")
      ;; Should exist now
      (should (file-exists-p kb-file))
      ;; Check content structure
      (with-temp-buffer
        (insert-file-contents kb-file)
        (let ((content (buffer-string)))
          (should (string-search "#+TITLE:" content))
          (should (string-search "* Architecture" content))
          (should (string-search "* Patterns" content))
          (should (string-search "* Gotchas" content))
          (should (string-search "* FAQ" content)))))))

;;; ============================================================
;;; Permission Tests
;;; ============================================================

(ert-deftest claude-agent-expert-test-check-permission-non-expert ()
  "Test that non-expert sessions allow all tools."
  :tags '(:unit :expert :permission)
  (should-not (claude-agent-expert--check-permission
               "write_file" nil "*claude:myproject*"))
  (should-not (claude-agent-expert--check-permission
               "dangerous_tool" nil "*scratch*")))

(ert-deftest claude-agent-expert-test-check-permission-allowed-tools ()
  "Test that experts can use allowed tools."
  :tags '(:unit :expert :permission)
  (dolist (tool '("read_file" "grep" "glob" "whoami" "expert_respond" "expert_kb"))
    (should-not (claude-agent-expert--check-permission
                 tool nil "*claude:myproject:expert*"))))

(ert-deftest claude-agent-expert-test-check-permission-denied-tools ()
  "Test that experts cannot use disallowed tools."
  :tags '(:unit :expert :permission)
  (dolist (tool '("write_file" "bash" "spawn_agent" "dangerous_tool"))
    (let ((result (claude-agent-expert--check-permission
                   tool nil "*claude:myproject:expert*")))
      (should result)
      (should (string-match-p "Permission denied" result)))))

(ert-deftest claude-agent-expert-test-check-permission-kb-edit-allowed ()
  "Test that experts can edit their KB file."
  :tags '(:unit :expert :permission)
  (claude-agent-expert-test-with-temp-kb
    (let ((kb-file (claude-agent-expert--kb-file "myproject")))
      (should-not (claude-agent-expert--check-permission
                   "lock_file"
                   `((file_path . ,kb-file))
                   "*claude:myproject:expert*")))))

(ert-deftest claude-agent-expert-test-check-permission-other-file-denied ()
  "Test that experts cannot edit non-KB files."
  :tags '(:unit :expert :permission)
  (let ((result (claude-agent-expert--check-permission
                 "lock_file"
                 '((file_path . "/some/other/file.el"))
                 "*claude:myproject:expert*")))
    (should result)
    (should (string-match-p "Permission denied" result))
    (should (string-match-p "knowledge base file" result))))

;;; ============================================================
;;; KB Operations Tests
;;; ============================================================

(ert-deftest claude-agent-expert-test-kb-search-empty ()
  "Test searching empty KB."
  :tags '(:unit :expert :kb)
  (claude-agent-expert-test-with-temp-kb
    (let* ((kb-file (claude-agent-expert--kb-file "test"))
           (result (claude-agent-expert--kb-search kb-file "anything")))
      (should (string-match-p "empty" result)))))

(ert-deftest claude-agent-expert-test-kb-search-with-results ()
  "Test searching KB with matching entries."
  :tags '(:unit :expert :kb)
  (claude-agent-expert-test-with-temp-kb
    (let ((kb-file (claude-agent-expert--kb-file "test")))
      ;; Create KB with content
      (claude-agent-expert--ensure-kb-file kb-file "test")
      (with-current-buffer (find-file-noselect kb-file)
        (goto-char (point-max))
        (insert "\n** Test Entry\nThis is about foobar.\n")
        (save-buffer)
        (kill-buffer))
      ;; Search for it
      (let* ((result (claude-agent-expert--kb-search kb-file "foobar"))
             (parsed (json-read-from-string result)))
        (should (> (cdr (assq 'count parsed)) 0))))))

(ert-deftest claude-agent-expert-test-kb-create ()
  "Test creating KB entries."
  :tags '(:unit :expert :kb)
  (claude-agent-expert-test-with-temp-kb
    (let ((kb-file (claude-agent-expert--kb-file "test")))
      (claude-agent-expert--ensure-kb-file kb-file "test")
      ;; Create an entry
      (let* ((result (claude-agent-expert--kb-create kb-file "New Entry" "gotcha"
                                                     "This is the content." nil))
             (parsed (json-read-from-string result)))
        (should (eq t (cdr (assq 'created parsed))))
        (should (string= "Gotchas" (cdr (assq 'parent parsed)))))
      ;; Verify it exists
      (with-temp-buffer
        (insert-file-contents kb-file)
        (should (string-match-p "\\*\\* New Entry" (buffer-string)))
        (should (string-match-p "This is the content" (buffer-string)))))))

(ert-deftest claude-agent-expert-test-kb-get ()
  "Test getting KB entries by title."
  :tags '(:unit :expert :kb)
  (claude-agent-expert-test-with-temp-kb
    (let ((kb-file (claude-agent-expert--kb-file "test")))
      (claude-agent-expert--ensure-kb-file kb-file "test")
      ;; Create an entry first
      (claude-agent-expert--kb-create kb-file "My Entry" "pattern" "Entry content here." nil)
      ;; Get it back
      (let* ((result (claude-agent-expert--kb-get kb-file "My Entry"))
             (parsed (json-read-from-string result)))
        (should (eq t (cdr (assq 'found parsed))))
        (should (string= "My Entry" (cdr (assq 'title parsed))))
        (should (string-match-p "Entry content" (cdr (assq 'content parsed)))))
      ;; Try to get non-existent entry
      (let* ((result (claude-agent-expert--kb-get kb-file "Nonexistent"))
             (parsed (json-read-from-string result)))
        (should (eq :json-false (cdr (assq 'found parsed))))))))

(ert-deftest claude-agent-expert-test-kb-update ()
  "Test updating/appending to KB entries."
  :tags '(:unit :expert :kb)
  (claude-agent-expert-test-with-temp-kb
    (let ((kb-file (claude-agent-expert--kb-file "test")))
      (claude-agent-expert--ensure-kb-file kb-file "test")
      ;; Create an entry
      (claude-agent-expert--kb-create kb-file "Update Me" "architecture" "Original." nil)
      ;; Update it
      (let* ((result (claude-agent-expert--kb-update kb-file "Update Me" "Appended."))
             (parsed (json-read-from-string result)))
        (should (eq t (cdr (assq 'updated parsed)))))
      ;; Verify content
      (let* ((result (claude-agent-expert--kb-get kb-file "Update Me"))
             (parsed (json-read-from-string result))
             (content (cdr (assq 'content parsed))))
        (should (string-match-p "Original" content))
        (should (string-match-p "Appended" content))))))

(ert-deftest claude-agent-expert-test-kb-list ()
  "Test listing all KB entries."
  :tags '(:unit :expert :kb)
  (claude-agent-expert-test-with-temp-kb
    (let ((kb-file (claude-agent-expert--kb-file "test")))
      (claude-agent-expert--ensure-kb-file kb-file "test")
      ;; List default entries
      (let* ((result (claude-agent-expert--kb-list kb-file))
             (parsed (json-read-from-string result)))
        ;; Should have at least the default headings
        (should (> (cdr (assq 'count parsed)) 0))))))

;;; ============================================================
;;; MCP Tool Registration Tests
;;; ============================================================

(ert-deftest claude-agent-expert-test-tools-registered ()
  "Test that expert tools are registered in the tool registry."
  :tags '(:unit :expert :registration)
  (should (gethash "ask_the_expert" claude-mcp-tools))
  (should (gethash "expert_respond" claude-mcp-tools))
  (should (gethash "expert_kb" claude-mcp-tools))
  (should (gethash "list_experts" claude-mcp-tools)))

(ert-deftest claude-agent-expert-test-tools-have-descriptions ()
  "Test that expert tools have descriptions."
  :tags '(:unit :expert :registration)
  (dolist (tool-name '("ask_the_expert" "expert_respond" "expert_kb" "list_experts"))
    (let ((tool-def (gethash tool-name claude-mcp-tools)))
      (should tool-def)
      (should (stringp (plist-get tool-def :description)))
      (should (> (length (plist-get tool-def :description)) 0)))))

;;; ============================================================
;;; list-experts Tests
;;; ============================================================

(ert-deftest claude-agent-expert-test-list-experts-empty ()
  "Test list_experts with no running experts."
  :tags '(:unit :expert :list)
  (claude-agent-expert-test-with-temp-kb
    (let* ((result (claude-agent-expert-mcp-list))
           (parsed (json-read-from-string result)))
      (should (vectorp (cdr (assq 'running parsed))))
      (should (= 0 (length (cdr (assq 'running parsed))))))))

(ert-deftest claude-agent-expert-test-list-experts-with-inactive ()
  "Test list_experts including inactive projects."
  :tags '(:unit :expert :list)
  (claude-agent-expert-test-with-temp-kb
    ;; Create a KB file to simulate an inactive project
    (claude-agent-expert--ensure-kb-file
     (claude-agent-expert--kb-file "old-project")
     "old-project")
    (let* ((result (claude-agent-expert-mcp-list t))
           (parsed (json-read-from-string result))
           (available (cdr (assq 'available parsed))))
      (should (> (length available) 0))
      (should (cl-find "old-project" available
                       :key (lambda (e) (cdr (assq 'project e)))
                       :test #'string=)))))

;;; ============================================================
;;; Pending Query Tests
;;; ============================================================

(ert-deftest claude-agent-expert-test-pending-query-storage ()
  "Test that pending queries are stored correctly."
  :tags '(:unit :expert :query)
  (claude-agent-expert-test-with-temp-kb
    (let ((expert-buffer "*claude:test:expert*"))
      ;; Store a pending query
      (puthash expert-buffer
               (list :caller "*claude:caller*"
                     :query "What is X?"
                     :timestamp (current-time))
               claude-agent-expert--pending-queries)
      ;; Retrieve it
      (let ((pending (gethash expert-buffer claude-agent-expert--pending-queries)))
        (should pending)
        (should (string= "*claude:caller*" (plist-get pending :caller)))
        (should (string= "What is X?" (plist-get pending :query)))))))

(provide 'claude-agent-expert-test)
;;; claude-agent-expert-test.el ends here
