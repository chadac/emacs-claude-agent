;;; claude-expert-integration-test.el --- Integration tests for expert system -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Integration tests for the Ask-the-Expert system using the mock REPL framework.
;; These tests verify the full flow of:
;; - Asking an expert a question
;; - Expert receiving and processing queries
;; - Expert responding with answers
;; - KB operations (create, search, update)

;;; Code:

(require 'claude-test-framework)
(require 'claude-test-dsl)
(require 'claude-agent-expert)

;;;; Test Helpers

(defvar claude-expert-test--temp-kb-dir nil
  "Temporary directory for KB files during tests.")

(defun claude-expert-test--setup ()
  "Set up expert test environment."
  (setq claude-expert-test--temp-kb-dir (make-temp-file "expert-kb-" t))
  (setq claude-agent-expert-kb-directory claude-expert-test--temp-kb-dir)
  ;; Clear any pending queries from previous tests
  (clrhash claude-agent-expert--pending-queries)
  (clrhash claude-agent-expert--sessions))

(defun claude-expert-test--teardown ()
  "Clean up expert test environment."
  (when (and claude-expert-test--temp-kb-dir
             (file-directory-p claude-expert-test--temp-kb-dir))
    (delete-directory claude-expert-test--temp-kb-dir t))
  (setq claude-expert-test--temp-kb-dir nil))

(defmacro claude-expert-test-with-env (&rest body)
  "Execute BODY with expert test environment set up."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn
         (claude-expert-test--setup)
         ,@body)
     (claude-expert-test--teardown)))

;;;; Unit Tests for Expert Helpers

(ert-deftest claude-expert-integration-buffer-name ()
  "Test expert buffer naming."
  :tags '(:integration :expert)
  (should (equal (claude-agent-expert--buffer-name "my-project")
                 "*claude:my-project:expert*")))

(ert-deftest claude-expert-integration-get-project-from-buffer ()
  "Test extracting project name from buffer name."
  :tags '(:integration :expert)
  (should (equal (claude-agent-expert--get-project-from-buffer "*claude:foo:expert*")
                 "foo"))
  (should (equal (claude-agent-expert--get-project-from-buffer "*claude:my-project:expert*")
                 "my-project"))
  (should-not (claude-agent-expert--get-project-from-buffer "*claude:foo*"))
  (should-not (claude-agent-expert--get-project-from-buffer nil)))

(ert-deftest claude-expert-integration-is-expert-session ()
  "Test expert session detection."
  :tags '(:integration :expert)
  (should (claude-agent-expert--is-expert-session-p "*claude:test:expert*"))
  (should-not (claude-agent-expert--is-expert-session-p "*claude:test*"))
  (should-not (claude-agent-expert--is-expert-session-p "*scratch*"))
  (should-not (claude-agent-expert--is-expert-session-p nil)))

;;;; KB File Tests

(ert-deftest claude-expert-integration-kb-file-path ()
  "Test KB file path generation."
  :tags '(:integration :expert)
  (claude-expert-test-with-env
    (let ((kb-file (claude-agent-expert--kb-file "test-project")))
      (should (string-match-p "test-project\\.org$" kb-file))
      (should (string-prefix-p claude-expert-test--temp-kb-dir kb-file)))))

(ert-deftest claude-expert-integration-ensure-kb-creates-file ()
  "Test that ensure-kb-file creates file with structure."
  :tags '(:integration :expert)
  (claude-expert-test-with-env
    (let ((kb-file (expand-file-name "new-project.org" claude-expert-test--temp-kb-dir)))
      (should-not (file-exists-p kb-file))
      (claude-agent-expert--ensure-kb-file kb-file "new-project")
      (should (file-exists-p kb-file))
      (let ((content (with-temp-buffer
                       (insert-file-contents kb-file)
                       (buffer-string))))
        (should (string-search "#+TITLE:" content))
        (should (string-search "new-project" content))
        (should (string-search "* Architecture" content))
        (should (string-search "* Gotchas" content))))))

;;;; Pending Query Tests

(ert-deftest claude-expert-integration-pending-query-storage ()
  "Test pending query hash table operations."
  :tags '(:integration :expert)
  (claude-expert-test-with-env
    ;; Store a pending query
    (puthash "*claude:proj:expert*"
             (list :caller "*claude:proj*"
                   :query "How does X work?"
                   :timestamp (current-time))
             claude-agent-expert--pending-queries)

    ;; Verify it's stored
    (let ((pending (gethash "*claude:proj:expert*" claude-agent-expert--pending-queries)))
      (should pending)
      (should (equal (plist-get pending :caller) "*claude:proj*"))
      (should (equal (plist-get pending :query) "How does X work?")))

    ;; Clear it
    (remhash "*claude:proj:expert*" claude-agent-expert--pending-queries)
    (should-not (gethash "*claude:proj:expert*" claude-agent-expert--pending-queries))))

;;;; Permission Tests

(ert-deftest claude-expert-integration-permission-read-tools ()
  "Test that read-only tools are allowed for experts."
  :tags '(:integration :expert)
  (dolist (tool '("read_file" "grep" "glob" "magit_status" "magit_log"))
    (let ((result (claude-agent-expert--check-permission
                   tool '() "*claude:proj:expert*")))
      (should-not result))))

(ert-deftest claude-expert-integration-permission-write-tools-denied ()
  "Test that write tools are denied for non-KB files."
  :tags '(:integration :expert)
  (claude-expert-test-with-env
    (let ((result (claude-agent-expert--check-permission
                   "lock_file"
                   '((file_path . "/some/other/file.el"))
                   "*claude:proj:expert*")))
      (should result)
      (should (string-match-p "Permission denied" result)))))

(ert-deftest claude-expert-integration-permission-kb-edit-allowed ()
  "Test that editing KB file is allowed."
  :tags '(:integration :expert)
  (claude-expert-test-with-env
    (let* ((kb-file (claude-agent-expert--kb-file "proj"))
           (result (claude-agent-expert--check-permission
                    "lock_file"
                    `((file_path . ,kb-file))
                    "*claude:proj:expert*")))
      (should-not result))))

(ert-deftest claude-expert-integration-permission-non-expert ()
  "Test that non-expert sessions bypass permission checks."
  :tags '(:integration :expert)
  (let ((result (claude-agent-expert--check-permission
                 "Bash" '((command . "rm -rf /")) "*claude:proj*")))
    ;; Non-expert sessions should return nil (allow everything)
    (should-not result)))

;;;; Worktree Resolution Tests

(ert-deftest claude-expert-integration-worktree-detection-regular ()
  "Test that regular git repos are detected correctly."
  :tags '(:integration :expert)
  (let ((temp-dir (make-temp-file "git-test-" t)))
    (unwind-protect
        (progn
          ;; Create a regular git repo
          (let ((default-directory temp-dir))
            (call-process "git" nil nil nil "init")
            (let ((result (claude-agent-expert--worktree-main-repo temp-dir)))
              ;; Should return nil for regular repos
              (should-not result))))
      (delete-directory temp-dir t))))

;;;; KB Operations Tests

(ert-deftest claude-expert-integration-kb-create-entry ()
  "Test creating a KB entry."
  :tags '(:integration :expert)
  (claude-expert-test-with-env
    ;; Ensure KB file exists
    (let ((kb-file (claude-agent-expert--kb-file "test-proj")))
      (claude-agent-expert--ensure-kb-file kb-file "test-proj")

      ;; Create an entry
      (let ((result (claude-agent-expert--kb-create
                     kb-file
                     "Test Pattern"
                     "pattern"
                     "This is a test pattern for doing X.")))
        (should (string-match-p "created" result))

        ;; Verify it's in the file
        (let ((content (with-temp-buffer
                         (insert-file-contents kb-file)
                         (buffer-string))))
          (should (string-search "Test Pattern" content))
          (should (string-search "test pattern for doing X" content)))))))

(ert-deftest claude-expert-integration-kb-search ()
  "Test searching KB entries."
  :tags '(:integration :expert)
  (claude-expert-test-with-env
    (let ((kb-file (claude-agent-expert--kb-file "test-proj")))
      (claude-agent-expert--ensure-kb-file kb-file "test-proj")

      ;; Create some entries
      (claude-agent-expert--kb-create "test-proj" "Auth Pattern" "pattern"
                                      "How to do authentication in this project.")
      (claude-agent-expert--kb-create "test-proj" "Database Gotcha" "gotcha"
                                      "Watch out for connection pooling issues.")

      ;; Search for auth
      (let ((result (claude-agent-expert--kb-search "test-proj" "authentication")))
        (should (string-search "Auth Pattern" result)))

      ;; Search for database
      (let ((result (claude-agent-expert--kb-search "test-proj" "connection")))
        (should (string-search "Database Gotcha" result))))))

(ert-deftest claude-expert-integration-kb-get-entry ()
  "Test getting a specific KB entry."
  :tags '(:integration :expert)
  (claude-expert-test-with-env
    (let ((kb-file (claude-agent-expert--kb-file "test-proj")))
      (claude-agent-expert--ensure-kb-file kb-file "test-proj")

      ;; Create an entry
      (claude-agent-expert--kb-create "test-proj" "Important Info" "architecture"
                                      "This describes the core architecture.")

      ;; Get it back
      (let ((result (claude-agent-expert--kb-get "test-proj" "Important Info")))
        (should (string-search "Important Info" result))
        (should (string-search "core architecture" result))))))

;;;; MCP Tool Registration Tests

(ert-deftest claude-expert-integration-tools-registered ()
  "Test that expert tools are registered."
  :tags '(:integration :expert)
  (should (gethash "ask_the_expert" claude-mcp-tools))
  (should (gethash "expert_respond" claude-mcp-tools))
  (should (gethash "expert_kb" claude-mcp-tools))
  (should (gethash "list_experts" claude-mcp-tools)))

(ert-deftest claude-expert-integration-tools-have-inject-agent-name ()
  "Test that expert_respond and expert_kb have inject-agent-name flag."
  :tags '(:integration :expert)
  (let ((respond-tool (gethash "expert_respond" claude-mcp-tools))
        (kb-tool (gethash "expert_kb" claude-mcp-tools)))
    (should respond-tool)
    (should kb-tool)
    (should (plist-get respond-tool :inject-agent-name))
    (should (plist-get kb-tool :inject-agent-name))))

;;;; Integration Scenario Tests (using mock framework)

(claude-test-scenario "Expert query flow simulation"
  ;; Simulate an agent asking for expert help
  (user-sends "Please ask the expert about the authentication system")

  ;; Agent would call ask_the_expert tool
  (agent-thinking t)
  (agent-uses-tool "mcp__emacs__ask_the_expert" "ask-1"
                   '((project_path . "/tmp/test-project")
                     (query . "How does authentication work?")))

  ;; Tool returns query sent status
  (tool-result "ask-1" '((status . "query_sent")
                         (expert_buffer . "*claude:test-project:expert*")
                         (message . "Query sent to expert")))

  ;; Agent acknowledges
  (agent-responds "I've asked the expert about authentication. Waiting for response.")
  (agent-ready)

  ;; Verify the flow was recorded
  (buffer-should-contain "ask_the_expert")
  (buffer-should-contain "authentication"))

(claude-test-scenario "Expert responds to query"
  ;; Simulate expert receiving a query and responding
  (user-sends "QUERY FROM *claude:proj*: How does caching work?")

  (agent-thinking t)

  ;; Expert searches KB first
  (agent-uses-tool "mcp__emacs__expert_kb" "kb-1"
                   '((action . "search")
                     (query . "caching")))
  (tool-result "kb-1" '((entries . "No matching entries found")))

  ;; Expert reads some files
  (agent-uses-tool "mcp__emacs__read_file" "read-1"
                   '((file_path . "/tmp/proj/cache.py")))
  (tool-result "read-1" '((content . "# Cache implementation...")))

  ;; Expert creates KB entry
  (agent-uses-tool "mcp__emacs__expert_kb" "kb-2"
                   '((action . "create")
                     (title . "Caching System")
                     (kb_type . "architecture")
                     (content . "The caching layer uses Redis...")))
  (tool-result "kb-2" '((status . "Entry created")))

  ;; Expert responds
  (agent-uses-tool "mcp__emacs__expert_respond" "resp-1"
                   '((response . "The caching system uses Redis with a 5-minute TTL...")))
  (tool-result "resp-1" '((status . "response_sent")))

  (agent-responds "I've sent my response about the caching system.")
  (agent-ready)

  ;; Verify expert workflow
  (buffer-should-contain "expert_kb")
  (buffer-should-contain "expert_respond")
  (buffer-should-contain "caching"))

(provide 'claude-expert-integration-test)
;;; claude-expert-integration-test.el ends here
