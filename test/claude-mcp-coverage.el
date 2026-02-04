;;; claude-mcp-coverage.el --- Test coverage check for MCP tools -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;; Provides `claude-mcp-check-test-coverage' which compares registered
;; MCP tool names against tools that appear in test files.
;; When run as part of the ERT test suite, missing tests are reported
;; as test failures.
;;
;; Run with:
;;   emacs -batch -l ert -l test/claude-mcp-coverage.el -f ert-run-tests-batch

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load test helper (handles circular dependency between claude-mcp and sub-modules)
(defvar claude-mcp-coverage--test-dir
  (file-name-directory (or load-file-name buffer-file-name
                          (expand-file-name "test/claude-mcp-coverage.el"
                                            default-directory)))
  "Directory containing test files.")

(add-to-list 'load-path claude-mcp-coverage--test-dir)
(add-to-list 'load-path (file-name-directory (directory-file-name claude-mcp-coverage--test-dir)))
(require 'test-helper)

;; Load all tool-defining modules to populate the tool registry
(condition-case nil (require 'claude-mcp-messaging) (error nil))
(condition-case nil (require 'claude-mcp-magit) (error nil))
(condition-case nil (require 'claude-kb) (error nil))
(condition-case nil (require 'claude-oneshot) (error nil))
(condition-case nil (require 'todo) (error nil))

;;; Coverage Check Implementation

(defun claude-mcp-coverage--get-registered-tools ()
  "Return a sorted list of all registered MCP tool names."
  (let ((tools '()))
    (maphash (lambda (name _def) (push name tools)) claude-mcp-tools)
    (sort tools #'string<)))

(defun claude-mcp-coverage--get-tested-tools (test-dir)
  "Return a sorted list of tool names that have tests in TEST-DIR.
Scans test files for tool names appearing in test function names,
:tags lists, or test body content."
  (let ((tested-tools '())
        (test-files (directory-files test-dir t "-test\\.el$")))
    (dolist (file test-files)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((content (buffer-string)))
          ;; Scan for tool names from the registry that appear in test content.
          ;; We look for the tool name (with underscores as in the registry)
          ;; appearing in test function names, tags, or assertion bodies.
          (maphash
           (lambda (tool-name _def)
             (let ((patterns (list
                              ;; Tool name in ert-deftest name (with hyphens)
                              (regexp-quote (replace-regexp-in-string "_" "-" tool-name))
                              ;; Tool name as a tag
                              (format ":%s" (replace-regexp-in-string "_" "-" tool-name))
                              ;; Tool name in a string (e.g., gethash "tool_name")
                              (format "\"%s\"" tool-name)
                              ;; Tool name as symbol reference
                              (format "'%s" tool-name))))
               (when (cl-some (lambda (pat)
                                (string-match-p pat content))
                              patterns)
                 (cl-pushnew tool-name tested-tools :test #'equal))))
           claude-mcp-tools))))
    (sort tested-tools #'string<)))

(defun claude-mcp-check-test-coverage (&optional test-dir)
  "Check test coverage for MCP tools.
TEST-DIR defaults to the test/ directory next to this file.
Returns a plist with:
  :registered - list of all registered tool names
  :tested - list of tool names with tests
  :untested - list of tool names without tests
  :coverage-pct - percentage of tools with tests"
  (let* ((test-dir (or test-dir
                       claude-mcp-coverage--test-dir
                       (expand-file-name "test/" default-directory)))
         (registered (claude-mcp-coverage--get-registered-tools))
         (tested (claude-mcp-coverage--get-tested-tools test-dir))
         (untested (cl-set-difference registered tested :test #'equal))
         (coverage-pct (if (> (length registered) 0)
                           (* 100.0 (/ (float (length tested))
                                       (length registered)))
                         100.0)))
    (list :registered registered
          :tested tested
          :untested (sort untested #'string<)
          :coverage-pct coverage-pct)))

(defun claude-mcp-coverage-report (&optional test-dir)
  "Print a human-readable coverage report.
TEST-DIR defaults to the test/ directory."
  (let* ((result (claude-mcp-check-test-coverage test-dir))
         (registered (plist-get result :registered))
         (tested (plist-get result :tested))
         (untested (plist-get result :untested))
         (pct (plist-get result :coverage-pct)))
    (concat
     (format "MCP Tool Test Coverage: %.1f%% (%d/%d tools)\n\n"
             pct (length tested) (length registered))
     (if untested
         (concat "UNTESTED TOOLS:\n"
                 (mapconcat (lambda (name) (format "  - %s" name))
                            untested "\n")
                 "\n\n")
       "All tools have tests!\n\n")
     (format "TESTED TOOLS (%d):\n" (length tested))
     (mapconcat (lambda (name) (format "  + %s" name))
                tested "\n"))))

;;; ============================================================
;;; ERT Tests for Coverage Check
;;; ============================================================

(ert-deftest claude-mcp-coverage-test-check-function-exists ()
  "Test that the coverage check function exists and returns expected structure."
  :tags '(:unit :mcp :coverage)
  (let ((result (claude-mcp-check-test-coverage)))
    (should (listp result))
    (should (plist-get result :registered))
    (should (plist-get result :tested))
    (should (numberp (plist-get result :coverage-pct)))))

(ert-deftest claude-mcp-coverage-test-registered-tools-nonempty ()
  "Test that there are registered tools."
  :tags '(:unit :mcp :coverage)
  (let ((result (claude-mcp-check-test-coverage)))
    (should (> (length (plist-get result :registered)) 0))))

(ert-deftest claude-mcp-coverage-test-report-format ()
  "Test that the coverage report is formatted correctly."
  :tags '(:unit :mcp :coverage)
  (let ((report (claude-mcp-coverage-report)))
    (should (stringp report))
    (should (string-match-p "MCP Tool Test Coverage" report))
    (should (string-match-p "%" report))))

(ert-deftest claude-mcp-coverage-test-core-tools-have-tests ()
  "Test that core tools (lock, unlock, edit, etc.) have tests.
This test fails if a core tool is missing test coverage."
  :tags '(:unit :mcp :coverage)
  (let* ((result (claude-mcp-check-test-coverage))
         (tested (plist-get result :tested))
         (core-tools '("lock" "unlock" "edit" "read_buffer"
                       "get_buffer_content" "search_buffer"
                       "eval" "buffer_info" "list_buffers")))
    (dolist (tool core-tools)
      (should (member tool tested)))))

(ert-deftest claude-mcp-coverage-test-messaging-tools-have-tests ()
  "Test that messaging tools have tests."
  :tags '(:unit :mcp :coverage)
  (let* ((result (claude-mcp-check-test-coverage))
         (tested (plist-get result :tested)))
    (dolist (tool '("check_messages" "message_board_summary" "list_agents"))
      (should (member tool tested)))))

(ert-deftest claude-mcp-coverage-test-todo-tools-have-tests ()
  "Test that TODO tools have tests.
Skipped when todo.el couldn't be loaded (requires org-roam)."
  :tags '(:unit :mcp :coverage)
  (skip-unless (featurep 'todo))
  (let* ((result (claude-mcp-check-test-coverage))
         (tested (plist-get result :tested)))
    (dolist (tool '("todo_current" "todo_list" "todo_create"
                    "todo_add_progress" "todo_update_status"
                    "todo_acceptance_criteria" "todo_check_acceptance"))
      (should (member tool tested)))))

(ert-deftest claude-mcp-coverage-test-kb-tools-have-tests ()
  "Test that KB tools have tests.
Skipped when claude-kb.el couldn't be loaded (requires org-roam)."
  :tags '(:unit :mcp :coverage)
  (skip-unless (featurep 'claude-kb))
  (let* ((result (claude-mcp-check-test-coverage))
         (tested (plist-get result :tested)))
    (dolist (tool '("kb_create" "kb_search" "kb_get" "kb_update" "kb_list"))
      (should (member tool tested)))))

(ert-deftest claude-mcp-coverage-test-oneshot-tools-have-tests ()
  "Test that oneshot tools have tests."
  :tags '(:unit :mcp :coverage)
  (let* ((result (claude-mcp-check-test-coverage))
         (tested (plist-get result :tested)))
    (dolist (tool '("done" "update_target"))
      (should (member tool tested)))))

(provide 'claude-mcp-coverage)
;;; claude-mcp-coverage.el ends here
