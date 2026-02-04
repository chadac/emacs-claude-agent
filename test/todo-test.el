;;; todo-test.el --- Tests for TODO MCP tools -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0") (org-roam "2.0"))

;;; Commentary:
;; Unit tests for TODO management MCP tools in todo.el.
;; Covers: todo_create, todo_list, todo_update_status,
;; todo_acceptance_criteria, todo_check_acceptance, todo_update_acceptance,
;; todo_add_progress and helper functions.
;;
;; Note: Tests that require org-roam-db are skipped when org-roam is
;; not available. Tests for file-based operations use temp directories.
;;
;; Run with:
;;   emacs -batch -l ert -l test/todo-test.el -f ert-run-tests-batch

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load test helper (handles circular dependency between claude-mcp and sub-modules)
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'test-helper)
;; Try to load todo.el - some tests need org-roam
(condition-case nil
    (require 'todo)
  (error nil))

;;; Test Utilities

(defvar todo-test--temp-dir nil
  "Temporary directory for test TODO files.")

(defmacro todo-test-with-temp-todo (content &rest body)
  "Execute BODY with a temp TODO file containing CONTENT.
Binds `todo-file' to the file path."
  (declare (indent 1))
  `(let* ((temp-dir (make-temp-file "todo-test-" t))
          (todo-file (expand-file-name "test-todo.org" temp-dir))
          (inhibit-message t))
     (unwind-protect
         (progn
           (with-temp-file todo-file
             (insert ,content))
           ,@body)
       ;; Clean up any visiting buffers
       (dolist (buf (buffer-list))
         (when (and (buffer-file-name buf)
                    (string-prefix-p temp-dir (buffer-file-name buf)))
           (with-current-buffer buf
             (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory temp-dir t))))

;;; ============================================================
;;; Slug Helper Tests
;;; ============================================================

(ert-deftest todo-test-slugify ()
  "Test slug generation from text."
  :tags '(:unit :mcp :todo)
  (when (fboundp 'org-roam-todo--slugify)
    (should (string= "add-feature" (org-roam-todo--slugify "Add Feature")))
    (should (string= "fix-bug-123" (org-roam-todo--slugify "Fix Bug #123")))
    (should (string= "hello-world" (org-roam-todo--slugify "hello world")))))

(ert-deftest todo-test-project-name ()
  "Test extracting project name from root."
  :tags '(:unit :mcp :todo)
  (when (fboundp 'org-roam-todo--project-name)
    (should (string= "my-project" (org-roam-todo--project-name "/path/to/my-project/")))
    (should (string= "my-project" (org-roam-todo--project-name "/path/to/my-project")))))

;;; ============================================================
;;; Acceptance Criteria Tests (file-based, no org-roam needed)
;;; ============================================================

(ert-deftest todo-test-get-acceptance-criteria ()
  "Test getting acceptance criteria from a TODO file."
  :tags '(:unit :mcp :todo)
  (when (fboundp 'org-roam-todo-mcp-get-acceptance-criteria)
    (todo-test-with-temp-todo
      ":PROPERTIES:
:ID: test123
:PROJECT_NAME: test-project
:PROJECT_ROOT: /tmp/test
:STATUS: active
:END:
#+title: Test TODO

** Task Description
Do something.

** Acceptance Criteria
- [ ] First criterion
- [X] Second criterion (done)
- [ ] Third criterion

** Progress Log
"
      (let ((result (org-roam-todo-mcp-get-acceptance-criteria todo-file)))
        (should (stringp result))
        (let ((parsed (json-read-from-string result)))
          (should (= 3 (length parsed)))
          ;; First item should be unchecked
          (should (string= "First criterion" (cdr (assq 'text (aref parsed 0)))))
          (should (eq :json-false (cdr (assq 'checked (aref parsed 0)))))
          ;; Second item should be checked
          (should (string= "Second criterion (done)" (cdr (assq 'text (aref parsed 1)))))
          (should (eq t (cdr (assq 'checked (aref parsed 1))))))))))

(ert-deftest todo-test-check-acceptance ()
  "Test checking an acceptance criteria item."
  :tags '(:unit :mcp :todo)
  (when (fboundp 'org-roam-todo-mcp-check-acceptance)
    (todo-test-with-temp-todo
      ":PROPERTIES:
:ID: test123
:PROJECT_NAME: test-project
:PROJECT_ROOT: /tmp/test
:STATUS: active
:END:
#+title: Test TODO

** Task Description
Do something.

** Acceptance Criteria
- [ ] First criterion
- [ ] Second criterion

** Progress Log
"
      ;; Check the first criterion
      (let ((result (org-roam-todo-mcp-check-acceptance "First criterion" t todo-file)))
        (should (string-match-p "Checked" result)))
      ;; Verify it was checked in the file
      (with-temp-buffer
        (insert-file-contents todo-file)
        (should (string-match-p "\\[X\\] First criterion" (buffer-string)))
        (should (string-match-p "\\[ \\] Second criterion" (buffer-string)))))))

(ert-deftest todo-test-uncheck-acceptance ()
  "Test unchecking an acceptance criteria item."
  :tags '(:unit :mcp :todo)
  (when (fboundp 'org-roam-todo-mcp-check-acceptance)
    (todo-test-with-temp-todo
      ":PROPERTIES:
:ID: test123
:PROJECT_NAME: test-project
:PROJECT_ROOT: /tmp/test
:STATUS: active
:END:
#+title: Test TODO

** Task Description
Do something.

** Acceptance Criteria
- [X] Done criterion
- [ ] Undone criterion

** Progress Log
"
      ;; Uncheck the done criterion
      (let ((result (org-roam-todo-mcp-check-acceptance "Done criterion" nil todo-file)))
        (should (string-match-p "Unchecked" result)))
      ;; Verify it was unchecked
      (with-temp-buffer
        (insert-file-contents todo-file)
        (should (string-match-p "\\[ \\] Done criterion" (buffer-string)))))))

(ert-deftest todo-test-check-acceptance-not-found ()
  "Test checking a nonexistent acceptance criteria item."
  :tags '(:unit :mcp :todo)
  (when (fboundp 'org-roam-todo-mcp-check-acceptance)
    (todo-test-with-temp-todo
      ":PROPERTIES:
:ID: test123
:PROJECT_NAME: test-project
:PROJECT_ROOT: /tmp/test
:STATUS: active
:END:
#+title: Test TODO

** Acceptance Criteria
- [ ] Existing criterion

** Progress Log
"
      (should-error (org-roam-todo-mcp-check-acceptance "Nonexistent item" t todo-file)
                    :type 'error))))

;;; ============================================================
;;; Update Status Tests
;;; ============================================================

(ert-deftest todo-test-update-status ()
  "Test updating TODO status."
  :tags '(:unit :mcp :todo)
  (when (fboundp 'org-roam-todo-mcp-update-status)
    (todo-test-with-temp-todo
      ":PROPERTIES:
:ID: test123
:PROJECT_NAME: test-project
:PROJECT_ROOT: /tmp/test
:STATUS: draft
:END:
#+title: Test TODO

** Task Description
Do something.
"
      (let ((result (org-roam-todo-mcp-update-status "active" todo-file)))
        (should (string-match-p "active" result)))
      ;; Verify file was updated
      (with-temp-buffer
        (insert-file-contents todo-file)
        (should (string-match-p ":STATUS: active" (buffer-string)))))))

(ert-deftest todo-test-update-status-invalid ()
  "Test updating to invalid status errors."
  :tags '(:unit :mcp :todo)
  (when (fboundp 'org-roam-todo-mcp-update-status)
    (todo-test-with-temp-todo
      ":PROPERTIES:
:ID: test123
:PROJECT_NAME: test-project
:PROJECT_ROOT: /tmp/test
:STATUS: draft
:END:
#+title: Test TODO
"
      (should-error (org-roam-todo-mcp-update-status "invalid-status" todo-file)
                    :type 'error))))

;;; ============================================================
;;; Add Progress Tests
;;; ============================================================

(ert-deftest todo-test-add-progress ()
  "Test adding a progress entry."
  :tags '(:unit :mcp :todo)
  (when (fboundp 'org-roam-todo-mcp-add-progress)
    (todo-test-with-temp-todo
      ":PROPERTIES:
:ID: test123
:PROJECT_NAME: test-project
:PROJECT_ROOT: /tmp/test
:STATUS: active
:END:
#+title: Test TODO

** Task Description
Do something.

** Progress Log

"
      (let ((result (org-roam-todo-mcp-add-progress "Did something important" todo-file)))
        (should (stringp result)))
      ;; Verify progress was added
      (with-temp-buffer
        (insert-file-contents todo-file)
        (should (string-match-p "Did something important" (buffer-string)))))))

(ert-deftest todo-test-add-progress-creates-section ()
  "Test that add-progress creates Progress Log section if missing."
  :tags '(:unit :mcp :todo)
  (when (fboundp 'org-roam-todo-mcp-add-progress)
    (todo-test-with-temp-todo
      ":PROPERTIES:
:ID: test123
:PROJECT_NAME: test-project
:PROJECT_ROOT: /tmp/test
:STATUS: active
:END:
#+title: Test TODO Without Progress Log

** Task Description
Do something.
"
      (org-roam-todo-mcp-add-progress "First progress entry" todo-file)
      (with-temp-buffer
        (insert-file-contents todo-file)
        (should (string-match-p "Progress Log" (buffer-string)))
        (should (string-match-p "First progress entry" (buffer-string)))))))

;;; ============================================================
;;; Update Acceptance Criteria Tests
;;; ============================================================

(ert-deftest todo-test-update-acceptance ()
  "Test replacing all acceptance criteria."
  :tags '(:unit :mcp :todo)
  (when (fboundp 'org-roam-todo-mcp-update-acceptance)
    (todo-test-with-temp-todo
      ":PROPERTIES:
:ID: test123
:PROJECT_NAME: test-project
:PROJECT_ROOT: /tmp/test
:STATUS: active
:END:
#+title: Test TODO

** Task Description
Do something.

** Acceptance Criteria
- [ ] Old criterion 1
- [X] Old criterion 2

** Progress Log
"
      (let ((new-criteria '(((text . "New criterion A") (checked . t))
                            ((text . "New criterion B") (checked . :json-false)))))
        (org-roam-todo-mcp-update-acceptance new-criteria todo-file))
      ;; Verify criteria were replaced
      (with-temp-buffer
        (insert-file-contents todo-file)
        (let ((content (buffer-string)))
          (should (string-match-p "New criterion A" content))
          (should (string-match-p "New criterion B" content))
          (should-not (string-match-p "Old criterion" content)))))))

;;; ============================================================
;;; Create TODO Tests (requires org-roam)
;;; ============================================================

(ert-deftest todo-test-create-requires-args ()
  "Test that create requires project_root and title."
  :tags '(:unit :mcp :todo)
  (when (fboundp 'org-roam-todo-mcp-create)
    (should-error (org-roam-todo-mcp-create nil "title")
                  :type 'error)
    (should-error (org-roam-todo-mcp-create "/tmp" nil)
                  :type 'error)))

;;; ============================================================
;;; Tool Registration Tests
;;; ============================================================

(ert-deftest todo-test-tools-registered ()
  "Test that TODO tools are registered in the tool registry."
  :tags '(:unit :mcp :todo :registration)
  (skip-unless (featurep 'todo))
  (should (gethash "todo_current" claude-mcp-tools))
  (should (gethash "todo_list" claude-mcp-tools))
  (should (gethash "todo_create" claude-mcp-tools))
  (should (gethash "todo_add_progress" claude-mcp-tools))
  (should (gethash "todo_update_status" claude-mcp-tools))
  (should (gethash "todo_acceptance_criteria" claude-mcp-tools))
  (should (gethash "todo_check_acceptance" claude-mcp-tools))
  (should (gethash "todo_update_acceptance" claude-mcp-tools))
  (should (gethash "todo_complete" claude-mcp-tools)))

(ert-deftest todo-test-tools-have-descriptions ()
  "Test that TODO tools have descriptions."
  :tags '(:unit :mcp :todo :registration)
  (skip-unless (featurep 'todo))
  (dolist (tool-name '("todo_current" "todo_list" "todo_create"
                       "todo_add_progress" "todo_update_status"
                       "todo_acceptance_criteria" "todo_check_acceptance"
                       "todo_update_acceptance" "todo_complete"))
    (let ((tool-def (gethash tool-name claude-mcp-tools)))
      (should tool-def)
      (should (stringp (plist-get tool-def :description)))
      (should (> (length (plist-get tool-def :description)) 0)))))

(provide 'todo-test)
;;; todo-test.el ends here
