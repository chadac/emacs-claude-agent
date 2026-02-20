;;; claude-agent-repl-test.el --- Tests for claude-agent-repl.el -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;; Test suite for claude-agent-repl.el REPL functionality.
;;
;; These tests avoid requiring heavy dependencies (org-roam, etc.) by:
;; - Testing keybindings by parsing the source file directly
;; - Testing pure functions that don't require full module loading
;;
;; For tests that need the full module loaded, use the integration test suite.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add parent directory to load path
(defvar claude-repl-test--project-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name)))
  "Root directory of the project.")

(add-to-list 'load-path claude-repl-test--project-root)

;;; Keybinding Tests (source parsing - no module load required)

(ert-deftest claude-repl-test-show-tool-result-keybinding-in-source ()
  "Test that C-c ' is bound to `claude-agent-show-tool-result' in the source."
  :tags '(:unit :repl :keybindings)
  ;; Parse the source file to verify the keybinding exists
  (let ((source-file (expand-file-name "claude-agent-repl.el" claude-repl-test--project-root)))
    (should (file-exists-p source-file))
    (with-temp-buffer
      (insert-file-contents source-file)
      ;; Look for the keybinding definition
      (goto-char (point-min))
      ;; Should find: (define-key map (kbd "C-c '") #'claude-agent-show-tool-result)
      (should (search-forward "(define-key map (kbd \"C-c '\") #'claude-agent-show-tool-result)" nil t)))))

(ert-deftest claude-repl-test-keybindings-documented-correctly ()
  "Test that documented keybindings match actual bindings in source."
  :tags '(:unit :repl :keybindings)
  (let ((source-file (expand-file-name "claude-agent-repl.el" claude-repl-test--project-root)))
    (should (file-exists-p source-file))
    (with-temp-buffer
      (insert-file-contents source-file)
      ;; The hint text should match the actual keybinding
      ;; Hint says: "C-c ' full result"
      (goto-char (point-min))
      (when (search-forward "C-c ' full result" nil t)
        ;; Now verify the actual binding exists
        (goto-char (point-min))
        (should (search-forward "(define-key map (kbd \"C-c '\") #'claude-agent-show-tool-result)" nil t))))))

(ert-deftest claude-repl-test-essential-keybindings-in-source ()
  "Test that essential REPL keybindings are defined in source."
  :tags '(:unit :repl :keybindings)
  (let ((source-file (expand-file-name "claude-agent-repl.el" claude-repl-test--project-root)))
    (with-temp-buffer
      (insert-file-contents source-file)
      ;; Tool result viewing - C-c '
      (goto-char (point-min))
      (should (search-forward "\"C-c '\"" nil t))
      ;; TAB for toggle popup
      (goto-char (point-min))
      (should (search-forward "(kbd \"TAB\") #'claude-agent-toggle-tool-popup" nil t))
      ;; Navigation - { and }
      (goto-char (point-min))
      (should (search-forward "(kbd \"{\") #'claude-agent-previous-section" nil t))
      (goto-char (point-min))
      (should (search-forward "(kbd \"}\") #'claude-agent-next-section" nil t))
      ;; Input - i and RET
      (goto-char (point-min))
      (should (search-forward "(kbd \"i\") #'claude-agent-goto-input" nil t))
      (goto-char (point-min))
      (should (search-forward "(kbd \"RET\") #'claude-agent-goto-input" nil t)))))

;;; Tool Result Lookup Tests (pure functions, minimal deps)
;;
;; These tests define local stubs for the functions being tested,
;; extracted from the source, to avoid loading the full module.

(defvar-local claude-agent--tool-results nil
  "Test stub for tool results alist.")

(defun claude-repl-test--find-tool-result-at-point ()
  "Test version of `claude-agent--find-tool-result-at-point'.
Extracted from claude-agent-repl.el for isolated testing."
  (let ((line-start (line-beginning-position))
        (line-end (line-end-position))
        (result nil))
    (dolist (entry claude-agent--tool-results)
      (let ((marker (car entry)))
        (when (and (marker-position marker)
                   (>= (marker-position marker) line-start)
                   (<= (marker-position marker) line-end))
          (setq result entry))))
    (when result
      (cons (nth 1 result) (nth 2 result)))))

(ert-deftest claude-repl-test-find-tool-result-at-point-empty ()
  "Test finding tool result with no results stored."
  :tags '(:unit :repl :tool-results)
  (with-temp-buffer
    (setq-local claude-agent--tool-results nil)
    (insert "some text\n")
    (goto-char (point-min))
    (should (null (claude-repl-test--find-tool-result-at-point)))))

(ert-deftest claude-repl-test-find-tool-result-at-point-found ()
  "Test finding tool result when cursor is on correct line."
  :tags '(:unit :repl :tool-results)
  (with-temp-buffer
    (insert "○ read› /tmp/test.txt\n")
    (insert "some other text\n")
    (let ((marker (copy-marker 1)))
      (setq-local claude-agent--tool-results
                  (list (list marker "Read" "file content here")))
      (goto-char 1)
      (let ((result (claude-repl-test--find-tool-result-at-point)))
        (should result)
        (should (equal (car result) "Read"))
        (should (equal (cdr result) "file content here"))))))

(ert-deftest claude-repl-test-find-tool-result-at-point-wrong-line ()
  "Test finding tool result returns nil when on different line."
  :tags '(:unit :repl :tool-results)
  (with-temp-buffer
    (insert "○ read› /tmp/test.txt\n")
    (insert "some other text\n")
    (let ((marker (copy-marker 1)))
      (setq-local claude-agent--tool-results
                  (list (list marker "Read" "file content here")))
      (goto-char (point-max))
      (forward-line -1)
      (should (null (claude-repl-test--find-tool-result-at-point))))))

(ert-deftest claude-repl-test-find-tool-result-multiple-tools ()
  "Test finding correct tool result with multiple tools."
  :tags '(:unit :repl :tool-results)
  (with-temp-buffer
    (insert "○ read› /tmp/file1.txt\n")
    (let ((marker1 (copy-marker 1)))
      (insert "○ read› /tmp/file2.txt\n")
      (goto-char (point-min))
      (forward-line 1)
      (let ((marker2 (copy-marker (point))))
        (setq-local claude-agent--tool-results
                    (list (list marker2 "Read" "content of file2")
                          (list marker1 "Read" "content of file1")))
        ;; Check first line
        (goto-char 1)
        (let ((result (claude-repl-test--find-tool-result-at-point)))
          (should result)
          (should (equal (cdr result) "content of file1")))
        ;; Check second line
        (goto-char (marker-position marker2))
        (let ((result (claude-repl-test--find-tool-result-at-point)))
          (should result)
          (should (equal (cdr result) "content of file2")))))))

;;; Tool Result Storage Structure Tests

(ert-deftest claude-repl-test-tool-results-structure ()
  "Test that tool results have correct 3-element list structure."
  :tags '(:unit :repl :tool-results)
  (with-temp-buffer
    (let ((marker (point-marker)))
      (setq-local claude-agent--tool-results nil)
      ;; Simulate pushing a result (as dispatch-message does)
      (push (list marker "TestTool" "test content")
            claude-agent--tool-results)
      (let ((entry (car claude-agent--tool-results)))
        (should (= (length entry) 3))
        (should (markerp (nth 0 entry)))
        (should (equal (nth 1 entry) "TestTool"))
        (should (equal (nth 2 entry) "test content"))))))

;;; Diff Formatter Tests (pure function)

(defun claude-repl-test--format-diff-output (content)
  "Test version of `claude-agent--format-diff-output'.
Extracted from claude-agent-repl.el for isolated testing."
  (let ((lines (split-string content "\n")))
    (mapconcat
     (lambda (line)
       (cond
        ((string-prefix-p "- " line)
         (propertize line 'face 'diff-removed))
        ((string-prefix-p "+ " line)
         (propertize line 'face 'diff-added))
        (t line)))
     lines "\n")))

(ert-deftest claude-repl-test-format-diff-output ()
  "Test diff output formatting applies faces correctly."
  :tags '(:unit :repl :formatters)
  (let ((diff-content "- old line\n+ new line\n  unchanged"))
    (let ((formatted (claude-repl-test--format-diff-output diff-content)))
      (should (stringp formatted))
      ;; First line (removed) should have face
      (should (eq (get-text-property 0 'face formatted) 'diff-removed))
      ;; Find the + line and check its face
      (let ((plus-pos (string-match "\\+ new line" formatted)))
        (should plus-pos)
        (should (eq (get-text-property plus-pos 'face formatted) 'diff-added))))))

(ert-deftest claude-repl-test-format-diff-preserves-content ()
  "Test diff formatting preserves line content."
  :tags '(:unit :repl :formatters)
  (let* ((diff-content "- removed\n+ added\n  context")
         (formatted (claude-repl-test--format-diff-output diff-content)))
    (should (string-match-p "- removed" formatted))
    (should (string-match-p "\\+ added" formatted))
    (should (string-match-p "context" formatted))))

(provide 'claude-agent-repl-test)
;;; claude-agent-repl-test.el ends here
