;;; claude-oneshot-test.el --- Tests for oneshot agent MCP tools -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;; Unit tests for oneshot agent MCP tools in claude-oneshot.el.
;; Covers: done, update_target, and helper functions.
;; Note: Full agent lifecycle tests are integration-level and
;; covered in integration-specs.org.
;;
;; Run with:
;;   emacs -batch -l ert -l test/claude-oneshot-test.el -f ert-run-tests-batch

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load test helper (handles circular dependency between claude-mcp and sub-modules)
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'test-helper)
;; Conditionally load claude-oneshot
(condition-case nil
    (require 'claude-oneshot)
  (error nil))

;;; ============================================================
;;; Helper Function Tests
;;; ============================================================

(ert-deftest claude-oneshot-test-generate-buffer-name ()
  "Test buffer name generation."
  :tags '(:unit :mcp :oneshot)
  (when (fboundp 'claude-oneshot--generate-buffer-name)
    (let ((name1 (claude-oneshot--generate-buffer-name 'line))
          (name2 (claude-oneshot--generate-buffer-name 'region)))
      (should (stringp name1))
      (should (stringp name2))
      (should (string-match-p "oneshot-line" name1))
      (should (string-match-p "oneshot-region" name2))
      ;; Should be unique
      (should-not (string= name1 name2)))))

(ert-deftest claude-oneshot-test-create-border-string ()
  "Test border string creation."
  :tags '(:unit :mcp :oneshot)
  (when (fboundp 'claude-oneshot--create-border-string)
    (let ((border (claude-oneshot--create-border-string "test label")))
      (should (stringp border))
      (should (string-match-p "test label" border))
      (should (string-match-p "--" border)))))

(ert-deftest claude-oneshot-test-wrap-text ()
  "Test text wrapping."
  :tags '(:unit :mcp :oneshot)
  (when (fboundp 'claude-oneshot--wrap-text)
    (let ((lines (claude-oneshot--wrap-text "short" 80)))
      (should (= 1 (length lines)))
      (should (string= "short" (car lines))))))

(ert-deftest claude-oneshot-test-get-scope-system-prompt ()
  "Test scope-specific system prompt generation."
  :tags '(:unit :mcp :oneshot)
  (when (fboundp 'claude-oneshot--get-scope-system-prompt)
    ;; Test line scope
    (let ((prompt (claude-oneshot--get-scope-system-prompt
                   'line
                   '(:file "/test/file.el"
                     :start-line 10
                     :end-line 10
                     :content "test content"))))
      (should (stringp prompt))
      (should (string-match-p "LINE" prompt))
      (should (string-match-p "line 10" prompt))
      (should (string-match-p "test content" prompt)))
    ;; Test buffer scope
    (let ((prompt (claude-oneshot--get-scope-system-prompt
                   'buffer
                   '(:file "/test/file.el"))))
      (should (string-match-p "BUFFER" prompt))
      (should (string-match-p "/test/file.el" prompt)))))

(ert-deftest claude-oneshot-test-get-allowed-tools ()
  "Test getting allowed tools for different scopes."
  :tags '(:unit :mcp :oneshot)
  (when (fboundp 'claude-oneshot--get-allowed-tools-for-scope)
    ;; Line scope
    (let ((tools (claude-oneshot--get-allowed-tools-for-scope
                  'line
                  '(:file "/test/file.el"))))
      (should (listp tools))
      (should (member "mcp__emacs__done" tools)))
    ;; Directory scope
    (let ((tools (claude-oneshot--get-allowed-tools-for-scope
                  'directory
                  '(:directory "/test/"))))
      (should (listp tools))
      (should (member "mcp__emacs__done" tools)))
    ;; Project scope
    (let ((tools (claude-oneshot--get-allowed-tools-for-scope
                  'project
                  '(:project "/test/"))))
      (should (listp tools))
      (should (member "mcp__emacs__done" tools)))))

;;; ============================================================
;;; Done Function Tests
;;; ============================================================

(ert-deftest claude-oneshot-test-done-not-oneshot ()
  "Test calling done when not in a oneshot buffer."
  :tags '(:unit :mcp :oneshot)
  (when (fboundp 'claude-mcp-done)
    (let ((inhibit-message t)
          (result (claude-mcp-done "test message")))
      ;; Should succeed but indicate it's not a oneshot agent
      (should (stringp result))
      (should (string-match-p "not a oneshot" result)))))

(ert-deftest claude-oneshot-test-done-no-message ()
  "Test calling done without a message."
  :tags '(:unit :mcp :oneshot)
  (when (fboundp 'claude-mcp-done)
    (let ((inhibit-message t)
          (result (claude-mcp-done nil)))
      (should (stringp result)))))

;;; ============================================================
;;; Update Target Tests
;;; ============================================================

(ert-deftest claude-oneshot-test-update-target-not-oneshot ()
  "Test update-target when not in a oneshot buffer."
  :tags '(:unit :mcp :oneshot)
  (when (fboundp 'claude-mcp-update-target)
    (let ((inhibit-message t)
          (result (claude-mcp-update-target "/test/file.el" 1 10)))
      ;; Should return the result string (no-op when not in oneshot)
      (should (stringp result)))))

;;; ============================================================
;;; Overlay Tests
;;; ============================================================

(ert-deftest claude-oneshot-test-create-target-overlay ()
  "Test creating a target overlay."
  :tags '(:unit :mcp :oneshot)
  (when (fboundp 'claude-oneshot--create-target-overlay)
    (with-temp-buffer
      (insert "line1\nline2\nline3\n")
      (let ((ov (claude-oneshot--create-target-overlay
                 (current-buffer) 1 15)))
        (should (overlayp ov))
        (should (overlay-get ov 'claude-oneshot))
        ;; Clean up
        (delete-overlay ov)))))

(ert-deftest claude-oneshot-test-clear-target-overlay ()
  "Test clearing target overlays."
  :tags '(:unit :mcp :oneshot)
  (when (fboundp 'claude-oneshot--clear-target-overlay)
    (with-temp-buffer
      (insert "line1\nline2\nline3\n")
      (claude-oneshot--create-target-overlay (current-buffer) 1 15)
      ;; Should have oneshot overlays
      (should (cl-some (lambda (ov) (overlay-get ov 'claude-oneshot))
                       (overlays-in (point-min) (point-max))))
      ;; Clear them
      (claude-oneshot--clear-target-overlay (current-buffer))
      ;; Should be gone
      (should-not (cl-some (lambda (ov) (overlay-get ov 'claude-oneshot))
                           (overlays-in (point-min) (point-max)))))))

;;; ============================================================
;;; Tool Registration Tests
;;; ============================================================

(ert-deftest claude-oneshot-test-tools-registered ()
  "Test that oneshot tools are registered in the tool registry."
  :tags '(:unit :mcp :oneshot :registration)
  (should (gethash "done" claude-mcp-tools))
  (should (gethash "update_target" claude-mcp-tools)))

(ert-deftest claude-oneshot-test-tools-have-descriptions ()
  "Test that oneshot tools have descriptions."
  :tags '(:unit :mcp :oneshot :registration)
  (dolist (tool-name '("done" "update_target"))
    (let ((tool-def (gethash tool-name claude-mcp-tools)))
      (should tool-def)
      (should (stringp (plist-get tool-def :description)))
      (should (> (length (plist-get tool-def :description)) 0)))))

(provide 'claude-oneshot-test)
;;; claude-oneshot-test.el ends here
