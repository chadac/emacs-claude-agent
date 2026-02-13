;;; test-helper.el --- Common test setup for claude-agent tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Common test setup for all test files.
;; All test files should load this before requiring modules.
;;
;; NOTE: This file intentionally does NOT require claude-mcp or other modules
;; with heavy dependencies (like org-roam). Each test file should require
;; only the modules it needs to test.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add the project root to load-path
(defvar test-helper--project-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name)))
  "Root directory of the project.")

(add-to-list 'load-path test-helper--project-root)

;; Don't require claude-mcp here - it has heavy dependencies (org-roam via claude-kb)
;; Each test file should require only the modules it needs.

(provide 'test-helper)
;;; test-helper.el ends here
