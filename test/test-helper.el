;;; test-helper.el --- Common test setup for claude-agent tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Common test setup for all test files.
;; All test files should load this before requiring modules.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add the project root to load-path
(defvar test-helper--project-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name)))
  "Root directory of the project.")

(add-to-list 'load-path test-helper--project-root)

;; Load the main module (circular dependency has been fixed by extracting
;; the tool registry to claude-mcp-registry.el)
(require 'claude-mcp nil t)

(provide 'test-helper)
;;; test-helper.el ends here
