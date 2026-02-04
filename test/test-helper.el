;;; test-helper.el --- Common test setup for claude-agent tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Handles the circular dependency between claude-mcp.el and
;; claude-mcp-messaging.el by loading files in the right order.
;; All test files should load this before requiring modules.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add the project root to load-path
(defvar test-helper--project-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name)))
  "Root directory of the project.")

(add-to-list 'load-path test-helper--project-root)

;; Break the circular dependency by loading claude-mcp.el with
;; the requires temporarily stubbed out.
(defvar test-helper--loaded nil "Whether test-helper has already loaded modules.")

(unless test-helper--loaded
  ;; Step 1: Load just the tool registry and core functions from claude-mcp.el
  ;; by temporarily making the sub-module requires no-ops.
  (let ((original-require (symbol-function 'require)))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 ;; Skip the circular deps on first load
                 (if (memq feature '(claude-mcp-messaging claude-mcp-magit
                                     claude-mcp-notes claude-kb))
                     (progn
                       ;; Provide a stub so the require doesn't fail later
                       (unless (featurep feature)
                         nil))
                   (funcall original-require feature filename noerror)))))
      (load (expand-file-name "claude-mcp.el" test-helper--project-root) nil t))
    ;; Step 2: Now load the sub-modules (they require claude-mcp which is now loaded)
    (condition-case nil (require 'claude-mcp-messaging) (error nil))
    (condition-case nil (require 'claude-mcp-magit) (error nil))
    (condition-case nil (require 'claude-mcp-notes) (error nil))
    (condition-case nil (require 'claude-kb) (error nil)))
  (setq test-helper--loaded t))

(provide 'test-helper)
;;; test-helper.el ends here
