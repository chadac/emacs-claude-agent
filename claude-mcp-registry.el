;;; claude-mcp-registry.el --- MCP tool registry and macro -*- lexical-binding: t; -*-

;; This file is part of Claude.

;;; Commentary:

;; This file contains the core MCP tool registry infrastructure:
;; - The claude-mcp-tools hash table
;; - The claude-mcp-deftool macro for registering tools
;; - Helper functions for exporting and managing tools
;;
;; This is separated from claude-mcp.el to break the circular dependency
;; between claude-mcp.el and claude-mcp-messaging.el. Both files can
;; require this registry file without creating a cycle.

;;; Code:

(require 'cl-lib)

;;;; Tool Registry

(defvar claude-mcp-tools (make-hash-table :test 'equal)
  "Registry of MCP tools. Key is tool name string, value is plist.")

(defmacro claude-mcp-deftool (name docstring &rest args)
  "Define an MCP tool NAME with DOCSTRING.
NAME uses Lisp conventions (dashes), automatically converted to underscores for MCP.
ARGS is a plist with :function, :safe, :needs-session-cwd, and :args keys.

:safe t marks tool as safe (no side effects, can be pre-authorized)
:needs-session-cwd t marks tool as needing session context (default-directory binding)

Example:
  (claude-mcp-deftool get-buffer-content
    \"Get the content of an Emacs buffer.\"
    :function #'claude-mcp-get-buffer-content
    :safe t
    :needs-session-cwd t
    :args ((buffer-name string :required \"Name of the buffer\")
           (tail-lines integer \"Optional: last N lines\")))"
  (declare (indent 2) (doc-string 2))
  (let ((mcp-name (replace-regexp-in-string "-" "_" (symbol-name name)))
        (quoted-args (cl-loop for (key val) on args by #'cddr
                              append (list key (if (eq key :args) `',val val)))))
    `(puthash ,mcp-name
              (list :description ,docstring
                    ,@quoted-args)
              claude-mcp-tools)))

(defun claude-mcp--convert-args (args)
  "Convert ARGS list to hash table for JSON export.
Each arg is (name type [:required] description).
Converts dashes to underscores in arg names."
  (let ((result (make-hash-table :test 'equal)))
    (dolist (arg args)
      (let* ((name (replace-regexp-in-string "-" "_" (symbol-name (nth 0 arg))))
             (type (symbol-name (nth 1 arg)))
             (rest (nthcdr 2 arg))
             (required (eq (car rest) :required))
             (desc (if required (cadr rest) (car rest))))
        (puthash name
                 `((type . ,type)
                   (required . ,(if required t :json-false))
                   (description . ,(or desc "")))
                 result)))
    result))

(defun claude-mcp-export-tools ()
  "Export registered tools as JSON for MCP server.
Called by Python server via emacsclient to get tool definitions."
  (let ((tools (make-hash-table :test 'equal)))
    (maphash
     (lambda (name def)
       (let ((tool-def `((description . ,(or (plist-get def :description) ""))
                         (function . ,(symbol-name (plist-get def :function)))
                         (safe . ,(if (plist-get def :safe) t :json-false))
                         (needs_session_cwd . ,(if (plist-member def :needs-session-cwd)
                                                   (if (plist-get def :needs-session-cwd) t :json-false)
                                                 t))  ; default to t if not specified
                         (args . ,(claude-mcp--convert-args (plist-get def :args))))))
         ;; Add context hint if specified
         (when-let ((context (plist-get def :context)))
           (push (cons 'context context) tool-def))
         (puthash name tool-def tools)))
     claude-mcp-tools)
    (json-encode tools)))

(defun claude-mcp-remove-tool (name)
  "Remove tool NAME from the registry."
  (remhash (if (symbolp name) (symbol-name name) name) claude-mcp-tools))

(defun claude-mcp-list-tools ()
  "List all registered tool names."
  (let (names)
    (maphash (lambda (k _v) (push k names)) claude-mcp-tools)
    (sort names #'string<)))

(provide 'claude-mcp-registry)
;;; claude-mcp-registry.el ends here
