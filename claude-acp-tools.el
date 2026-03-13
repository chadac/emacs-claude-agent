;;; claude-acp-tools.el --- ACP tool request handlers -*- lexical-binding: t; -*-

;; This file is part of Claude Agent.

;;; Commentary:

;; This module handles ACP tool-related requests from the agent.
;; When the ACP agent wants to read/write files or execute commands,
;; it sends requests that we intercept and handle in Emacs.
;;
;; This allows us to:
;; - Read from open Emacs buffers (including unsaved changes)
;; - Use our lock-based edit workflow for writes
;; - Run permission checks before tool execution
;; - Integrate with the MCP tool system

;;; Code:

(require 'claude-acp)
(require 'map)

;; Forward declarations
(declare-function claude-agent-permission-check "claude-agent-permissions")

;;;; Terminal/execute handler (placeholder for Phase 2)

;; The terminal/execute request type allows the agent to run shell commands.
;; For now, the built-in ACP agent handles this directly.
;; In Phase 2, we'll intercept these to add permission checks.

;;;; File operation utilities

(defun claude-acp-tools--resolve-path (path)
  "Resolve PATH relative to the session working directory."
  (if (file-name-absolute-p path)
      path
    (expand-file-name path (or (bound-and-true-p claude-mcp--cwd)
                               default-directory))))

(defun claude-acp-tools--buffer-content-at (path &optional line limit)
  "Get content from buffer visiting PATH, starting at LINE with LIMIT.
Returns nil if no buffer is visiting the file."
  (when-let ((buffer (find-buffer-visiting path)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (when (and line (> line 1))
            (forward-line (1- line)))
          (let ((start (point))
                (end (if limit
                         (progn (forward-line limit) (point))
                       (point-max))))
            (buffer-substring-no-properties start end)))))))

(provide 'claude-acp-tools)
;;; claude-acp-tools.el ends here
