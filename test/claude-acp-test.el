;;; claude-acp-test.el --- Tests for claude-acp.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the ACP backend module.

;;; Code:

(require 'ert)

;; Add package root and vendor dirs to load-path for testing
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root-dir (file-name-directory (directory-file-name test-dir)))
       (vendor-acp-dir (expand-file-name "vendor/acp.el" root-dir)))
  (add-to-list 'load-path root-dir)
  (when (file-directory-p vendor-acp-dir)
    (add-to-list 'load-path vendor-acp-dir)))

(require 'claude-acp)

;;;; Module loading tests

(ert-deftest claude-acp-test-module-loads ()
  "Test that claude-acp module loads successfully."
  :tags '(:unit :acp)
  (should (featurep 'claude-acp)))

(ert-deftest claude-acp-test-acp-dependency ()
  "Test that acp.el dependency is available."
  :tags '(:unit :acp)
  (should (featurep 'acp)))

;;;; Configuration tests

(ert-deftest claude-acp-test-default-protocol-version ()
  "Test default protocol version."
  :tags '(:unit :acp)
  (should (stringp claude-acp-protocol-version))
  (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$"
                          claude-acp-protocol-version)))

(ert-deftest claude-acp-test-default-authentication ()
  "Test default authentication is login-based."
  :tags '(:unit :acp)
  (should (plist-get claude-acp-authentication :login)))

(ert-deftest claude-acp-test-environment-variables-login ()
  "Test environment variables for login auth."
  :tags '(:unit :acp)
  (let ((claude-acp-authentication '(:login t)))
    (let ((vars (claude-acp--environment-variables)))
      (should (member "ANTHROPIC_API_KEY=" vars)))))

(ert-deftest claude-acp-test-environment-variables-api-key ()
  "Test environment variables for API key auth."
  :tags '(:unit :acp)
  (let ((claude-acp-authentication '(:api-key "test-key-123")))
    (let ((vars (claude-acp--environment-variables)))
      (should (member "ANTHROPIC_API_KEY=test-key-123" vars)))))

(ert-deftest claude-acp-test-environment-variables-api-key-fn ()
  "Test environment variables for API key function auth."
  :tags '(:unit :acp)
  (let ((claude-acp-authentication '(:api-key-fn (lambda () "dynamic-key"))))
    (let ((vars (claude-acp--environment-variables)))
      (should (member "ANTHROPIC_API_KEY=dynamic-key" vars)))))

;;;; MCP server config tests

(ert-deftest claude-acp-test-mcp-server-config ()
  "Test MCP server configuration generation."
  :tags '(:unit :acp)
  ;; Mock claude--package-root
  (cl-letf (((symbol-function 'claude--package-root)
             (lambda () "/mock/package/root/")))
    (let ((config (claude-acp--mcp-server-config "/tmp/project" "*claude:project*")))
      (should config)
      (should (assoc "emacs" config))
      (let ((emacs-config (cdr (assoc "emacs" config))))
        (should (equal (cdr (assoc 'command emacs-config)) "uv"))))))

;;;; Session update dispatch tests

(ert-deftest claude-acp-test-handle-session-update-assistant-text ()
  "Test that assistant text chunks dispatch correctly."
  :tags '(:unit :acp)
  (let ((dispatched-messages nil)
        (claude-acp--current-assistant-text ""))
    (cl-letf (((symbol-function 'claude-agent--dispatch-message)
               (lambda (type msg)
                 (push (cons type msg) dispatched-messages))))
      ;; First chunk should trigger assistant_start + assistant_text
      (claude-acp--handle-session-update
       '((sessionUpdate . ((type . "agent_message_chunk")
                           (text . "Hello")))))
      (should (= (length dispatched-messages) 2))
      (should (equal (caar dispatched-messages) "assistant_text"))
      (should (equal (car (cadr dispatched-messages)) "assistant_start")))))

(ert-deftest claude-acp-test-handle-session-update-tool-call ()
  "Test that tool call updates dispatch correctly."
  :tags '(:unit :acp)
  (let ((dispatched-messages nil)
        (claude-acp--current-assistant-text ""))
    (cl-letf (((symbol-function 'claude-agent--dispatch-message)
               (lambda (type msg)
                 (push (cons type msg) dispatched-messages))))
      (claude-acp--handle-session-update
       '((sessionUpdate . ((type . "tool_call")
                           (toolCall . ((name . "Read")
                                        (id . "tc_123")
                                        (input . ((file_path . "/tmp/test.txt")))))))))
      (should (= (length dispatched-messages) 1))
      (should (equal (caar dispatched-messages) "tool_call")))))

(ert-deftest claude-acp-test-maybe-end-assistant-message ()
  "Test that assistant message blocks are properly ended."
  :tags '(:unit :acp)
  (let ((dispatched-messages nil))
    (cl-letf (((symbol-function 'claude-agent--dispatch-message)
               (lambda (type msg)
                 (push (cons type msg) dispatched-messages))))
      ;; When no text accumulated, should not dispatch
      (let ((claude-acp--current-assistant-text ""))
        (claude-acp--maybe-end-assistant-message)
        (should (= (length dispatched-messages) 0)))
      ;; When text accumulated, should dispatch assistant_end
      (let ((claude-acp--current-assistant-text "some text"))
        (claude-acp--maybe-end-assistant-message)
        (should (= (length dispatched-messages) 1))
        (should (equal (caar dispatched-messages) "assistant_end"))))))

;;;; File operation tests

(ert-deftest claude-acp-test-extract-buffer-text-full ()
  "Test extracting full buffer text."
  :tags '(:unit :acp)
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3\n")
    (let ((text (claude-acp--extract-buffer-text 1 nil)))
      (should (string= text "line 1\nline 2\nline 3\n")))))

(ert-deftest claude-acp-test-extract-buffer-text-offset ()
  "Test extracting buffer text with line offset."
  :tags '(:unit :acp)
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3\n")
    (let ((text (claude-acp--extract-buffer-text 2 nil)))
      (should (string= text "line 2\nline 3\n")))))

(ert-deftest claude-acp-test-extract-buffer-text-limit ()
  "Test extracting buffer text with limit."
  :tags '(:unit :acp)
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3\n")
    (let ((text (claude-acp--extract-buffer-text 1 2)))
      (should (string= text "line 1\nline 2\n")))))

;;;; Permission handling tests

(ert-deftest claude-acp-test-scope-to-option-id ()
  "Test mapping permission scope to ACP option ID."
  :tags '(:unit :acp)
  ;; With matching options
  (let ((options (vector '((id . "allow_once")) '((id . "allow_session")))))
    (should (equal (claude-acp--scope-to-option-id :once options) "allow_once"))
    (should (equal (claude-acp--scope-to-option-id :session options) "allow_session")))
  ;; Without options
  (should (equal (claude-acp--scope-to-option-id :once nil) "allow_once"))
  (should (equal (claude-acp--scope-to-option-id :session nil) "allow_session"))
  (should (equal (claude-acp--scope-to-option-id :always nil) "allow_always")))

(provide 'claude-acp-test)
;;; claude-acp-test.el ends here
