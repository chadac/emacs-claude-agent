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

(ert-deftest claude-acp-test-handle-session-update-usage ()
  "Test that usage/cost updates dispatch correctly."
  :tags '(:unit :acp)
  (let ((dispatched-messages nil))
    (cl-letf (((symbol-function 'claude-agent--dispatch-message)
               (lambda (type msg)
                 (push (cons type msg) dispatched-messages))))
      (claude-acp--handle-session-update
       '((sessionUpdate . ((type . "usage")
                           (costUsd . 0.042)
                           (inputTokens . 1500)
                           (outputTokens . 300)))))
      ;; Should dispatch both result (for cost) and progress (for tokens)
      (should (>= (length dispatched-messages) 2))
      (let ((result-msg (cl-find "result" dispatched-messages :key #'car :test #'equal))
            (progress-msg (cl-find "progress" dispatched-messages :key #'car :test #'equal)))
        (should result-msg)
        (should progress-msg)))))

(ert-deftest claude-acp-test-handle-session-update-tool-call-update ()
  "Test that tool_call_update with completed status dispatches tool_result."
  :tags '(:unit :acp)
  (let ((dispatched-messages nil))
    (cl-letf (((symbol-function 'claude-agent--dispatch-message)
               (lambda (type msg)
                 (push (cons type msg) dispatched-messages))))
      (claude-acp--handle-session-update
       '((sessionUpdate . ((type . "tool_call_update")
                           (toolCall . ((id . "tc_123")
                                        (name . "Read")
                                        (status . "completed")
                                        (content . "file contents here")))))))
      ;; Should dispatch tool_result and tool_end
      (should (= (length dispatched-messages) 2))
      (should (equal (caar dispatched-messages) "tool_end"))
      (should (equal (car (cadr dispatched-messages)) "tool_result")))))

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

(ert-deftest claude-acp-test-permission-request-sets-policy-checked ()
  "Test that ACP permission dispatch includes policy_checked flag."
  :tags '(:unit :acp)
  (let ((dispatched-data nil)
        (claude-acp--pending-permission-request-id nil))
    (cl-letf (((symbol-function 'claude-agent-permission-handle-request)
               (lambda (_tool _input) nil))  ; No policy match
              ((symbol-function 'claude-agent--dispatch-message)
               (lambda (_type data)
                 (setq dispatched-data data))))
      (claude-acp--handle-permission-request
       42
       '((toolName . "Read")
         (toolInput . ((file_path . "/tmp/test.txt")))))
      ;; Should have dispatched with policy_checked flag
      (should dispatched-data)
      (should (cdr (assq 'policy_checked dispatched-data))))))

(ert-deftest claude-acp-test-permission-auto-allow-sends-response ()
  "Test that auto-allow policy sends ACP response directly."
  :tags '(:unit :acp)
  (let ((response-sent nil)
        (claude-acp--client 'mock-client)
        (claude-acp--pending-permission-request-id nil))
    (cl-letf (((symbol-function 'claude-agent-permission-handle-request)
               (lambda (_tool _input)
                 '(:allow :scope :session)))
              ((symbol-function 'claude-acp--send-permission-response)
               (lambda (req-id option-id cancelled)
                 (setq response-sent (list req-id option-id cancelled)))))
      (claude-acp--handle-permission-request
       42
       '((toolName . "Read")
         (options . [((id . "allow_once")) ((id . "allow_session"))])))
      ;; Should have sent response with session scope
      (should response-sent)
      (should (equal (car response-sent) 42))
      (should (equal (cadr response-sent) "allow_session")))))

;;;; Backend abstraction layer tests
;;
;; These tests require the backend abstraction functions from claude-agent-repl.el.
;; Since claude-agent-repl has heavy dependencies (org, transient, etc.), we define
;; stubs of the functions here that match the real implementations, avoiding the
;; need to load the full module.

(defvar claude-agent--process nil)
(defvar claude-agent--pending-system-messages nil)

(unless (fboundp 'claude-agent--backend-alive-p)
  (defun claude-agent--backend-alive-p ()
    "Test stub: check if agent backend is alive."
    (or (and claude-agent--process
             (process-live-p claude-agent--process))
        (and (bound-and-true-p claude-acp--backend-active)
             (claude-acp--process-live-p))))

  (defun claude-agent--backend-send-json (msg)
    "Test stub: send JSON message to backend."
    (cond
     ((bound-and-true-p claude-acp--backend-active)
      (let ((type (cdr (assq 'type msg)))
            (text (cdr (assq 'text msg))))
        (pcase type
          ("message" (claude-acp--send-user-message text))
          ("system_message"
           (when text
             (setq claude-agent--pending-system-messages
                   (append (or claude-agent--pending-system-messages nil)
                           (list text)))))
          ("interrupt" (claude-acp-cancel))
          ("quit" (claude-acp-shutdown))
          ("permission_response"
           (let ((action (cdr (assq 'action msg))))
             (claude-acp--send-permission-response-bridge action nil))))))
     ((and claude-agent--process (process-live-p claude-agent--process))
      (process-send-string claude-agent--process
                           (concat (json-encode msg) "\n")))))

  (defun claude-agent--backend-send-permission-response (action &optional tool-use-id)
    "Test stub: send permission response."
    (if (bound-and-true-p claude-acp--backend-active)
        (claude-acp--send-permission-response-bridge action tool-use-id)
      (when (and claude-agent--process (process-live-p claude-agent--process))
        (process-send-string claude-agent--process
                             (concat (json-encode `((type . "permission_response")
                                                    (action . ,action))) "\n")))))

  (defun claude-agent--backend-shutdown ()
    "Test stub: shutdown backend."
    (cond
     ((bound-and-true-p claude-acp--backend-active)
      (claude-acp-shutdown)
      (setq claude-acp--backend-active nil))
     ((and claude-agent--process (process-live-p claude-agent--process))
      (delete-process claude-agent--process)
      (setq claude-agent--process nil))))

  (defun claude-agent--backend-interrupt ()
    "Test stub: interrupt backend."
    (if (bound-and-true-p claude-acp--backend-active)
        (claude-acp-cancel)
      (when (and claude-agent--process (process-live-p claude-agent--process))
        (process-send-string claude-agent--process
                             (concat (json-encode '((type . "interrupt"))) "\n"))))))

(ert-deftest claude-acp-test-backend-alive-p-no-backend ()
  "Test backend-alive-p returns nil when no backend is active."
  :tags '(:unit :acp :backend)
  (with-temp-buffer
    (setq-local claude-agent--process nil)
    (setq-local claude-acp--backend-active nil)
    (should-not (claude-agent--backend-alive-p))))

(ert-deftest claude-acp-test-backend-alive-p-acp-active ()
  "Test backend-alive-p returns t when ACP backend is active."
  :tags '(:unit :acp :backend)
  (with-temp-buffer
    (setq-local claude-agent--process nil)
    (setq-local claude-acp--backend-active t)
    (setq-local claude-acp--client '((:process . mock-process)))
    (cl-letf (((symbol-function 'claude-acp--process-live-p)
               (lambda () t)))
      (should (claude-agent--backend-alive-p)))))

(ert-deftest claude-acp-test-backend-send-json-acp-message ()
  "Test backend-send-json dispatches messages to ACP backend."
  :tags '(:unit :acp :backend)
  (with-temp-buffer
    (setq-local claude-acp--backend-active t)
    (let ((sent-text nil))
      (cl-letf (((symbol-function 'claude-acp--send-user-message)
                 (lambda (text) (setq sent-text text))))
        (claude-agent--backend-send-json '((type . "message") (text . "hello")))
        (should (equal sent-text "hello"))))))

(ert-deftest claude-acp-test-backend-send-json-acp-system-message ()
  "Test backend-send-json queues system messages for ACP backend."
  :tags '(:unit :acp :backend)
  (with-temp-buffer
    (setq-local claude-acp--backend-active t)
    (setq-local claude-agent--pending-system-messages nil)
    (claude-agent--backend-send-json '((type . "system_message") (text . "reminder")))
    (should (equal claude-agent--pending-system-messages '("reminder")))))

(ert-deftest claude-acp-test-backend-send-json-acp-interrupt ()
  "Test backend-send-json calls cancel for interrupt type."
  :tags '(:unit :acp :backend)
  (with-temp-buffer
    (setq-local claude-acp--backend-active t)
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'claude-acp-cancel)
                 (lambda () (setq cancelled t))))
        (claude-agent--backend-send-json '((type . "interrupt")))
        (should cancelled)))))

(ert-deftest claude-acp-test-backend-send-permission-response-acp ()
  "Test backend-send-permission-response routes to ACP bridge."
  :tags '(:unit :acp :backend)
  (with-temp-buffer
    (setq-local claude-acp--backend-active t)
    (let ((bridge-action nil))
      (cl-letf (((symbol-function 'claude-acp--send-permission-response-bridge)
                 (lambda (action _id) (setq bridge-action action))))
        (claude-agent--backend-send-permission-response "allow_once" "tc_123")
        (should (equal bridge-action "allow_once"))))))

(ert-deftest claude-acp-test-backend-shutdown-acp ()
  "Test backend-shutdown calls ACP shutdown."
  :tags '(:unit :acp :backend)
  (with-temp-buffer
    (setq-local claude-acp--backend-active t)
    (let ((shutdown-called nil))
      (cl-letf (((symbol-function 'claude-acp-shutdown)
                 (lambda () (setq shutdown-called t))))
        (claude-agent--backend-shutdown)
        (should shutdown-called)
        (should-not claude-acp--backend-active)))))

(ert-deftest claude-acp-test-backend-interrupt-acp ()
  "Test backend-interrupt calls ACP cancel."
  :tags '(:unit :acp :backend)
  (with-temp-buffer
    (setq-local claude-acp--backend-active t)
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'claude-acp-cancel)
                 (lambda () (setq cancelled t))))
        (claude-agent--backend-interrupt)
        (should cancelled)))))

(ert-deftest claude-acp-test-pending-system-messages-prepended ()
  "Test that pending system messages are prepended to next ACP prompt."
  :tags '(:unit :acp :backend)
  (with-temp-buffer
    (setq-local claude-acp--backend-active t)
    (setq-local claude-agent--pending-system-messages '("reminder1" "reminder2"))
    (let ((sent-text nil))
      (cl-letf (((symbol-function 'claude-acp--send-user-message)
                 (lambda (text) (setq sent-text text))))
        ;; Simulate what dispatch-user-message does for ACP:
        ;; prepend system messages and send
        (let ((final-text "do the thing"))
          (when claude-agent--pending-system-messages
            (let ((sys-text (mapconcat #'identity claude-agent--pending-system-messages "\n\n")))
              (setq final-text (concat "<system-reminder>\n" sys-text "\n</system-reminder>\n\n" final-text))
              (setq claude-agent--pending-system-messages nil)))
          (claude-agent--backend-send-json `((type . "message") (text . ,final-text))))
        (should (string-match-p "reminder1" sent-text))
        (should (string-match-p "reminder2" sent-text))
        (should (string-match-p "do the thing" sent-text))
        ;; Pending messages should be cleared
        (should-not claude-agent--pending-system-messages)))))

(provide 'claude-acp-test)
;;; claude-acp-test.el ends here
