;;; claude-test-dsl.el --- DSL macros for readable REPL integration tests -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This module provides a declarative DSL for writing readable integration
;; tests for the Claude REPL.  It builds on claude-test-framework.el.
;;
;; Example:
;;   (claude-test-scenario "Basic conversation"
;;     (user-sends "hello")
;;     (agent-thinking t)
;;     (agent-responds "Hi there!")
;;     (agent-ready)
;;     (buffer-should-contain "Hi there!"))

;;; Code:

(require 'claude-test-framework)
(require 'cl-lib)

;;;; Scenario Macro

(defmacro claude-test-scenario (name &rest body)
  "Define an integration test scenario with NAME.
BODY contains DSL commands that are executed in a mock session context."
  (declare (indent 1) (debug t))
  (let ((test-name (intern (concat "claude-integration-" 
                                   (replace-regexp-in-string " " "-" name)))))
    `(ert-deftest ,test-name ()
       ,(format "Integration test: %s" name)
       :tags '(:integration :scenario)
       (claude-test-with-mock-session
         ,@body))))

;;;; DSL Commands - Sending Messages

(defun user-sends (text)
  "Simulate user sending TEXT to the agent."
  (claude-test-user-sends text))

(defun user-sends-and-wait (text)
  "Simulate user sending TEXT and wait for agent to become ready."
  (claude-test-user-sends text)
  ;; In synchronous tests, we just proceed
  ;; In async tests, this would wait for ready signal
  )

;;;; DSL Commands - Agent Responses

(defun agent-sends (msg)
  "Inject raw MSG from the agent."
  (claude-test-agent-sends msg))

(defun agent-thinking (status)
  "Set agent thinking STATUS."
  (claude-test-agent-sends `((type . "thinking") 
                             (thinking . ,(if status t :json-false)))))

(defun agent-responds (text)
  "Agent responds with TEXT."
  (claude-test-agent-sends `((type . "assistant_message")
                             (text . ,text))))

(defun agent-ready ()
  "Signal that agent is ready for input."
  (claude-test-agent-sends '((type . "ready"))))

(defun agent-uses-tool (tool-name tool-id args)
  "Agent calls TOOL-NAME with TOOL-ID and ARGS."
  (claude-test-agent-sends `((type . "tool_use")
                             (id . ,tool-id)
                             (tool . ,tool-name)
                             (input . ,args))))

(defun tool-result (tool-id result)
  "Inject tool RESULT for TOOL-ID."
  (claude-test-agent-sends `((type . "tool_result")
                             (id . ,tool-id)
                             (result . ,result))))

;;;; DSL Commands - Assertions

(defun buffer-should-contain (text)
  "Assert that buffer contains TEXT."
  (unless (claude-test-buffer-contains text)
    (ert-fail (format "Buffer should contain: %s\nBuffer content:\n%s"
                      text (claude-test-buffer-content)))))

(defun buffer-should-match (regexp)
  "Assert that buffer matches REGEXP."
  (unless (claude-test-buffer-matches regexp)
    (ert-fail (format "Buffer should match: %s\nBuffer content:\n%s"
                      regexp (claude-test-buffer-content)))))

(defun buffer-should-not-contain (text)
  "Assert that buffer does NOT contain TEXT."
  (when (claude-test-buffer-contains text)
    (ert-fail (format "Buffer should NOT contain: %s" text))))

(defun should-be-thinking ()
  "Assert that agent is in thinking state."
  (unless (claude-test-is-thinking-p)
    (ert-fail "Agent should be thinking but is not")))

(defun should-be-ready ()
  "Assert that agent is ready (not thinking)."
  (unless (claude-test-is-ready-p)
    (ert-fail "Agent should be ready but is thinking")))

(defun should-have-sent (expected)
  "Assert that EXPECTED message was sent to agent."
  (claude-test-expect-sent expected))

(defun last-sent-should-be (expected)
  "Assert that the last sent message matches EXPECTED."
  (let ((actual (claude-test-last-sent)))
    (unless (claude-test--message-matches-p actual expected)
      (ert-fail (format "Last sent should be: %S\nActual: %S" expected actual)))))

;;;; DSL Commands - MCP Tool Mocking

(defvar claude-test--tool-mocks (make-hash-table :test 'equal)
  "Hash table of mocked MCP tools.")

(defun mock-tool (tool-name handler)
  "Mock TOOL-NAME to use HANDLER function.
HANDLER receives args alist and returns result alist."
  (puthash tool-name handler claude-test--tool-mocks))

(defun mock-tool-returns (tool-name result)
  "Mock TOOL-NAME to always return RESULT."
  (puthash tool-name (lambda (_args) result) claude-test--tool-mocks))

(defun clear-tool-mocks ()
  "Clear all tool mocks."
  (clrhash claude-test--tool-mocks))

(defun get-tool-mock (tool-name)
  "Get the mock handler for TOOL-NAME, or nil."
  (gethash tool-name claude-test--tool-mocks))

;;;; DSL Commands - State Management

(defun with-temp-files (files &rest body)
  "Create temporary FILES and execute BODY.
FILES is a list of (filename . content) pairs."
  (let ((created-files '()))
    (unwind-protect
        (progn
          (dolist (file files)
            (let ((path (expand-file-name (car file) claude-test--temp-dir)))
              (make-directory (file-name-directory path) t)
              (with-temp-file path
                (insert (cdr file)))
              (push path created-files)))
          (eval `(progn ,@body)))
      ;; Cleanup
      (dolist (f created-files)
        (when (file-exists-p f)
          (delete-file f))))))

;;;; DSL Commands - Async/Timing

(defvar claude-test--async-timeout 5.0
  "Default timeout in seconds for async operations.")

(defun wait-for (predicate &optional timeout message)
  "Wait for PREDICATE to return non-nil, with TIMEOUT seconds.
Signals error with MESSAGE if timeout expires."
  (let ((timeout (or timeout claude-test--async-timeout))
        (start (float-time))
        (message (or message "Condition not met within timeout")))
    (while (and (not (funcall predicate))
                (< (- (float-time) start) timeout))
      (accept-process-output nil 0.05))
    (unless (funcall predicate)
      (ert-fail message))))

(defun wait-until-ready (&optional timeout)
  "Wait until agent is ready, with optional TIMEOUT."
  (wait-for #'claude-test-is-ready-p timeout "Agent did not become ready"))

(defun wait-until-thinking (&optional timeout)
  "Wait until agent starts thinking, with optional TIMEOUT."
  (wait-for #'claude-test-is-thinking-p timeout "Agent did not start thinking"))

(defun wait-for-buffer-contains (text &optional timeout)
  "Wait until buffer contains TEXT, with optional TIMEOUT."
  (wait-for (lambda () (claude-test-buffer-contains text))
            timeout
            (format "Buffer never contained: %s" text)))

;;;; Example Tests

(claude-test-scenario "Agent responds to greeting"
  (user-sends "Hello!")
  (agent-thinking t)
  (agent-responds "Hi there! How can I help you today?")
  (agent-thinking nil)
  (agent-ready)
  (should-be-ready)
  (should-have-sent '((type . "message") (text . "Hello!"))))

(claude-test-scenario "Agent uses tool"
  (user-sends "Read the file")
  (agent-thinking t)
  (agent-uses-tool "mcp__emacs__read_file" "tool-123" 
                   '((file_path . "/tmp/test.txt")))
  (tool-result "tool-123" '((content . "file contents here")))
  (agent-responds "The file contains: file contents here")
  (agent-ready))

(provide 'claude-test-dsl)
;;; claude-test-dsl.el ends here
