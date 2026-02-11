;;; claude-test-framework.el --- Mock process infrastructure for REPL integration tests -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This module provides a mock process infrastructure for testing the Claude
;; REPL without spawning actual Python processes.  Tests can:
;;
;; - Send messages as if from the user
;; - Inject responses as if from the agent
;; - Assert on buffer contents and state
;; - Test async flows synchronously
;;
;; Usage:
;;   (claude-test-with-mock-session
;;     (claude-test-user-sends "hello")
;;     (claude-test-agent-sends '((type . "assistant_message") (text . "Hi!")))
;;     (should (claude-test-buffer-contains "Hi!")))

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ert)

;;;; Variables

(defvar claude-test--mock-active nil
  "Non-nil when mock process infrastructure is active.")

(defvar claude-test--sent-messages nil
  "List of messages sent to the mock process (newest first).")

(defvar claude-test--mock-buffer nil
  "The mock Claude buffer for testing.")

(defvar claude-test--mock-process nil
  "Mock process object.")

(defvar claude-test--response-queue nil
  "Queue of responses to be injected.")

(defvar claude-test--temp-dir nil
  "Temporary directory for test sessions.")

(defvar claude-test--received-messages nil
  "List of messages received from the agent (newest first).")

;;;; Stub buffer-local variables (standalone mode)

(defvar-local claude--cwd nil
  "Stub for claude--cwd.")

(defvar-local claude-agent--pending-output ""
  "Stub for claude-agent--pending-output.")

(defvar-local claude-agent--thinking-status nil
  "Stub for claude-agent--thinking-status.")

(defvar-local claude-agent--message-queue nil
  "Stub for claude-agent--message-queue.")

(defvar-local claude-agent--message-count 0
  "Stub for claude-agent--message-count.")

(defvar-local claude-agent--is-resumed nil
  "Stub for claude-agent--is-resumed.")

(defvar-local claude-agent--session-info nil
  "Stub for claude-agent--session-info.")

(defvar-local claude-agent--process nil
  "Stub for claude-agent--process.")

;;;; Stub message handling (standalone mode)

(defun claude-test--stub-handle-output (output)
  "Stub handler for OUTPUT from agent process.
Parses NDJSON lines and dispatches to stub message handler."
  (setq claude-agent--pending-output
        (concat claude-agent--pending-output output))
  
  ;; Process complete lines
  (while (string-match "\n" claude-agent--pending-output)
    (let ((line (substring claude-agent--pending-output 0 (match-beginning 0))))
      (setq claude-agent--pending-output
            (substring claude-agent--pending-output (match-end 0)))
      (claude-test--stub-process-line line))))

(defun claude-test--stub-process-line (line)
  "Process a single LINE of NDJSON output (stub)."
  (when (and line (not (string-empty-p (string-trim line))))
    (condition-case nil
        (let* ((msg (json-read-from-string line))
               (msg-type (cdr (assq 'type msg))))
          (claude-test--stub-dispatch msg-type msg))
      (error nil))))

(defun claude-test--stub-dispatch (msg-type msg)
  "Dispatch message MSG based on MSG-TYPE (stub)."
  (push msg claude-test--received-messages)
  (pcase msg-type
    ("ready"
     (setq claude-agent--thinking-status nil))
    
    ("thinking"
     (setq claude-agent--thinking-status 
           (eq (cdr (assq 'thinking msg)) t)))
    
    ("session_info"
     (setq claude-agent--session-info msg))
    
    ("assistant_message"
     (let ((text (cdr (assq 'text msg))))
       (when text
         (goto-char (point-max))
         (insert "\n[Assistant]\n" text "\n"))))
    
    ("tool_use"
     (let ((tool (cdr (assq 'tool msg)))
           (id (cdr (assq 'id msg))))
       (goto-char (point-max))
       (insert (format "\n[Tool: %s (id: %s)]\n" tool id))))
    
    ("tool_result"
     (let ((id (cdr (assq 'id msg))))
       (goto-char (point-max))
       (insert (format "\n[Tool Result: %s]\n" id))))))

;;;; Mock Process

(defun claude-test--make-mock-process (name buffer)
  "Create a mock process object with NAME and BUFFER."
  (let ((proc (list 'mock-process
                    :name name
                    :buffer buffer
                    :status 'run
                    :filter nil
                    :sentinel nil)))
    proc))

(defun claude-test--mock-process-p (obj)
  "Return t if OBJ is a mock process."
  (and (listp obj) (eq (car obj) 'mock-process)))

(defun claude-test--mock-process-live-p (proc)
  "Return t if mock PROC is live."
  (and (claude-test--mock-process-p proc)
       (eq (plist-get (cdr proc) :status) 'run)))

(defun claude-test--mock-process-buffer (proc)
  "Return buffer for mock PROC."
  (when (claude-test--mock-process-p proc)
    (plist-get (cdr proc) :buffer)))

(defun claude-test--mock-process-name (proc)
  "Return name for mock PROC."
  (when (claude-test--mock-process-p proc)
    (plist-get (cdr proc) :name)))

;;;; Advice Functions

(defun claude-test--advice-start-process (orig-fn name buffer program &rest args)
  "Advice for `start-process' that returns mock process when testing.
ORIG-FN is the original function, NAME and BUFFER identify the process,
PROGRAM and ARGS are the command (ignored in mock mode)."
  (if claude-test--mock-active
      (let ((proc (claude-test--make-mock-process name buffer)))
        (setq claude-test--mock-process proc)
        ;; Simulate initial ready message after a short delay
        (run-at-time 0.01 nil #'claude-test--send-initial-ready)
        proc)
    (apply orig-fn name buffer program args)))

(defun claude-test--advice-process-send-string (orig-fn proc string)
  "Advice for `process-send-string' that captures messages when testing.
ORIG-FN is the original function, PROC is the process, STRING is the message."
  (if (and claude-test--mock-active
           (claude-test--mock-process-p proc))
      ;; Capture the message
      (let ((trimmed (string-trim string)))
        (when (not (string-empty-p trimmed))
          (condition-case nil
              (let ((msg (json-read-from-string trimmed)))
                (push msg claude-test--sent-messages))
            (error
             ;; Not JSON, store as string
             (push trimmed claude-test--sent-messages)))))
    (funcall orig-fn proc string)))

(defun claude-test--advice-process-live-p (orig-fn proc)
  "Advice for `process-live-p' to handle mock processes.
ORIG-FN is the original function, PROC is the process."
  (if (claude-test--mock-process-p proc)
      (claude-test--mock-process-live-p proc)
    (funcall orig-fn proc)))

(defun claude-test--advice-process-buffer (orig-fn proc)
  "Advice for `process-buffer' to handle mock processes.
ORIG-FN is the original function, PROC is the process."
  (if (claude-test--mock-process-p proc)
      (claude-test--mock-process-buffer proc)
    (funcall orig-fn proc)))

(defun claude-test--advice-process-name (orig-fn proc)
  "Advice for `process-name' to handle mock processes.
ORIG-FN is the original function, PROC is the process."
  (if (claude-test--mock-process-p proc)
      (claude-test--mock-process-name proc)
    (funcall orig-fn proc)))

(defun claude-test--advice-set-process-filter (orig-fn proc filter)
  "Advice for `set-process-filter' to handle mock processes.
ORIG-FN is the original function, PROC is the process, FILTER is the filter function."
  (if (claude-test--mock-process-p proc)
      (plist-put (cdr proc) :filter filter)
    (funcall orig-fn proc filter)))

(defun claude-test--advice-set-process-sentinel (orig-fn proc sentinel)
  "Advice for `set-process-sentinel' to handle mock processes.
ORIG-FN is the original function, PROC is the process, SENTINEL is the sentinel function."
  (if (claude-test--mock-process-p proc)
      (plist-put (cdr proc) :sentinel sentinel)
    (funcall orig-fn proc sentinel)))

(defun claude-test--advice-set-process-coding-system (orig-fn proc &rest args)
  "Advice for `set-process-coding-system' to handle mock processes.
ORIG-FN is the original function, PROC is the process, ARGS are coding systems."
  (unless (claude-test--mock-process-p proc)
    (apply orig-fn proc args)))

(defun claude-test--advice-set-process-query-on-exit-flag (orig-fn proc flag)
  "Advice for `set-process-query-on-exit-flag' to handle mock processes.
ORIG-FN is the original function, PROC is the process, FLAG is the flag value."
  (unless (claude-test--mock-process-p proc)
    (funcall orig-fn proc flag)))

(defun claude-test--advice-delete-process (orig-fn proc)
  "Advice for `delete-process' to handle mock processes.
ORIG-FN is the original function, PROC is the process."
  (if (claude-test--mock-process-p proc)
      (plist-put (cdr proc) :status 'exit)
    (funcall orig-fn proc)))

;;;; Mock Setup/Teardown

(defun claude-test--install-mock ()
  "Install mock process infrastructure."
  (setq claude-test--mock-active t)
  (setq claude-test--sent-messages nil)
  (setq claude-test--received-messages nil)
  (setq claude-test--response-queue nil)

  ;; Add advice to intercept process functions
  (advice-add 'start-process :around #'claude-test--advice-start-process)
  (advice-add 'process-send-string :around #'claude-test--advice-process-send-string)
  (advice-add 'process-live-p :around #'claude-test--advice-process-live-p)
  (advice-add 'process-buffer :around #'claude-test--advice-process-buffer)
  (advice-add 'process-name :around #'claude-test--advice-process-name)
  (advice-add 'set-process-filter :around #'claude-test--advice-set-process-filter)
  (advice-add 'set-process-sentinel :around #'claude-test--advice-set-process-sentinel)
  (advice-add 'set-process-coding-system :around #'claude-test--advice-set-process-coding-system)
  (advice-add 'set-process-query-on-exit-flag :around #'claude-test--advice-set-process-query-on-exit-flag)
  (advice-add 'delete-process :around #'claude-test--advice-delete-process))

(defun claude-test--uninstall-mock ()
  "Remove mock process infrastructure."
  (setq claude-test--mock-active nil)

  ;; Remove advice
  (advice-remove 'start-process #'claude-test--advice-start-process)
  (advice-remove 'process-send-string #'claude-test--advice-process-send-string)
  (advice-remove 'process-live-p #'claude-test--advice-process-live-p)
  (advice-remove 'process-buffer #'claude-test--advice-process-buffer)
  (advice-remove 'process-name #'claude-test--advice-process-name)
  (advice-remove 'set-process-filter #'claude-test--advice-set-process-filter)
  (advice-remove 'set-process-sentinel #'claude-test--advice-set-process-sentinel)
  (advice-remove 'set-process-coding-system #'claude-test--advice-set-process-coding-system)
  (advice-remove 'set-process-query-on-exit-flag #'claude-test--advice-set-process-query-on-exit-flag)
  (advice-remove 'delete-process #'claude-test--advice-delete-process)

  ;; Cleanup
  (when (and claude-test--mock-buffer (buffer-live-p claude-test--mock-buffer))
    (kill-buffer claude-test--mock-buffer))
  (setq claude-test--mock-buffer nil)
  (setq claude-test--mock-process nil)
  (setq claude-test--sent-messages nil)

  ;; Clean up temp dir
  (when (and claude-test--temp-dir (file-directory-p claude-test--temp-dir))
    (delete-directory claude-test--temp-dir t))
  (setq claude-test--temp-dir nil))

;;;; Helper Functions

(defun claude-test--send-initial-ready ()
  "Send the initial ready message to bootstrap the session."
  (when (and claude-test--mock-active claude-test--mock-buffer)
    (claude-test-agent-sends '((type . "session_start")))
    (claude-test-agent-sends '((type . "session_info")
                               (model . "test-model")
                               (session_id . "test-session-123")))
    (claude-test-agent-sends '((type . "ready")))))

(defun claude-test--create-temp-dir ()
  "Create a temporary directory for test session."
  (let ((dir (make-temp-file "claude-test-" t)))
    (setq claude-test--temp-dir dir)
    dir))

;;;; Public API

(defun claude-test-setup-session (&optional work-dir)
  "Set up a mock Claude session in WORK-DIR.
If WORK-DIR is nil, creates a temporary directory."
  (let* ((dir (or work-dir (claude-test--create-temp-dir)))
         (buffer-name (format "*claude:%s*" (expand-file-name dir))))
    ;; Create buffer
    (setq claude-test--mock-buffer (get-buffer-create buffer-name))
    (with-current-buffer claude-test--mock-buffer
      (setq default-directory dir)
      ;; Set up buffer-local variables that claude-agent-mode expects
      (setq-local claude--cwd dir)
      (setq-local claude-agent--pending-output "")
      (setq-local claude-agent--thinking-status nil)
      (setq-local claude-agent--message-queue nil)
      (setq-local claude-agent--message-count 0)
      (setq-local claude-agent--is-resumed nil)
      (setq-local claude-agent--session-info nil)
      ;; Don't call full claude-agent-mode as it has dependencies
      ;; Just set up minimal state for testing
      (setq-local claude-agent--process claude-test--mock-process)
      ;; Insert initial content
      (insert "[Claude Test Session]\n\n"))
    claude-test--mock-buffer))

(defun claude-test-user-sends (text)
  "Simulate user sending TEXT to the agent."
  (push `((type . "message") (text . ,text)) claude-test--sent-messages)
  ;; Also insert into buffer for visibility
  (when claude-test--mock-buffer
    (with-current-buffer claude-test--mock-buffer
      (goto-char (point-max))
      (insert "\n[User]\n" text "\n"))))

(defun claude-test-agent-sends (msg)
  "Inject agent message MSG into the REPL.
MSG should be an alist representing a JSON message."
  (when claude-test--mock-buffer
    (with-current-buffer claude-test--mock-buffer
      ;; Simulate the process filter receiving output
      (let ((json-str (concat (json-encode msg) "\n")))
        (claude-test--stub-handle-output json-str)))))

(defun claude-test-last-sent ()
  "Return the last message sent to the agent."
  (car claude-test--sent-messages))

(defun claude-test-sent-messages ()
  "Return all messages sent to the agent (newest first)."
  claude-test--sent-messages)

(defun claude-test-clear-sent ()
  "Clear the sent messages list."
  (setq claude-test--sent-messages nil))

(defun claude-test-buffer-contains (text)
  "Return t if the test buffer contains TEXT."
  (when claude-test--mock-buffer
    (with-current-buffer claude-test--mock-buffer
      (save-excursion
        (goto-char (point-min))
        (search-forward text nil t)))))

(defun claude-test-buffer-matches (regexp)
  "Return t if the test buffer matches REGEXP."
  (when claude-test--mock-buffer
    (with-current-buffer claude-test--mock-buffer
      (save-excursion
        (goto-char (point-min))
        (re-search-forward regexp nil t)))))

(defun claude-test-buffer-content ()
  "Return the full content of the test buffer."
  (when claude-test--mock-buffer
    (with-current-buffer claude-test--mock-buffer
      (buffer-string))))

(defun claude-test-is-thinking-p ()
  "Return t if the agent is in thinking state."
  (when claude-test--mock-buffer
    (with-current-buffer claude-test--mock-buffer
      (bound-and-true-p claude-agent--thinking-status))))

(defun claude-test-is-ready-p ()
  "Return t if the agent is ready (not thinking)."
  (not (claude-test-is-thinking-p)))

(defun claude-test-expect-sent (expected)
  "Assert that EXPECTED message was sent to the agent.
EXPECTED can be a partial alist - only specified keys are checked."
  (let ((found nil))
    (dolist (msg claude-test--sent-messages)
      (when (claude-test--message-matches-p msg expected)
        (setq found t)))
    (unless found
      (ert-fail (format "Expected message not found: %S\nSent messages: %S"
                        expected claude-test--sent-messages)))
    found))

(defun claude-test--message-matches-p (actual expected)
  "Return t if ACTUAL message matches EXPECTED (partial match)."
  (catch 'mismatch
    (dolist (pair expected)
      (let ((key (car pair))
            (val (cdr pair)))
        (unless (equal (cdr (assq key actual)) val)
          (throw 'mismatch nil))))
    t))

;;;; Macro for Test Isolation

(defmacro claude-test-with-mock-session (&rest body)
  "Execute BODY with a mock Claude session.
Sets up mock infrastructure, creates a test session, and cleans up afterward."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn
         (claude-test--install-mock)
         (claude-test-setup-session)
         ,@body)
     (claude-test--uninstall-mock)))

;;;; Self-tests

(ert-deftest claude-test-framework-mock-process ()
  "Test that mock process infrastructure works."
  :tags '(:unit :test-framework)
  (claude-test-with-mock-session
    (should claude-test--mock-active)
    (should claude-test--mock-buffer)
    (should (buffer-live-p claude-test--mock-buffer))))

(ert-deftest claude-test-framework-user-sends ()
  "Test that user messages are captured."
  :tags '(:unit :test-framework)
  (claude-test-with-mock-session
    (claude-test-user-sends "hello world")
    (should (equal (cdr (assq 'text (claude-test-last-sent)))
                   "hello world"))
    (should (equal (cdr (assq 'type (claude-test-last-sent)))
                   "message"))))

(ert-deftest claude-test-framework-expect-sent ()
  "Test the expect-sent assertion."
  :tags '(:unit :test-framework)
  (claude-test-with-mock-session
    (claude-test-user-sends "test message")
    (claude-test-expect-sent '((type . "message")))
    (claude-test-expect-sent '((text . "test message")))))

(ert-deftest claude-test-framework-agent-sends ()
  "Test that agent messages are processed."
  :tags '(:unit :test-framework)
  (claude-test-with-mock-session
    (claude-test-agent-sends '((type . "assistant_message")
                               (text . "Hello from agent!")))
    (should (claude-test-buffer-contains "Hello from agent!"))))

(ert-deftest claude-test-framework-thinking-state ()
  "Test thinking state tracking."
  :tags '(:unit :test-framework)
  (claude-test-with-mock-session
    (should (claude-test-is-ready-p))
    (claude-test-agent-sends '((type . "thinking") (thinking . t)))
    (should (claude-test-is-thinking-p))
    (claude-test-agent-sends '((type . "thinking") (thinking . :json-false)))
    (should (claude-test-is-ready-p))))

(ert-deftest claude-test-framework-buffer-contains ()
  "Test buffer content assertions."
  :tags '(:unit :test-framework)
  (claude-test-with-mock-session
    (claude-test-user-sends "findme")
    (should (claude-test-buffer-contains "findme"))
    (should-not (claude-test-buffer-contains "nothere"))))

(ert-deftest claude-test-framework-tool-use ()
  "Test tool use message handling."
  :tags '(:unit :test-framework)
  (claude-test-with-mock-session
    (claude-test-agent-sends '((type . "tool_use")
                               (id . "tool-123")
                               (tool . "mcp__emacs__read_file")
                               (input . ((file_path . "/tmp/test.txt")))))
    (should (claude-test-buffer-contains "mcp__emacs__read_file"))
    (should (claude-test-buffer-contains "tool-123"))))

(provide 'claude-test-framework)
;;; claude-test-framework.el ends here
