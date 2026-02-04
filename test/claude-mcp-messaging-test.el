;;; claude-mcp-messaging-test.el --- Tests for messaging MCP tools -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;; Unit tests for the multi-agent messaging system in claude-mcp-messaging.el.
;; Covers: message queue operations, check_messages, message_board_summary,
;; list_agents, and tool registration.
;;
;; Run with:
;;   emacs -batch -l ert -l test/claude-mcp-messaging-test.el -f ert-run-tests-batch

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load test helper (handles circular dependency between claude-mcp and sub-modules)
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'test-helper)

;;; Test Utilities

(defmacro claude-mcp-messaging-test-with-clean-queues (&rest body)
  "Execute BODY with a fresh message queue system."
  (declare (indent 0))
  `(let ((claude-mcp-message-queues (make-hash-table :test 'equal))
         (inhibit-message t))
     ,@body))

;;; ============================================================
;;; Message Queue Tests
;;; ============================================================

(ert-deftest claude-mcp-messaging-test-queue-add ()
  "Test adding a message to the queue."
  :tags '(:unit :mcp :messaging)
  (claude-mcp-messaging-test-with-clean-queues
    (let ((count (claude-mcp-message-queue-add "*claude:test*" "hello" "*claude:sender*")))
      (should (= 1 count))
      ;; Add another
      (let ((count2 (claude-mcp-message-queue-add "*claude:test*" "world" "*claude:sender*")))
        (should (= 2 count2))))))

(ert-deftest claude-mcp-messaging-test-queue-get ()
  "Test getting messages from the queue."
  :tags '(:unit :mcp :messaging)
  (claude-mcp-messaging-test-with-clean-queues
    (claude-mcp-message-queue-add "*claude:test*" "hello" "*claude:sender*")
    (claude-mcp-message-queue-add "*claude:test*" "world" "*claude:sender2*")
    (let ((messages (claude-mcp-message-queue-get "*claude:test*")))
      (should (= 2 (length messages)))
      (should (string= "hello" (plist-get (car messages) :message)))
      (should (string= "*claude:sender*" (plist-get (car messages) :sender)))
      (should (string= "world" (plist-get (cadr messages) :message))))))

(ert-deftest claude-mcp-messaging-test-queue-get-with-clear ()
  "Test getting messages with clear flag."
  :tags '(:unit :mcp :messaging)
  (claude-mcp-messaging-test-with-clean-queues
    (claude-mcp-message-queue-add "*claude:test*" "hello" "*claude:sender*")
    ;; Get with clear
    (let ((messages (claude-mcp-message-queue-get "*claude:test*" t)))
      (should (= 1 (length messages))))
    ;; Queue should now be empty
    (let ((messages (claude-mcp-message-queue-get "*claude:test*")))
      (should (= 0 (length messages))))))

(ert-deftest claude-mcp-messaging-test-queue-get-empty ()
  "Test getting messages from empty queue."
  :tags '(:unit :mcp :messaging)
  (claude-mcp-messaging-test-with-clean-queues
    (let ((messages (claude-mcp-message-queue-get "*claude:nonexistent*")))
      (should (= 0 (length messages))))))

(ert-deftest claude-mcp-messaging-test-queue-peek ()
  "Test peeking at queue count."
  :tags '(:unit :mcp :messaging)
  (claude-mcp-messaging-test-with-clean-queues
    (should (= 0 (claude-mcp-message-queue-peek "*claude:test*")))
    (claude-mcp-message-queue-add "*claude:test*" "hello" "*claude:sender*")
    (should (= 1 (claude-mcp-message-queue-peek "*claude:test*")))
    (claude-mcp-message-queue-add "*claude:test*" "world" "*claude:sender*")
    (should (= 2 (claude-mcp-message-queue-peek "*claude:test*")))))

(ert-deftest claude-mcp-messaging-test-queue-clear ()
  "Test clearing the queue."
  :tags '(:unit :mcp :messaging)
  (claude-mcp-messaging-test-with-clean-queues
    (claude-mcp-message-queue-add "*claude:test*" "hello" "*claude:sender*")
    (claude-mcp-message-queue-clear "*claude:test*")
    (should (= 0 (claude-mcp-message-queue-peek "*claude:test*")))))

(ert-deftest claude-mcp-messaging-test-queue-format-empty ()
  "Test formatting empty queue."
  :tags '(:unit :mcp :messaging)
  (claude-mcp-messaging-test-with-clean-queues
    (let ((result (claude-mcp-message-queue-format "*claude:test*")))
      (should (string= "No queued messages." result)))))

(ert-deftest claude-mcp-messaging-test-queue-format-with-messages ()
  "Test formatting queue with messages."
  :tags '(:unit :mcp :messaging)
  (claude-mcp-messaging-test-with-clean-queues
    (claude-mcp-message-queue-add "*claude:test*" "hello there" "*claude:sender*")
    (let ((result (claude-mcp-message-queue-format "*claude:test*")))
      (should (stringp result))
      (should (string-match-p "1 queued message" result))
      (should (string-match-p "hello there" result))
      (should (string-match-p "\\*claude:sender\\*" result))
      (should (string-match-p "IMPORTANT" result)))))

;;; ============================================================
;;; check_messages Tests
;;; ============================================================

(ert-deftest claude-mcp-messaging-test-check-messages-happy-path ()
  "Test checking messages for a buffer."
  :tags '(:unit :mcp :messaging)
  (claude-mcp-messaging-test-with-clean-queues
    (let ((buf (get-buffer-create " *test-check-msg*")))
      (unwind-protect
          (progn
            (claude-mcp-message-queue-add (buffer-name buf) "test message" "*claude:sender*")
            (let ((result (claude-mcp-check-messages (buffer-name buf))))
              (should (stringp result))
              (should (string-match-p "test message" result))))
        (kill-buffer buf)))))

(ert-deftest claude-mcp-messaging-test-check-messages-with-clear ()
  "Test checking messages with clear flag."
  :tags '(:unit :mcp :messaging)
  (claude-mcp-messaging-test-with-clean-queues
    (let ((buf (get-buffer-create " *test-check-clear*")))
      (unwind-protect
          (progn
            (claude-mcp-message-queue-add (buffer-name buf) "msg1" "*claude:s*")
            ;; Check with clear
            (claude-mcp-check-messages (buffer-name buf) t)
            ;; Queue should be empty now
            (let ((result (claude-mcp-check-messages (buffer-name buf))))
              (should (string-match-p "No queued messages" result))))
        (kill-buffer buf)))))

(ert-deftest claude-mcp-messaging-test-check-messages-nonexistent-buffer ()
  "Test checking messages for nonexistent buffer errors."
  :tags '(:unit :mcp :messaging)
  (should-error (claude-mcp-check-messages "nonexistent-xyz")
                :type 'error))

;;; ============================================================
;;; Message Board Tests
;;; ============================================================

(ert-deftest claude-mcp-messaging-test-board-get-buffer ()
  "Test getting/creating the message board buffer."
  :tags '(:unit :mcp :messaging)
  (let ((inhibit-message t))
    ;; Kill any existing board
    (when-let ((buf (get-buffer claude-mcp-message-board-buffer)))
      (kill-buffer buf))
    (let ((buf (claude-mcp-message-board-get-buffer)))
      (unwind-protect
          (progn
            (should (bufferp buf))
            (should (buffer-live-p buf))
            (with-current-buffer buf
              (should (eq major-mode 'org-mode))
              (should (string-match-p "Log" (buffer-string)))))
        (kill-buffer buf)))))

(ert-deftest claude-mcp-messaging-test-board-log ()
  "Test logging a message to the board."
  :tags '(:unit :mcp :messaging)
  (let ((inhibit-message t))
    (when-let ((buf (get-buffer claude-mcp-message-board-buffer)))
      (kill-buffer buf))
    (unwind-protect
        (progn
          (claude-mcp-message-board-log "*claude:sender*" "*claude:recipient*" "test message")
          (let ((content (claude-mcp-message-board-get)))
            (should (string-match-p "\\*claude:recipient\\*" content))
            (should (string-match-p "\\*claude:sender\\*" content))
            (should (string-match-p "test message" content))))
      (claude-mcp-message-board-clear))))

(ert-deftest claude-mcp-messaging-test-board-summary-empty ()
  "Test summary with no messages."
  :tags '(:unit :mcp :messaging)
  (let ((inhibit-message t))
    (claude-mcp-message-board-clear)
    (unwind-protect
        (let ((result (claude-mcp-message-board-summary)))
          (should (string-match-p "No messages logged" result)))
      (claude-mcp-message-board-clear))))

(ert-deftest claude-mcp-messaging-test-board-summary-with-messages ()
  "Test summary with messages logged."
  :tags '(:unit :mcp :messaging)
  (let ((inhibit-message t))
    (claude-mcp-message-board-clear)
    (unwind-protect
        (progn
          (claude-mcp-message-board-log "*claude:a*" "*claude:b*" "msg1")
          (claude-mcp-message-board-log "*claude:a*" "*claude:b*" "msg2")
          (let ((result (claude-mcp-message-board-summary)))
            (should (string-match-p "Message Board Summary" result))
            (should (string-match-p "\\*claude:a\\*" result))
            (should (string-match-p "\\*claude:b\\*" result))
            ;; Should show count
            (should (string-match-p "2 messages" result))))
      (claude-mcp-message-board-clear))))

;;; ============================================================
;;; list_agents Tests
;;; ============================================================

(ert-deftest claude-mcp-messaging-test-list-agents ()
  "Test listing agents returns JSON."
  :tags '(:unit :mcp :messaging)
  (let ((inhibit-message t))
    (let ((result (claude-mcp-list-agents)))
      ;; Should return valid JSON string (at minimum an empty array)
      (should (stringp result)))))

;;; ============================================================
;;; Tool Registration Tests
;;; ============================================================

(ert-deftest claude-mcp-messaging-test-tools-registered ()
  "Test that messaging tools are registered in the tool registry."
  :tags '(:unit :mcp :messaging :registration)
  (should (gethash "spawn_agent" claude-mcp-tools))
  (should (gethash "list_agents" claude-mcp-tools))
  (should (gethash "message_agent" claude-mcp-tools))
  (should (gethash "check_messages" claude-mcp-tools))
  (should (gethash "message_board_summary" claude-mcp-tools)))

(ert-deftest claude-mcp-messaging-test-tools-have-descriptions ()
  "Test that messaging tools have descriptions."
  :tags '(:unit :mcp :messaging :registration)
  (dolist (tool-name '("spawn_agent" "list_agents" "message_agent"
                       "check_messages" "message_board_summary"))
    (let ((tool-def (gethash tool-name claude-mcp-tools)))
      (should tool-def)
      (should (stringp (plist-get tool-def :description)))
      (should (> (length (plist-get tool-def :description)) 0)))))

(provide 'claude-mcp-messaging-test)
;;; claude-mcp-messaging-test.el ends here
