;;; claude-mcp-messaging-test.el --- Tests for messaging MCP tools -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;; Unit tests for the multi-agent messaging system in claude-mcp-messaging.el.
;; Covers: message queue operations, queue peek/pop-from, message_board_summary,
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

;;; ============================================================
;;; Queue peek-from / pop-from Tests (for send_and_wait support)
;;; ============================================================

(ert-deftest claude-mcp-messaging-test-queue-peek-from ()
  "Test peeking at messages from a specific sender."
  :tags '(:unit :mcp :messaging)
  (claude-mcp-messaging-test-with-clean-queues
    ;; Empty queue returns nil
    (should-not (claude-mcp-message-queue-peek-from "*claude:test*" "*claude:sender*"))
    ;; Add messages from different senders
    (claude-mcp-message-queue-add "*claude:test*" "from-a" "*claude:a*")
    (claude-mcp-message-queue-add "*claude:test*" "from-b" "*claude:b*")
    ;; Peek for specific sender
    (let ((msg (claude-mcp-message-queue-peek-from "*claude:test*" "*claude:b*")))
      (should msg)
      (should (string= "from-b" (plist-get msg :message)))
      (should (string= "*claude:b*" (plist-get msg :sender))))
    ;; Peek for non-existent sender returns nil
    (should-not (claude-mcp-message-queue-peek-from "*claude:test*" "*claude:c*"))
    ;; Peek doesn't remove the message
    (should (= 2 (claude-mcp-message-queue-peek "*claude:test*")))))

(ert-deftest claude-mcp-messaging-test-queue-pop-from ()
  "Test popping a message from a specific sender."
  :tags '(:unit :mcp :messaging)
  (claude-mcp-messaging-test-with-clean-queues
    ;; Empty queue returns nil
    (should-not (claude-mcp-message-queue-pop-from "*claude:test*" "*claude:sender*"))
    ;; Add messages from different senders
    (claude-mcp-message-queue-add "*claude:test*" "from-a" "*claude:a*")
    (claude-mcp-message-queue-add "*claude:test*" "from-b" "*claude:b*")
    (claude-mcp-message-queue-add "*claude:test*" "from-a-2" "*claude:a*")
    ;; Pop from sender b
    (let ((msg (claude-mcp-message-queue-pop-from "*claude:test*" "*claude:b*")))
      (should msg)
      (should (string= "from-b" (plist-get msg :message))))
    ;; Only 2 messages remain
    (should (= 2 (claude-mcp-message-queue-peek "*claude:test*")))
    ;; Pop from sender a gets the first one
    (let ((msg (claude-mcp-message-queue-pop-from "*claude:test*" "*claude:a*")))
      (should msg)
      (should (string= "from-a" (plist-get msg :message))))
    ;; One message remains (from-a-2)
    (should (= 1 (claude-mcp-message-queue-peek "*claude:test*")))
    ;; Pop the last one
    (let ((msg (claude-mcp-message-queue-pop-from "*claude:test*" "*claude:a*")))
      (should msg)
      (should (string= "from-a-2" (plist-get msg :message))))
    ;; Queue is empty
    (should (= 0 (claude-mcp-message-queue-peek "*claude:test*")))))

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
;;; Backward Compatibility Tests
;;; ============================================================

(ert-deftest claude-mcp-messaging-test-message-agent-alias ()
  "Test that claude-mcp-message-agent is an alias for claude-mcp-send-message."
  :tags '(:unit :mcp :messaging)
  (should (eq (symbol-function 'claude-mcp-message-agent)
              (symbol-function 'claude-mcp-send-message))))

;;; ============================================================
;;; Tool Registration Tests
;;; ============================================================

(ert-deftest claude-mcp-messaging-test-tools-registered ()
  "Test that messaging tools are registered in the tool registry."
  :tags '(:unit :mcp :messaging :registration)
  (should (gethash "spawn_agent" claude-mcp-tools))
  (should (gethash "list_agents" claude-mcp-tools))
  (should (gethash "send_message" claude-mcp-tools))
  (should (gethash "message_board_summary" claude-mcp-tools))
  ;; check_messages and message_agent should NOT be registered
  (should-not (gethash "check_messages" claude-mcp-tools))
  (should-not (gethash "message_agent" claude-mcp-tools))
  ;; send_and_wait is a native Python tool, not in the elisp registry
  (should-not (gethash "send_and_wait" claude-mcp-tools)))

(ert-deftest claude-mcp-messaging-test-tools-have-descriptions ()
  "Test that messaging tools have descriptions."
  :tags '(:unit :mcp :messaging :registration)
  (dolist (tool-name '("spawn_agent" "list_agents" "send_message"
                       "message_board_summary"))
    (let ((tool-def (gethash tool-name claude-mcp-tools)))
      (should tool-def)
      (should (stringp (plist-get tool-def :description)))
      (should (> (length (plist-get tool-def :description)) 0)))))

(ert-deftest claude-mcp-messaging-test-send-message-description ()
  "Test that send_message description mentions send_and_wait."
  :tags '(:unit :mcp :messaging :registration)
  (let* ((tool-def (gethash "send_message" claude-mcp-tools))
         (desc (plist-get tool-def :description)))
    (should (string-match-p "send_and_wait" desc))
    (should (string-match-p "fire-and-forget\\|without waiting" desc))))

(provide 'claude-mcp-messaging-test)
;;; claude-mcp-messaging-test.el ends here
