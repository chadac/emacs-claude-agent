;;; claude-mcp.el --- MCP integration for Claude -*- lexical-binding: t; -*-
;; Author: Christopher Poile <cpoile@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: claudecode ai emacs llm tools
;; URL: https://github.com/cpoile/claudemacs
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Functions designed to be called via emacsclient by Claude AI
;; for programmatic interaction with Emacs buffers.
;;
;; These functions provide a safe, well-defined API for Claude to:
;; - Read buffer contents
;; - Insert and modify text
;; - Query buffer information
;; - Send input to REPL buffers

;;; Code:

(require 'comint nil 'noerror)
(require 'claude-mcp-messaging)
(require 'claude-mcp-magit)
(require 'claude-mcp-notes)

;;;; Tool Registry
;;
;; MCP tools are registered in Emacs and exported to the Python server.
;; This replaces the YAML-based tool definitions.

(defvar claude-mcp-tools (make-hash-table :test 'equal)
  "Registry of MCP tools. Key is tool name string, value is plist.")

(defmacro claude-mcp-deftool (name docstring &rest args)
  "Define an MCP tool NAME with DOCSTRING.
NAME uses Lisp conventions (dashes), automatically converted to underscores for MCP.
ARGS is a plist with :function, :safe, and :args keys.

Example:
  (claude-mcp-deftool get-buffer-content
    \"Get the content of an Emacs buffer.\"
    :function #\\='claude-mcp-get-buffer-content
    :safe t
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
       (puthash name
                `((description . ,(or (plist-get def :description) ""))
                  (function . ,(symbol-name (plist-get def :function)))
                  (safe . ,(if (plist-get def :safe) t :json-false))
                  (args . ,(claude-mcp--convert-args (plist-get def :args))))
                tools))
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

;; Dynamic variable for session context (set by MCP server via let binding)
(defvar claude-session-cwd nil
  "The working directory for the current Claude session.
Set by the MCP server via a let binding to provide session context.
This must be defvar'd to be dynamically scoped in lexical-binding mode.")

;;;; Buffer Content Operations

(defun claude-mcp-insert-in-buffer (buffer-name text)
  "Insert TEXT into BUFFER-NAME at point.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (progn
        (with-current-buffer buffer-name
          (insert text))
        (format "Inserted %d characters into buffer '%s'" (length text) buffer-name))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claude-mcp-get-buffer-content (buffer-name &optional tail-lines head-lines start-line end-line)
  "Return the content of BUFFER-NAME.
If TAIL-LINES is provided, return only the last TAIL-LINES lines.
If HEAD-LINES is provided, return only the first HEAD-LINES lines.
If START-LINE and END-LINE are provided, return lines in that range (1-indexed, inclusive).
Only one of TAIL-LINES, HEAD-LINES, or START-LINE/END-LINE should be used.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (cond
         ;; Line range
         ((and start-line end-line)
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- start-line))
            (let ((start-pos (point)))
              (forward-line (1+ (- end-line start-line)))
              (buffer-substring-no-properties start-pos (point)))))
         ;; Head lines
         (head-lines
          (save-excursion
            (goto-char (point-min))
            (forward-line head-lines)
            (buffer-substring-no-properties (point-min) (point))))
         ;; Tail lines
         (tail-lines
          (save-excursion
            (goto-char (point-max))
            (forward-line (- tail-lines))
            (buffer-substring-no-properties (point) (point-max))))
         ;; Full buffer
         (t
          (buffer-substring-no-properties (point-min) (point-max)))))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claude-mcp-get-region (buffer-name start end)
  "Return content of BUFFER-NAME from START to END.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (buffer-substring-no-properties start end))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claude-mcp-search-buffer (buffer-name pattern &optional context-before context-after case-insensitive limit)
  "Search for PATTERN in BUFFER-NAME and return matches with context.
CONTEXT-BEFORE: number of lines to show before each match (default 0)
CONTEXT-AFTER: number of lines to show after each match (default 0)
CASE-INSENSITIVE: if non-nil, ignore case (default nil)
LIMIT: maximum number of matches to return (default nil for unlimited)

Returns matches as a formatted string similar to grep output, where:
- Lines before the match have '  ' prefix
- The matching line has '> ' prefix and shows the line number
- Lines after the match have '  ' prefix
- Match groups are separated by '--'

Designed to be called via emacsclient by Claude AI."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))

  (with-current-buffer buffer-name
    (let ((output '())
          (case-fold-search case-insensitive)
          (before (or context-before 0))
          (after (or context-after 0))
          (count 0))
      (save-excursion
        (goto-char (point-min))
        (while (and (re-search-forward pattern nil t)
                    (or (not limit) (< count limit)))
          (let* ((match-line (line-number-at-pos))
                 (match-line-content (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))
                 (lines '()))

            ;; Collect context before
            (when (> before 0)
              (save-excursion
                (forward-line (- before))
                (dotimes (_ before)
                  (let ((line-num (line-number-at-pos)))
                    (push (format "%6d:  %s" line-num
                                  (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))
                          lines))
                  (forward-line 1))))

            ;; Add the matching line with > prefix
            (push (format "%6d:> %s" match-line match-line-content) lines)

            ;; Collect context after
            (when (> after 0)
              (save-excursion
                (forward-line 1)
                (dotimes (_ after)
                  (unless (eobp)
                    (let ((line-num (line-number-at-pos)))
                      (push (format "%6d:  %s" line-num
                                    (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position)))
                            lines))
                    (forward-line 1)))))

            ;; Add this match group to output
            (setq output (append output (nreverse lines)))
            (setq count (1+ count))

            ;; Add separator between matches (but not after the last one)
            (when (and (< count (or limit most-positive-fixnum))
                       (not (eobp)))
              (setq output (append output (list "--"))))

            ;; Move to next line to avoid matching same line multiple times
            (forward-line 1))))

      ;; Return as newline-separated string
      (mapconcat 'identity output "\n"))))

(defun claude-mcp-replace-region (buffer-name start end text)
  "Replace content in BUFFER-NAME from START to END with TEXT.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (progn
        (with-current-buffer buffer-name
          (delete-region start end)
          (goto-char start)
          (insert text))
        (format "Replaced region [%d:%d] with %d characters in buffer '%s'"
                start end (length text) buffer-name))
    (error "Buffer '%s' does not exist" buffer-name)))

;;;; Buffer Navigation

(defun claude-mcp-goto-point (buffer-name position)
  "Move point to POSITION in BUFFER-NAME.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (progn
        (with-current-buffer buffer-name
          (goto-char position))
        (format "Moved to position %d in buffer '%s'" position buffer-name))
    (error "Buffer '%s' does not exist" buffer-name)))

;;;; Buffer Information

(defun claude-mcp-list-buffers ()
  "Return a list of buffer names.
Designed to be called via emacsclient by Claude AI."
  (mapcar #'buffer-name (buffer-list)))

(defun claude-mcp-buffer-info (buffer-name)
  "Return information about BUFFER-NAME as a property list.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (list :name buffer-name
              :file (buffer-file-name)
              :modified (buffer-modified-p)
              :size (buffer-size)
              :major-mode major-mode
              :point (point)
              :point-min (point-min)
              :point-max (point-max)))
    (error "Buffer '%s' does not exist" buffer-name)))

;;;; REPL Integration

(defun claude-mcp-send-to-eat-terminal (buffer-name text)
  "Send TEXT to eat terminal in BUFFER-NAME and submit with return.
Designed for eat-mode terminals like Claude buffers.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (if (and (boundp 'eat-terminal) eat-terminal)
            (progn
              (eat-term-send-string eat-terminal text)
              (eat-term-input-event eat-terminal 1 'return)
              (format "Sent input to eat terminal '%s'" buffer-name))
          (error "Buffer '%s' is not an eat terminal" buffer-name)))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claude-mcp-send-input (buffer-name text)
  "Insert TEXT into BUFFER-NAME and send input (useful for REPL buffers).
Tries eat-terminal, comint-send-input, eshell-send-input, or just inserts with newline.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (cond
         ;; eat-mode terminals (like claudemacs)
         ((and (boundp 'eat-terminal) eat-terminal)
          ;; Clear any partial input first with Ctrl+U
          (eat-term-send-string eat-terminal "\C-u")
          (eat-term-send-string eat-terminal text)
          (eat-term-input-event eat-terminal 1 'return)
          (format "Sent input to eat terminal '%s'" buffer-name))
         ;; comint-mode buffers
         ((and (boundp 'comint-mode) (derived-mode-p 'comint-mode))
          (goto-char (point-max))
          (insert text)
          (comint-send-input)
          (format "Sent input to comint buffer '%s'" buffer-name))
         ;; eshell
         ((and (boundp 'eshell-mode) (derived-mode-p 'eshell-mode))
          (goto-char (point-max))
          (insert text)
          (eshell-send-input)
          (format "Sent input to eshell buffer '%s'" buffer-name))
         ;; fallback: just insert with newline
         (t
          (goto-char (point-max))
          (insert text)
          (insert "\n")
          (format "Inserted text with newline to buffer '%s'" buffer-name))))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claude-mcp-exec-in-eat-terminal (buffer-name command &optional timeout)
  "Execute COMMAND in eat terminal BUFFER-NAME and wait for completion.
Returns the output of the command. TIMEOUT defaults to 30 seconds.
Designed to be called via emacsclient by Claude AI.

This function uses eat's shell integration if available (via
eat--shell-prompt-begin text property) for reliable prompt detection.
Falls back to heuristic-based detection if shell integration is not enabled.

Note: This function blocks but uses non-blocking waits to avoid freezing Emacs."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (if (and (boundp 'eat-terminal) eat-terminal)
            (let* ((timeout-secs (or timeout 30))
                   (start-pos (point-max))
                   (start-time (current-time))
                   (has-shell-integration (and (boundp 'eat--shell-prompt-begin)
                                               eat--shell-prompt-begin))
                   (initial-prompt-pos (when has-shell-integration
                                        (save-excursion
                                          (goto-char (point-max))
                                          (when (get-text-property (point) 'eat--shell-prompt-end)
                                            (point)))))
                   (last-size 0)
                   (stable-count 0))
              ;; Send the command
              (eat-term-send-string eat-terminal command)
              (eat-term-input-event eat-terminal 1 'return)

              ;; Wait for command to complete
              (catch 'done
                (while (< (float-time (time-subtract (current-time) start-time))
                         timeout-secs)
                  ;; Process any pending output without blocking UI
                  (accept-process-output nil 0.05 nil t)

                  ;; Check completion based on shell integration or heuristics
                  (if has-shell-integration
                      ;; Use shell integration: look for new prompt
                      (save-excursion
                        (goto-char (point-max))
                        (when (and (get-text-property (point) 'eat--shell-prompt-end)
                                  (or (null initial-prompt-pos)
                                      (> (point) initial-prompt-pos)))
                          (throw 'done t)))
                    ;; Fall back to heuristic detection
                    (let ((current-size (buffer-size)))
                      (if (= current-size last-size)
                          (setq stable-count (1+ stable-count))
                        (setq stable-count 0
                              last-size current-size))
                      ;; If stable, check for prompt patterns
                      (when (>= stable-count 3)
                        (let ((recent-text (buffer-substring-no-properties
                                           (max (point-min) (- (point-max) 300))
                                           (point-max))))
                          (when (string-match-p "[$#%>❯λ][ \t]*\\(?:\n\\|$\\|\\[\\)" recent-text)
                            (throw 'done t))))))))

              ;; Capture output
              (let* ((output (buffer-substring-no-properties start-pos (point-max)))
                     (lines (split-string output "\n" t)))
                ;; Remove first line (command echo) if it matches the command
                (when (and lines (string-match-p (regexp-quote command) (car lines)))
                  (setq lines (cdr lines)))
                ;; Join and return
                (string-trim (string-join lines "\n"))))
          (error "Buffer '%s' is not an eat terminal" buffer-name)))
    (error "Buffer '%s' does not exist" buffer-name)))

;;;; Notes - See claude-mcp-notes.el for all notes functionality
;; All notes functions have been moved to claude-mcp-notes.el
;; They are re-exported from that module for backward compatibility

;;;; Buffer Watching and Streaming

(defun claude-mcp-watch-buffer (buffer-name &optional timeout stable-time)
  "Watch BUFFER-NAME until content stabilizes or TIMEOUT seconds.
Returns buffer content after no changes for STABLE-TIME seconds (default 0.5).
TIMEOUT defaults to 30 seconds.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (let ((timeout-secs (or timeout 30))
            (stable-secs (or stable-time 0.5))
            (start-time (current-time))
            (last-content "")
            (last-change-time (current-time)))
        (catch 'done
          (while (< (float-time (time-subtract (current-time) start-time)) timeout-secs)
            (let ((current-content (with-current-buffer buffer-name
                                     (buffer-substring-no-properties (point-min) (point-max)))))
              (if (string= current-content last-content)
                  ;; Content stable - check if stable long enough
                  (when (>= (float-time (time-subtract (current-time) last-change-time)) stable-secs)
                    (throw 'done current-content))
                ;; Content changed - reset timer
                (setq last-content current-content
                      last-change-time (current-time))))
            (accept-process-output nil 0.1)))
        ;; Timeout - return current content
        (with-current-buffer buffer-name
          (buffer-substring-no-properties (point-min) (point-max))))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claude-mcp-watch-for-pattern (buffer-name pattern &optional timeout)
  "Watch BUFFER-NAME until PATTERN appears or TIMEOUT seconds.
Returns plist with :match, :line, and :pos, or nil if timeout.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (let ((timeout-secs (or timeout 30))
            (start-time (current-time)))
        (catch 'found
          (while (< (float-time (time-subtract (current-time) start-time)) timeout-secs)
            (with-current-buffer buffer-name
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward pattern nil t)
                  (throw 'found (list :match (match-string 0)
                                      :line (thing-at-point 'line t)
                                      :pos (match-beginning 0))))))
            (accept-process-output nil 0.1))
          nil))
    (error "Buffer '%s' does not exist" buffer-name)))

(defun claude-mcp-send-and-watch (buffer-name input &optional done-pattern timeout)
  "Send INPUT to BUFFER-NAME and watch until DONE-PATTERN or stable.
If DONE-PATTERN is provided, wait for it. Otherwise wait for stability.
Returns new content added after sending input.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (let ((start-pos (with-current-buffer buffer-name (point-max))))
        ;; Send input
        (with-current-buffer buffer-name
          (cond
           ((and (boundp 'eat-terminal) eat-terminal)
            (eat-term-send-string eat-terminal input)
            (eat-term-input-event eat-terminal 1 'return))
           ((derived-mode-p 'comint-mode)
            (goto-char (point-max))
            (insert input)
            (comint-send-input))
           (t
            (goto-char (point-max))
            (insert input "\n"))))
        ;; Watch for completion
        (if done-pattern
            (claude-mcp-watch-for-pattern buffer-name done-pattern timeout)
          ;; Return new content after stabilization
          (claude-mcp-watch-buffer buffer-name timeout 1.0)
          (with-current-buffer buffer-name
            (buffer-substring-no-properties start-pos (point-max)))))
    (error "Buffer '%s' does not exist" buffer-name)))

;;;; Agent Spawning

(defun claude-mcp--send-to-agent-when-ready (buffer-name prompt &optional attempt)
  "Send PROMPT to BUFFER-NAME when eat-terminal is ready.
ATTEMPT is the retry count (max 20 attempts, ~10 seconds total)."
  (let ((attempt (or attempt 0)))
    (if (>= attempt 20)
        (message "Failed to send prompt to %s: terminal not ready after 10s" buffer-name)
      (let ((buf (get-buffer buffer-name)))
        (if (and buf (buffer-live-p buf))
            (with-current-buffer buf
              (if (and (boundp 'eat-terminal) eat-terminal)
                  ;; Terminal ready - send the prompt
                  (progn
                    (eat-term-send-string eat-terminal prompt)
                    (run-at-time 0.1 nil
                                 (lambda (b)
                                   (when (buffer-live-p b)
                                     (with-current-buffer b
                                       (when (and (boundp 'eat-terminal) eat-terminal)
                                         (eat-term-input-event eat-terminal 1 'return)))))
                                 buf)
                    (message "Sent initial prompt to %s" buffer-name))
                ;; Not ready yet - retry
                (run-at-time 0.5 nil
                             #'claude-mcp--send-to-agent-when-ready
                             buffer-name prompt (1+ attempt))))
          ;; Buffer doesn't exist yet - retry
          (run-at-time 0.5 nil
                       #'claude-mcp--send-to-agent-when-ready
                       buffer-name prompt (1+ attempt)))))))

(defun claude-mcp-spawn-agent (directory &optional agent-name)
  "Spawn a new Claude agent in DIRECTORY.
Optional AGENT-NAME provides a custom name suffix for the buffer.
Returns the buffer name of the new agent.
Designed to be called via MCP by Claude AI."
  (require 'claude-agent)
  (let* ((work-dir (expand-file-name directory))
         (short-name (file-name-nondirectory (directory-file-name work-dir)))
         (buf-name (if agent-name
                       (format "*claude:%s:%s*" short-name agent-name)
                     (format "*claude:%s*" short-name))))
    ;; Check if session already exists
    (if (get-buffer buf-name)
        buf-name  ; Return existing buffer name
      ;; Start new session using claude-agent-run
      (let ((buf (claude-agent-run work-dir)))
        ;; Rename if agent-name provided
        (when agent-name
          (with-current-buffer buf
            (rename-buffer buf-name)))
        buf-name))))

(defun claude-mcp-list-agents ()
  "List all running Claude agent sessions.
Returns a list of (buffer-name directory) pairs.
Designed to be called via emacsclient by Claude AI."
  (let (agents)
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        (when (string-match "^\\*claude:\\(.*\\)\\*$" name)
          (push (list name (match-string 1 name)) agents))))
    (or agents "No agents running")))

(defun claude-mcp-message-agent (buffer-name message &optional from-buffer)
  "Send MESSAGE to the agent in BUFFER-NAME.
This sends the message as user input to the Claude session.
Optional FROM-BUFFER identifies the sender for the message queue.
Designed to be called via MCP by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (if (and (boundp 'claude-agent--process)
                 claude-agent--process
                 (process-live-p claude-agent--process))
            ;; New claude-agent buffer system
            (let ((formatted-message (if from-buffer
                                         (format "[From %s]: %s" from-buffer message)
                                       message)))
              ;; Insert message into input area and send
              (goto-char (point-max))
              (let ((inhibit-read-only t))
                (insert formatted-message))
              (claude-agent-send)
              (format "Sent message to %s" buffer-name))
          ;; Fallback: try old eat-terminal system
          (if (and (boundp 'eat-terminal) eat-terminal)
              (let ((buf (current-buffer)))
                (eat-term-send-string eat-terminal message)
                (run-at-time 0.1 nil
                             (lambda (b)
                               (when (buffer-live-p b)
                                 (with-current-buffer b
                                   (when (and (boundp 'eat-terminal) eat-terminal)
                                     (eat-term-input-event eat-terminal 1 'return)))))
                             buf)
                (format "Sent message to %s" buffer-name))
            (error "Buffer '%s' is not a Claude agent buffer" buffer-name))))
    (error "Buffer '%s' does not exist" buffer-name)))

;;;; Session Management

(defun claude-mcp-restart-and-resume (&optional buffer-name)
  "Restart the Claude session in BUFFER-NAME and resume the conversation.
If BUFFER-NAME is not provided, uses `claude-session-cwd' (set by MCP server).
This reloads elisp files and restarts the MCP server with any code changes.
Designed to be called via emacsclient by Claude AI."
  (require 'claude-mcp-messaging)
  (let* ((target-buffer (or buffer-name
                            ;; Find the agent buffer from MCP session cwd
                            (when (and (boundp 'claude-session-cwd) claude-session-cwd)
                              (claude-mcp-find-agent-by-cwd claude-session-cwd))
                            ;; Error if we can't determine the session
                            (error "Cannot determine Claude session - claude-session-cwd not set"))))
    ;; Check if this is a Claude buffer
    (if (and (get-buffer target-buffer)
             (string-match-p "^\\*claude:" target-buffer))
        (let ((work-dir (with-current-buffer target-buffer
                         (or claude--cwd
                             ;; Fallback: extract directory from buffer name
                             ;; *claude:/path/to/dir/* or *claude:/path/to/dir:agent-name*
                             (when (string-match "^\\*claude:\\([^:]+\\)" target-buffer)
                               (match-string 1 target-buffer))))))
          (unless work-dir
            (error "Cannot determine working directory for buffer '%s'" target-buffer))
          ;; Use run-at-time to defer execution so we can return a response first
          ;; Pass work-dir and buffer name to claude-restart to target the correct session
          (run-at-time 0.5 nil
                       (lambda (dir buf)
                         (require 'claudemacs)
                         ;; Call claude-restart with both work-dir and buffer-name
                         (claude-restart dir buf))
                       work-dir target-buffer)
          (format "Restart scheduled for %s (buffer: %s) - session will reload elisp files, restart MCP server, and resume shortly"
                  work-dir target-buffer))
      (error "Buffer '%s' is not a Claude buffer" target-buffer))))

;;;; Project Shell for Bash Execution


(defun claude-mcp-bash-hook-script ()
  "Generate shell hook script for bash/zsh command completion callbacks.
Returns a string containing the hook setup script."
  "
__claude_post_command() {
    local exit_code=$?
    # Run in subshell to hide job control messages
    if command -v curl >/dev/null 2>&1; then
        (curl -X POST -H 'Content-Type: application/json' \\
             -d '{\"shell_id\":\"'$CLAUDE_MCP_SHELL_ID'\",\"exit_code\":'$exit_code'}' \\
             \"http://localhost:$CLAUDE_MCP_PORT/bash-command\" >/dev/null 2>&1 &)
    elif command -v python3 >/dev/null 2>&1; then
        (python3 -c \"
import urllib.request, json
try:
    data = json.dumps({'shell_id':'$CLAUDE_MCP_SHELL_ID','exit_code':$exit_code}).encode()
    req = urllib.request.Request('http://localhost:$CLAUDE_MCP_PORT/bash-command', data=data, headers={'Content-Type':'application/json'})
    urllib.request.urlopen(req, timeout=1)
except: pass
\" &)
    fi
    return $exit_code
}

if [ -n \"$BASH_VERSION\" ]; then
    if [ -n \"$PROMPT_COMMAND\" ]; then
        PROMPT_COMMAND=\"__claude_post_command; $PROMPT_COMMAND\"
    else
        PROMPT_COMMAND=\"__claude_post_command\"
    fi
elif [ -n \"$ZSH_VERSION\" ]; then
    if ! (( \${precmd_functions[(I)__claude_post_command]} )); then
        precmd_functions+=(__claude_post_command)
    fi
fi
")

(defun claude-mcp-inject-bash-hooks (buffer-name)
  "Inject bash/zsh completion hooks into shell BUFFER-NAME.
This sets up PROMPT_COMMAND/precmd_functions to call back to MCP server."
  (when-let ((buf (get-buffer buffer-name)))
    (with-current-buffer buf
      (when (and (boundp 'eat-terminal) eat-terminal)
        ;; Write hook script to a temp file and source it
        (let* ((temp-file (make-temp-file "claude-hooks-" nil ".sh"))
               (hook-script (claude-mcp-bash-hook-script)))
          (with-temp-file temp-file
            (insert hook-script))
          ;; Source the file in the shell
          (eat-term-send-string eat-terminal (format "source %s && rm %s" temp-file temp-file))
          (eat-term-input-event eat-terminal 1 'return)
          (message "Injected bash hooks into %s via %s" buffer-name temp-file))))))

(defun claude-mcp-get-project-shell (directory &optional mcp-port)
  "Get or create an eat shell for DIRECTORY with optional MCP-PORT.
Returns the buffer name. Creates the shell if it doesn't exist.
If MCP-PORT is provided, sets up bash/zsh hooks for event-driven command completion.
Designed to be called via emacsclient by Claude AI."
  (require 'eat)
  (let* ((work-dir (expand-file-name directory))
         ;; eat-make wraps name with *...*, so we use name without asterisks
         (shell-name (format "eat-shell:%s" (file-name-nondirectory
                                              (directory-file-name work-dir))))
         (buffer-name (format "*%s*" shell-name)))
    (if (get-buffer buffer-name)
        ;; Buffer exists - return it (hooks were already injected when created)
        buffer-name
      ;; Create new shell with environment variables for HTTP callback
      (let* ((default-directory work-dir)
             ;; Set environment variables for the shell process
             ;; Use buffer-name as shell_id so Python and shell agree on the identifier
             (process-environment
              (if mcp-port
                  (append process-environment
                         (list (format "CLAUDE_MCP_PORT=%d" mcp-port)
                               (format "CLAUDE_MCP_SHELL_ID=%s" buffer-name)))
                process-environment)))
        (with-current-buffer (eat-make shell-name
                                       (or (getenv "SHELL") "/bin/bash")
                                       nil)
          (setq-local default-directory work-dir))

        ;; Inject hooks asynchronously after shell is ready
        (when mcp-port
          (run-at-time 2.0 nil #'claude-mcp-inject-bash-hooks buffer-name))

        buffer-name))))

(defun claude-mcp-project-shell-ready-p (buffer-name)
  "Check if project shell BUFFER-NAME has an active eat terminal.
Returns t if ready, nil otherwise."
  (when-let ((buf (get-buffer buffer-name)))
    (with-current-buffer buf
      (and (boundp 'eat-terminal) eat-terminal t))))

(defun claude-mcp-interrupt-shell (buffer-name)
  "Send interrupt signal (Ctrl+C) to shell BUFFER-NAME.
This kills the currently running command and returns to the prompt.
Designed to be called via emacsclient by Claude AI."
  (if (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (if (and (boundp 'eat-terminal) eat-terminal)
            (progn
              ;; Send Ctrl+C (interrupt signal)
              (eat-term-send-string-as-yank eat-terminal "\C-c")
              (format "Sent interrupt signal (Ctrl+C) to %s" buffer-name))
          (error "Buffer '%s' is not an eat terminal" buffer-name)))
    (error "Buffer '%s' does not exist" buffer-name)))

;;;; Elisp Debugging and Formatting

(defun claude-mcp-elisp-check-parens (file-path)
  "Check FILE-PATH for unbalanced parentheses in elisp.
Analyzes top-level forms and reports which ones have unbalanced parens.
Returns a structured report with line numbers and error descriptions.
Designed to be called via emacsclient by Claude AI."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file-path)
        (emacs-lisp-mode)
        (let ((forms nil)
              (line-num 1)
              (start-pos (point-min)))
          ;; Parse all top-level forms
          (goto-char (point-min))
          (condition-case parse-err
              (while (not (eobp))
                (let* ((form-start (point))
                       (form-start-line (line-number-at-pos form-start)))
                  (condition-case form-err
                      (progn
                        ;; Try to read the form
                        (forward-sexp 1)
                        (let ((form-end (point)))
                          ;; Successfully parsed this form
                          (push (list :status "ok"
                                    :start-line form-start-line
                                    :end-line (line-number-at-pos form-end)
                                    :text (buffer-substring-no-properties
                                           form-start
                                           (min form-end (+ form-start 100))))
                                forms))
                        ;; Skip whitespace and comments to next form
                        (forward-comment most-positive-fixnum))
                    (scan-error
                     ;; This form has unbalanced parens
                     (let* ((error-pos (or (nth 2 form-err) form-start))
                            (error-line (line-number-at-pos error-pos))
                            (form-text (buffer-substring-no-properties
                                       form-start
                                       (min (point-max) (+ form-start 200)))))
                       (push (list :status "error"
                                 :start-line form-start-line
                                 :error-line error-line
                                 :error (nth 1 form-err)
                                 :text (substring form-text 0 (min 200 (length form-text))))
                             forms))
                     ;; Try to skip to next top-level form
                     (goto-char (point-max))))))
            (end-of-file
             ;; Hit premature end of file - entire rest of file has issues
             (let ((remaining-text (buffer-substring-no-properties
                                   start-pos
                                   (min (point-max) (+ start-pos 200)))))
               (push (list :status "error"
                         :start-line (line-number-at-pos start-pos)
                         :error "Premature end of file"
                         :text (substring remaining-text 0 (min 200 (length remaining-text))))
                     forms))))

          ;; Format the report
          (let ((errors (seq-filter (lambda (f) (equal (plist-get f :status) "error"))
                                   (nreverse forms)))
                (total-forms (length forms)))
            (if errors
                (format "Found %d forms, %d with errors:\n\n%s"
                       total-forms
                       (length errors)
                       (mapconcat
                        (lambda (err)
                          (format "Line %d: %s\n  Preview: %s..."
                                 (plist-get err :start-line)
                                 (plist-get err :error)
                                 (string-trim (plist-get err :text))))
                        errors
                        "\n\n"))
              (format "All %d forms are balanced correctly" total-forms)))))
    (file-error
     (format "Error reading file: %s" (error-message-string err)))
    (error
     (format "Error checking parens: %s" (error-message-string err)))))

(defun claude-mcp-elisp-format (file-path)
  "Format FILE-PATH using elisp-format if available.
Returns formatted content if elisp-format is available, or error message.
Designed to be called via emacsclient by Claude AI."
  (let ((elisp-format-bin (executable-find "elisp-format")))
    (if elisp-format-bin
        (condition-case err
            (with-temp-buffer
              (let ((exit-code (call-process elisp-format-bin nil t nil file-path)))
                (if (zerop exit-code)
                    (buffer-substring-no-properties (point-min) (point-max))
                  (format "elisp-format exited with code %d:\n%s"
                         exit-code
                         (buffer-substring-no-properties (point-min) (point-max))))))
          (error
           (format "Error running elisp-format: %s" (error-message-string err))))
      "elisp-format not found in PATH. Install it to enable elisp formatting.")))

;;;; Setup and Integration

(defun claude-mcp-get-cli-path ()
  "Get the path to the claude-cli executable.
Assumes it's in the same directory as this file."
  (let* ((this-file (or load-file-name
                        buffer-file-name
                        (locate-library "claude-ai")))
         (this-dir (when this-file (file-name-directory this-file))))
    (if this-dir
        (expand-file-name "claude-cli" this-dir)
      (error "Cannot determine claude-ai.el location"))))

(defun claude-mcp-setup-claude-environment ()
  "Add claude-cli to PATH and set up environment for Claude.
This should be called during Claude startup to expose the CLI to Claude."
  (let ((cli-dir (file-name-directory (claude-mcp-get-cli-path))))
    ;; Add to PATH via setenv (affects child processes)
    (setenv "PATH" (concat cli-dir ":" (getenv "PATH")))
    ;; Set CLAUDE_AGENT_SOCKET using the actual server-socket-dir
    (when (and (boundp 'server-socket-dir)
               server-socket-dir
               (server-running-p))
      (let ((socket-file (expand-file-name "server" server-socket-dir)))
        (when (file-exists-p socket-file)
          (setenv "CLAUDE_AGENT_SOCKET" socket-file))))))

(defun claude-mcp-clear-buffer (buffer-name)
  "Clear the terminal content in BUFFER-NAME by truncating the buffer.
This removes accumulated history to improve performance.
Designed to be called via MCP by Claude AI."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))

  (let ((original-size 0)
        (new-size 0))
    (with-current-buffer buffer-name
      (unless (and (boundp 'eat-terminal) eat-terminal)
        (error "Buffer '%s' is not a Claude buffer (no eat-terminal)" buffer-name))

      (let ((process (eat-term-parameter eat-terminal 'eat--process)))
        (unless (and process (process-live-p process))
          (error "Claude agent in '%s' is not running" buffer-name))

        ;; Capture original size
        (setq original-size (buffer-size))

        ;; Truncate buffer if too large
        (when (> original-size 50000)
          (let ((inhibit-read-only t))
            ;; Keep only last 10000 chars
            (delete-region (point-min) (max (point-min) (- (point-max) 10000)))
            (setq new-size (buffer-size))))))

    (if (> original-size 50000)
        (format "Truncated buffer %s: %d → %d chars (removed %d)"
                buffer-name original-size new-size (- original-size new-size))
      (format "Buffer %s is only %d chars, no truncation needed" buffer-name original-size))))

;;;; Tool Registrations
;;
;; Register core buffer tools with the MCP server

(claude-mcp-deftool get-buffer-content
  "Get the content of an Emacs buffer. Can optionally get head/tail lines or a specific line range."
  :function #'claude-mcp-get-buffer-content
  :safe t
  :args ((buffer-name string :required "Name of the buffer (e.g. 'main.py', '*scratch*')")
         (tail-lines integer "Optional: get only the last N lines")
         (head-lines integer "Optional: get only the first N lines")
         (start-line integer "Optional: start line for range (1-indexed, requires end-line)")
         (end-line integer "Optional: end line for range (1-indexed, inclusive, requires start-line)")))

(claude-mcp-deftool list-buffers
  "List all open buffers in Emacs."
  :function #'claude-mcp-list-buffers
  :safe t
  :args ())

(claude-mcp-deftool buffer-info
  "Get detailed information about a buffer (file path, size, major mode, cursor position, etc.)."
  :function #'claude-mcp-buffer-info
  :safe t
  :args ((buffer-name string :required "Name of the buffer")))

(claude-mcp-deftool search-buffer
  "Search for a pattern in a buffer and return matches with context lines (similar to grep). Supports regex patterns and context control."
  :function #'claude-mcp-search-buffer
  :safe t
  :args ((buffer-name string :required "Name of the buffer to search")
         (pattern string :required "Regular expression pattern to search for")
         (context-before integer "Number of lines to show before each match (like grep -B)")
         (context-after integer "Number of lines to show after each match (like grep -A)")
         (case-insensitive boolean "If true, ignore case when searching (like grep -i)")
         (limit integer "Maximum number of matches to return")))

(claude-mcp-deftool get-region
  "Get content from a specific region in a buffer by character positions."
  :function #'claude-mcp-get-region
  :safe t
  :args ((buffer-name string :required "Name of the buffer")
         (start integer :required "Start position (1-indexed)")
         (end integer :required "End position (1-indexed)")))

(claude-mcp-deftool clear-buffer
  "Clear the terminal content in a claudemacs buffer. Useful when the buffer gets too large and causing performance issues."
  :function #'claude-mcp-clear-buffer
  :safe nil
  :args ((buffer-name string :required "Name of the claudemacs buffer to clear")))

(defun claude-mcp-eval (expression)
  "Evaluate EXPRESSION as Emacs Lisp and return the result.
EXPRESSION should be a string containing valid Emacs Lisp code."
  (let ((result (eval (read expression))))
    (prin1-to-string result)))

(claude-mcp-deftool eval
  "[EXECUTE] Evaluate an Emacs Lisp expression directly. Use this for arbitrary elisp evaluation like changing themes, settings, or running commands."
  :function #'claude-mcp-eval
  :safe nil
  :args ((expression string :required "The Emacs Lisp expression to evaluate")))

(provide 'claude-mcp)
;;; claude-mcp.el ends here
