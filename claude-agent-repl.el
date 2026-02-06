;;; claude-agent.el --- Claude interaction buffer -*- lexical-binding: t; -*-

;; This file is part of Claude.
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This module provides a single-buffer interface for interacting with Claude.
;; The buffer is organized into distinct sections:
;;
;; 1. Header Section: Session name (read-only)
;; 2. Log Section: Conversation history (read-only, append-only)
;; 3. Status Section: Model/cost/session info + thinking indicator (read-only)
;; 4. Input Section: Header line (read-only) + editable typing area
;;
;; The Python agent outputs structured markers which are parsed and formatted.

;;; Code:

(require 'ansi-color)
(require 'org)
(require 'transient)
(require 'claude-mcp)
(require 'claude-transient)

;; Declare function from claude-agent.el (can't require due to circular dependency)
(declare-function claude--package-root "claude-agent")
(declare-function claude-agent--get-agent-dir "claude-agent")

;; Forward declarations for todo integration
(declare-function org-roam-todo--query-todos "todo")

;;;; Customization

(defgroup claude-agent nil
  "Claude interaction buffer."
  :group 'Claude)

(defcustom claude-agent-python-command "uv"
  "Command to run Python for the agent wrapper."
  :type 'string
  :group 'claude-agent)

(defcustom claude-agent-enable-mcp t
  "Whether to enable the Emacs MCP server for Claude sessions.
When non-nil, Claude can interact with Emacs buffers via MCP tools.
Requires the Emacs server to be running (`server-start')."
  :type 'boolean
  :group 'claude-agent)

(defcustom claude-agent-disallowed-tools '()
  "Base list of tools that should be disallowed for the Claude agent.
These are combined with `claude-agent-disallowed-tools-extra' at runtime.
Example: Setting to \\='(\"WebSearch\") would disable web search."
  :type '(repeat string)
  :group 'claude-agent)

(defcustom claude-agent-disallowed-tools-extra '()
  "Additional disallowed tools, appended to `claude-agent-disallowed-tools'.
Intended for use in .dir-locals.el so worktree-specific restrictions
can be added without overwriting the base list."
  :type '(repeat string)
  :safe #'listp
  :group 'claude-agent)

(defcustom claude-agent-auto-reject-rules nil
  "Base list of auto-reject rules for tool permissions.
These are combined with `claude-agent-auto-reject-rules-extra' at runtime.
Each element is a plist with keys:
  :path-prefix PATH   - reject tools operating on files under PATH
  :pattern PATTERN     - tool permission pattern (e.g. \"Edit(/path/*)\")
  :message MESSAGE     - rejection reason shown to the agent
Set base rules here; use `claude-agent-auto-reject-rules-extra' in
.dir-locals.el to add worktree-specific rules on top."
  :type '(repeat (plist :key-type symbol :value-type string))
  :safe #'listp
  :group 'claude-agent)

(defcustom claude-agent-auto-reject-rules-extra nil
  "Additional auto-reject rules, appended to `claude-agent-auto-reject-rules'.
Intended for use in .dir-locals.el so worktree-specific confinement
rules can be added without overwriting base rules.
Same format as `claude-agent-auto-reject-rules'."
  :type '(repeat (plist :key-type symbol :value-type string))
  :safe #'listp
  :group 'claude-agent)

(defun claude-agent--effective-auto-reject-rules ()
  "Return the effective auto-reject rules (base + extra).
Combines `claude-agent-auto-reject-rules' and
`claude-agent-auto-reject-rules-extra'."
  (append claude-agent-auto-reject-rules
          claude-agent-auto-reject-rules-extra))

(defun claude-agent--effective-disallowed-tools ()
  "Return the effective disallowed tools list (base + extra).
Combines `claude-agent-disallowed-tools' and
`claude-agent-disallowed-tools-extra'."
  (append claude-agent-disallowed-tools
          claude-agent-disallowed-tools-extra))

(defcustom claude-agent-extra-system-prompt nil
  "Extra text appended to the system prompt.
When set, this text is appended to the system prompt passed to the agent.
Can be set via .dir-locals.el for worktree-specific instructions."
  :type '(choice (const nil) string)
  :safe #'stringp
  :group 'claude-agent)

(defcustom claude-agent-system-hooks nil
  "List of system message hooks injected into Claude conversations.
Hooks are evaluated on the Emacs side before each user message is sent.
Matched hooks have their messages sent as system_message commands to the
Python agent, which wraps them in <system-reminder> tags and appends
them to the user message.

Each entry is a plist with keys:
  :name      - unique hook name (string, required)
  :trigger   - when to fire: \"every_n\", \"on_start\", \"on_resume\" (required)
  :interval  - for every_n triggers, the message interval (integer, default 10)
  :message   - static message string (optional)
  :message-fn - function returning a message string, called at fire time (optional)
  :elisp-fn  - elisp expression string, evaluated at fire time via `eval' (optional)

Resolution priority: :elisp-fn > :message-fn > :message.
If a resolver returns nil, the hook is skipped for that message.

Can be set via .dir-locals.el for project-specific hook configuration.

Example:
  \\='((:name \"lint-reminder\"
     :trigger \"every_n\"
     :interval 15
     :message \"Remember to run make lint before committing.\")
    (:name \"project-rules\"
     :trigger \"on_start\"
     :message \"This project uses conventional commits.\")
    (:name \"todo-reminder\"
     :trigger \"every_n\"
     :interval 15
     :elisp-fn \"(claude-agent--todo-acceptance-reminder)\"))"
  :type '(repeat (plist :key-type symbol :value-type sexp))
  :safe #'listp
  :group 'claude-agent)

(defun claude-agent--todo-acceptance-reminder ()
  "Generate a TODO status and acceptance criteria reminder message.
Returns a formatted string based on the current TODO's status:
- active: Shows acceptance criteria and encourages work on unchecked items
- review: Tells the agent to wait for user feedback
- other statuses: Returns nil (no reminder needed)
This is intended to be used as an :elisp-fn for a system message hook."
  (when (and (fboundp 'org-roam-todo-mcp-get-current)
             (fboundp 'org-roam-todo-mcp-get-acceptance-criteria))
    (condition-case nil
        (let* ((current (org-roam-todo-mcp-get-current))
               (parsed (json-read-from-string current))
               (title (alist-get 'title parsed))
               (status (alist-get 'status parsed)))
          (when title
            (pcase status
              ("active"
               ;; Active: show criteria, encourage work
               (let* ((criteria-json (org-roam-todo-mcp-get-acceptance-criteria))
                      (criteria (json-read-from-string criteria-json))
                      (lines '()))
                 (seq-doseq (item criteria)
                   (let ((text (alist-get 'text item))
                         (checked (alist-get 'checked item)))
                     (push (format "- [%s] %s"
                                   (if (eq checked t) "X" " ")
                                   text)
                           lines)))
                 (when lines
                   (format "TASK REMINDER: You are working on: %s\nStatus: active\n\nAcceptance Criteria:\n%s\n\nStay focused on completing unchecked items."
                           title
                           (mapconcat #'identity (nreverse lines) "\n")))))
              ("review"
               ;; Review: tell agent to wait for feedback
               (format "TASK STATUS: Your task \"%s\" is in REVIEW status.\nYou have completed your work and it is awaiting user review.\nDo NOT make additional changes unless the user provides feedback.\nWhen the user sends you a message, the status will automatically change back to 'active'."
                       title))
              ;; For draft/done/rejected, no reminder needed
              (_ nil))))
      (error nil))))

(defun claude-agent--hook-should-fire-p (hook message-count is-resumed)
  "Return non-nil if HOOK should fire given MESSAGE-COUNT and IS-RESUMED.
HOOK is a plist from `claude-agent-system-hooks'.
MESSAGE-COUNT is the 1-based index of the current user message.
IS-RESUMED is non-nil if this session was resumed from a previous one."
  (let ((trigger (plist-get hook :trigger))
        (interval (or (plist-get hook :interval) 10)))
    (pcase trigger
      ("every_n"
       (and (> interval 0)
            (= 1 (mod message-count interval))))
      ("on_start"
       (and (= message-count 1) (not is-resumed)))
      ("on_resume"
       (and (= message-count 1) is-resumed))
      (_ nil))))

(defun claude-agent--resolve-hook-message (hook)
  "Resolve the message text for HOOK.
Resolution priority: :elisp-fn > :message-fn > :message.
Returns a string or nil if no message could be resolved."
  (let ((elisp-fn (plist-get hook :elisp-fn))
        (message-fn (plist-get hook :message-fn))
        (message (plist-get hook :message)))
    (or
     ;; :elisp-fn - evaluate elisp expression directly
     (when elisp-fn
       (condition-case err
           (let ((result (eval (read elisp-fn))))
             (when (stringp result) result))
         (error
          (message "Claude agent: hook %s elisp-fn error: %s"
                   (plist-get hook :name) (error-message-string err))
          nil)))
     ;; :message-fn - call function
     (when (and message-fn (functionp message-fn))
       (condition-case err
           (let ((result (funcall message-fn)))
             (when (stringp result) result))
         (error
          (message "Claude agent: hook %s message-fn error: %s"
                   (plist-get hook :name) (error-message-string err))
          nil)))
     ;; :message - static string
     message)))

(defun claude-agent--evaluate-hooks ()
  "Evaluate all system message hooks and return list of message strings.
Uses buffer-local `claude-agent--message-count' and `claude-agent--is-resumed'
to determine which hooks should fire."
  (let ((messages '()))
    (dolist (hook claude-agent-system-hooks)
      (when (claude-agent--hook-should-fire-p
             hook claude-agent--message-count claude-agent--is-resumed)
        (when-let ((msg (claude-agent--resolve-hook-message hook)))
          (push msg messages))))
    (nreverse messages)))

(defun claude-agent--send-system-message (text)
  "Send a system_message command to the agent process.
TEXT is the message string to inject into the next user message."
  (when (and claude-agent--process
             (process-live-p claude-agent--process)
             text
             (not (string-empty-p text)))
    (process-send-string
     claude-agent--process
     (concat (json-encode `((type . "system_message") (text . ,text))) "\n"))))

(defun claude-agent-send-test-system-message (text)
  "Send a test system message TEXT to the agent for debugging.
This injects a system message that will be displayed in the REPL
with the \"system>\" prompt and appended to the next user message.
Useful for testing the system message display pipeline."
  (interactive "sSystem message: ")
  (claude-agent--send-system-message text))

(defun claude-agent--maybe-revert-review-status ()
  "If current buffer's TODO is in review status, revert to active.
When the user sends a message to an agent whose TODO is in review status,
this automatically changes the status back to active (since feedback implies
more work is needed) and sends a system message notifying the agent."
  (when (and claude-agent--work-dir
             (fboundp 'org-roam-todo--query-todos))
    (condition-case nil
        (let* ((expanded-dir (directory-file-name
                              (expand-file-name claude-agent--work-dir)))
               (todo (cl-find-if
                      (lambda (td)
                        (let ((wpath (plist-get td :worktree-path)))
                          (and wpath
                               (string= (directory-file-name
                                         (expand-file-name wpath))
                                        expanded-dir))))
                      (org-roam-todo--query-todos))))
          (when (and todo (string= (plist-get todo :status) "review"))
            (let ((file (plist-get todo :file))
                  (title (plist-get todo :title)))
              ;; Update status directly in the file
              (when file
                (with-current-buffer (find-file-noselect file)
                  (save-excursion
                    (goto-char (point-min))
                    (when (re-search-forward "^:STATUS:\\s-*.+$" nil t)
                      (replace-match ":STATUS: active")))
                  (save-buffer))
                ;; Send system notification to agent
                (claude-agent--send-system-message
                 (format "STATUS CHANGE: Your task \"%s\" has been moved from 'review' back to 'active'. The user has provided feedback below. Please review their message and continue working on the task."
                         (or title "current task")))))))
      (error nil))))

(defun claude-agent--dispatch-user-message (text)
  "Send user message TEXT with system hook injection.
Increments the message count, evaluates hooks, sends any system messages
to the agent process, then sends the user message.  Also displays
system messages in the REPL buffer.
If the associated TODO is in review status, auto-reverts to active."
  ;; Check for review -> active transition before sending
  (claude-agent--maybe-revert-review-status)
  (setq claude-agent--message-count (1+ claude-agent--message-count))
  ;; Evaluate and send system message hooks
  (let ((hook-messages (claude-agent--evaluate-hooks)))
    (dolist (msg hook-messages)
      (claude-agent--send-system-message msg)))
  ;; Send the actual user message
  (process-send-string
   claude-agent--process
   (concat (json-encode `((type . "message") (text . ,text))) "\n")))

;;;; Faces

(defface claude-agent-header-face
  '((((class color) (background dark))
     (:foreground "#56b6c2" :slant italic))
    (((class color) (background light))
     (:foreground "#0184bc" :slant italic)))
  "Face for the header section."
  :group 'claude-agent)

(defface claude-agent-user-header-face
  '((((class color) (background dark))
     (:foreground "#61afef" :weight bold))
    (((class color) (background light))
     (:foreground "#4078f2" :weight bold)))
  "Face for user message headers."
  :group 'claude-agent)

(defface claude-agent-user-face
  '((((class color) (background dark))
     (:foreground "#c8ccd4"))
    (((class color) (background light))
     (:foreground "#383a42")))
  "Face for user message text."
  :group 'claude-agent)

(defface claude-agent-assistant-header-face
  '((((class color) (background dark))
     (:foreground "#c678dd" :weight bold))
    (((class color) (background light))
     (:foreground "#a626a4" :weight bold)))
  "Face for assistant message headers."
  :group 'claude-agent)

(defface claude-agent-assistant-face
  '((((class color) (background dark))
     (:foreground "#e5e5e5"))
    (((class color) (background light))
     (:foreground "#1a1a1a")))
  "Face for assistant message text."
  :group 'claude-agent)

(defface claude-agent-tool-face
  '((((class color) (background dark))
     (:foreground "#e5c07b" :slant italic))
    (((class color) (background light))
     (:foreground "#986801" :slant italic)))
  "Face for tool call indicators."
  :group 'claude-agent)

(defface claude-agent-status-face
  '((((class color) (background dark))
     (:foreground "#56b6c2" :slant italic))
    (((class color) (background light))
     (:foreground "#0184bc" :slant italic)))
  "Face for status info section (model, cost, session)."
  :group 'claude-agent)

(defface claude-agent-thinking-face
  '((((class color) (background dark))
     (:foreground "#98c379" :weight bold))
    (((class color) (background light))
     (:foreground "#50a14f" :weight bold)))
  "Face for thinking indicator."
  :group 'claude-agent)

(defface claude-agent-progress-face
  '((((class color) (background dark))
     (:foreground "#61afef"))
    (((class color) (background light))
     (:foreground "#4078f2")))
  "Face for progress indicators."
  :group 'claude-agent)

(defface claude-agent-progress-header-face
  '((((class color) (background dark))
     (:foreground "#5c6370" :slant italic))
    (((class color) (background light))
     (:foreground "#a0a1a7" :slant italic)))
  "Face for progress section header."
  :group 'claude-agent)

(defface claude-agent-compacting-face
  '((((class color) (background dark))
     (:foreground "#e5c07b" :weight bold :slant italic))
    (((class color) (background light))
     (:foreground "#986801" :weight bold :slant italic)))
  "Face for compacting indicator (yellow/warning color)."
  :group 'claude-agent)

(defface claude-agent-todo-pending-face
  '((((class color) (background dark))
     (:foreground "#5c6370"))
    (((class color) (background light))
     (:foreground "#a0a1a7")))
  "Face for pending todo items."
  :group 'claude-agent)

(defface claude-agent-todo-in-progress-face
  '((((class color) (background dark))
     (:foreground "#61afef" :weight bold))
    (((class color) (background light))
     (:foreground "#4078f2" :weight bold)))
  "Face for in-progress todo items."
  :group 'claude-agent)

(defface claude-agent-todo-completed-face
  '((((class color) (background dark))
     (:foreground "#98c379" :strike-through t))
    (((class color) (background light))
     (:foreground "#50a14f" :strike-through t)))
  "Face for completed todo items."
  :group 'claude-agent)

(defface claude-agent-proposal-pending-face
  '((((class color) (background dark))
     (:foreground "#d19a66" :weight bold))
    (((class color) (background light))
     (:foreground "#c18401" :weight bold)))
  "Face for pending proposal indicator in the status bar."
  :group 'claude-agent)

(defface claude-agent-error-face
  '((((class color) (background dark))
     (:foreground "#e06c75" :weight bold))
    (((class color) (background light))
     (:foreground "#e45649" :weight bold)))
  "Face for error messages."
  :group 'claude-agent)

(defface claude-agent-session-face
  '((((class color) (background dark))
     (:foreground "#56b6c2" :slant italic))
    (((class color) (background light))
     (:foreground "#0184bc" :slant italic)))
  "Face for session info messages."
  :group 'claude-agent)

(defface claude-agent-system-header-face
  '((((class color) (background dark))
     (:foreground "#5c6370" :weight bold))
    (((class color) (background light))
     (:foreground "#a0a1a7" :weight bold)))
  "Face for system message headers (the \"system>\" prompt)."
  :group 'claude-agent)

(defface claude-agent-system-message-face
  '((((class color) (background dark))
     (:foreground "#5c6370" :slant italic))
    (((class color) (background light))
     (:foreground "#a0a1a7" :slant italic)))
  "Face for system message body text shown in the REPL."
  :group 'claude-agent)

(defface claude-agent-input-header-face
  '((((class color) (background dark))
     (:foreground "#5c6370" :weight bold))
    (((class color) (background light))
     (:foreground "#a0a1a7" :weight bold)))
  "Face for the input area header."
  :group 'claude-agent)

(defface claude-agent-diff-removed
  '((t :inherit diff-refine-removed))
  "Face for removed lines in diff display."
  :group 'claude-agent)

(defface claude-agent-diff-added
  '((t :inherit diff-refine-added))
  "Face for added lines in diff display."
  :group 'claude-agent)

(defface claude-agent-diff-header
  '((((class color) (background dark))
     (:foreground "#5c6370"))
    (((class color) (background light))
     (:foreground "#a0a1a7")))
  "Face for diff box drawing characters."
  :group 'claude-agent)

(defface claude-agent-file-link
  '((t :inherit link :underline t))
  "Face for clickable file paths."
  :group 'claude-agent)

(defface claude-agent-line-number
  '((((class color) (background dark))
     (:foreground "#5c6370"))
    (((class color) (background light))
     (:foreground "#a0a1a7")))
  "Face for line numbers in file content display."
  :group 'claude-agent)

(defface claude-agent-header-model-face
  '((((class color) (background dark))
     (:foreground "#61afef" :weight bold))
    (((class color) (background light))
     (:foreground "#4078f2" :weight bold)))
  "Face for model name in header line."
  :group 'claude-agent)

(defface claude-agent-header-cost-face
  '((((class color) (background dark))
     (:foreground "#98c379"))
    (((class color) (background light))
     (:foreground "#50a14f")))
  "Face for cost in header line."
  :group 'claude-agent)

(defface claude-agent-header-session-face
  '((((class color) (background dark))
     (:foreground "#5c6370"))
    (((class color) (background light))
     (:foreground "#a0a1a7")))
  "Face for session ID in header line."
  :group 'claude-agent)


;;;; Buffer-local variables - Section markers
;;
;; Buffer has 2 zones with different update semantics:
;;
;;   [STATIC]  - Header + completed conversation turns
;;               Append-only, never modified after written
;;               Ends at `static-end-marker`
;;
;;   [DYNAMIC] - Current in-progress turn + status bar + permission dialog
;;               Fully deleted and rebuilt on each update
;;               Content stored in variables, rendered fresh each time
;;
;; User input is handled in a separate dedicated buffer (*claude-input:*),
;; shown in a small window below the REPL when the user wants to type.
;; The REPL buffer itself is fully read-only.

(defvar-local claude-agent--process nil
  "The agent process for this session.")

(defvar-local claude-agent--static-end-marker nil
  "Marker for end of static section (start of dynamic section).
Everything before this is completed content that never changes.")

;;;; Buffer-local variables - Input buffer

(defvar-local claude-agent--input-buffer nil
  "The dedicated input buffer for this Claude session.")

(defvar-local claude-agent--input-window nil
  "The window displaying the input buffer, or nil if hidden.")

(defvar-local claude-agent--follow-mode t
  "Non-nil when auto-scrolling to follow new content.
Set to t when user is at bottom; set to nil when user scrolls up.")

;;;; Buffer-local variables - State

(defvar-local claude-agent--parse-state nil
  "Current parsing state: nil, user, assistant, tool, error, session.")

(defvar-local claude-agent--pending-output ""
  "Buffer for incomplete lines from process output.")

(defvar-local claude-agent--session-info nil
  "Plist with session info: :model :session-id :cost.")

(defvar-local claude-agent--available-models nil
  "Available models from the SDK, as a list of alists.
Each alist has keys: value, displayName, description.
Populated dynamically from the agent's get_server_info() call.")

(defvar-local claude-agent--mcp-server-status nil
  "List of MCP server status objects from the agent.
Each element is an alist with keys: name, status.")

(defvar-local claude-agent--input-history nil
  "History of inputs sent to Claude.")

(defvar-local claude-agent--input-history-index 0
  "Current position in input history.")

;;;; Buffer-local variables - Thinking status

(defconst claude-agent--spinner-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames for the spinner animation.")



(defvar-local claude-agent--spinner-index 0
  "Current index in spinner frames.")

(defvar-local claude-agent--spinner-timer nil
  "Timer for spinner animation.")

(defvar-local claude-agent--thinking-start-time nil
  "Time when thinking started, for elapsed time display.")

(defvar-local claude-agent--thinking-status nil
  "Current thinking status text, or nil if not thinking.")

(defvar-local claude-agent--input-tokens 0
  "Input token count for current turn.")

(defvar-local claude-agent--output-tokens 0
  "Output token count for current turn.")

;;;; Buffer-local variables - Progress indicators

(defvar-local claude-agent--progress-indicators nil
  "Hash table of active progress indicators.
Keys are progress IDs, values are plists with :message and :start-time.")

(defvar-local claude-agent--progress-visible t
  "Whether the progress section is visible.")

(defvar-local claude-agent--compacting nil
  "Non-nil when the conversation is being compacted.
This is set when Claude is summarizing the conversation history.")

(defvar-local claude-agent--todos-visible t
  "Whether the todo list section is visible.")

(defvar-local claude-agent--todos nil
  "List of current todo items.
Each item is an alist with keys: content, status, activeForm.")

(defvar-local claude-agent--has-conversation nil
  "Non-nil if conversation has started (first message sent).")

(defvar-local claude-agent--tool-results nil
  "Alist mapping tool call positions to their results.
Each entry is (MARKER NAME . RESULT-STRING).")

(defvar-local claude-agent--pending-tools nil
  "Hash table tracking pending tool calls by tool_use_id.
Keys are tool_use_id strings, values are plists with:
  :marker - buffer position marker for the tool call
  :name - tool name string
  :status-overlay - overlay for the status icon (○/✓/✗)")

(defvar-local claude-agent--denied-tools nil
  "Hash table tracking tool_use_ids that were permission-denied.
When a permission_denied event arrives, the tool_use_id is added here.
When the subsequent tool_result arrives, we check this set to show
a distinct 🚫 indicator instead of the normal ✗ error icon.")

(defvar-local claude-agent--work-dir nil
  "The working directory for this Claude session.")

;;;; Buffer-local variables - Message queue

(defvar-local claude-agent--message-queue nil
  "List of messages queued while agent is busy.
Each is a string.  Messages are appended to the end (FIFO order)
and sent from the front when the agent becomes ready.")

;;;; Buffer-local variables - System message hooks state

(defvar-local claude-agent--message-count 0
  "Number of user messages sent in this session.
Used by system message hooks to determine when to fire.")

(defvar-local claude-agent--is-resumed nil
  "Whether this session was resumed from a previous session.
Used by on_start/on_resume hooks to determine if they should fire.")

(defface claude-agent-queued-face
  '((((class color) (background dark))
     (:foreground "#5c6370" :slant italic))
    (((class color) (background light))
     (:foreground "#a0a1a7" :slant italic)))
  "Face for queued messages (grayed out)."
  :group 'claude-agent)

(defface claude-agent-queued-header-face
  '((((class color) (background dark))
     (:foreground "#5c6370" :weight bold))
    (((class color) (background light))
     (:foreground "#a0a1a7" :weight bold)))
  "Face for queued message headers."
  :group 'claude-agent)

(defface claude-agent-queue-highlight-face
  '((((class color) (background dark))
     (:background "#3e4451"))
    (((class color) (background light))
     (:background "#e5e5e6")))
  "Face for highlighting the queued message under the cursor."
  :group 'claude-agent)
(defvar-local claude-agent--queue-highlight-overlay nil
  "Overlay used to highlight the queued message at point.")

;;;; Mode definition

(defvar claude-agent-mode-map
  (let ((map (make-sparse-keymap)))
    ;; C-c C-c opens input or sends if input has text
    (define-key map (kbd "C-c C-c") #'claude-agent-send-or-open-input)
    (define-key map (kbd "C-<return>") #'claude-agent-send-or-open-input)
    (define-key map (kbd "C-c C-k") #'claude-agent-interrupt)
    (define-key map (kbd "C-c C-q") #'claude-agent-quit)
    (define-key map (kbd "C-c C-d") #'claude-agent-queue-delete)
    ;; Transient menu (C-c c for "claude")
    (define-key map (kbd "C-c c") #'claude-menu)
    map)
  "Keymap for `claude-agent-mode'.")

(defvar claude-agent-log-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Magit-style single-key bindings (always active in read-only REPL)
    ;; Model
    (define-key map (kbd "m") #'claude-agent-set-model)
    (define-key map (kbd "$") #'claude-agent-show-cost)
    ;; MCP
    (define-key map (kbd "M l") #'claude-agent-mcp-list)
    (define-key map (kbd "M s") #'claude-agent-show-mcp-status)
    (define-key map (kbd "M a") #'claude-agent-mcp-add)
    (define-key map (kbd "M r") #'claude-agent-mcp-remove)
    ;; Session control
    (define-key map (kbd "c") #'claude-agent-compact)
    (define-key map (kbd "C") #'claude-agent-clear)
    (define-key map (kbd "q") #'claude-agent-quit)
    (define-key map (kbd "k") #'claude-agent-interrupt)
    ;; Navigation - go to input
    (define-key map (kbd "i") #'claude-agent-goto-input)
    (define-key map (kbd "RET") #'claude-agent-goto-input)
    ;; Tool result viewing
    (define-key map (kbd "'") #'claude-agent-show-tool-result)
    (define-key map (kbd "TAB") #'claude-agent-toggle-tool-popup)
    ;; Navigation between messages/tool calls
    (define-key map (kbd "{") #'claude-agent-previous-section)
    (define-key map (kbd "}") #'claude-agent-next-section)
    ;; Follow mode
    (define-key map (kbd "f") #'claude-agent-toggle-follow)
    ;; Help
    (define-key map (kbd "?") #'claude-menu)
    map)
  "Keymap for the read-only REPL buffer.
Single-key bindings always active since buffer is fully read-only.")

;; Make log-mode-map the parent so single-key bindings always work
(set-keymap-parent claude-agent-mode-map claude-agent-log-mode-map)

(define-derived-mode claude-agent-mode fundamental-mode "Claude"
  "Major mode for Claude interaction buffer.
The buffer is fully read-only.  User input is handled in a separate
dedicated input buffer shown in a split window below."
  :group 'claude-agent
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local buffer-read-only t)
  (visual-line-mode 1)
  ;; Set up org-mode fontification without org-mode keybindings
  (require 'org)
  (org-set-font-lock-defaults)
  (font-lock-mode 1)
  ;; Disable flycheck and company to prevent expensive syntax parsing
  (when (bound-and-true-p flycheck-mode)
    (flycheck-mode -1))
  (when (bound-and-true-p company-mode)
    (company-mode -1))
  ;; Ensure our keybindings are set
  (use-local-map claude-agent-mode-map)
  ;; Set up post-command-hook
  (add-hook 'post-command-hook #'claude-agent--post-command-hook nil t)
  ;; Evil: entering insert state opens input window
  (add-hook 'evil-insert-state-entry-hook
            #'claude-agent--on-insert-state-entry nil t)
  ;; Add C-c c for transient menu
  (local-set-key (kbd "C-c c") #'claude-menu)
  ;; Clean up on buffer kill
  (add-hook 'kill-buffer-hook #'claude-agent--cleanup-on-kill nil t))

(defun claude-agent--cleanup-on-kill ()
  "Clean up resources when the REPL buffer is killed."
  (when claude-agent--spinner-timer
    (cancel-timer claude-agent--spinner-timer)
    (setq claude-agent--spinner-timer nil))
  ;; Kill input buffer too
  (when (and claude-agent--input-buffer
             (buffer-live-p claude-agent--input-buffer))
    (kill-buffer claude-agent--input-buffer)))

;;;; Helper functions

(defun claude-agent--should-follow-p ()
  "Return non-nil if the window cursor is in or past the dynamic section.
Must be called BEFORE buffer modifications to get an accurate read.
Uses `window-point' (not `point') since process filters run in buffer
context where `point' may be stale."
  (when-let ((win (get-buffer-window (current-buffer))))
    (let ((wp (window-point win))
          (static-end (and claude-agent--static-end-marker
                          (marker-position claude-agent--static-end-marker))))
      ;; Follow if cursor is at or past the start of the dynamic section
      (and static-end (>= wp static-end)))))

(defun claude-agent--maybe-follow ()
  "Scroll to bottom if `claude-agent--follow-mode' is active.
The flag should be set by callers (before buffer changes) via
`claude-agent--should-follow-p'."
  (when claude-agent--follow-mode
    (when-let ((win (get-buffer-window (current-buffer))))
      (with-selected-window win
        (goto-char (point-max))
        (recenter -1)))))

(defun claude-agent--update-follow-mode ()
  "No-op. Follow mode is determined before render by `should-follow-p'."
  nil)

(defun claude-agent-toggle-follow ()
  "Toggle auto-follow mode."
  (interactive)
  (setq claude-agent--follow-mode (not claude-agent--follow-mode))
  (when claude-agent--follow-mode
    (claude-agent--maybe-follow))
  (claude-agent--update-header-line)
  (message "Follow mode %s" (if claude-agent--follow-mode "ON" "OFF")))
;;;; Input buffer

(defvar claude-agent-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claude-agent-send)
    (define-key map (kbd "C-<return>") #'claude-agent-send)
    (define-key map (kbd "C-c C-k") #'claude-agent-input-dismiss)
    (define-key map (kbd "M-p") #'claude-agent-previous-input)
    (define-key map (kbd "M-n") #'claude-agent-next-input)
    (define-key map (kbd "C-c c") #'claude-menu)
    map)
  "Keymap for the Claude input buffer.")

(defvar-local claude-agent--input-parent-buffer nil
  "The REPL buffer that owns this input buffer.")

(defvar-local claude-agent--input-placeholder-overlay nil
  "Overlay for placeholder text in the input buffer.")

(define-derived-mode claude-agent-input-mode fundamental-mode "Claude Input"
  "Major mode for the Claude input buffer."
  :group 'claude-agent
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (visual-line-mode 1)
  (when (bound-and-true-p flycheck-mode)
    (flycheck-mode -1))
  (when (bound-and-true-p company-mode)
    (company-mode -1))
  (use-local-map claude-agent-input-mode-map)
  (add-hook 'post-command-hook #'claude-agent--input-update-placeholder nil t))

;; Evil integration: start in insert state for input buffer
(with-eval-after-load 'evil
  (evil-set-initial-state 'claude-agent-input-mode 'insert))

(defun claude-agent--input-update-placeholder ()
  "Update placeholder visibility in the input buffer."
  (when (eq major-mode 'claude-agent-input-mode)
    (if (string-blank-p (buffer-string))
        (unless claude-agent--input-placeholder-overlay
          (let ((ov (make-overlay (point-min) (point-min))))
            (overlay-put ov 'before-string
                         (propertize "Enter your message... (C-c C-c to send)"
                                     'face '(:foreground "#5c6370" :slant italic)))
            (overlay-put ov 'evaporate nil)
            (setq claude-agent--input-placeholder-overlay ov)))
      (when claude-agent--input-placeholder-overlay
        (delete-overlay claude-agent--input-placeholder-overlay)
        (setq claude-agent--input-placeholder-overlay nil)))))

(defun claude-agent--get-or-create-input-buffer ()
  "Get or create the input buffer for this Claude session."
  (or (and claude-agent--input-buffer
           (buffer-live-p claude-agent--input-buffer)
           claude-agent--input-buffer)
      (let* ((repl-buf (current-buffer))
             (name (format "*claude-input:%s*"
                           (replace-regexp-in-string
                            "^\\*claude:" ""
                            (replace-regexp-in-string "\\*$" "" (buffer-name)))))
             (buf (get-buffer-create name)))
        (with-current-buffer buf
          (claude-agent-input-mode)
          (setq claude-agent--input-parent-buffer repl-buf))
        (setq claude-agent--input-buffer buf)
        buf)))

(defun claude-agent--show-input-window ()
  "Show the input buffer in a small window below the REPL.
If already visible, just select it."
  (interactive)
  (claude-agent--in-base-buffer
   (let* ((input-buf (claude-agent--get-or-create-input-buffer))
          (repl-win (get-buffer-window (current-buffer)))
          (existing (get-buffer-window input-buf)))
     (cond
      (existing
       (select-window existing))
      (repl-win
       (let ((input-win (split-window repl-win -5 'below)))
         (set-window-buffer input-win input-buf)
         (set-window-dedicated-p input-win t)
         (setq claude-agent--input-window input-win)
         (select-window input-win)))
      (t
       (pop-to-buffer input-buf))))))

(defun claude-agent--close-input-window ()
  "Close the input window and kill the input buffer.
Selects the REPL window after closing and ensures evil normal state
to prevent the insert-state hook from re-opening the window."
  (when claude-agent--input-buffer
    (let ((buf claude-agent--input-buffer)
          (repl-win (get-buffer-window (current-buffer))))
      ;; Delete the window first
      (let ((win (get-buffer-window buf)))
        (when (and win (window-live-p win))
          (delete-window win)))
      ;; Kill the buffer
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (setq claude-agent--input-buffer nil)
      (setq claude-agent--input-window nil)
      ;; Return focus to REPL
      (when (and repl-win (window-live-p repl-win))
        (select-window repl-win)
        ;; Switch to normal state so the insert-state hook doesn't
        ;; immediately re-open the input window
        (when (and (bound-and-true-p evil-mode)
                   (fboundp 'evil-normal-state))
          (evil-normal-state))))))

(defun claude-agent--input-window-visible-p ()
  "Return non-nil if the input window is currently visible."
  (and claude-agent--input-buffer
       (buffer-live-p claude-agent--input-buffer)
       (get-buffer-window claude-agent--input-buffer)))

(defun claude-agent-input-dismiss ()
  "Close the input window and kill the input buffer."
  (interactive)
  (let ((parent (or claude-agent--input-parent-buffer (current-buffer))))
    (with-current-buffer parent
      (claude-agent--close-input-window))))

(defun claude-agent--add-tool-tooltip (marker content)
  "Add a tooltip with CONTENT preview to the tool call at MARKER."
  (when (and marker (marker-position marker))
    (let* ((line-end (save-excursion
                       (goto-char (marker-position marker))
                       (line-end-position)))
           ;; Truncate content for tooltip (first 500 chars, max 10 lines)
           (preview (if (> (length content) 500)
                        (concat (substring content 0 500) "\n...")
                      content))
           (preview (let ((lines (split-string preview "\n")))
                      (if (> (length lines) 10)
                          (concat (string-join (seq-take lines 10) "\n") "\n...")
                        preview))))
      ;; Create overlay for the whole tool call line
      (let ((ov (make-overlay (marker-position marker) line-end)))
        (overlay-put ov 'help-echo preview)
        (overlay-put ov 'claude-agent-tooltip t)
        (overlay-put ov 'evaporate t)))))

(defun claude-agent--tool-result-is-error-p (content)
  "Check if tool result CONTENT indicates an error."
  (and content
       (string-match-p
        (rx (or (seq line-start (or "error" "Error" "ERROR"))
                (seq line-start "<tool_use_error>")
                (seq line-start "⚠")
                (seq "Error:" (+ any))
                (seq "failed" (+ any))
                (seq "No " (or "files" "matches") " found")))
        content)))

(defun claude-agent--update-tool-status (overlay status)
  "Update the tool status OVERLAY to show STATUS.
STATUS should be `success', `error', or `denied'."
  (when (and overlay (overlay-buffer overlay))
    (let ((inhibit-read-only t)
          (start (overlay-start overlay))
          (end (overlay-end overlay)))
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (pcase status
          ('success
           (insert "✓ ")
           (move-overlay overlay start (point))
           (overlay-put overlay 'face 'claude-agent-tool-status-success-face))
          ('denied
           (insert "🚫 ")
           (move-overlay overlay start (point))
           (overlay-put overlay 'face 'claude-agent-tool-status-denied-face))
          ('error
           (insert "✗ ")
           (move-overlay overlay start (point))
           (overlay-put overlay 'face 'claude-agent-tool-status-error-face)))))))

(defvar-local claude-agent--tool-popup-enabled t
  "When non-nil, show tool result popup automatically when on a tool line.")

(defvar claude-agent--tool-popup-buffer " *claude-tool-popup*"
  "Buffer name for the tool result posframe.")

(defun claude-agent-toggle-tool-popup ()
  "Toggle automatic tool result popup display."
  (interactive)
  (setq claude-agent--tool-popup-enabled (not claude-agent--tool-popup-enabled))
  (if claude-agent--tool-popup-enabled
      (message "Tool popup enabled")
    (claude-agent--hide-tool-popup)
    (message "Tool popup disabled")))

(defun claude-agent--hide-tool-popup ()
  "Hide the tool result posframe."
  (when (and (fboundp 'posframe-hide)
             (get-buffer claude-agent--tool-popup-buffer))
    (posframe-hide claude-agent--tool-popup-buffer)))

(defface claude-agent-popup-hint-face
  '((((class color) (background dark))
     (:foreground "#5c6370" :slant italic))
    (((class color) (background light))
     (:foreground "#a0a1a7" :slant italic)))
  "Face for hint text in tool popup."
  :group 'claude-agent)

(defun claude-agent--show-tool-popup (content)
  "Show CONTENT in a posframe below the current line."
  (when (fboundp 'posframe-show)
    (let* ((max-lines 15)
           (max-chars 1000)
           ;; Truncate content
           (preview (if (> (length content) max-chars)
                        (concat (substring content 0 max-chars) "\n...")
                      content))
           (lines (split-string preview "\n"))
           (preview (if (> (length lines) max-lines)
                        (concat (string-join (seq-take lines max-lines) "\n") "\n...")
                      preview))
           ;; Add hints at the bottom
           (hints "\n─────────────────────────────────\nC-c ' full result  |  TAB disable popup")
           (full-content (concat preview hints)))
      (posframe-show claude-agent--tool-popup-buffer
                     :string full-content
                     :position (line-end-position)
                     :background-color "#1e1e1e"
                     :foreground-color "#abb2bf"
                     :border-color "#5c6370"
                     :border-width 1
                     :left-fringe 8
                     :right-fringe 8))))

(defun claude-agent--update-tool-popup ()
  "Update tool popup based on current cursor position.
Called from `post-command-hook'."
  (when (and claude-agent--tool-popup-enabled
             ;; Don't update during buffer modifications
             (not inhibit-read-only)
             ;; Only in claude-agent buffers
             (eq major-mode 'claude-agent-mode))
    (condition-case nil
        (if-let ((result (claude-agent--find-tool-result-at-point)))
            (let* ((name (car result))
                   (content (cdr result))
                   (formatter (cdr (assoc name claude-agent-tool-formatters)))
                   (formatted (if formatter (funcall formatter content) content)))
              (claude-agent--show-tool-popup formatted))
          (claude-agent--hide-tool-popup))
      ;; Silently ignore errors to prevent buffer corruption
      (error (claude-agent--hide-tool-popup)))))

(defvar claude-agent-tool-formatters
  '(("mcp__emacs__edit" . claude-agent--format-diff-output)
    ("Edit" . claude-agent--format-diff-output)
    ("Write" . claude-agent--format-diff-output))
  "Alist mapping tool names to formatter functions.
Each formatter takes a result string and returns a propertized string.")

(defun claude-agent--format-diff-output (content)
  "Format CONTENT as a diff with colored +/- lines."
  (let ((lines (split-string content "\n")))
    (mapconcat
     (lambda (line)
       (cond
        ((string-prefix-p "- " line)
         (propertize line 'face 'claude-agent-diff-removed))
        ((string-prefix-p "+ " line)
         (propertize line 'face 'claude-agent-diff-added))
        (t line)))
     lines "\n")))

(defun claude-agent--find-tool-result-at-point ()
  "Find the tool result for the tool call on the current line.
Returns (NAME . RESULT) cons or nil if not found."
  (let ((line-start (line-beginning-position))
        (line-end (line-end-position))
        (result nil))
    ;; Find a tool marker on this line
    (dolist (entry claude-agent--tool-results)
      (let ((marker (car entry)))
        (when (and (marker-position marker)
                   (>= (marker-position marker) line-start)
                   (<= (marker-position marker) line-end))
          (setq result entry))))
    ;; Return (NAME . RESULT) or nil
    (when result
      (cons (nth 1 result) (nth 2 result)))))

(defun claude-agent-show-tool-result ()
  "Show the result of the tool call at point in a popup buffer.
Like `org-edit-special' (C-c ') for source blocks."
  (interactive)
  (if-let ((result (claude-agent--find-tool-result-at-point)))
      (let* ((name (car result))
             (content (cdr result))
             (formatter (cdr (assoc name claude-agent-tool-formatters)))
             (formatted (if formatter (funcall formatter content) content))
             (buf (get-buffer-create "*claude-tool-result*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert formatted)
            (goto-char (point-min))
            (special-mode)))
        (display-buffer buf '(display-buffer-below-selected
                              (window-height . 0.4))))
    (message "No tool result found at point")))

(defun claude-agent-goto-input ()
  "Open the input window for typing a message."
  (interactive)
  (claude-agent--in-base-buffer
   (claude-agent--show-input-window)))

(defun claude-agent--section-header-p ()
  "Return non-nil if current line is a section header (message or tool call)."
  (save-excursion
    (beginning-of-line)
    (or (looking-at "^you> ")
        (looking-at "^claude> ")
        (looking-at "^[a-z-]+/[a-z-]+›")  ; MCP tool: server/tool›
        (looking-at "^[a-z]+›"))))          ; Built-in tool: edit›, grep›, etc.

(defun claude-agent-next-section ()
  "Move to the next message or tool call."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (claude-agent--section-header-p)))
      (forward-line 1))
    (if (eobp)
        (progn
          (goto-char start)
          (message "No more sections"))
      (beginning-of-line))))

(defun claude-agent-previous-section ()
  "Move to the previous message or tool call."
  (interactive)
  (let ((start (point)))
    (beginning-of-line)
    (when (claude-agent--section-header-p)
      (forward-line -1))
    (while (and (not (bobp))
                (not (claude-agent--section-header-p)))
      (forward-line -1))
    (if (bobp)
        (if (claude-agent--section-header-p)
            (beginning-of-line)
          (goto-char start)
          (message "No more sections"))
      (beginning-of-line))))

(defmacro claude-agent--in-base-buffer (&rest body)
  "Execute BODY in the base buffer (for polymode compatibility)."
  `(let ((base (or (buffer-base-buffer) (current-buffer))))
     (with-current-buffer base
       ,@body)))

(defun claude-agent--on-insert-state-entry ()
  "Open input window when entering evil insert state."
  (when (eq major-mode 'claude-agent-mode)
    (claude-agent--show-input-window)))

(defun claude-agent--post-command-hook ()
  "Hook run after each command in the Claude buffer."
  (claude-agent--update-tool-popup)
  (claude-agent--update-queue-highlight)
  (claude-agent--update-follow-mode))

;;;; Section management
;;
;; Two-zone architecture:
;; - Static section: Append-only (header + conversation log)
;; - Dynamic section: Re-rendered from state (status bar + permission dialog)

(defun claude-agent--init-buffer (session-name)
  "Initialize buffer with section structure for SESSION-NAME."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Insert header
    (let ((start (point)))
      (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
      (insert (format " Claude Session: %s\n" session-name))
      (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
      (add-text-properties start (point)
                           (list 'face 'claude-agent-header-face
                                 'fontified t)))
    ;; Mark end of static section
    (setq claude-agent--static-end-marker (point-marker))
    (set-marker-insertion-type claude-agent--static-end-marker nil)
    ;; Initialize header line
    (claude-agent--update-header-line)
    ;; Position cursor at end
    (goto-char (point-max))))

(defun claude-agent--apply-face (start end face)
  "Apply FACE to region from START to END using overlay."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'priority 100)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'claude-agent-styled t))
  ;; Also apply to indirect buffers (polymode)
  (let ((base (current-buffer)))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (eq (buffer-base-buffer buf) base))
        (with-current-buffer buf
          (let ((ov (make-overlay start end)))
            (overlay-put ov 'face face)
            (overlay-put ov 'priority 100)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'claude-agent-styled t)))))))

;;;; Dynamic section management
;;
;; Two-zone architecture:
;; - Static section: Append-only log content (header + conversation)
;; - Dynamic section: Re-rendered from state (status bar + permission dialog)
;;
;; `append-to-static` appends to the static section directly.
;; `render-dynamic-section` clears and re-renders dynamic section from state.

(defun claude-agent--append-to-static (text)
  "Append TEXT to the static section and re-render dynamic section.
This is a convenience wrapper for `append-to-log' without styling."
  (claude-agent--append-to-log text nil nil))

(defun claude-agent--render-dynamic-section (&optional pre-follow)
  "Render the dynamic section (status bar + permission dialog).
Clears everything after static-end-marker and re-renders from state.
The buffer is fully read-only; user input happens in a dedicated input buffer.
PRE-FOLLOW controls auto-scroll behavior:
  - non-nil: use as the pre-computed follow decision (caller already
    checked `should-follow-p' before modifying the buffer)
  - nil (default): compute follow from current window state before
    deleting the dynamic section."
  (let* ((inhibit-read-only t)
         ;; Determine follow BEFORE any buffer modifications.
         ;; When caller pre-computed follow, use that value directly.
         (should-follow (or pre-follow
                            (claude-agent--should-follow-p)))
         ;; Save absolute cursor position (for restoring if in static section)
         (saved-point (point))
         ;; Save cursor offset relative to static-end (if in dynamic section)
         (in-dynamic (and claude-agent--static-end-marker
                         (marker-position claude-agent--static-end-marker)
                         (>= (point) claude-agent--static-end-marker)))
         (dynamic-offset (when in-dynamic
                           (- (point) claude-agent--static-end-marker))))
    ;; Set follow mode from pre-computed value
    (setq claude-agent--follow-mode should-follow)
    ;; Clear queue highlight overlay (will be recreated by post-command-hook)
    (when claude-agent--queue-highlight-overlay
      (delete-overlay claude-agent--queue-highlight-overlay)
      (setq claude-agent--queue-highlight-overlay nil))
    ;; Clear overlays in dynamic section
    (when (and claude-agent--static-end-marker
               (marker-position claude-agent--static-end-marker))
      (dolist (ov (overlays-in claude-agent--static-end-marker (point-max)))
        (when (or (overlay-get ov 'claude-agent-styled)
                  (overlay-get ov 'claude-permission-face))
          (delete-overlay ov)))
      ;; Delete everything from static-end to end of buffer
      (delete-region claude-agent--static-end-marker (point-max)))

    ;; Position at start of dynamic section
    (goto-char (or claude-agent--static-end-marker (point-max)))

    ;; === INSERT STATUS BAR ===
    (when claude-agent--has-conversation
      (claude-agent--insert-status-bar))

    ;; === RENDER PERMISSION DIALOG IF ACTIVE ===
    (when claude-agent--permission-data
      (claude-agent--render-permission-content))

    ;; Restore cursor position
    (cond
     ;; Cursor was in dynamic section — restore relative to static-end
     ((and in-dynamic dynamic-offset (>= dynamic-offset 0))
      (goto-char (min (+ claude-agent--static-end-marker dynamic-offset)
                      (point-max))))
     ;; Cursor was in static section — restore absolute position
     ((not in-dynamic)
      (goto-char (min saved-point (point-max))))
     ;; Fallback
     (t (goto-char (point-max))))

    ;; Update header line with latest info
    (claude-agent--update-header-line)

    ;; Follow mode scroll
    (claude-agent--maybe-follow)))

;;;; Status bar rendering

(defun claude-agent--format-elapsed-time (start-time)
  "Format elapsed time since START-TIME as Xm Ys."
  (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
         (minutes (floor (/ elapsed 60)))
         (seconds (floor (mod elapsed 60))))
    (if (> minutes 0)
        (format "%dm%ds" minutes seconds)
      (format "%ds" seconds))))

(defun claude-agent--insert-status-bar ()
  "Insert the status bar content at point.
Called by `render-dynamic-section'. Assumes point is positioned correctly."
  ;; === Thinking indicator (if active) ===
  (when claude-agent--thinking-status
    (let ((start (point))
          (spinner (nth claude-agent--spinner-index
                        claude-agent--spinner-frames))
          (elapsed (if claude-agent--thinking-start-time
                       (claude-agent--format-elapsed-time
                        claude-agent--thinking-start-time)
                     "0s"))
          (tokens (format "(+%d/-%d)"
                          claude-agent--input-tokens
                          claude-agent--output-tokens)))
      (insert (format "\n%s %s %s %s (C-c C-k to interrupt)\n"
                      spinner
                      claude-agent--thinking-status
                      elapsed
                      tokens))
      (claude-agent--apply-face start (point)
                                (if claude-agent--compacting
                                    'claude-agent-compacting-face
                                  'claude-agent-thinking-face))))

  ;; === Compacting indicator (standalone, when not also thinking) ===
  (when (and claude-agent--compacting (not claude-agent--thinking-status))
    (let ((start (point))
          (spinner (nth claude-agent--spinner-index
                        claude-agent--spinner-frames)))
      (insert (format "\n%s 📦 Compacting conversation...\n" spinner))
      (claude-agent--apply-face start (point) 'claude-agent-compacting-face)))

  ;; === Progress indicators (if any and visible) ===
  (when (and claude-agent--progress-indicators
             claude-agent--progress-visible
             (> (hash-table-count claude-agent--progress-indicators) 0))
    (maphash
     (lambda (_id info)
       (let* ((label (plist-get info :label))
              (percent (or (plist-get info :percent) 0))
              (start-time (plist-get info :start-time))
              (elapsed (if start-time
                           (claude-agent--format-elapsed-time start-time)
                         ""))
              (bar-width 30)
              (filled (round (* bar-width (/ (min (max percent 0) 100.0) 100.0))))
              (empty (- bar-width filled)))
         ;; Insert label with percentage and elapsed time
         (let ((start (point)))
           (insert (format "\n  %s (%d%%) %s\n"
                           (or label "Working...")
                           (round percent)
                           elapsed))
           (claude-agent--apply-face start (point) 'claude-agent-progress-face))
         ;; Insert progress bar
         (let ((start (point)))
           (insert (format "  ▐%s%s▌\n"
                           (make-string filled ?█)
                           (make-string empty ?░)))
           (claude-agent--apply-face start (point) 'claude-agent-progress-face))))
     claude-agent--progress-indicators))

  ;; === Todo list (if any active and visible) ===
  ;; Hide when all todos are completed
  (let ((has-active-todos (and claude-agent--todos
                               (seq-some (lambda (todo)
                                           (let ((status (cdr (assq 'status todo))))
                                             (not (equal status "completed"))))
                                         claude-agent--todos))))
    (when (and has-active-todos claude-agent--todos-visible)
      (insert "\n")
      (dolist (todo claude-agent--todos)
        (let* ((content (cdr (assq 'content todo)))
               (status (cdr (assq 'status todo)))
               (active-form (cdr (assq 'activeForm todo)))
               (checkbox (pcase status
                           ("completed" "[X]")
                           ("in_progress" "[-]")
                           (_ "[ ]")))
               (face (pcase status
                       ("completed" 'claude-agent-todo-completed-face)
                       ("in_progress" 'claude-agent-todo-in-progress-face)
                       (_ 'claude-agent-todo-pending-face)))
               (text (if (equal status "in_progress")
                         (or active-form content)
                       content))
               (start (point)))
          (insert (format "  - %s %s\n" checkbox text))
          (claude-agent--apply-face start (point) face)))))

  ;; === Pending proposal indicator ===
  (when (claude-mcp-proposal-has-pending-p)
    (let* ((title (plist-get claude-mcp--pending-proposal :title))
           (start (point)))
      (insert (format "\n  📋 Proposal waiting for review: %s\n     C-c c P to review  |  C-c C-c accept  |  C-c C-k reject\n"
                      (or title "untitled")))
      (claude-agent--apply-face start (point) 'claude-agent-proposal-pending-face)))

  ;; === Queued messages (if any) ===
  ;; Rendered like dimmed user messages ("you>" but grayed out).
  ;; Each message region gets a `claude-queue-index' text property so that
  ;; `claude-agent-queue-delete' can identify which item the cursor is on.
  ;; Cursor-sensitive highlighting is handled by `claude-agent--update-queue-highlight'.
  (when claude-agent--message-queue
    (let ((queue-index 0))
      (dolist (msg claude-agent--message-queue)
        (let ((region-start (point)))
          ;; Header — mirrors "you> " but dimmed
          (let ((hdr-start (point)))
            (insert "you> ")
            (claude-agent--apply-face hdr-start (point) 'claude-agent-queued-header-face))
          ;; Message body
          (let ((body-start (point)))
            (insert msg "\n")
            (claude-agent--apply-face body-start (point) 'claude-agent-queued-face))
          ;; Tag the whole region with the queue index for cursor-based deletion
          (put-text-property region-start (point) 'claude-queue-index queue-index)
          (setq queue-index (1+ queue-index))))))
  ;; Status info (model, cost, session) is shown in header-line-format
  (insert "\n"))

(defun claude-agent--update-header-line ()
  "Update the header-line-format with model, cost, and session info."
  (let* ((model (or (plist-get claude-agent--session-info :model) "..."))
         (cost (or (plist-get claude-agent--session-info :cost) 0))
         (session-id (or (plist-get claude-agent--session-info :session-id) "..."))
         (short-session (if (> (length session-id) 8)
                            (substring session-id 0 8)
                          session-id))
         (thinking-indicator (if claude-agent--thinking-status " ⏳" ""))
         (proposal-indicator (if (claude-mcp-proposal-has-pending-p)
                                 (propertize " 📋 Proposal"
                                             'face 'claude-agent-proposal-pending-face)
                               "")))
    (setq header-line-format
          (list
           (propertize (format " %s " model)
                       'face 'claude-agent-header-model-face)
           " │ "
           (propertize (format "$%.4f" cost)
                       'face 'claude-agent-header-cost-face)
           " │ "
           (propertize (format "%s" short-session)
                       'face 'claude-agent-header-session-face)
           thinking-indicator
           proposal-indicator))
    (force-mode-line-update)))
(defun claude-agent--spinner-tick (buf)
  "Advance spinner in BUF and update in-place (lightweight).
BUF is captured at timer creation time so the tick always runs in
the correct buffer context, even when the user has switched away."
  (if (not (buffer-live-p buf))
      ;; Buffer was killed -- cancel this orphaned timer to prevent leaks.
      ;; We cannot access the buffer-local timer var, so scan timer-list.
      (dolist (timer timer-list)
        (when (and (eq (timer--function timer) #'claude-agent--spinner-tick)
                   (equal (timer--args timer) (list buf)))
          (cancel-timer timer)))
    (with-current-buffer buf
      (when claude-agent--thinking-status
        (setq claude-agent--spinner-index
              (mod (1+ claude-agent--spinner-index)
                   (length claude-agent--spinner-frames)))
        ;; Only update the spinner/elapsed time, don't rebuild everything
        (claude-agent--update-spinner-display)))))

(defun claude-agent--update-spinner-display ()
  "Update spinner and elapsed time in-place without full rebuild."
  (when (and claude-agent--thinking-status
             claude-agent--static-end-marker
             (marker-position claude-agent--static-end-marker))
    (let ((inhibit-read-only t)
          (spinner (nth claude-agent--spinner-index
                        claude-agent--spinner-frames))
          (elapsed (if claude-agent--thinking-start-time
                       (claude-agent--format-elapsed-time
                        claude-agent--thinking-start-time)
                     "0s"))
          (tokens (format "(+%d/-%d)"
                          claude-agent--input-tokens
                          claude-agent--output-tokens)))
      (save-excursion
        ;; Find the thinking indicator line (starts after static-end-marker)
        (goto-char claude-agent--static-end-marker)
        ;; Skip the first newline
        (when (looking-at "\n")
          (forward-char 1))
        ;; Now we should be at the start of the spinner line
        (when (looking-at ".*?\\(C-c C-k to interrupt\\)")
          (let ((line-start (point))
                (line-end (line-end-position)))
            ;; Replace the line
            (delete-region line-start line-end)
            (insert (format "%s %s %s %s (C-c C-k to interrupt)"
                            spinner
                            claude-agent--thinking-status
                            elapsed
                            tokens))
            ;; Reapply the face
            (claude-agent--apply-face line-start (point) 'claude-agent-thinking-face)))))))

(defun claude-agent--set-thinking (status)
  "Set thinking STATUS, or clear if nil."
  ;; Cancel existing timer
  (when claude-agent--spinner-timer
    (cancel-timer claude-agent--spinner-timer)
    (setq claude-agent--spinner-timer nil))

  (setq claude-agent--thinking-status status)

  (if status
      (progn
        ;; Start timing if not already
        (unless claude-agent--thinking-start-time
          (setq claude-agent--thinking-start-time (current-time)))
        ;; Start spinner timer, passing current buffer so the tick
        ;; always runs in the correct context (run-with-timer is global,
        ;; but our state variables are buffer-local).
        (let ((buf (current-buffer)))
          (setq claude-agent--spinner-timer
                (run-with-timer 0.1 0.1 #'claude-agent--spinner-tick buf))))
    ;; Clear timing when done
    (setq claude-agent--thinking-start-time nil))

  ;; Rebuild dynamic section (handles cursor positioning)
  (claude-agent--render-dynamic-section))

;;;; Content helpers

(defun claude-agent--count-diff-lines (old-string new-string)
  "Count lines removed and added from OLD-STRING and NEW-STRING.
Returns a cons cell (REMOVED . ADDED)."
  (let ((removed (if (and old-string (not (string-empty-p old-string)))
                     (length (split-string old-string "\n"))
                   0))
        (added (if (and new-string (not (string-empty-p new-string)))
                   (length (split-string new-string "\n"))
                 0)))
    (cons removed added)))

(defun claude-agent--format-diff-content (old-string new-string)
  "Format OLD-STRING and NEW-STRING as a diff string for storage.
Returns a string with - and + prefixed lines."
  (let ((result ""))
    ;; Old lines (removed)
    (when (and old-string (not (string-empty-p old-string)))
      (dolist (line (split-string old-string "\n"))
        (setq result (concat result "- " line "\n"))))
    ;; New lines (added)
    (when (and new-string (not (string-empty-p new-string)))
      (dolist (line (split-string new-string "\n"))
        (setq result (concat result "+ " line "\n"))))
    result))

(defun claude-agent--format-write-content (content)
  "Format CONTENT as diff-like output for Write tool popup.
Returns a string with + prefixed lines (all additions)."
  (let ((result ""))
    (when (and content (not (string-empty-p content)))
      (dolist (line (split-string content "\n"))
        (setq result (concat result "+ " line "\n"))))
    result))

(defun claude-agent--insert-diff (file-path old-string new-string)
  "Insert a diff display for FILE-PATH with OLD-STRING and NEW-STRING.
Inserts directly at point with proper faces and clickable link."
  (let ((inhibit-read-only t))
    ;; Tool header in new terse format (using overlays to survive font-lock)
    (let ((start (point)))
      (insert "edit")
      (claude-agent--apply-face start (point) 'claude-agent-tool-name-face))
    (let ((start (point)))
      (insert "› ")
      (claude-agent--apply-face start (point) 'claude-agent-tool-arrow-face))
    (insert-text-button file-path
                        'action (lambda (_btn)
                                  (find-file-other-window
                                   (button-get _btn 'file-path)))
                        'file-path file-path
                        'face 'claude-agent-tool-file-face
                        'help-echo "Click to open file"
                        'follow-link t)
    (insert "\n")
    ;; Old lines (removed)
    (when (and old-string (not (string-empty-p old-string)))
      (dolist (line (split-string old-string "\n"))
        (let ((line-start (point)))
          (insert "- " line "\n")
          (claude-agent--apply-face line-start (point) 'claude-agent-diff-removed))))
    ;; New lines (added)
    (when (and new-string (not (string-empty-p new-string)))
      (dolist (line (split-string new-string "\n"))
        (let ((line-start (point)))
          (insert "+ " line "\n")
          (claude-agent--apply-face line-start (point) 'claude-agent-diff-added))))))

(defun claude-agent--insert-edit-summary (file-path old-string new-string)
  "Insert a compact edit summary for FILE-PATH with line counts.
Shows format: ○ edit› filename.el (+N/-M)
The full diff is stored in tool-results for popup display.
Returns the status overlay for later updates (e.g., permission denied)."
  (let* ((inhibit-read-only t)
         (counts (claude-agent--count-diff-lines old-string new-string))
         (removed (car counts))
         (added (cdr counts))
         (filename (file-name-nondirectory file-path))
         (summary (format "%s (+%d/-%d)" filename added removed))
         (status-ov nil))
    ;; Status icon (pending until tool result confirms success or denial)
    (let ((icon-start (point)))
      (insert "○ ")
      (setq status-ov (make-overlay icon-start (point)))
      (overlay-put status-ov 'face 'claude-agent-tool-status-pending-face)
      (overlay-put status-ov 'priority 100)
      (overlay-put status-ov 'claude-tool-status t)
      (overlay-put status-ov 'evaporate t))
    ;; Tool header
    (let ((start (point)))
      (insert "edit")
      (claude-agent--apply-face start (point) 'claude-agent-tool-name-face))
    (let ((start (point)))
      (insert "› ")
      (claude-agent--apply-face start (point) 'claude-agent-tool-arrow-face))
    ;; Clickable filename with line counts
    (insert-text-button summary
                        'action (lambda (_btn)
                                  (find-file-other-window
                                   (button-get _btn 'file-path)))
                        'file-path file-path
                        'face 'claude-agent-tool-file-face
                        'help-echo "Click to open file, hover for diff preview"
                        'follow-link t)
    (insert "\n")
    ;; Return the status overlay for callers to store
    status-ov))

(defface claude-agent-tool-name-face
  '((((class color) (background dark))
     (:foreground "#e5c07b" :weight bold))
    (((class color) (background light))
     (:foreground "#986801" :weight bold)))
  "Face for tool names in tool calls (orange, like headers)."
  :group 'claude-agent)

(defface claude-agent-tool-arrow-face
  '((((class color) (background dark))
     (:foreground "#e5c07b"))
    (((class color) (background light))
     (:foreground "#986801")))
  "Face for the arrow separator in tool calls."
  :group 'claude-agent)

(defface claude-agent-tool-cmd-face
  '((((class color) (background dark))
     (:foreground "#abb2bf"))
    (((class color) (background light))
     (:foreground "#383a42")))
  "Face for command text in tool calls."
  :group 'claude-agent)

(defface claude-agent-tool-file-face
  '((((class color) (background dark))
     (:foreground "#61afef"))
    (((class color) (background light))
     (:foreground "#4078f2")))
  "Face for file paths in tool calls."
  :group 'claude-agent)

(defface claude-agent-tool-pattern-face
  '((((class color) (background dark))
     (:foreground "#98c379"))
    (((class color) (background light))
     (:foreground "#50a14f")))
  "Face for patterns (glob, grep) in tool calls."
  :group 'claude-agent)

(defface claude-agent-tool-continuation-face
  '((((class color) (background dark))
     (:foreground "#5c6370"))
    (((class color) (background light))
     (:foreground "#a0a1a7")))
  "Face for continuation markers in multi-line tool calls."
  :group 'claude-agent)

(defface claude-agent-tool-status-pending-face
  '((((class color) (background dark))
     (:foreground "#e5c07b"))
    (((class color) (background light))
     (:foreground "#986801")))
  "Face for pending tool status icon (yellow circle)."
  :group 'claude-agent)

(defface claude-agent-tool-status-success-face
  '((((class color) (background dark))
     (:foreground "#98c379"))
    (((class color) (background light))
     (:foreground "#50a14f")))
  "Face for successful tool status icon (green checkmark)."
  :group 'claude-agent)

(defface claude-agent-tool-status-error-face
  '((((class color) (background dark))
     (:foreground "#e06c75"))
    (((class color) (background light))
     (:foreground "#e45649")))
  "Face for error tool status icon (red X)."
  :group 'claude-agent)

(defface claude-agent-tool-status-denied-face
  '((((class color) (background dark))
     (:foreground "#e06c75"))
    (((class color) (background light))
     (:foreground "#e45649")))
  "Face for permission-denied tool status icon (🚫)."
  :group 'claude-agent)
(defun claude-agent--format-bash-multiline (command)
  "Format a multi-line bash COMMAND with pipe continuations."
  (let ((lines (split-string command "\n")))
    (if (= (length lines) 1)
        ;; Single line - just return propertized command
        (propertize command 'face 'claude-agent-tool-cmd-face)
      ;; Multi-line - add continuation markers
      (let ((result ""))
        (dotimes (i (length lines))
          (let ((line (nth i lines)))
            (if (= i 0)
                (setq result (concat result (propertize line 'face 'claude-agent-tool-cmd-face) "\n"))
              (setq result (concat result
                                   (propertize "   │ " 'face 'claude-agent-tool-continuation-face)
                                   (propertize line 'face 'claude-agent-tool-cmd-face)
                                   "\n")))))
        ;; Remove trailing newline since caller adds it
        (substring result 0 -1)))))

(defun claude-agent--format-tool-name (tool-name)
  "Format TOOL-NAME for display.
Converts MCP tools like 'mcp__emacs__reload_file' to 'emacs/reload-file'."
  (if (string-prefix-p "mcp__" tool-name)
      ;; MCP tool: mcp__server__tool_name -> server/tool-name
      (let* ((without-prefix (substring tool-name 5))  ; Remove "mcp__"
             (parts (split-string without-prefix "__"))
             (server (car parts))
             (tool (mapconcat #'identity (cdr parts) "_")))
        (concat server "/" (replace-regexp-in-string "_" "-" tool)))
    ;; Regular tool: just lowercase
    (downcase tool-name)))

(defun claude-agent--insert-tool-call (tool-name args-string &optional tool-use-id)
  "Insert a tool call display for TOOL-NAME with ARGS-STRING.
Uses terse format: ○ toolname› args with appropriate faces.
The status icon (○) is updated to ✓ or ✗ when tool result arrives.
TOOL-USE-ID is the unique identifier for this tool invocation."
  (let* ((inhibit-read-only t)
         (tool-lower (claude-agent--format-tool-name tool-name))
         (tool-marker nil)
         (status-overlay nil)
         ;; Compute follow BEFORE buffer modifications
         (should-follow (claude-agent--should-follow-p)))
    ;; Delete dynamic section
    (delete-region claude-agent--static-end-marker (point-max))
    (goto-char claude-agent--static-end-marker)
    ;; Mark position for this tool call
    (setq tool-marker (copy-marker (point)))

    ;; Insert pending status icon with overlay for later update
    (let ((icon-start (point)))
      (insert "○ ")
      (setq status-overlay (make-overlay icon-start (point)))
      (overlay-put status-overlay 'face 'claude-agent-tool-status-pending-face)
      (overlay-put status-overlay 'priority 100)
      (overlay-put status-overlay 'claude-tool-status t)
      (overlay-put status-overlay 'evaporate t))

    ;; Insert tool name with overlay (survives font-lock)
    (let ((start (point)))
      (insert tool-lower)
      (claude-agent--apply-face start (point) 'claude-agent-tool-name-face))
    ;; Insert arrow with overlay
    (let ((start (point)))
      (insert "› ")
      (claude-agent--apply-face start (point) 'claude-agent-tool-arrow-face))
    ;; Insert args with overlay based on tool type
    (let ((start (point))
          (face (cond
                 ((string= tool-name "Bash") nil)
                 ((member tool-name '("Read" "Write" "Edit")) 'claude-agent-tool-file-face)
                 ((member tool-name '("Glob" "Grep")) 'claude-agent-tool-pattern-face)
                 (t 'claude-agent-tool-cmd-face))))
      (if (string= tool-name "Bash")
          (insert (claude-agent--format-bash-multiline args-string))
        (insert args-string))
      (when face
        (claude-agent--apply-face start (point) face)))
    (insert "\n")

    ;; Update static marker
    (set-marker claude-agent--static-end-marker (point))

    ;; Rebuild dynamic section with pre-computed follow
    (claude-agent--render-dynamic-section should-follow)

    ;; Register in pending-tools hash table if we have a tool-use-id
    (when tool-use-id
      (unless claude-agent--pending-tools
        (setq claude-agent--pending-tools (make-hash-table :test 'equal)))
      (puthash tool-use-id
               (list :marker tool-marker
                     :name tool-name
                     :status-overlay status-overlay)
               claude-agent--pending-tools))
    ;; Return tool info for callers that need it
    (list :marker tool-marker
          :name tool-name
          :status-overlay status-overlay)))

(defun claude-agent--insert-tool-result-start ()
  "Insert the start of a tool result section."
  (claude-agent--append-to-log " #+begin_example\n" nil " "))

(defun claude-agent--insert-tool-result-end ()
  "Insert the end of a tool result section."
  (claude-agent--append-to-log " #+end_example\n" nil))

(defun claude-agent--insert-bash-tool (command)
  "Insert a Bash tool call with COMMAND."
  ;; Just use the standard tool call format, no special src block
  (claude-agent--insert-tool-call "Bash" command))

(defvar-local claude-agent--current-read-file nil
  "File path for current Read tool being displayed.")

(defun claude-agent--insert-read-tool (file-path)
  "Insert a Read tool call with FILE-PATH."
  (setq claude-agent--current-read-file file-path)
  ;; Just use the standard tool call format
  (claude-agent--insert-tool-call "Read" file-path))

(defun claude-agent--format-read-line (line)
  "Format a LINE from Read tool output with prettier line numbers.
Input format: '     N→content' where N is line number."
  (if (string-match "^\\( *\\)\\([0-9]+\\)→\\(.*\\)$" line)
      (let ((line-num (match-string 2 line))
            (content (match-string 3 line)))
        (cons (format "%4s│ " line-num) content))
    ;; Not a numbered line, return as-is
    (cons nil line)))

(defun claude-agent--insert-read-content (content)
  "Insert Read tool CONTENT with formatted line numbers.
Expects content in the format from Claude's Read tool."
  (let* ((inhibit-read-only t)
         (lines (split-string content "\n"))
         ;; Remove trailing empty lines
         (trimmed-lines (let ((result lines))
                          (while (and result (string-empty-p (car (last result))))
                            (setq result (butlast result)))
                          result))
         ;; Compute follow BEFORE buffer modifications
         (should-follow (claude-agent--should-follow-p)))
    ;; Delete dynamic section
    (delete-region claude-agent--static-end-marker (point-max))
    (goto-char claude-agent--static-end-marker)
    ;; Insert each line with formatted line numbers
    (dolist (line trimmed-lines)
      (let ((parsed (claude-agent--format-read-line line)))
        (if (car parsed)
            ;; Line with number
            (progn
              (let ((num-start (point)))
                (insert " " (car parsed))
                (claude-agent--apply-face num-start (point) 'claude-agent-line-number))
              (insert (cdr parsed) "\n"))
          ;; Plain line (no number)
          (insert " " (cdr parsed) "\n"))))
    ;; Update static marker
    (set-marker claude-agent--static-end-marker (point))
    ;; Rebuild dynamic section with pre-computed follow
    (claude-agent--render-dynamic-section should-follow)))

(defun claude-agent--append-to-log (text &optional face virtual-indent)
  "Append TEXT to the static section (conversation log).
If FACE is non-nil, apply it as an overlay to the inserted text.
If VIRTUAL-INDENT is non-nil, apply it as line-prefix/wrap-prefix.
After appending, re-renders the dynamic section (status bar + permissions)."
  (let ((inhibit-read-only t)
        (saved-point (point))
        ;; Compute follow BEFORE any buffer modifications
        (should-follow (claude-agent--should-follow-p)))
    ;; Delete everything from static-end to end (dynamic section)
    (delete-region claude-agent--static-end-marker (point-max))
    ;; Insert new static content with styling
    (goto-char claude-agent--static-end-marker)
    (let ((start (point)))
      (insert text)
      ;; Apply face overlay if specified
      (when face
        (claude-agent--apply-face start (point) face))
      ;; Apply virtual indent if specified
      (when virtual-indent
        (put-text-property start (point) 'line-prefix virtual-indent)
        (put-text-property start (point) 'wrap-prefix virtual-indent))
      ;; Mark as fontified to prevent org-mode font-lock from interfering
      (put-text-property start (point) 'fontified t))
    (set-marker claude-agent--static-end-marker (point))
    ;; Restore point before render (so render-dynamic-section sees correct position)
    (goto-char (min saved-point (point-max)))
    ;; Re-render dynamic section, passing pre-computed follow decision
    (claude-agent--render-dynamic-section should-follow)))
;;;; Process filter - parsing NDJSON messages

(defun claude-agent--process-filter (proc output)
  "Process filter for agent PROC handling OUTPUT."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (claude-agent--handle-output output)))))

(defun claude-agent--handle-output (output)
  "Handle OUTPUT from the agent process, parsing NDJSON messages."
  (setq claude-agent--pending-output
        (concat claude-agent--pending-output output))

  ;; Process complete lines (each line is a JSON message)
  (while (string-match "\n" claude-agent--pending-output)
    (let ((line (substring claude-agent--pending-output 0 (match-beginning 0))))
      (setq claude-agent--pending-output
            (substring claude-agent--pending-output (match-end 0)))
      (claude-agent--process-json-line line))))

(defun claude-agent--process-json-line (line)
  "Process a single LINE of NDJSON output."
  (when (and line (not (string-empty-p (string-trim line))))
    (condition-case err
        (let* ((msg (json-read-from-string line))
               (msg-type (cdr (assq 'type msg))))
          (claude-agent--dispatch-message msg-type msg))
      (json-readtable-error
       (message "Claude agent: Invalid JSON: %s" line))
      (error
       (message "Claude agent: Error processing message: %s" (error-message-string err))))))

(defun claude-agent--dispatch-message (msg-type msg)
  "Dispatch message MSG based on MSG-TYPE."
  (pcase msg-type
    ;; Ready - clear thinking, send queued messages
    ("ready"
     (claude-agent--set-thinking nil)
     (when claude-agent--message-queue
       (claude-agent--send-next-queued)))

    ;; Session start
    ("session_start"
     nil)  ; Handled by buffer init

    ;; Session info - update model/session-id
    ("session_info"
     (when-let ((model (cdr (assq 'model msg))))
       (setq claude-agent--session-info
             (plist-put claude-agent--session-info :model model)))
     (when-let ((session-id (cdr (assq 'session_id msg))))
       (setq claude-agent--session-info
             (plist-put claude-agent--session-info :session-id session-id)))
     (claude-agent--render-dynamic-section))

    ;; Available models - update dynamic model list from SDK
    ("available_models"
     (when-let ((models (cdr (assq 'models msg))))
       (setq claude-agent--available-models
             (seq-into models 'list))))

    ;; Thinking status
    ("thinking"
     (let ((status (cdr (assq 'status msg))))
       (unless claude-agent--thinking-start-time
         (setq claude-agent--input-tokens 0
               claude-agent--output-tokens 0
               claude-agent--thinking-start-time (current-time)))
       (claude-agent--set-thinking (or status "Thinking..."))))

    ;; Progress - update token counts
    ("progress"
     (when-let ((input (cdr (assq 'input_tokens msg))))
       (setq claude-agent--input-tokens input))
     (when-let ((output (cdr (assq 'output_tokens msg))))
       (setq claude-agent--output-tokens output)))

    ;; Result - update cost
    ("result"
     (when-let ((cost (cdr (assq 'cost_usd msg))))
       (setq claude-agent--session-info
             (plist-put claude-agent--session-info :cost cost)))
     ;; Clear compacting status on result
     (setq claude-agent--compacting nil)
     (claude-agent--render-dynamic-section))

    ;; Compacting - conversation is being summarized
    ("compacting"
     (let ((status (cdr (assq 'status msg))))
       (if (equal status "start")
           (progn
             (setq claude-agent--compacting t)
             (claude-agent--append-to-log
              "\n📦 Compacting conversation history...\n"
              'claude-agent-compacting-face))
         ;; status is "end" or similar
         (setq claude-agent--compacting nil))
       (claude-agent--render-dynamic-section)))

    ;; Todo update - update todo list display
    ("todo_update"
     (let ((todos (cdr (assq 'todos msg))))
       ;; Convert vector to list if needed (JSON arrays come as vectors)
       (setq claude-agent--todos (if (vectorp todos)
                                     (append todos nil)
                                   todos))
       (claude-agent--render-dynamic-section)))

    ;; MCP status
    ("mcp_status"
     (let ((servers (cdr (assq 'servers msg))))
       (setq claude-agent--mcp-server-status servers)
       (let ((failed (seq-filter
                      (lambda (s) (not (equal (cdr (assq 'status s)) "connected")))
                      servers)))
         (when failed
           (claude-agent--append-to-log
            (format "\n⚠ MCP server issue: %s\n"
                    (mapconcat (lambda (s)
                                 (format "%s (%s)"
                                         (cdr (assq 'name s))
                                         (cdr (assq 'status s))))
                               failed ", "))
            'claude-agent-error-face)))))

    ;; Permission request
    ("permission_request"
     (claude-agent--set-thinking "Awaiting permission...")
     (claude-agent--show-permission-prompt msg))

    ;; Permission granted (informational)
    ("permission_granted"
     nil)  ; Could show notification if desired

    ;; Permission denied - track tool_use_id for visual indicator on tool_result
    ("permission_denied"
     (let ((tool-use-id (cdr (assq 'tool_use_id msg))))
       (when tool-use-id
         (unless claude-agent--denied-tools
           (setq claude-agent--denied-tools (make-hash-table :test 'equal)))
         (puthash tool-use-id t claude-agent--denied-tools))))

    ;; User message start
    ("user_start"
     (setq claude-agent--parse-state 'user)
     (setq claude-agent--has-conversation t)
     (claude-agent--append-to-log "you> " 'claude-agent-user-header-face))

    ;; User message text
    ("user_text"
     (let ((text (cdr (assq 'text msg))))
       (claude-agent--append-to-log (concat text "\n") 'claude-agent-user-face "  ")))

    ;; User message end
    ("user_end"
     (setq claude-agent--parse-state nil))

    ;; Assistant message start
    ("assistant_start"
     (setq claude-agent--parse-state 'assistant)
     (claude-agent--append-to-log "claude> " 'claude-agent-assistant-header-face))

    ;; Assistant message text
    ("assistant_text"
     (let ((text (cdr (assq 'text msg))))
       (claude-agent--append-to-log (concat text "\n") nil "  ")))

    ;; Assistant message end
    ("assistant_end"
     (setq claude-agent--parse-state nil))

    ;; System message start
    ("system_start"
     (setq claude-agent--parse-state 'system)
     (claude-agent--append-to-log "system> " 'claude-agent-system-header-face))

    ;; System message text
    ("system_text"
     (let ((text (cdr (assq 'text msg))))
       (claude-agent--append-to-log (concat text "\n") 'claude-agent-system-message-face "  ")))

    ;; System message end
    ("system_end"
     (setq claude-agent--parse-state nil))

    ;; Tool call - all tools use the same simple format now
    ("tool_call"
     (let* ((name (cdr (assq 'name msg)))
            (input (cdr (assq 'input msg)))
            (tool-use-id (cdr (assq 'tool_use_id msg)))
            (args-str (claude-agent--format-tool-input-for-display name input)))
       (setq claude-agent--parse-state 'tool)
       ;; Insert tool call and register in pending-tools hash table
       (claude-agent--insert-tool-call name args-str tool-use-id)))

    ;; Tool result - store for later viewing with C-c ' and add tooltip
    ("tool_result"
     (let* ((content (cdr (assq 'content msg)))
            (tool-use-id (cdr (assq 'tool_use_id msg)))
            (tool-info (and tool-use-id claude-agent--pending-tools
                            (gethash tool-use-id claude-agent--pending-tools)))
            (tool-marker (plist-get tool-info :marker))
            (tool-name (plist-get tool-info :name))
            (status-overlay (plist-get tool-info :status-overlay))
            (is-denied (and tool-use-id claude-agent--denied-tools
                            (gethash tool-use-id claude-agent--denied-tools))))
       (when (and tool-marker content)
         (push (list tool-marker tool-name content)
               claude-agent--tool-results)
         ;; Update status icon: 🚫 for permission denied, ✗ for error, ✓ for success
         (when status-overlay
           (claude-agent--update-tool-status
            status-overlay
            (cond
             (is-denied 'denied)
             ((claude-agent--tool-result-is-error-p content) 'error)
             (t 'success))))
         ;; Clean up denied tracking
         (when is-denied
           (remhash tool-use-id claude-agent--denied-tools))
         ;; Add tooltip to the tool call line
         (claude-agent--add-tool-tooltip tool-marker content))))

    ;; Tool end - clean up from pending-tools
    ("tool_end"
     (let ((tool-use-id (cdr (assq 'tool_use_id msg))))
       (when (and tool-use-id claude-agent--pending-tools)
         (remhash tool-use-id claude-agent--pending-tools)))
     (setq claude-agent--parse-state nil)
     (claude-agent--set-thinking "Thinking..."))

    ;; Edit tool (compact summary display with diff in popup)
    ("edit_tool"
     (let* ((file-path (cdr (assq 'file_path msg)))
            (old-string (cdr (assq 'old_string msg)))
            (new-string (cdr (assq 'new_string msg)))
            (tool-use-id (cdr (assq 'tool_use_id msg)))
            (tool-marker nil)
            (edit-status-overlay nil))
       (setq claude-agent--parse-state 'tool)
       (claude-agent--set-thinking (format "Editing: %s" (file-name-nondirectory file-path)))
       ;; Format diff content for storage in tool-results
       (let ((diff-content (claude-agent--format-diff-content old-string new-string)))
         ;; Compute follow BEFORE buffer modifications
         (let ((should-follow (claude-agent--should-follow-p)))
           ;; Insert compact summary instead of full diff
           (let ((inhibit-read-only t))
             (delete-region claude-agent--static-end-marker (point-max))
             (goto-char claude-agent--static-end-marker)
             ;; Track marker position before inserting
             (setq tool-marker (copy-marker (point)))
             (setq edit-status-overlay
                   (claude-agent--insert-edit-summary file-path old-string new-string))
             (set-marker claude-agent--static-end-marker (point))
             ;; Store in tool-results for popup viewing
             (push (list tool-marker "Edit" diff-content)
                   claude-agent--tool-results)
             ;; Register in pending-tools hash table if we have a tool-use-id
             (when tool-use-id
               (unless claude-agent--pending-tools
                 (setq claude-agent--pending-tools (make-hash-table :test 'equal)))
               (puthash tool-use-id
                        (list :marker tool-marker
                              :name "Edit"
                              :status-overlay edit-status-overlay)
                        claude-agent--pending-tools))
             ;; Add tooltip to the summary line
             (claude-agent--add-tool-tooltip tool-marker diff-content)
             ;; Rebuild dynamic section with pre-computed follow
             (claude-agent--render-dynamic-section should-follow))))))

    ;; Write tool (compact display with content stored for popup)
    ("write_tool"
     (let* ((file-path (cdr (assq 'file_path msg)))
            (content (cdr (assq 'content msg)))
            (tool-use-id (cdr (assq 'tool_use_id msg)))
            (write-content (claude-agent--format-write-content content))
            (tool-info nil))
       (setq claude-agent--parse-state 'tool)
       (claude-agent--set-thinking (format "Writing: %s" (file-name-nondirectory file-path)))
       ;; Insert compact tool call line - returns tool info plist
       (setq tool-info (claude-agent--insert-tool-call "Write" file-path tool-use-id))
       ;; Store in tool-results for popup viewing
       (push (list (plist-get tool-info :marker) "Write" write-content)
             claude-agent--tool-results)
       ;; Add tooltip
       (claude-agent--add-tool-tooltip (plist-get tool-info :marker) write-content)))

    ;; Session message (system notifications)
    ("session_message_start"
     (setq claude-agent--parse-state 'session))

    ("session_message_text"
     (let ((text (cdr (assq 'text msg))))
       (claude-agent--append-to-log (concat text "\n") 'claude-agent-session-face)))

    ("session_message_end"
     (setq claude-agent--parse-state nil))

    ;; Retry status - transient error being retried
    ("retry_status"
     (let ((attempt (cdr (assq 'attempt msg)))
           (max-retries (cdr (assq 'max_retries msg)))
           (error-detail (cdr (assq 'error msg)))
           (delay (cdr (assq 'delay_seconds msg))))
       (let ((retry-msg (format "⟳ Transient error, retrying (%s/%s) in %ss: %s"
                                attempt max-retries delay error-detail)))
         (claude-agent--append-to-log
          (format "\n%s\n" retry-msg)
          'claude-agent-session-face)
         (message "Claude agent: %s" retry-msg)
         (claude-agent--set-thinking
          (format "Retrying (%s/%s) in %ss..." attempt max-retries delay)))))

    ;; Error
    ("error"
     (let ((message-text (cdr (assq 'message msg)))
           (traceback (cdr (assq 'traceback msg))))
       (claude-agent--append-to-log
        (format "\n⚠ Error: %s\n" message-text)
        'claude-agent-error-face)
       (when traceback
         (claude-agent--append-to-log
          (format "Traceback:\n%s\n" traceback)
          'claude-agent-error-face))))

    ;; Unknown message type
    (_
     (message "Claude agent: Unknown message type: %s" msg-type))))

(defun claude-agent--format-tool-input-for-display (tool-name tool-input)
  "Format TOOL-INPUT for display based on TOOL-NAME."
  (cond
   ((member tool-name '("Read" "Write" "Edit"))
    (cdr (assq 'file_path tool-input)))
   ((string= tool-name "Bash")
    (cdr (assq 'command tool-input)))
   ((string= tool-name "Glob")
    (cdr (assq 'pattern tool-input)))
   ((string= tool-name "Grep")
    (let ((pattern (cdr (assq 'pattern tool-input)))
          (path (cdr (assq 'path tool-input))))
      (if path (format "%s, %s" pattern path) pattern)))
   ((string= tool-name "WebFetch")
    (cdr (assq 'url tool-input)))
   ((string= tool-name "Task")
    (cdr (assq 'description tool-input)))
   (t
    (let ((first-val (cdar tool-input)))
      (if first-val
          (format "%s" first-val)
        "")))))

;;;; Permission prompt UI

(defvar-local claude-agent--permission-data nil
  "Current permission request data being displayed.")

(defvar-local claude-agent--permission-queue nil
  "Queue of pending permission requests waiting to be shown.
Each element is permission data (an alist with tool_use_id, tool_name, tool_input).")

(defvar-local claude-agent--permission-selection 0
  "Currently selected option in permission prompt (0-3).")

(defface claude-agent-permission-box-face
  '((((class color) (background dark))
     (:foreground "#e5c07b" :background "#3e4451" :box (:line-width 1 :color "#5c6370")))
    (((class color) (background light))
     (:foreground "#986801" :background "#e5e5e6" :box (:line-width 1 :color "#a0a1a7"))))
  "Face for permission dialog box."
  :group 'claude-agent)

(defface claude-agent-permission-selected-face
  '((((class color) (background dark))
     (:foreground "#282c34" :background "#61afef" :weight bold))
    (((class color) (background light))
     (:foreground "#fafafa" :background "#4078f2" :weight bold)))
  "Face for selected option in permission dialog."
  :group 'claude-agent)

(defface claude-agent-permission-option-face
  '((((class color) (background dark))
     (:foreground "#abb2bf"))
    (((class color) (background light))
     (:foreground "#383a42")))
  "Face for unselected options in permission dialog."
  :group 'claude-agent)
(defun claude-agent--format-tool-input (tool-name tool-input)
  "Format TOOL-INPUT for display based on TOOL-NAME."
  (cond
   ((string= tool-name "Read")
    (cdr (assq 'file_path tool-input)))
   ((string= tool-name "Write")
    (cdr (assq 'file_path tool-input)))
   ((string= tool-name "Edit")
    (cdr (assq 'file_path tool-input)))
   ((string= tool-name "Bash")
    (cdr (assq 'command tool-input)))
   (t (format "%s" tool-input))))

(defun claude-agent--generate-permission-pattern (tool-name tool-input scope)
  "Generate permission pattern for TOOL-NAME with TOOL-INPUT at SCOPE level."
  (pcase scope
    ('once
     (pcase tool-name
       ("Read" (format "Read(%s)" (cdr (assq 'file_path tool-input))))
       ("Write" (format "Write(%s)" (cdr (assq 'file_path tool-input))))
       ("Edit" (format "Edit(%s)" (cdr (assq 'file_path tool-input))))
       ("Bash" (format "Bash(%s)" (cdr (assq 'command tool-input))))
       (_ (format "%s" tool-name))))
    ('session
     (pcase tool-name
       ("Read" (format "Read(%s)" (cdr (assq 'file_path tool-input))))
       ("Write" (format "Write(%s)" (cdr (assq 'file_path tool-input))))
       ("Edit" (format "Edit(%s)" (cdr (assq 'file_path tool-input))))
       ("Bash"
        (let* ((cmd (cdr (assq 'command tool-input)))
               (first-word (car (split-string cmd))))
          (format "Bash(%s:*)" first-word)))
       (_ (format "%s(*)" tool-name))))
    ('always
     (pcase tool-name
       ("Read"
        (let* ((path (cdr (assq 'file_path tool-input)))
               (dir (file-name-directory path)))
          (format "Read(%s*)" (or dir "/"))))
       ("Write"
        (let* ((path (cdr (assq 'file_path tool-input)))
               (dir (file-name-directory path)))
          (format "Write(%s*)" (or dir "/"))))
       ("Edit"
        (let* ((path (cdr (assq 'file_path tool-input)))
               (dir (file-name-directory path)))
          (format "Edit(%s*)" (or dir "/"))))
       ("Bash"
        (let* ((cmd (cdr (assq 'command tool-input)))
               (first-word (car (split-string cmd))))
          (format "Bash(%s:*)" first-word)))
       (_ (format "%s(*)" tool-name))))))

(defvar-local claude-agent--permission-overlay-specs nil
  "List of (start end face) specs for permission dialog overlays.")

(defun claude-agent--apply-permission-overlays ()
  "Apply permission overlays in the current buffer using saved specs."
  (when claude-agent--permission-overlay-specs
    ;; Remove existing permission overlays in this buffer
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'claude-permission-face)
        (delete-overlay ov)))
    ;; Apply new overlays
    (dolist (spec claude-agent--permission-overlay-specs)
      (let ((ov (make-overlay (nth 0 spec) (nth 1 spec))))
        (overlay-put ov 'face (nth 2 spec))
        (overlay-put ov 'priority 1000)
        (overlay-put ov 'evaporate nil)
        (overlay-put ov 'claude-permission-face t)))))

(defun claude-agent--render-permission-content ()
  "Insert the permission dialog content at point.
Called by `render-dynamic-section' when in permission mode.
Uses compact inline format when in text-with-permission mode."
  (when claude-agent--permission-data
    (let* ((tool-name (cdr (assq 'tool_name claude-agent--permission-data)))
           (tool-input (cdr (assq 'tool_input claude-agent--permission-data)))
           (input-str (claude-agent--format-tool-input tool-name tool-input))
           (sel claude-agent--permission-selection)
           (inhibit-read-only t)
           (compact t)  ;; Always use compact inline format
           (overlay-specs nil))
      ;; Helper to insert and record overlay spec
      (cl-flet ((insert-styled (text face)
                  (let ((start (point)))
                    (insert text)
                    (push (list start (point) face) overlay-specs))))
        (if compact
            ;; Compact 3-line format for inline display
            (let ((short-options '("once" "session" "always" "deny")))
              ;; Line 1: Tool being requested
              (insert-styled "⚡ " 'claude-agent-session-face)
              (insert-styled (format "%s(%s)" tool-name input-str) 'claude-agent-tool-face)
              (insert "\n")
              ;; Line 2: Options as inline buttons
              (insert "  ")
              (dotimes (i 4)
                (let* ((selected (= i sel))
                       (label (nth i short-options))
                       (face (if selected
                                 'claude-agent-permission-selected-face
                               'claude-agent-permission-option-face)))
                  (insert-styled (format "[%d:%s]" (1+ i) label) face)
                  (when (< i 3) (insert " "))))
              (insert "\n")
              ;; Line 3: Hint
              (insert-styled "  ↑↓ navigate, RET confirm, C-1..C-4 direct, C-g deny" 'claude-agent-session-face)
              (insert "\n"))
          ;; Full format for standalone permission mode
          (let ((options '("Allow once" "Allow for this session" "Always allow" "Deny")))
            ;; Header
            (insert-styled "── Permission Request " 'claude-agent-input-header-face)
            (insert-styled (make-string 40 ?─) 'claude-agent-input-header-face)
            (insert "\n")
            ;; Tool info
            (insert-styled " Claude wants to run:\n" 'claude-agent-session-face)
            (insert-styled (format " %s(%s)\n\n" tool-name input-str) 'claude-agent-tool-face)
            ;; Options
            (dotimes (i 4)
              (let* ((selected (= i sel))
                     (checkbox (if selected "[X]" "[ ]"))
                     (label (nth i options))
                     (face (if selected
                               'claude-agent-permission-selected-face
                             'claude-agent-permission-option-face)))
                (insert-styled (format " %d. %s %s\n" (1+ i) checkbox label) face)))
            ;; Footer
            (insert-styled (make-string 62 ?─) 'claude-agent-input-header-face)
            (insert "\n"))))
      ;; Save overlay specs and apply
      (setq claude-agent--permission-overlay-specs (nreverse overlay-specs))
      (claude-agent--apply-permission-overlays)
      ;; Apply in indirect buffers too
      (let ((base (current-buffer)))
        (dolist (buf (buffer-list))
          (when (and (buffer-live-p buf)
                     (eq (buffer-base-buffer buf) base))
            (with-current-buffer buf
              (setq claude-agent--permission-overlay-specs
                    (buffer-local-value 'claude-agent--permission-overlay-specs base))
              (claude-agent--apply-permission-overlays))))))))

(defun claude-agent--render-permission-dialog ()
  "Re-render the permission dialog (updates selection state).
This is called when the user navigates options."
  (claude-agent--render-dynamic-section))

(defun claude-agent--show-permission-prompt (data)
  "Show permission prompt for DATA in the dynamic section.
If a permission dialog is already showing, queue this request."
  (if claude-agent--permission-data
      ;; Already showing a permission prompt - queue this one
      (progn
        (push data claude-agent--permission-queue)
        (claude-agent--set-thinking
         (format "Awaiting permission... (%d queued)"
                 (length claude-agent--permission-queue))))
    ;; No current permission prompt - show this one
    (setq claude-agent--permission-data data)
    (setq claude-agent--permission-selection 0)
    ;; Render the dialog (which now uses render-dynamic-section)
    (claude-agent--render-permission-dialog)
    ;; Set up keyboard navigation
    (claude-agent--setup-permission-keymap)))

(defun claude-agent--show-next-queued-permission ()
  "Show the next queued permission request, if any."
  (when claude-agent--permission-queue
    (let ((next-data (pop claude-agent--permission-queue)))
      ;; Show the next permission prompt
      (setq claude-agent--permission-data next-data)
      (setq claude-agent--permission-selection 0)
      (claude-agent--render-permission-dialog)
      (claude-agent--setup-permission-keymap)
      (when claude-agent--permission-queue
        (claude-agent--set-thinking
         (format "Awaiting permission... (%d queued)"
                 (length claude-agent--permission-queue)))))))

(defun claude-agent--permission-select-next ()
  "Move selection down in permission dialog."
  (interactive)
  (claude-agent--in-base-buffer
   (when claude-agent--permission-data
     (setq claude-agent--permission-selection
           (mod (1+ claude-agent--permission-selection) 4))
     (claude-agent--render-permission-dialog))))

(defun claude-agent--permission-select-prev ()
  "Move selection up in permission dialog."
  (interactive)
  (claude-agent--in-base-buffer
   (when claude-agent--permission-data
     (setq claude-agent--permission-selection
           (mod (1- claude-agent--permission-selection) 4))
     (claude-agent--render-permission-dialog))))

(defun claude-agent--permission-confirm ()
  "Confirm the current selection in permission dialog."
  (interactive)
  (claude-agent--in-base-buffer
   (when claude-agent--permission-data
     (pcase claude-agent--permission-selection
       (0 (claude-agent--send-permission-response "allow_once"))
       (1 (claude-agent--send-permission-response "allow_session"))
       (2 (claude-agent--send-permission-response "allow_always"))
       (3 (claude-agent--send-permission-response "deny"))))))

;; Minor mode for permission dialog - uses chord keys to not interfere with typing
(defvar claude-agent-permission-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation with M-n/M-p and arrow keys
    (define-key map (kbd "M-p") #'claude-agent--permission-select-prev)
    (define-key map (kbd "M-n") #'claude-agent--permission-select-next)
    (define-key map (kbd "<up>") #'claude-agent--permission-select-prev)
    (define-key map (kbd "<down>") #'claude-agent--permission-select-next)
    ;; Confirm with RET or C-c C-c
    (define-key map (kbd "RET") #'claude-agent--permission-confirm)
    (define-key map (kbd "C-c C-c") #'claude-agent--permission-confirm)
    ;; Direct selection with C-1 through C-4
    (define-key map (kbd "C-1") #'claude-agent-permit-once)
    (define-key map (kbd "C-2") #'claude-agent-permit-session)
    (define-key map (kbd "C-3") #'claude-agent-permit-always)
    (define-key map (kbd "C-4") #'claude-agent-deny)
    ;; C-g to deny (standard Emacs cancel)
    (define-key map (kbd "C-g") #'claude-agent-deny)
    map)
  "Keymap for permission dialog mode.")

;; Use emulation-mode-map-alists to give permission keymap highest priority
(defvar claude-agent--permission-emulation-map-alist nil
  "Alist for `emulation-mode-map-alists' to override other keymaps during permission.")

(define-minor-mode claude-agent-permission-mode
  "Minor mode for permission dialog interaction.
Uses chord keys so typing is not affected."
  :lighter " Permit"
  :keymap claude-agent-permission-mode-map
  (if claude-agent-permission-mode
      (progn
        ;; Use emulation-mode-map-alists for higher priority
        (setq claude-agent--permission-emulation-map-alist
              `((claude-agent-permission-mode . ,claude-agent-permission-mode-map)))
        (add-to-list 'emulation-mode-map-alists 'claude-agent--permission-emulation-map-alist)
        (message "Permission: ↑↓ navigate, RET confirm, C-1..C-4 direct, C-g deny"))
    ;; Remove from emulation alist when disabling
    (setq emulation-mode-map-alists
          (delq 'claude-agent--permission-emulation-map-alist emulation-mode-map-alists))))

(defun claude-agent--setup-permission-keymap ()
  "Set up keymap for permission prompt interaction."
  ;; Enable permission mode in the base buffer
  (claude-agent-permission-mode 1)
  ;; For polymode: also enable in all indirect buffers sharing this base
  (let ((base (or (buffer-base-buffer) (current-buffer))))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (eq (buffer-base-buffer buf) base))
        (with-current-buffer buf
          (claude-agent-permission-mode 1))))))

(defun claude-agent--send-permission-response (action)
  "Send permission response with ACTION to the agent process.
Restores text input mode and any saved input."
  (claude-agent--in-base-buffer
   (when claude-agent--permission-data
     (let* ((tool-name (cdr (assq 'tool_name claude-agent--permission-data)))
            (tool-input (cdr (assq 'tool_input claude-agent--permission-data)))
            (tool-use-id (cdr (assq 'tool_use_id claude-agent--permission-data)))
            (scope (pcase action
                     ("allow_once" 'once)
                     ("allow_session" 'session)
                     ("allow_always" 'always)
                     (_ nil)))
            (pattern (when scope
                       (claude-agent--generate-permission-pattern
                        tool-name tool-input scope)))
            (response-msg `((type . "permission_response")
                            (action . ,action)
                            (pattern . ,pattern)
                            ,@(when tool-use-id `((tool_use_id . ,tool-use-id))))))
       ;; Clear permission state and disable minor mode in all related buffers
       (setq claude-agent--permission-data nil)
       (setq claude-agent--permission-overlay-specs nil)
       (claude-agent-permission-mode -1)
       ;; For polymode: also disable in all indirect buffers sharing this base
       (let ((base (current-buffer)))
         (dolist (buf (buffer-list))
           (when (and (buffer-live-p buf)
                      (eq (buffer-base-buffer buf) base))
             (with-current-buffer buf
               (claude-agent-permission-mode -1)
               ;; Clear permission overlays in indirect buffers too
               (dolist (ov (overlays-in (point-min) (point-max)))
                 (when (overlay-get ov 'claude-permission-face)
                   (delete-overlay ov)))))))
       ;; Clear permission overlays in base buffer
       (dolist (ov (overlays-in (point-min) (point-max)))
         (when (overlay-get ov 'claude-permission-face)
           (delete-overlay ov)))
       ;; Send JSON response to process
       (when (and claude-agent--process
                  (process-live-p claude-agent--process))
         (process-send-string claude-agent--process
                              (concat (json-encode response-msg) "\n")))
       ;; Check if there are more queued permission requests
       (if claude-agent--permission-queue
           ;; Show the next queued permission
           (claude-agent--show-next-queued-permission)
         ;; No more queued - re-render and show thinking
         (claude-agent--render-dynamic-section)
         (claude-agent--set-thinking "Processing..."))))))

(defun claude-agent-permit-once ()
  "Allow the tool to run once."
  (interactive)
  (claude-agent--send-permission-response "allow_once"))

(defun claude-agent-permit-session ()
  "Allow the tool pattern for this session."
  (interactive)
  (claude-agent--send-permission-response "allow_session"))

(defun claude-agent-permit-always ()
  "Always allow this tool pattern (saves to settings)."
  (interactive)
  (claude-agent--send-permission-response "allow_always"))

(defun claude-agent-deny ()
  "Deny the permission request."
  (interactive)
  (claude-agent--send-permission-response "deny"))

;;;; Process management


(defun claude-agent--generate-mcp-config (work-dir buffer-name)
  "Generate MCP config file for emacs_mcp server.
WORK-DIR is the session working directory.
BUFFER-NAME is the Claude buffer name for this session.
Returns the path to the generated config file."
  (let* ((agent-dir (claude-agent--get-agent-dir))
         (emacs-mcp-dir (expand-file-name "../emacs_mcp" agent-dir))
         (config-file (make-temp-file "claude-mcp-config-" nil ".json"))
         (server-socket (expand-file-name (or (bound-and-true-p server-name) "server")
                                          (or (bound-and-true-p server-socket-dir)
                                              (expand-file-name "emacs" (temporary-file-directory)))))
         (config `((mcpServers
                    . ((emacs
                        . ((command . "uv")
                           (args . ["run" "--directory" ,emacs-mcp-dir
                                    "python" "-m" "emacs_mcp.server"])
                           (env . ((CLAUDE_AGENT_SOCKET . ,server-socket)
                                   (CLAUDE_AGENT_CWD . ,(expand-file-name work-dir))
                                   (CLAUDE_AGENT_BUFFER_NAME . ,buffer-name))))))))))
    (with-temp-file config-file
      (insert (json-encode config)))
    config-file))

(defvar-local claude-agent--mcp-config-file nil
  "Path to the MCP config file for this session.")

(defun claude-agent--validate-prerequisites ()
  "Validate that all required commands and directories exist.
Returns nil if valid, or an error message string if validation fails."
  (let ((agent-dir (claude-agent--get-agent-dir)))
    (cond
     ;; Check if command exists in PATH
     ((not (executable-find claude-agent-python-command))
      (format "Command '%s' not found in PATH. Please install it or set `claude-agent-python-command' to the correct command.\n\nFor uv installation, see: https://docs.astral.sh/uv/getting-started/installation/"
              claude-agent-python-command))
     ;; Check if agent directory exists
     ((not agent-dir)
      "Could not locate claude-agent library using (locate-library \"claude-agent\").\n\nPlease ensure claude-agent is properly installed and in your `load-path'.")
     ((not (file-directory-p agent-dir))
      (format "Agent directory not found: %s\n\nThe claude_agent Python package should be in the same directory as claude-agent.el."
              agent-dir))
     ;; Check if Python module exists
     ((not (file-exists-p (expand-file-name "claude_agent/__init__.py" agent-dir)))
      (format "Python module 'claude_agent' not found in: %s\n\nPlease ensure the claude_agent package is properly installed."
              agent-dir))
     ;; All checks passed
     (t nil))))

(defun claude-agent--start-process (work-dir buffer &optional resume-session continue-session model system-prompt additional-allowed-tools)
  "Start the Python agent process for WORK-DIR with BUFFER.
Optional RESUME-SESSION is a session ID to resume.
Optional CONTINUE-SESSION, if non-nil, continues the most recent session.
Optional MODEL is the model to use (e.g., 'sonnet', 'opus', 'haiku').
Optional SYSTEM-PROMPT is a custom system prompt (for oneshot agents).
Optional ADDITIONAL-ALLOWED-TOOLS is a list of extra tools to pre-authorize."
  ;; Validate prerequisites first
  (when-let ((error-msg (claude-agent--validate-prerequisites)))
    (error "Cannot start Claude agent:\n%s" error-msg))

  ;; Ensure Emacs server is running if MCP is enabled
  (when (and claude-agent-enable-mcp
             (not (bound-and-true-p server-process)))
    (message "Starting Emacs server for MCP...")
    (server-start))
  (let* ((agent-dir (claude-agent--get-agent-dir))
         (log-file (expand-file-name "claude-agent.log" work-dir))
         (buffer-name (buffer-name buffer))
         (mcp-config (when claude-agent-enable-mcp
                       (claude-agent--generate-mcp-config work-dir buffer-name)))
         (args (list "run" "--directory" agent-dir
                     "python" "-u" "-m" "claude_agent"  ; -u for unbuffered
                     "--work-dir" work-dir
                     "--log-file" log-file)))
    ;; Add MCP config if enabled
    (when mcp-config
      (setq args (append args (list "--mcp-config" mcp-config)))
      ;; Store MCP config path for cleanup
      (with-current-buffer buffer
        (setq claude-agent--mcp-config-file mcp-config)))
    ;; Add resume or continue flags
    (when resume-session
      (setq args (append args (list "--resume" resume-session))))
    (when continue-session
      (setq args (append args (list "--continue"))))
    ;; Add model if specified
    (when model
      (setq args (append args (list "--model" model))))
    ;; Append extra system prompt if set (e.g., from .dir-locals.el)
    (when claude-agent-extra-system-prompt
      (setq system-prompt
            (if system-prompt
                (concat system-prompt "\n\n" claude-agent-extra-system-prompt)
              claude-agent-extra-system-prompt)))
    ;; Add system prompt if specified (for oneshot agents or extra system prompt)
    ;; Write to temp file to avoid shell escaping issues with multiline prompts
    (when system-prompt
      (let ((prompt-file (make-temp-file "claude-system-prompt-" nil ".txt")))
        (with-temp-file prompt-file
          (insert system-prompt))
        (setq args (append args (list "--system-prompt-file" prompt-file)))
        ;; Store for cleanup
        (with-current-buffer buffer
          (setq-local claude-agent--system-prompt-file prompt-file))))
    ;; Add safe MCP tools as --allowedTools (pre-authorized, no permission prompts)
    ;; Also include any additional allowed tools passed by caller
    (let ((all-allowed-tools
           (append (when claude-agent-enable-mcp
                     (claude-mcp-get-safe-tools-for-cli))
                   additional-allowed-tools)))
      (when all-allowed-tools
        (setq args (append args (list "--allowed-tools"
                                      (mapconcat #'identity all-allowed-tools ","))))))
    ;; Write auto-reject config if rules exist (base + extra from .dir-locals.el)
    (let ((effective-reject-rules (claude-agent--effective-auto-reject-rules)))
      (when effective-reject-rules
        (let ((reject-file (make-temp-file "claude-auto-reject-" nil ".json"))
              (rules (mapcar (lambda (rule)
                               (let (alist)
                                 (when (plist-get rule :path-prefix)
                                   (push (cons "path_prefix" (plist-get rule :path-prefix)) alist))
                                 (when (plist-get rule :pattern)
                                   (push (cons "pattern" (plist-get rule :pattern)) alist))
                                 (when (plist-get rule :message)
                                   (push (cons "message" (plist-get rule :message)) alist))
                                 alist))
                             effective-reject-rules)))
          (with-temp-file reject-file
            (insert (json-encode rules)))
          (setq args (append args (list "--auto-reject-config" reject-file))))))
    ;; Pass max-retries for transient API error handling
    (setq args (append args (list "--max-retries"
                                  (number-to-string claude-max-retries))))
    ;; System message hooks are now evaluated on the Emacs side
    ;; (see claude-agent--dispatch-user-message)
    ;; Use pipe (nil) instead of PTY to avoid focus-related buffering issues
    ;; Bind default-directory so the process starts in work-dir
    (let ((default-directory work-dir)
          (process-connection-type nil)
          (process-environment (cons "PYTHONUNBUFFERED=1" process-environment)))
      (condition-case err
          (let ((proc (apply #'start-process
                             "claude-agent"
                             buffer
                             claude-agent-python-command
                             args)))
            (set-process-coding-system proc 'utf-8 'utf-8)
            (set-process-filter proc #'claude-agent--process-filter)
            (set-process-sentinel proc #'claude-agent--process-sentinel)
            (set-process-query-on-exit-flag proc nil)
            proc)
        (error
         (error "Failed to start Claude agent process:\n\nCommand: %s %s\n\nError: %s\n\nPlease check that:\n- %s is installed and in your PATH\n- The agent directory exists: %s\n- Python is available"
                claude-agent-python-command
                (mapconcat #'identity args " ")
                (error-message-string err)
                claude-agent-python-command
                agent-dir))))))

(defun claude-agent--process-sentinel (proc event)
  "Handle process PROC state change EVENT."
  (when (memq (process-status proc) '(exit signal))
    (let ((buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (claude-agent--set-thinking nil)
          (claude-agent--append-to-log
           (format "\n[Process %s]\n" (string-trim event))
           'claude-agent-session-face)
          ;; Clean up MCP config file
          (when (and claude-agent--mcp-config-file
                     (file-exists-p claude-agent--mcp-config-file))
            (delete-file claude-agent--mcp-config-file)
            (setq claude-agent--mcp-config-file nil)))))))

;;;; User commands

(defun claude-agent--is-busy-p ()
  "Return t if the agent is currently busy (thinking)."
  claude-agent--thinking-status)

(defun claude-agent--render-queue ()
  "Render queued messages by rebuilding the dynamic section."
  (claude-agent--render-dynamic-section))

(defun claude-agent--send-next-queued ()
  "Send all queued messages at once when the agent becomes ready.
Concatenates all queued messages (FIFO order) into a single message
separated by blank lines, then dispatches it as one user message.
This avoids the problem of each queued message being interrupted
individually."
  (when (and claude-agent--message-queue
             (not (claude-agent--is-busy-p))
             claude-agent--process
             (process-live-p claude-agent--process))
    (let ((combined (mapconcat #'identity claude-agent--message-queue "\n\n")))
      (setq claude-agent--message-queue nil)
      (claude-agent--dispatch-user-message combined))))

(defun claude-agent--queue-index-at-point ()
  "Return the queue index of the queued message at point, or nil."
  (get-text-property (point) 'claude-queue-index))

(defun claude-agent--queue-region-bounds (pos)
  "Return (BEG . END) of the queue item region at POS, or nil.
Finds the contiguous region sharing the same `claude-queue-index' property."
  (when-let ((idx (get-text-property pos 'claude-queue-index)))
    (let ((beg pos) (end pos))
      ;; Walk backward to find start of this queue item
      (while (and (> beg (point-min))
                  (eql (get-text-property (1- beg) 'claude-queue-index) idx))
        (setq beg (1- beg)))
      ;; Walk forward to find end of this queue item
      (while (and (< end (point-max))
                  (eql (get-text-property end 'claude-queue-index) idx))
        (setq end (1+ end)))
      (cons beg end))))

(defun claude-agent--update-queue-highlight ()
  "Update the queue highlight overlay based on cursor position.
Called from `post-command-hook'.  When point is on a queued message,
highlights the entire message region; otherwise removes the highlight."
  (when (eq major-mode 'claude-agent-mode)
    (condition-case nil
        (let ((bounds (claude-agent--queue-region-bounds (point))))
          (if bounds
              (if claude-agent--queue-highlight-overlay
                  ;; Move existing overlay
                  (move-overlay claude-agent--queue-highlight-overlay
                                (car bounds) (cdr bounds))
                ;; Create new overlay
                (setq claude-agent--queue-highlight-overlay
                      (make-overlay (car bounds) (cdr bounds)))
                (overlay-put claude-agent--queue-highlight-overlay
                             'face 'claude-agent-queue-highlight-face)
                (overlay-put claude-agent--queue-highlight-overlay
                             'evaporate t))
            ;; No queue item at point — remove overlay
            (when claude-agent--queue-highlight-overlay
              (delete-overlay claude-agent--queue-highlight-overlay)
              (setq claude-agent--queue-highlight-overlay nil))))
      ;; Silently ignore errors to prevent buffer corruption
      (error
       (when claude-agent--queue-highlight-overlay
         (delete-overlay claude-agent--queue-highlight-overlay)
         (setq claude-agent--queue-highlight-overlay nil))))))

(defun claude-agent-queue-delete ()
  "Delete the queued message at point (C-c C-d).
Move cursor to a queued message in the dynamic section to delete it."
  (interactive)
  ;; Capture the queue index at point BEFORE switching to base buffer,
  ;; since `with-current-buffer' changes point to the base buffer's point.
  (let ((index (claude-agent--queue-index-at-point)))
    (claude-agent--in-base-buffer
     (if (null index)
         (message "No queued message at point (move cursor to a queued message)")
       (let* ((msg (nth index claude-agent--message-queue))
              (preview (truncate-string-to-width
                        (replace-regexp-in-string "\n" " " msg) 50)))
         (setq claude-agent--message-queue
               (append (seq-take claude-agent--message-queue index)
                       (seq-drop claude-agent--message-queue (1+ index))))
         (claude-agent--render-dynamic-section)
         (message "Deleted: \"%s\"" preview))))))

(defun claude-agent-send ()
  "Send the content of the input buffer to Claude, or queue if busy.
Called from the input buffer via `C-c C-c'."
  (interactive)
  (let* ((input-buf (if (eq major-mode 'claude-agent-input-mode)
                        (current-buffer)
                      ;; Called from REPL buffer — find the input buffer
                      (claude-agent--in-base-buffer claude-agent--input-buffer)))
         (parent-buf (when input-buf
                       (buffer-local-value 'claude-agent--input-parent-buffer input-buf))))
    (when (and input-buf parent-buf (buffer-live-p parent-buf))
      (let ((input (string-trim
                    (with-current-buffer input-buf
                      (buffer-substring-no-properties (point-min) (point-max))))))
        (unless (string-empty-p input)
          ;; Close the input window and kill the buffer
          (with-current-buffer parent-buf
            (claude-agent--close-input-window))
          ;; Process the input in the REPL buffer
          (with-current-buffer parent-buf
            ;; Add to history
            (push input claude-agent--input-history)
            (setq claude-agent--input-history-index 0)
            ;; If busy, queue the message; otherwise send directly
            (if (claude-agent--is-busy-p)
                (progn
                  (setq claude-agent--message-queue
                        (append claude-agent--message-queue (list input)))
                  (claude-agent--render-dynamic-section)
                  (message "Message queued (agent is busy)"))
              ;; Dispatch with hook evaluation
              (claude-agent--dispatch-user-message input)
              ;; Re-render dynamic section
              (claude-agent--render-dynamic-section))))))))

(defun claude-agent-send-or-open-input ()
  "Open the input window, or send if already in input buffer.
When called from the REPL buffer, opens the input window.
When called from the input buffer, sends the message."
  (interactive)
  (if (eq major-mode 'claude-agent-input-mode)
      (claude-agent-send)
    (claude-agent--in-base-buffer
     (claude-agent--show-input-window))))

(defun claude-agent-interrupt ()
  "Interrupt the current Claude operation."
  (interactive)
  (claude-agent--in-base-buffer
   (when (and claude-agent--process
              (process-live-p claude-agent--process))
     (process-send-string claude-agent--process
                          (concat (json-encode '((type . "interrupt"))) "\n")))))

(defun claude-agent-quit ()
  "Quit the Claude session."
  (interactive)
  (when (yes-or-no-p "Quit Claude session? ")
    (claude-agent--in-base-buffer
     ;; Clean up input window/buffer
     (claude-agent--close-input-window)
     (when (and claude-agent--process
                (process-live-p claude-agent--process))
       (process-send-string claude-agent--process
                            (concat (json-encode '((type . "quit"))) "\n"))))))

(defun claude-agent--send-json (msg)
  "Send MSG as JSON to the agent process.
MSG should be an alist that will be encoded as JSON."
  (claude-agent--in-base-buffer
   (when (and claude-agent--process
              (process-live-p claude-agent--process))
     (process-send-string claude-agent--process
                          (concat (json-encode msg) "\n")))))

(defun claude-agent-previous-input ()
  "Recall previous input from history in the input buffer."
  (interactive)
  (let ((parent (if (eq major-mode 'claude-agent-input-mode)
                    claude-agent--input-parent-buffer
                  (current-buffer))))
    (when (and parent (buffer-live-p parent))
      (with-current-buffer parent
        (when (and claude-agent--input-history
                   (< claude-agent--input-history-index
                      (length claude-agent--input-history)))
          (let ((input-buf (claude-agent--get-or-create-input-buffer)))
            (with-current-buffer input-buf
              (erase-buffer)
              (insert (nth claude-agent--input-history-index
                           claude-agent--input-history))
              (claude-agent--input-update-placeholder))
            (with-current-buffer parent
              (cl-incf claude-agent--input-history-index))))))))

(defun claude-agent-next-input ()
  "Recall next input from history in the input buffer."
  (interactive)
  (let ((parent (if (eq major-mode 'claude-agent-input-mode)
                    claude-agent--input-parent-buffer
                  (current-buffer))))
    (when (and parent (buffer-live-p parent))
      (with-current-buffer parent
        (when (> claude-agent--input-history-index 0)
          (cl-decf claude-agent--input-history-index)
          (let ((input-buf (claude-agent--get-or-create-input-buffer)))
            (with-current-buffer input-buf
              (erase-buffer)
              (when (> claude-agent--input-history-index 0)
                (insert (nth (1- claude-agent--input-history-index)
                             claude-agent--input-history)))
              (claude-agent--input-update-placeholder))))))))

;;;; MCP status functions

(defun claude-agent-mcp-server-status ()
  "Return list of MCP server statuses for current session.
Each element is an alist with keys: name, status."
  (claude-agent--in-base-buffer
   claude-agent--mcp-server-status))

;;;###autoload
(defun claude-agent-show-mcp-status ()
  "Display MCP server connection status for current Claude session."
  (interactive)
  (claude-agent--in-base-buffer
   (let ((status claude-agent--mcp-server-status))
     (if status
         (let ((msg (mapconcat
                     (lambda (s)
                       (let ((name (cdr (assq 'name s)))
                             (st (cdr (assq 'status s))))
                         (format "%s: %s" name
                                 (if (equal st "connected")
                                     (propertize st 'face 'success)
                                   (propertize st 'face 'error)))))
                     status "\n")))
           (message "MCP Servers:\n%s" msg))
       (message "No MCP status available (send a message first to initialize)")))))

;;;; Session history loading

(defun claude-agent--get-session-file (work-dir session-id)
  "Get the session file path for SESSION-ID in WORK-DIR."
  (let* ((encoded-dir (replace-regexp-in-string
                       "/" "-"
                       (directory-file-name (expand-file-name work-dir))))
         (sessions-dir (expand-file-name encoded-dir "~/.claude/projects/")))
    (expand-file-name (concat session-id ".jsonl") sessions-dir)))

(defun claude-agent--load-session-history (work-dir session-id &optional max-messages)
  "Load conversation history from SESSION-ID in WORK-DIR.
Returns a list of message plists with :role, :content, :timestamp.
Loads at most MAX-MESSAGES (default 50) most recent messages."
  (let* ((file (claude-agent--get-session-file work-dir session-id))
         (max-msgs (or max-messages 50))
         (messages nil))
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (json (condition-case nil
                           (json-read-from-string line)
                         (error nil))))
            (when json
              (let ((type (cdr (assq 'type json)))
                    (msg (cdr (assq 'message json)))
                    (ts (cdr (assq 'timestamp json))))
                (when (and msg (member type '("user" "assistant")))
                  (let* ((role (cdr (assq 'role msg)))
                         (content (cdr (assq 'content msg)))
                         ;; Handle content that's either a string or array
                         (text-content
                          (cond
                           ((stringp content) content)
                           ((vectorp content)
                            ;; Extract text from content array
                            (mapconcat
                             (lambda (item)
                               (when (equal (cdr (assq 'type item)) "text")
                                 (cdr (assq 'text item))))
                             content ""))
                           (t nil))))
                    (when (and role text-content (not (string-empty-p text-content)))
                      (push (list :role role
                                  :content text-content
                                  :timestamp ts)
                            messages)))))))
          (forward-line 1)))
      ;; Return most recent messages, in chronological order
      (let ((recent (seq-take (nreverse messages) max-msgs)))
        (nreverse recent)))))

(defun claude-agent--format-history-message (msg)
  "Format a history message MSG for display in the buffer."
  (let* ((role (plist-get msg :role))
         (content (plist-get msg :content))
         (timestamp (plist-get msg :timestamp))
         ;; Truncate very long messages for history display
         (max-len 500)
         (truncated (if (> (length content) max-len)
                        (concat (substring content 0 max-len) "\n[...truncated...]")
                      content)))
    (concat
     (if (equal role "user")
         (propertize "┌─ You" 'face 'claude-agent-user-header-face)
       (propertize "┌─ Claude" 'face 'claude-agent-assistant-header-face))
     (when timestamp
       (propertize (format " (%s)" (format-time-string "%m-%d %H:%M" (date-to-time timestamp)))
                   'face 'claude-agent-session-face))
     "\n"
     (propertize truncated
                 'face (if (equal role "user")
                          'claude-agent-user-face
                        'claude-agent-assistant-face))
     "\n\n")))

(defun claude-agent--insert-history-header ()
  "Insert the history section header."
  (let ((start (point)))
    (insert "─── Previous Conversation History ───────────────────────────\n\n")
    (claude-agent--apply-face start (point) 'claude-agent-session-face)))

(defun claude-agent--insert-history-footer ()
  "Insert the history section footer."
  (let ((start (point)))
    (insert "─── Resuming Session ────────────────────────────────────────\n\n")
    (claude-agent--apply-face start (point) 'claude-agent-session-face)))

(defun claude-agent--display-session-history (work-dir session-id)
  "Display conversation history from SESSION-ID in the current buffer."
  (let ((history (claude-agent--load-session-history work-dir session-id 20)))
    (when history
      (claude-agent--insert-history-header)
      (dolist (msg history)
        (let ((formatted (claude-agent--format-history-message msg)))
          (claude-agent--append-to-log formatted nil nil)))
      (claude-agent--insert-history-footer))))

;;;; Entry point

;;;###autoload
(defun claude-agent-run (work-dir &optional resume-session continue-session slug model additional-allowed-tools)
  "Start a Claude agent session for WORK-DIR.
Optional RESUME-SESSION is a session ID to resume.
Optional CONTINUE-SESSION, if non-nil, continues the most recent session.
Optional SLUG is a suffix for the buffer name (e.g., *claude:project:slug*).
Optional MODEL is the model to use (e.g., 'sonnet', 'opus', 'haiku').
Optional ADDITIONAL-ALLOWED-TOOLS is a list of extra tools to pre-authorize."
  (interactive
   (list (read-directory-name "Project directory: "
                              (or (vc-git-root default-directory)
                                  default-directory))))
  (let* ((expanded-dir (expand-file-name work-dir))
         (short-name (file-name-nondirectory
                      (directory-file-name expanded-dir)))
         (buf-name (if slug
                       (format "*claude:%s:%s*" short-name slug)
                     (format "*claude:%s*" short-name)))
         (buf (get-buffer-create buf-name)))

    ;; Set up buffer
    (with-current-buffer buf
      (claude-agent-mode)
      (claude-agent--init-buffer short-name)
      (setq claude-agent--parse-state nil
            claude-agent--pending-output ""
            claude-agent--session-info nil
            claude-agent--has-conversation nil
            claude-agent--work-dir expanded-dir
            claude-agent--message-count 0
            claude-agent--is-resumed (or resume-session continue-session)
            default-directory expanded-dir)

      ;; Apply .dir-locals.el from the work directory
      ;; This activates worktree-specific settings like auto-reject-rules
      ;; and extra-system-prompt before the process is started.
      (hack-dir-local-variables-non-file-buffer)

      ;; Display history if resuming a specific session
      (when resume-session
        (claude-agent--display-session-history expanded-dir resume-session)))

    ;; Start process with optional resume/continue/model/allowed-tools
    (let ((proc (claude-agent--start-process expanded-dir buf resume-session continue-session model nil additional-allowed-tools)))
      (with-current-buffer buf
        (setq claude-agent--process proc)))

    ;; Display buffer
    (pop-to-buffer buf)
    ;; Ensure default-directory is set correctly (defensive - should already be set)
    (with-current-buffer buf
      (setq default-directory expanded-dir))
    buf))

;;;; Transient Menu

(defvar claude-agent--fallback-models
  '(("default" . "Default (recommended)")
    ("sonnet" . "Sonnet")
    ("haiku" . "Haiku"))
  "Fallback model choices used before the SDK provides the dynamic list.")

(defun claude-agent--current-model ()
  "Get the current model from session info."
  (plist-get claude-agent--session-info :model))

(defun claude-agent--format-model-for-display (model-string)
  "Format MODEL-STRING for display.
Uses the dynamic model list if available, otherwise extracts key info."
  (if-let ((models claude-agent--available-models)
           (match (seq-find (lambda (m)
                              (equal (cdr (assq 'value m)) model-string))
                            models)))
      (cdr (assq 'displayName match))
    ;; Fallback: extract family name from model string
    (cond
     ((string-match "sonnet" model-string) "Sonnet")
     ((string-match "opus" model-string) "Opus")
     ((string-match "haiku" model-string) "Haiku")
     (t model-string))))

(defun claude-agent--model-candidates ()
  "Return model candidates for completion.
Uses the dynamic list from the SDK if available, otherwise falls back
to the hardcoded list.  Each candidate is a string with a text property
holding the model value to pass to the SDK."
  (if claude-agent--available-models
      (mapcar (lambda (m)
                (let* ((value (cdr (assq 'value m)))
                       (display-name (cdr (assq 'displayName m)))
                       (description (cdr (assq 'description m)))
                       (label (format "%s  (%s)" display-name description)))
                  (propertize label 'model-value value)))
              claude-agent--available-models)
    ;; Fallback before SDK info arrives
    (mapcar (lambda (pair)
              (propertize (cdr pair) 'model-value (car pair)))
            claude-agent--fallback-models)))

(defun claude-agent-set-model (model)
  "Change the model for the current session to MODEL.
This restarts the session with the new model while preserving the conversation."
  (interactive
   (let* ((candidates (claude-agent--model-candidates))
          (choice (completing-read "Model: " candidates nil t)))
     (list (get-text-property 0 'model-value choice))))
  (if (and claude-agent--process (process-live-p claude-agent--process))
      (let ((session-id (plist-get claude-agent--session-info :session-id))
            (work-dir claude-agent--work-dir))
        (if session-id
            (progn
              ;; Kill current process
              (delete-process claude-agent--process)
              (setq claude-agent--process nil)
              ;; Clear thinking state
              (claude-agent--set-thinking nil)
              ;; Notify user
              (claude-agent--append-to-log
               (format "\n🔄 Switching to %s model...\n" model)
               'claude-agent-session-face)
              ;; Start new process with same session ID but new model
              (let ((proc (claude-agent--start-process
                           work-dir (current-buffer) session-id nil model)))
                (setq claude-agent--process proc))
              (message "Restarting session with %s model..." model))
          (message "No session ID available - cannot switch model")))
    (message "No active Claude session")))

(defun claude-agent-mcp-list ()
  "List configured MCP servers."
  (interactive)
  (let ((output (shell-command-to-string "claude mcp list 2>/dev/null")))
    (if (string-match-p "No MCP servers" output)
        (message "No MCP servers configured")
      (with-current-buffer (get-buffer-create "*Claude MCP Servers*")
        (read-only-mode -1)
        (erase-buffer)
        (insert "MCP Servers\n")
        (insert "===========\n\n")
        (insert output)
        (read-only-mode 1)
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

(defun claude-agent-mcp-add ()
  "Add an MCP server interactively."
  (interactive)
  (let* ((name (read-string "Server name: "))
         (type (completing-read "Type: " '("stdio" "sse") nil t))
         (command-or-url (read-string (if (equal type "stdio")
                                          "Command: "
                                        "URL: "))))
    (if (equal type "stdio")
        (let ((args (read-string "Arguments (space-separated, optional): ")))
          (shell-command (format "claude mcp add %s %s %s"
                                 (shell-quote-argument name)
                                 (shell-quote-argument command-or-url)
                                 args)))
      (shell-command (format "claude mcp add --transport sse %s %s"
                             (shell-quote-argument name)
                             (shell-quote-argument command-or-url))))
    (message "Added MCP server: %s" name)))

(defun claude-agent-mcp-remove ()
  "Remove an MCP server."
  (interactive)
  (let* ((output (shell-command-to-string "claude mcp list --json 2>/dev/null"))
         (servers (ignore-errors (json-read-from-string output)))
         (names (mapcar (lambda (s) (cdr (assq 'name s))) servers)))
    (if names
        (let ((name (completing-read "Remove server: " names nil t)))
          (shell-command (format "claude mcp remove %s" (shell-quote-argument name)))
          (message "Removed MCP server: %s" name))
      (message "No MCP servers to remove"))))

(defun claude-agent-compact ()
  "Compact the conversation history.
Sends /compact as a message to Claude."
  (interactive)
  (if (and claude-agent--process (process-live-p claude-agent--process))
      (progn
        (claude-agent--send-json '((type . "message") (text . "/compact")))
        (message "Compacting conversation..."))
    (message "No active Claude session")))

(defun claude-agent-clear ()
  "Clear the conversation history and start fresh.
Sends /clear as a message to Claude."
  (interactive)
  (if (and claude-agent--process (process-live-p claude-agent--process))
      (when (yes-or-no-p "Clear conversation history? ")
        (claude-agent--send-json '((type . "message") (text . "/clear")))
        (message "Clearing conversation..."))
    (message "No active Claude session")))

(defun claude-agent-restart ()
  "Restart the Claude session, continuing the same conversation.
Kills the current process and starts a new one with --continue.
This reloads the MCP server and Python agent while preserving the session."
  (interactive)
  (claude-agent--in-base-buffer
   (unless claude-agent--work-dir
     (error "No work directory set for this session"))
   (let ((work-dir claude-agent--work-dir)
         (buf (current-buffer)))
     ;; Kill existing process
     (when (and claude-agent--process (process-live-p claude-agent--process))
       (delete-process claude-agent--process))
     ;; Clean up MCP config file if it exists
     (when (and claude-agent--mcp-config-file
                (file-exists-p claude-agent--mcp-config-file))
       (delete-file claude-agent--mcp-config-file))
     ;; Reset state but keep conversation markers
     (setq claude-agent--process nil
           claude-agent--mcp-config-file nil
           claude-agent--thinking-status nil
           claude-agent--progress-indicators nil)
     ;; Append restart message to log
     (claude-agent--append-to-log
      "\n⟳ Restarting session...\n"
      'claude-agent-session-face)
     ;; Start new process with --continue to resume the session
     (let ((proc (claude-agent--start-process work-dir buf nil t)))
       (setq claude-agent--process proc))
     (claude-agent--render-dynamic-section)
     ;; Send a message to the agent after a short delay to let it initialize
     (run-with-timer
      2 nil
      (lambda (buffer)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (and claude-agent--process
                       (process-live-p claude-agent--process))
              (process-send-string
               claude-agent--process
               (concat (json-encode
                        '((type . "message")
                          (text . "Session restarted. MCP server reloaded with any code changes. Please continue.")))
                       "\n"))))))
      buf)
     (message "Session restarted, MCP server reloaded."))))



(defun claude-agent-show-cost ()
  "Show token usage and cost for current session."
  (interactive)
  (let ((cost (plist-get claude-agent--session-info :cost))
        (input claude-agent--input-tokens)
        (output claude-agent--output-tokens))
    (message "Session cost: $%.4f | Last turn: %s in / %s out tokens"
             (or cost 0)
             (or input 0)
             (or output 0))))

;;;; Progress indicator management

(defun claude-agent-toggle-progress ()
  "Toggle visibility of progress indicators."
  (interactive)
  (setq claude-agent--progress-visible (not claude-agent--progress-visible))
  (claude-agent--render-dynamic-section)
  (message "Progress indicators %s" (if claude-agent--progress-visible "shown" "hidden")))

(defun claude-agent-toggle-todos ()
  "Toggle visibility of todo list."
  (interactive)
  (setq claude-agent--todos-visible (not claude-agent--todos-visible))
  (claude-agent--render-dynamic-section)
  (message "Todo list %s" (if claude-agent--todos-visible "shown" "hidden")))

(defun claude-agent-progress-start (label &optional id percent)
  "Start a progress indicator with LABEL at PERCENT (default 0).
Returns the progress ID. Optional ID allows specifying a custom identifier."
  (unless claude-agent--progress-indicators
    (setq claude-agent--progress-indicators (make-hash-table :test 'equal)))
  (let ((progress-id (or id (format "progress-%s" (format-time-string "%s%N"))))
        (pct (or percent 0)))
    (puthash progress-id
             (list :label label
                   :percent (if (numberp pct) pct (string-to-number pct))
                   :start-time (current-time))
             claude-agent--progress-indicators)
    (claude-agent--render-dynamic-section)
    progress-id))

(defun claude-agent-progress-update (id &optional label percent)
  "Update progress indicator ID.
LABEL updates the text label (nil keeps current).
PERCENT sets progress 0-100 (nil keeps current)."
  (when (and claude-agent--progress-indicators
             (gethash id claude-agent--progress-indicators))
    (let ((info (gethash id claude-agent--progress-indicators)))
      (when label
        (plist-put info :label label))
      (when percent
        (plist-put info :percent (if (numberp percent) percent (string-to-number percent))))
      (puthash id info claude-agent--progress-indicators))
    (claude-agent--render-dynamic-section))
  id)

(defun claude-agent-progress-stop (id &optional final-message)
  "Stop progress indicator ID.
Optional FINAL-MESSAGE is displayed briefly in the echo area."
  (when (and claude-agent--progress-indicators
             (gethash id claude-agent--progress-indicators))
    (remhash id claude-agent--progress-indicators)
    (claude-agent--render-dynamic-section)
    (when final-message
      (message "✓ %s" final-message)))
  "stopped")

(defun claude-agent--model-description ()
  "Return a description of the current model for transient."
  (let ((model (claude-agent--current-model)))
    (if model
        (format "Current: %s" (claude-agent--format-model-for-display model))
      "No model set")))

(defun claude-agent--session-description ()
  "Return session info description for transient."
  (let ((session-id (plist-get claude-agent--session-info :session-id)))
    (if session-id
        (format "Session: %s" (substring session-id 0 (min 8 (length session-id))))
      "No session")))

(provide 'claude-agent-repl)
;;; claude-agent-repl.el ends here
