;;; claude-transient.el --- Extensible transient menus for Claude -*- lexical-binding: t; -*-

;; Author: Claude
;; Keywords: tools, ai
;; Package-Requires: ((emacs "28.1") (transient "0.4"))

;;; Commentary:

;; Provides an extensible transient menu system for Claude.
;;
;; Two main menus:
;; - `claude-agent-menu': For use inside Claude agent buffers
;; - `claude-pair-menu': For use in regular buffers (pair programming)
;;
;; Both are bound to C-c c but dispatch based on context.
;;
;; Extensions can register additional menu items using:
;; - `claude-transient-register-agent-item': Add to agent menu
;; - `claude-transient-register-pair-item': Add to pair programming menu
;;
;; Registration will error if a keybinding conflict exists.
;;
;; Extensions can be conditional (only shown when a predicate returns t).

;;; Code:

(require 'transient)

;;;; Extension Registry

(defvar claude-transient--agent-extensions nil
  "List of registered extensions for the agent transient menu.
Each entry is a plist with :key, :description, :command, :group, :if.")

(defvar claude-transient--pair-extensions nil
  "List of registered extensions for the pair programming transient menu.
Each entry is a plist with :key, :description, :command, :group, :if.")

(defvar claude-transient--builtin-agent-keys '("m" "$" "M l" "M s" "M a" "M r"
                                                "c" "C" "q" "k" "p" "t" "i" "RET" "g")
  "List of built-in keys in the agent menu.")

(defvar claude-transient--builtin-pair-keys '("x" "t" "d" "f" "c" "C" "s" "r" "l")
  "List of built-in keys in the pair menu.")

(defun claude-transient--key-exists-p (extensions key)
  "Check if KEY already exists in EXTENSIONS list."
  (cl-find key extensions :key (lambda (ext) (plist-get ext :key)) :test #'string=))

(defun claude-transient-register-agent-item (key description command &optional group if-pred)
  "Register a new item for the Claude agent transient menu.
KEY is the keybinding (e.g. \"s\").
DESCRIPTION is the menu item description.
COMMAND is the function to call.
GROUP is an optional group name (default \"Extensions\").
IF-PRED is an optional predicate function; item only shown if it returns non-nil.

Signals an error if KEY is already registered."
  (when (claude-transient--key-exists-p claude-transient--agent-extensions key)
    (error "Key '%s' is already registered in agent transient menu" key))
  (when (member key claude-transient--builtin-agent-keys)
    (error "Key '%s' conflicts with built-in agent menu binding" key))
  (push (list :key key
              :description description
              :command command
              :group (or group "Extensions")
              :if if-pred)
        claude-transient--agent-extensions))

(defun claude-transient-register-pair-item (key description command &optional group if-pred)
  "Register a new item for the Claude pair programming transient menu.
KEY is the keybinding (e.g. \"s\").
DESCRIPTION is the menu item description.
COMMAND is the function to call.
GROUP is an optional group name (default \"Extensions\").
IF-PRED is an optional predicate function; item only shown if it returns non-nil.

Signals an error if KEY is already registered."
  (when (claude-transient--key-exists-p claude-transient--pair-extensions key)
    (error "Key '%s' is already registered in pair transient menu" key))
  (when (member key claude-transient--builtin-pair-keys)
    (error "Key '%s' conflicts with built-in pair menu binding" key))
  (push (list :key key
              :description description
              :command command
              :group (or group "Extensions")
              :if if-pred)
        claude-transient--pair-extensions))

(defun claude-transient-unregister-agent-item (key)
  "Unregister the agent menu item with KEY."
  (setq claude-transient--agent-extensions
        (cl-remove key claude-transient--agent-extensions
                   :key (lambda (ext) (plist-get ext :key))
                   :test #'string=)))

(defun claude-transient-unregister-pair-item (key)
  "Unregister the pair menu item with KEY."
  (setq claude-transient--pair-extensions
        (cl-remove key claude-transient--pair-extensions
                   :key (lambda (ext) (plist-get ext :key))
                   :test #'string=)))

;;;; Forward Declarations

(declare-function claude-agent-set-model "claude-agent")
(declare-function claude-agent-show-cost "claude-agent")
(declare-function claude-agent-mcp-list "claude-agent")
(declare-function claude-agent-show-mcp-status "claude-agent")
(declare-function claude-agent-mcp-add "claude-agent")
(declare-function claude-agent-mcp-remove "claude-agent")
(declare-function claude-agent-compact "claude-agent")
(declare-function claude-agent-clear "claude-agent")
(declare-function claude-agent-quit "claude-agent")
(declare-function claude-agent-interrupt "claude-agent")
(declare-function claude-agent-toggle-progress "claude-agent")
(declare-function claude-agent-toggle-todos "claude-agent")
(declare-function claude-agent-goto-input "claude-agent")
(declare-function claude-agent--session-description "claude-agent")
(declare-function claude-agent--format-model-for-display "claude-agent")
(declare-function claude-agent--current-model "claude-agent")
(declare-function claude-mcp-magit-commit-approve "claude-mcp")

(declare-function claude-pair-point-action "claude-pair")
(declare-function claude-pair-point-action-test "claude-pair")
(declare-function claude-pair-point-action-doc "claude-pair")
(declare-function claude-pair-point-action-fix "claude-pair")
(declare-function claude-pair-send-comments "claude-pair")

;;;; Agent Transient Menu

(defvar claude-agent--progress-visible)
(defvar claude-agent--todos-visible)
(defvar claude-mcp-magit--pending-commit)

;;;###autoload (autoload 'claude-agent-menu "claude-transient" nil t)
(transient-define-prefix claude-agent-menu ()
  "Claude Agent command menu.

In the log area, single-key bindings are active (like magit).
In the input area, keys insert text normally.
Press 'i' or RET in the log area to jump to input."
  [:description
   (lambda () (concat "Claude Agent  "
                      (propertize (claude-agent--session-description) 'face 'transient-value)))
   ""]
  [["Model"
    ("m" "Change model" claude-agent-set-model
     :description (lambda () (concat "Model  " (propertize (or (claude-agent--format-model-for-display
                                                                 (or (claude-agent--current-model) ""))
                                                               "none")
                                                           'face 'transient-value))))
    ("$" "Show cost/tokens" claude-agent-show-cost)]
   ["MCP Servers  (M prefix)"
    ("M l" "List servers" claude-agent-mcp-list)
    ("M s" "Show status" claude-agent-show-mcp-status)
    ("M a" "Add server" claude-agent-mcp-add)
    ("M r" "Remove server" claude-agent-mcp-remove)]
   ["Session"
    ("c" "Compact history" claude-agent-compact)
    ("C" "Clear history" claude-agent-clear)
    ("q" "Quit session" claude-agent-quit)
    ("k" "Interrupt" claude-agent-interrupt)]
   ["View"
    ("p" "Toggle progress" claude-agent-toggle-progress
     :description (lambda () (concat "Progress "
                                     (propertize (if (bound-and-true-p claude-agent--progress-visible) "visible" "hidden")
                                                 'face 'transient-value))))
    ("t" "Toggle todos" claude-agent-toggle-todos
     :description (lambda () (concat "Todos "
                                     (propertize (if (bound-and-true-p claude-agent--todos-visible) "visible" "hidden")
                                                 'face 'transient-value))))]
   ["Navigation"
    ("i" "Go to input" claude-agent-goto-input)
    ("RET" "Go to input" claude-agent-goto-input)]
   ["Git"
    ("g" "Approve commit" claude-mcp-magit-commit-approve
     :if (lambda () (bound-and-true-p claude-mcp-magit--pending-commit)))]])

;;;; Pair Programming Transient Menu

;; Forward declarations for session commands
(declare-function claude-agent-run "claude-agent")
(declare-function claude-list-sessions "claude-sessions")

(defun claude-transient--session-exists-for-dir (dir)
  "Check if a Claude session already exists for DIR.
Returns the buffer if found, nil otherwise."
  (let ((expanded-dir (expand-file-name dir)))
    (cl-find-if
     (lambda (buf)
       (with-current-buffer buf
         (and (boundp 'claude-agent--process)
              claude-agent--process
              (process-live-p claude-agent--process)
              (boundp 'claude-agent--work-dir)
              claude-agent--work-dir
              (string= (expand-file-name claude-agent--work-dir)
                       expanded-dir))))
     (buffer-list))))

(defun claude-transient--read-slug ()
  "Read a slug from the user, allowing only lowercase letters and dashes."
  (let ((slug (read-string "Session name (lowercase, dashes only): ")))
    (if (string-match-p "^[a-z][a-z-]*$" slug)
        slug
      (message "Invalid slug: use only lowercase letters and dashes, starting with a letter")
      (sit-for 1)
      (claude-transient--read-slug))))

(defun claude-transient-start-session ()
  "Start a new Claude session for the current project.
If a session already exists for this project, always prompts for a slug
to create a named session (e.g., *claude:project:my-slug*)."
  (interactive)
  (require 'claude-agent)
  (let* ((dir (or (when-let ((proj (project-current)))
                    (project-root proj))
                  default-directory))
         (existing (claude-transient--session-exists-for-dir dir)))
    (if existing
        (let ((slug (claude-transient--read-slug)))
          (claude-agent-run dir nil nil slug))
      ;; No existing session - ask if they want a named session or default
      (if (y-or-n-p "Create named session? ")
          (let ((slug (claude-transient--read-slug)))
            (claude-agent-run dir nil nil slug))
        (claude-agent-run dir)))))

(defun claude-transient-switch-session ()
  "Switch to an existing Claude session or list all sessions."
  (interactive)
  (require 'claude-sessions)
  (let ((buffers (cl-remove-if-not
                  (lambda (buf)
                    (with-current-buffer buf
                      (and (boundp 'claude-agent--process)
                           claude-agent--process
                           (process-live-p claude-agent--process))))
                  (buffer-list))))
    (if (null buffers)
        (message "No active Claude sessions")
      (if (= (length buffers) 1)
          (pop-to-buffer (car buffers))
        (let ((choice (completing-read "Switch to session: "
                                       (mapcar #'buffer-name buffers)
                                       nil t)))
          (pop-to-buffer choice))))))

;;;; Resume Previous Sessions

(defun claude-transient--get-project-sessions-dir ()
  "Get the Claude projects directory for the current project.
Returns nil if not in a project or directory doesn't exist."
  (when-let* ((dir (or (when-let ((proj (project-current)))
                         (project-root proj))
                       default-directory))
              (expanded (expand-file-name dir))
              ;; Convert path to Claude's format:
              ;; /home/foo/.bar/baz -> -home-foo--bar-baz
              ;; 1. Remove trailing slash
              ;; 2. Replace . with - (so /. becomes --)
              ;; 3. Replace / with -
              (no-trailing (directory-file-name expanded))
              (dots-to-dash (replace-regexp-in-string "\\." "-" no-trailing))
              (encoded (replace-regexp-in-string "/" "-" dots-to-dash))
              (sessions-dir (expand-file-name encoded "~/.claude/projects/")))
    (when (file-directory-p sessions-dir)
      sessions-dir)))

(defun claude-transient--parse-session-file (file)
  "Parse a session JSONL FILE and extract metadata.
Returns a plist with :id, :timestamp, :summary, :cwd, or nil if invalid."
  (condition-case nil
      (when (and (file-exists-p file)
                 (> (file-attribute-size (file-attributes file)) 0))
        (let* ((session-id (file-name-base file))
               (first-user-msg nil)
               (last-timestamp nil)
               (cwd nil))
          ;; Read first few lines to get metadata
          (with-temp-buffer
            (insert-file-contents file nil 0 50000) ; Read first 50KB
            (goto-char (point-min))
            (while (and (not (eobp)) (< (line-number-at-pos) 20))
              (let* ((line (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))
                     (json (condition-case nil
                               (json-read-from-string line)
                             (error nil))))
                (when json
                  (let ((type (cdr (assq 'type json)))
                        (msg (cdr (assq 'message json)))
                        (ts (cdr (assq 'timestamp json)))
                        (dir (cdr (assq 'cwd json))))
                    (when dir (setq cwd dir))
                    (when ts (setq last-timestamp ts))
                    ;; Get first user message as summary
                    (when (and (equal type "user")
                               (not first-user-msg)
                               msg)
                      (let ((content (cdr (assq 'content msg))))
                        (when (stringp content)
                          (setq first-user-msg
                                (truncate-string-to-width
                                 (replace-regexp-in-string "[\n\r]+" " " content)
                                 60 nil nil "..."))))))))
              (forward-line 1)))
          (when (or first-user-msg last-timestamp)
            (list :id session-id
                  :timestamp last-timestamp
                  :summary (or first-user-msg "(empty session)")
                  :cwd cwd
                  :file file))))
    (error nil)))

(defun claude-transient--format-session-for-display (session)
  "Format SESSION plist for display in completing-read."
  (let* ((id (plist-get session :id))
         (summary (plist-get session :summary))
         (timestamp (plist-get session :timestamp))
         (short-id (substring id 0 8))
         (time-str (if timestamp
                       (format-time-string "%Y-%m-%d %H:%M"
                                          (date-to-time timestamp))
                     "unknown")))
    (format "%s  %s  %s" short-id time-str summary)))

(defun claude-transient--session-has-content-p (session)
  "Return non-nil if SESSION has meaningful content.
Filters out warmup sessions and empty sessions."
  (let ((summary (plist-get session :summary)))
    (and summary
         (not (equal summary "(empty session)"))
         (not (string-match-p "\\`[Ww]armup" summary)))))

(defun claude-transient--get-previous-sessions ()
  "Get list of previous sessions for the current project.
Returns a list of (display-string . session-plist) pairs, sorted by date.
Filters out warmup and empty sessions."
  (when-let ((sessions-dir (claude-transient--get-project-sessions-dir)))
    (let* ((files (directory-files sessions-dir t "\\.jsonl$"))
           (sessions (delq nil (mapcar #'claude-transient--parse-session-file files)))
           ;; Filter out warmup and empty sessions
           (sessions (cl-remove-if-not #'claude-transient--session-has-content-p sessions)))
      ;; Sort by timestamp, most recent first
      (setq sessions (sort sessions
                           (lambda (a b)
                             (string> (or (plist-get a :timestamp) "")
                                      (or (plist-get b :timestamp) "")))))
      ;; Return as alist for completing-read
      (mapcar (lambda (s)
                (cons (claude-transient--format-session-for-display s) s))
              sessions))))

(defun claude-transient-resume-session ()
  "Resume a previous Claude session from disk.
Shows a dropdown of previous sessions with timestamps and summaries.
If a session already exists for this project, prompts for a slug."
  (interactive)
  (require 'claude-agent)
  (let* ((dir (or (when-let ((proj (project-current)))
                    (project-root proj))
                  default-directory))
         (sessions (claude-transient--get-previous-sessions)))
    (if (null sessions)
        (message "No previous sessions found for this project")
      (let* ((choice (completing-read "Resume session: "
                                      (mapcar #'car sessions)
                                      nil t))
             (session (cdr (assoc choice sessions)))
             (session-id (plist-get session :id))
             (existing (claude-transient--session-exists-for-dir dir))
             (slug (when existing (claude-transient--read-slug))))
        (when session-id
          (claude-agent-run dir session-id nil slug))))))

;;;###autoload (autoload 'claude-pair-menu "claude-transient" nil t)
(transient-define-prefix claude-pair-menu ()
  "Claude pair programming commands."
  ["Point Actions"
   ("x" "Action at point" claude-pair-point-action)
   ("t" "Write test" claude-pair-point-action-test)
   ("d" "Add documentation" claude-pair-point-action-doc)
   ("f" "Fix issue" claude-pair-point-action-fix)]
  ["Comments"
   ("c" "Send CLAUDE: comments" claude-pair-send-comments)
   ("C" "Send project comments" (lambda () (interactive) (claude-pair-send-comments t)))]
  ["Sessions"
   ("s" "Start session" claude-transient-start-session)
   ("r" "Resume previous" claude-transient-resume-session)
   ("w" "Switch active" claude-transient-switch-session)
   ("l" "List sessions" claude-list-sessions)])

;;;; Unified Dispatcher

(defun claude-transient--in-agent-buffer-p ()
  "Return non-nil if current buffer is a Claude agent buffer."
  (and (boundp 'claude-agent--process)
       claude-agent--process))

;;;###autoload
(defun claude-menu ()
  "Show the appropriate Claude transient menu based on context.
In a Claude agent buffer, shows the agent menu.
In other buffers, shows the pair programming menu."
  (interactive)
  (if (claude-transient--in-agent-buffer-p)
      (claude-agent-menu)
    (claude-pair-menu)))

(provide 'claude-transient)
;;; claude-transient.el ends here
