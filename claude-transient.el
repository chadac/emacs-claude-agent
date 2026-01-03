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

(defun claude-transient-start-session ()
  "Start a new Claude session for the current project."
  (interactive)
  (require 'claude-agent)
  (let ((dir (or (when-let ((proj (project-current)))
                   (project-root proj))
                 default-directory)))
    (claude-agent-run dir)))

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
   ("r" "Switch session" claude-transient-switch-session)
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
