;;; claude-agent-permissions.el --- Permission policy system for Claude Agent -*- lexical-binding: t; -*-

;; This file is part of Claude Agent.
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This module provides a flexible permission policy system for Claude Agent.
;; It allows configuring how permission requests are handled:
;;
;; 1. :deny-all - Auto-deny all permission requests with a reason
;; 2. :allow-all - Auto-allow all requests (dangerous, requires explicit opt-in)
;; 3. :rules - Evaluate permission rules in order, first match wins
;; 4. nil - Fall through to interactive UI (default behavior)
;;
;; Rules can match on:
;; - Tool name (exact or regex)
;; - File paths (prefix or regex)
;; - Combinations with :and, :or, :not
;;
;; Actions:
;; - :allow - Auto-grant with optional scope (:once, :session, :always)
;; - :deny - Auto-deny with optional message
;; - :prompt - Fall through to interactive UI

;;; Code:

(require 'cl-lib)

;;;; Customization

(defgroup claude-agent-permissions nil
  "Permission policy system for Claude Agent."
  :group 'claude-agent)

(defcustom claude-agent-permission-rules nil
  "Global permission rules evaluated in order.
Each rule is a plist with:
  :match   - Matching criteria (see below)
  :action  - :allow, :deny, or :prompt
  :message - Optional message (for :deny) or log note
  :scope   - :once, :session, or :always (for :allow, default :once)

Matching criteria (:match) can be:
  (:tool TOOL-NAME)            - Match specific tool name exactly
  (:tool-regex REGEX)          - Match tool name by regex
  (:path-prefix PATH)          - Match file operations under PATH
  (:path-regex REGEX)          - Match file paths by regex
  (:and MATCHER MATCHER ...)   - All matchers must match
  (:or MATCHER MATCHER ...)    - Any matcher must match
  (:not MATCHER)               - Invert match

Example:
  \\='((:match (:tool \"Read\")
     :action :allow
     :scope :session)
    (:match (:and (:tool-regex \"Edit\\\\|Write\")
                  (:path-prefix \"/etc\"))
     :action :deny
     :message \"Cannot edit system files\")
    (:match (:tool \"Bash\")
     :action :prompt))"
  :type '(repeat (plist :key-type symbol :value-type sexp))
  :safe #'listp
  :group 'claude-agent-permissions)

;;;; Buffer-local variables

(defvar-local claude-agent-permission-policy nil
  "Buffer-local permission policy for this Claude session.
Can be:
  - nil: Use global rules, fall back to prompting (default)
  - :deny-all: Auto-deny all requests with optional reason
  - :allow-all: Auto-allow all requests (dangerous!)
  - :rules: Evaluate against buffer-local + global rules")

(defvar-local claude-agent-permission-deny-reason nil
  "Buffer-local reason for denial when policy is :deny-all.
If nil, uses a default message.")

(defvar-local claude-agent-permission-rules-local nil
  "Buffer-local permission rules, evaluated before global rules.
Same format as `claude-agent-permission-rules'.")

;;;; Path extraction

(defun claude-agent-permission--extract-path (tool-name tool-input)
  "Extract file path from TOOL-INPUT for TOOL-NAME.
Returns the path string or nil if the tool doesn't operate on files."
  (cond
   ;; Standard Claude tools with file_path
   ((member tool-name '("Read" "Write" "Edit" "NotebookEdit"))
    (cdr (assq 'file_path tool-input)))

   ;; Glob and Grep have optional path
   ((member tool-name '("Glob" "Grep"))
    (or (cdr (assq 'path tool-input))
        ;; Default to current directory if no path specified
        nil))

   ;; MCP emacs tools with file_path
   ((and (string-prefix-p "mcp__emacs__" tool-name)
         (assq 'file_path tool-input))
    (cdr (assq 'file_path tool-input)))

   ;; MCP emacs tools with buffer_name that might map to a file
   ((and (string-prefix-p "mcp__emacs__" tool-name)
         (assq 'buffer_name tool-input))
    ;; Buffer names like "foo.el" might map to files, but we can't be sure
    ;; Return nil to avoid false matches
    nil)

   ;; Bash commands - try to extract file paths from common patterns
   ((string= tool-name "Bash")
    (let ((command (cdr (assq 'command tool-input))))
      (when command
        (claude-agent-permission--extract-path-from-bash command))))

   ;; Task tool - check the prompt for file references
   ((string= tool-name "Task")
    nil)  ; Tasks are too complex to extract paths from

   ;; Default: no path
   (t nil)))

(defun claude-agent-permission--extract-path-from-bash (command)
  "Try to extract a file path from a COMMAND string.
Returns the first likely file path found, or nil."
  ;; Common patterns that operate on files
  (cond
   ;; cd /path/to/dir
   ((string-match "^cd\\s-+\\([^;&|]+\\)" command)
    (string-trim (match-string 1 command)))

   ;; cat/less/more/head/tail /path/to/file
   ((string-match "^\\(cat\\|less\\|more\\|head\\|tail\\|vim\\|nano\\|emacs\\)\\s-+\\([^|;&]+\\)" command)
    (string-trim (match-string 2 command)))

   ;; rm/mv/cp source paths
   ((string-match "^\\(rm\\|mv\\|cp\\)\\s-+\\(?:-[a-zA-Z]+\\s-+\\)*\\([^|;&]+\\)" command)
    (let ((args (string-trim (match-string 2 command))))
      ;; Return the first non-flag argument
      (car (seq-filter (lambda (s) (not (string-prefix-p "-" s)))
                       (split-string args)))))

   ;; mkdir/rmdir
   ((string-match "^\\(mkdir\\|rmdir\\)\\s-+\\(?:-[a-zA-Z]+\\s-+\\)*\\([^|;&]+\\)" command)
    (string-trim (match-string 2 command)))

   ;; git commands in specific directories
   ((string-match "git\\s-+-C\\s-+\\([^\\s]+\\)" command)
    (match-string 1 command))

   ;; No recognizable path pattern
   (t nil)))

;;;; Rule matching

(defun claude-agent-permission--match-rule (matcher tool-name tool-input)
  "Check if MATCHER matches TOOL-NAME and TOOL-INPUT.
Returns t if matched, nil otherwise."
  (condition-case err
      (pcase matcher
        ;; Exact tool name match
        (`(:tool ,name)
         (string= tool-name name))

        ;; Tool name regex match
        (`(:tool-regex ,regex)
         (string-match-p regex tool-name))

        ;; Path prefix match
        (`(:path-prefix ,prefix)
         (when-let ((path (claude-agent-permission--extract-path tool-name tool-input)))
           (string-prefix-p (expand-file-name prefix)
                            (expand-file-name path))))

        ;; Path regex match
        (`(:path-regex ,regex)
         (when-let ((path (claude-agent-permission--extract-path tool-name tool-input)))
           (string-match-p regex path)))

        ;; AND combinator - all must match
        (`(:and . ,matchers)
         (cl-every (lambda (m)
                     (claude-agent-permission--match-rule m tool-name tool-input))
                   matchers))

        ;; OR combinator - any must match
        (`(:or . ,matchers)
         (cl-some (lambda (m)
                    (claude-agent-permission--match-rule m tool-name tool-input))
                  matchers))

        ;; NOT combinator - invert match
        (`(:not ,m)
         (not (claude-agent-permission--match-rule m tool-name tool-input)))

        ;; Unknown matcher - log warning and don't match
        (_
         (message "Claude agent: Unknown permission matcher format: %S" matcher)
         nil))
    ;; Handle errors gracefully
    (error
     (message "Claude agent: Error evaluating permission matcher %S: %s"
              matcher (error-message-string err))
     nil)))

(defun claude-agent-permission--evaluate-rules (tool-name tool-input rules)
  "Evaluate RULES against TOOL-NAME and TOOL-INPUT.
Returns the action plist (:action ACTION :scope SCOPE :message MSG)
of the first matching rule, or nil if no rule matches."
  (catch 'found
    (dolist (rule rules)
      (let ((matcher (plist-get rule :match))
            (action (plist-get rule :action)))
        (when (and matcher action)
          (when (claude-agent-permission--match-rule matcher tool-name tool-input)
            (throw 'found
                   (list :action action
                         :scope (or (plist-get rule :scope) :once)
                         :message (plist-get rule :message)))))))
    nil))

;;;; Main permission handler

(defun claude-agent-permission-handle-request (tool-name tool-input)
  "Handle permission request for TOOL-NAME with TOOL-INPUT.
Evaluates the current buffer's permission policy and rules.

Returns one of:
  (:allow :scope SCOPE :pattern PATTERN) - Auto-allow with scope
  (:deny :message MESSAGE)               - Auto-deny with reason
  nil                                    - Show interactive prompt

This function should be called from `claude-agent--show-permission-prompt'
before showing the interactive UI."
  (let ((policy claude-agent-permission-policy))
    (pcase policy
      ;; Deny-all policy (for sandboxed/expert agents)
      (:deny-all
       (let ((reason (or claude-agent-permission-deny-reason
                         "This session auto-denies all permission requests")))
         (message "Claude agent: Auto-denied %s (policy: deny-all)" tool-name)
         (list :deny :message reason)))

      ;; Allow-all policy (dangerous, requires explicit opt-in)
      (:allow-all
       (message "Claude agent: Auto-allowed %s (policy: allow-all)" tool-name)
       (list :allow :scope :session :pattern (format "%s(*)" tool-name)))

      ;; Rules-based evaluation
      (:rules
       (let* ((all-rules (append claude-agent-permission-rules-local
                                 claude-agent-permission-rules))
              (result (claude-agent-permission--evaluate-rules
                       tool-name tool-input all-rules)))
         (when result
           (pcase (plist-get result :action)
             (:allow
              (let* ((scope (plist-get result :scope))
                     (pattern (claude-agent-permission--generate-pattern
                               tool-name tool-input scope)))
                (message "Claude agent: Auto-allowed %s (rule match, scope: %s)"
                         tool-name scope)
                (list :allow :scope scope :pattern pattern)))
             (:deny
              (let ((msg (or (plist-get result :message)
                             "Denied by permission rule")))
                (message "Claude agent: Auto-denied %s (rule match)" tool-name)
                (list :deny :message msg)))
             (:prompt
              ;; Explicit prompt action - fall through to UI
              nil)
             (_
              ;; Unknown action - fall through
              nil)))))

      ;; nil or unrecognized policy - check global rules then fall through
      (_
       (if claude-agent-permission-rules
           (let ((result (claude-agent-permission--evaluate-rules
                          tool-name tool-input claude-agent-permission-rules)))
             (if result
                 (pcase (plist-get result :action)
                   (:allow
                    (let* ((scope (plist-get result :scope))
                           (pattern (claude-agent-permission--generate-pattern
                                     tool-name tool-input scope)))
                      (message "Claude agent: Auto-allowed %s (global rule, scope: %s)"
                               tool-name scope)
                      (list :allow :scope scope :pattern pattern)))
                   (:deny
                    (let ((msg (or (plist-get result :message)
                                   "Denied by permission rule")))
                      (message "Claude agent: Auto-denied %s (global rule)" tool-name)
                      (list :deny :message msg)))
                   (:prompt nil)
                   (_ nil))
               ;; No matching rule - fall through to UI
               nil))
         ;; No global rules - fall through to UI
         nil)))))

(defun claude-agent-permission--generate-pattern (tool-name tool-input scope)
  "Generate permission pattern for TOOL-NAME with TOOL-INPUT at SCOPE level.
This mirrors the pattern generation in the existing permission system."
  (pcase scope
    (:once
     (pcase tool-name
       ("Read" (format "Read(%s)" (cdr (assq 'file_path tool-input))))
       ("Write" (format "Write(%s)" (cdr (assq 'file_path tool-input))))
       ("Edit" (format "Edit(%s)" (cdr (assq 'file_path tool-input))))
       ("Bash" (format "Bash(%s)" (cdr (assq 'command tool-input))))
       (_ (format "%s" tool-name))))
    (:session
     (pcase tool-name
       ("Read" (format "Read(%s)" (cdr (assq 'file_path tool-input))))
       ("Write" (format "Write(%s)" (cdr (assq 'file_path tool-input))))
       ("Edit" (format "Edit(%s)" (cdr (assq 'file_path tool-input))))
       ("Bash"
        (let* ((cmd (cdr (assq 'command tool-input)))
               (first-word (car (split-string cmd))))
          (format "Bash(%s:*)" first-word)))
       (_ (format "%s(*)" tool-name))))
    (:always
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
       (_ (format "%s(*)" tool-name))))
    ;; Default to once
    (_ (format "%s" tool-name))))

;;;; Convenience functions for setting policies

(defun claude-agent-permission-set-deny-all (&optional reason)
  "Set the current buffer's permission policy to deny-all.
Optional REASON is shown when denying requests."
  (setq-local claude-agent-permission-policy :deny-all)
  (when reason
    (setq-local claude-agent-permission-deny-reason reason))
  (message "Permission policy set to :deny-all"))

(defun claude-agent-permission-set-allow-all ()
  "Set the current buffer's permission policy to allow-all.
WARNING: This is dangerous and auto-allows all permission requests!"
  (when (yes-or-no-p "WARNING: :allow-all will auto-approve ALL permission requests. Continue? ")
    (setq-local claude-agent-permission-policy :allow-all)
    (message "Permission policy set to :allow-all (DANGEROUS!)")))

(defun claude-agent-permission-set-rules ()
  "Set the current buffer's permission policy to rules-based evaluation."
  (setq-local claude-agent-permission-policy :rules)
  (message "Permission policy set to :rules"))

(defun claude-agent-permission-clear-policy ()
  "Clear the current buffer's permission policy (use global rules)."
  (setq-local claude-agent-permission-policy nil)
  (setq-local claude-agent-permission-deny-reason nil)
  (setq-local claude-agent-permission-rules-local nil)
  (message "Permission policy cleared (using global rules)"))

;;;; Integration helpers

(defun claude-agent-permission-scope-to-action (scope)
  "Convert internal SCOPE symbol to API action string."
  (pcase scope
    (:once "allow_once")
    (:session "allow_session")
    (:always "allow_always")
    (_ "allow_once")))

(provide 'claude-agent-permissions)
;;; claude-agent-permissions.el ends here
