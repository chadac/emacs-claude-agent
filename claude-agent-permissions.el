;;; claude-agent-permissions.el --- Permission system for Claude Agent -*- lexical-binding: t; -*-

;; This file is part of Claude Agent.
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This module provides a unified permission system for Claude Agent.
;; All permission decisions are made by evaluating rules in order:
;;
;; 1. Buffer-local `claude-agent-permission-rules-local` (agent-type specific)
;; 2. Project `claude-agent-project-permission-rules` (from .claude/settings.local.el)
;; 3. Global `claude-agent-permission-rules` (user's init.el)
;; 4. Fall through to :prompt (if no rule matches)
;;
;; Rules can match on:
;; - t                    - Always matches (catch-all)
;; - Tool name (exact or regex)
;; - File paths (prefix or regex)
;; - Custom predicates
;; - Combinations with :and, :or, :not
;;
;; Actions:
;; - :allow - Auto-grant with optional scope (:once, :session, :always)
;; - :deny  - Auto-deny with optional reason
;; - :prompt - Fall through to interactive UI

;;; Code:

(require 'cl-lib)

;;;; Customization

(defgroup claude-agent-permissions nil
  "Permission system for Claude Agent."
  :group 'claude-agent)

(defcustom claude-agent-permission-rules nil
  "Global permission rules evaluated in order.
Each rule is a plist with:
  :match   - Matching criteria (see below)
  :action  - :allow, :deny, or :prompt
  :reason  - Optional message (for :deny)
  :scope   - :once, :session, or :always (for :allow, default :once)

Matching criteria (:match) can be:
  t                            - Always matches (catch-all)
  (:tool TOOL-NAME)            - Match specific tool name exactly
  (:tool-regex REGEX)          - Match tool name by regex
  (:path-prefix PATH)          - Match file operations under PATH
  (:path-regex REGEX)          - Match file paths by regex
  (:predicate FN)              - Call (FN TOOL-NAME TOOL-INPUT), match if truthy
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
     :reason \"Cannot edit system files\")
    (:match t :action :deny :reason \"Sandboxed session\"))"
  :type '(repeat (plist :key-type symbol :value-type sexp))
  :safe #'listp
  :group 'claude-agent-permissions)

;;;; Buffer-local variables

(defvar-local claude-agent-permission-rules-local nil
  "Buffer-local permission rules, evaluated FIRST before all other rules.
Same format as `claude-agent-permission-rules'.

Agent types set this to define their permission model:
- Oneshot: \\='((:match t :action :deny :reason \"...\"))
- Expert: \\='((:match (:tool-regex \"read.*\") :action :allow) ...)
- TODO auto-mode: \\='((:match (:predicate auto-mode-active-p) :action :deny))")

;;;; Project-level rules

(defvar-local claude-agent-project-permission-rules nil
  "Project-level permission rules from .claude/settings.local.el.
Evaluated AFTER buffer-local rules, BEFORE global rules.
Same format as `claude-agent-permission-rules'.")

;;;; SDK Tool Path Args Registry

(defvar claude-agent-sdk-tool-path-args
  '(("Read" . ("file_path"))
    ("Edit" . ("file_path"))
    ("Write" . ("file_path"))
    ("Glob" . ("path"))
    ("Grep" . ("path"))
    ("NotebookEdit" . ("notebook_path"))
    ("Bash" . nil))  ; Bash doesn't have path args (command is opaque)
  "Alist mapping SDK tool names to their path-typed argument names.
Used by :path-prefix matcher. Only needed for tools NOT defined via deftool.")

(defvar claude-agent-mcp-tool-path-args (make-hash-table :test 'equal)
  "Hash table mapping MCP tool names to their path-typed argument names.
Populated by `claude-mcp-deftool' when using path type for arguments.")

;;;; Path extraction

(defun claude-agent-permission--get-tool-path-args (tool-name)
  "Get list of path-typed argument names for TOOL-NAME.
Checks MCP tool registry first, then SDK tool registry."
  (or (gethash tool-name claude-agent-mcp-tool-path-args)
      (cdr (assoc tool-name claude-agent-sdk-tool-path-args))))

(defun claude-agent-permission--extract-paths (tool-name tool-input)
  "Extract file paths from TOOL-INPUT for TOOL-NAME.
Returns a list of path strings found in the tool's path-typed arguments."
  (let ((path-args (claude-agent-permission--get-tool-path-args tool-name)))
    (if path-args
        ;; Use registered path args
        (cl-loop for arg in path-args
                 for key = (intern (concat ":" arg))
                 for value = (plist-get tool-input key)
                 ;; Also try alist format (from Python agent)
                 for alist-value = (cdr (assq (intern arg) tool-input))
                 for final-value = (or value alist-value)
                 when (stringp final-value)
                 collect final-value)
      ;; Fallback: try common path argument names
      (let ((paths nil))
        (dolist (key '(file_path path notebook_path directory))
          (when-let ((value (or (plist-get tool-input (intern (concat ":" (symbol-name key))))
                                (cdr (assq key tool-input)))))
            (when (stringp value)
              (push value paths))))
        (nreverse paths)))))

(defun claude-agent-permission--extract-path (tool-name tool-input)
  "Extract file path from TOOL-INPUT for TOOL-NAME.
Returns the first path string or nil if the tool doesn't operate on files.
For backwards compatibility - prefer `claude-agent-permission--extract-paths'."
  (car (claude-agent-permission--extract-paths tool-name tool-input)))

(defun claude-agent-permission--extract-path-from-bash (command)
  "Try to extract a file path from a bash COMMAND string.
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

(defun claude-agent-permission--match-path-prefix (prefix tool-name tool-input)
  "Return non-nil if any path in TOOL-INPUT starts with PREFIX.
TOOL-NAME is used to determine which arguments contain paths."
  (let ((paths (claude-agent-permission--extract-paths tool-name tool-input))
        (expanded-prefix (expand-file-name prefix)))
    (cl-some (lambda (path)
               (string-prefix-p expanded-prefix (expand-file-name path)))
             paths)))

(defun claude-agent-permission--match-rule (matcher tool-name tool-input)
  "Check if MATCHER matches TOOL-NAME and TOOL-INPUT.
Returns t if matched, nil otherwise."
  (condition-case err
      (pcase matcher
        ;; Catch-all matcher
        ('t t)
        (`t t)

        ;; Predicate matcher - call function with tool context
        (`(:predicate ,fn)
         (funcall fn tool-name tool-input))

        ;; Exact tool name match
        (`(:tool ,name)
         (string= tool-name name))

        ;; Tool name regex match
        (`(:tool-regex ,regex)
         (string-match-p regex tool-name))

        ;; Path prefix match (using new multi-path extraction)
        (`(:path-prefix ,prefix)
         (claude-agent-permission--match-path-prefix prefix tool-name tool-input))

        ;; Path regex match
        (`(:path-regex ,regex)
         (let ((paths (claude-agent-permission--extract-paths tool-name tool-input)))
           (cl-some (lambda (path) (string-match-p regex path)) paths)))

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
Returns the action plist (:action ACTION :scope SCOPE :reason MSG)
of the first matching rule, or nil if no rule matches."
  (catch 'found
    (dolist (rule rules)
      (let ((matcher (plist-get rule :match))
            (action (plist-get rule :action)))
        (when action
          (when (claude-agent-permission--match-rule matcher tool-name tool-input)
            (throw 'found
                   (list :action action
                         :scope (or (plist-get rule :scope) :once)
                         :reason (or (plist-get rule :reason)
                                     (plist-get rule :message))))))))  ; :message for backwards compat
    nil))

;;;; Main permission check API

(defun claude-agent-permission-check (tool-name tool-input)
  "Check if TOOL-NAME with TOOL-INPUT should be allowed.
Returns a plist:
  (:decision :allow :scope SCOPE :pattern PATTERN)  - Allow the tool
  (:decision :deny :reason R)                       - Deny with reason R
  (:decision :prompt)                               - Show interactive UI

The decision is made by evaluating rules in order:
1. Buffer-local `claude-agent-permission-rules-local`
2. Project `claude-agent-project-permission-rules`
3. Global `claude-agent-permission-rules`
4. Fall through to :prompt (if no rule matches)"
  ;; Evaluate rules in order: local > project > global
  (let* ((all-rules (append claude-agent-permission-rules-local
                            claude-agent-project-permission-rules
                            claude-agent-permission-rules))
         (result (claude-agent-permission--evaluate-rules tool-name tool-input all-rules)))
    (if result
        (pcase (plist-get result :action)
          (:allow
           (let* ((scope (plist-get result :scope))
                  (pattern (claude-agent-permission--generate-pattern
                            tool-name tool-input scope)))
             (message "Claude agent: Auto-allowed %s (rule match, scope: %s)"
                      tool-name scope)
             (list :decision :allow :scope scope :pattern pattern)))
          (:deny
           (let ((reason (or (plist-get result :reason)
                             "Denied by permission rule")))
             (message "Claude agent: Auto-denied %s (rule match)" tool-name)
             (list :decision :deny :reason reason)))
          (:prompt
           (list :decision :prompt))
          (_
           (list :decision :prompt)))
      ;; No matching rule - fall through to prompt
      (list :decision :prompt))))

;;;; Legacy API wrapper

(defun claude-agent-permission-handle-request (tool-name tool-input)
  "Handle permission request for TOOL-NAME with TOOL-INPUT.
This is a wrapper around `claude-agent-permission-check' that returns
the result in the legacy format expected by existing callers.

Returns one of:
  (:allow :scope SCOPE :pattern PATTERN) - Auto-allow with scope
  (:deny :message MESSAGE)               - Auto-deny with reason
  nil                                    - Show interactive prompt"
  (let ((result (claude-agent-permission-check tool-name tool-input)))
    (pcase (plist-get result :decision)
      (:allow
       (list :allow
             :scope (plist-get result :scope)
             :pattern (plist-get result :pattern)))
      (:deny
       (list :deny :message (plist-get result :reason)))
      (:prompt nil))))

;;;; Pattern generation

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

;;;; Project settings file support

(defun claude-agent-load-project-settings ()
  "Load permission rules from .claude/settings.local.el if it exists.
Loads from current project root's .claude/settings.local.el."
  (when-let* ((project-root (or (when (bound-and-true-p claude--cwd)
                                  claude--cwd)
                                (when (fboundp 'claude--project-root)
                                  (claude--project-root))
                                default-directory))
              (settings-file (expand-file-name ".claude/settings.local.el" project-root)))
    (when (file-exists-p settings-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents settings-file)
            (let ((form (read (current-buffer))))
              ;; The file should contain a setq for claude-agent-project-permission-rules
              ;; or just the rules list directly
              (cond
               ((and (listp form)
                     (eq (car form) 'setq)
                     (eq (cadr form) 'claude-agent-project-permission-rules))
                (setq-local claude-agent-project-permission-rules (eval (caddr form))))
               ((listp form)
                ;; Assume it's a rules list directly
                (setq-local claude-agent-project-permission-rules form)))
              (message "Loaded project permissions from %s" settings-file)))
        (error
         (message "Error loading project permissions from %s: %s"
                  settings-file (error-message-string err)))))))

(defun claude-agent-save-project-rule (rule)
  "Append RULE to .claude/settings.local.el, creating file if needed.
RULE should be a permission rule plist."
  (when-let* ((project-root (or (when (bound-and-true-p claude--cwd)
                                  claude--cwd)
                                (when (fboundp 'claude--project-root)
                                  (claude--project-root))
                                default-directory))
              (settings-dir (expand-file-name ".claude" project-root))
              (settings-file (expand-file-name "settings.local.el" settings-dir)))
    ;; Ensure .claude directory exists
    (unless (file-directory-p settings-dir)
      (make-directory settings-dir t))
    ;; Load existing rules or start fresh
    (let ((existing-rules (when (file-exists-p settings-file)
                            (condition-case nil
                                (with-temp-buffer
                                  (insert-file-contents settings-file)
                                  (let ((form (read (current-buffer))))
                                    (cond
                                     ((and (listp form)
                                           (eq (car form) 'setq)
                                           (eq (cadr form) 'claude-agent-project-permission-rules))
                                      (eval (caddr form)))
                                     ((listp form) form)
                                     (t nil))))
                              (error nil)))))
      ;; Add new rule (avoid duplicates)
      (unless (member rule existing-rules)
        (push rule existing-rules))
      ;; Write back
      (with-temp-file settings-file
        (insert ";;; .claude/settings.local.el --- Project permission rules -*- lexical-binding: t; -*-\n\n")
        (insert ";; Auto-generated by claude-agent permission prompts.\n")
        (insert ";; You may edit this file manually.\n\n")
        (insert "(setq claude-agent-project-permission-rules\n")
        (insert "      '")
        (pp existing-rules (current-buffer))
        (insert ")\n"))
      ;; Update buffer-local copy
      (setq-local claude-agent-project-permission-rules existing-rules)
      (message "Saved permission rule to %s" settings-file))))

(defun claude-agent-edit-project-settings ()
  "Open .claude/settings.local.el for manual editing."
  (interactive)
  (when-let* ((project-root (or (when (bound-and-true-p claude--cwd)
                                  claude--cwd)
                                (when (fboundp 'claude--project-root)
                                  (claude--project-root))
                                default-directory))
              (settings-file (expand-file-name ".claude/settings.local.el" project-root)))
    (find-file settings-file)))

;;;; Convenience functions

(defun claude-agent-permission-clear-rules ()
  "Clear the current buffer's permission rules."
  (interactive)
  (setq-local claude-agent-permission-rules-local nil)
  (setq-local claude-agent-project-permission-rules nil)
  (message "Permission rules cleared"))

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
