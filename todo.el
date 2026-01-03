;;; todo.el --- Org-roam TODO management with Claude integration -*- lexical-binding: t; -*-
;; Author: Claude + Chad Crawford
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.0"))
;; Keywords: org-roam todo ai emacs llm tools
;; URL: https://github.com/cpoile/Claude
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Org-roam based TODO management with optional Claude/claudemacs integration.
;;
;; This provides a simple workflow:
;; 1. Create a TODO node for a project: `org-roam-todo-capture' (C-c n t t)
;; 2. From the TODO node, choose how to execute:
;;    - `org-roam-todo-send-to-main' (C-c c t) - Send to main session
;;    - `org-roam-todo-create-worktree' (C-c c w) - Create worktree + new session
;;    - `org-roam-todo-start-claude' (C-c n t c) - Start Claude on selected TODO
;; 3. View all TODOs: `org-roam-todo-list' (C-c n t l)
;;
;; TODO nodes are stored in org-roam as: projects/{project}/todo-{slug}.org
;; with properties:
;;   :PROJECT_NAME: short project name
;;   :PROJECT_ROOT: full path to project
;;   :STATUS: draft | active | done | rejected
;;   :WORKTREE_PATH: (set when worktree is created)
;;   :WORKTREE_BRANCH: (set when worktree is created)

;;; Code:

(require 'org-roam)
(require 'cl-lib)
(require 'tabulated-list)
(require 'json)

;; Forward declarations
(declare-function projectile-project-root "projectile")
(declare-function projectile-known-projects "projectile")
(declare-function claude-spawn-agent "claude")
(declare-function eat-term-send-string "eat")
(declare-function claude-mcp-deftool "claude-mcp")

;;;; Customization

(defgroup org-roam-todo nil
  "Org-roam TODO management with Claude integration."
  :group 'org-roam
  :group 'claude-agent)

(defcustom org-roam-todo-worktree-directory
  (expand-file-name "claude-worktrees" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "Base directory for storing worktrees.
Worktrees are created as {this-dir}/{project-name}/{branch-slug}/"
  :type 'directory
  :group 'org-roam-todo)

;;;; Project Selection

(defun org-roam-todo--worktree-main-repo (dir)
  "If DIR is a git worktree, return the main repository path.
Otherwise return nil."
  (let ((git-dir (expand-file-name ".git" dir)))
    (when (file-regular-p git-dir)
      ;; .git is a file, meaning this is a worktree
      (with-temp-buffer
        (insert-file-contents git-dir)
        (when (re-search-forward "gitdir: \\(.+\\)" nil t)
          (let ((gitdir (match-string 1)))
            ;; gitdir points to .git/worktrees/<name>
            ;; We want the parent repo's root
            (when (string-match "/\\.git/worktrees/" gitdir)
              (let ((main-git-dir (substring gitdir 0 (match-beginning 0))))
                (expand-file-name main-git-dir)))))))))

(defun org-roam-todo--infer-project ()
  "Infer the current project from context.
Checks for worktrees and maps them back to the main repository."
  (let* ((current-dir (or (and (fboundp 'projectile-project-root)
                               (projectile-project-root))
                          default-directory))
         ;; Check if we're in a worktree and get main repo
         (main-repo (org-roam-todo--worktree-main-repo current-dir)))
    (or main-repo current-dir)))

(defun org-roam-todo--select-project ()
  "Prompt user to select a git project.
Returns the project root path. Defaults to inferred project from context."
  (let* ((inferred (org-roam-todo--infer-project))
         (projects (if (and (fboundp 'projectile-known-projects)
                            (projectile-known-projects))
                       ;; Filter to only git repos
                       (seq-filter
                        (lambda (p)
                          (file-directory-p (expand-file-name ".git" p)))
                        (projectile-known-projects))
                     nil)))
    (if projects
        (completing-read "Project: " projects nil t nil nil inferred)
      (read-directory-name "Git project root: " inferred))))

(defun org-roam-todo--project-name (project-root)
  "Get short project name from PROJECT-ROOT."
  (file-name-nondirectory (directory-file-name project-root)))

;;;; Slug Helpers

(defun org-roam-todo--slugify (text)
  "Convert TEXT to a branch-safe slug."
  (let* ((slug (downcase text))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
         (slug (replace-regexp-in-string "^-\\|-$" "" slug)))
    slug))

(defun org-roam-todo--default-branch-name (title)
  "Generate default branch name from TITLE."
  (let ((slug (org-roam-todo--slugify title)))
    (format "feature/%s" slug)))

;;;; Node Property Helpers

(defun org-roam-todo--get-property (property)
  "Get PROPERTY from the current org-roam node."
  (org-entry-get (point-min) property))

(defun org-roam-todo--set-property (property value)
  "Set PROPERTY to VALUE in the current org-roam node."
  (save-excursion
    (goto-char (point-min))
    (org-set-property property value)))

(defun org-roam-todo--node-p ()
  "Return non-nil if current buffer is an org-roam TODO node."
  (and (derived-mode-p 'org-mode)
       (buffer-file-name)
       (save-excursion
         (goto-char (point-min))
         (re-search-forward "^:PROJECT_ROOT:" nil t))))

;;;; TODO Capture

;;;###autoload
(defun org-roam-todo-capture (&optional project-root)
  "Capture a new TODO for a projectile project.
If PROJECT-ROOT is nil, prompts for project selection."
  (interactive)
  (unless (featurep 'org-roam)
    (user-error "org-roam is required"))
  (let* ((project-root (or project-root (org-roam-todo--select-project)))
         (project-name (org-roam-todo--project-name project-root))
         (project-dir (expand-file-name (concat "projects/" project-name) org-roam-directory))
         ;; Generate timestamps with random suffix to ensure uniqueness
         (id-timestamp (format "%s%04x" (format-time-string "%Y%m%dT%H%M%S") (random 65536)))
         (date-stamp (format-time-string "%Y-%m-%d")))
    ;; Ensure project directory exists
    (unless (file-directory-p project-dir)
      (make-directory project-dir t))
    ;; Set up capture template dynamically
    (let ((org-roam-capture-templates
           `(("t" "Project TODO" plain "%?"
              :target (file+head
                       ,(concat "projects/" project-name "/todo-${slug}.org")
                       ,(format ":PROPERTIES:
:ID: %s
:PROJECT_NAME: %s
:PROJECT_ROOT: %s
:STATUS: draft
:CREATED: %s
:END:
#+title: ${title}
#+filetags: :todo:%s:

** Task Description

** Acceptance Criteria
- [ ]

** Progress Log

" id-timestamp project-name project-root date-stamp project-name))
              :unnarrowed t))))
      (org-roam-capture))))

;;;###autoload
(defun org-roam-todo-capture-project ()
  "Capture a new TODO for the current project.
Auto-infers project from context (including worktree detection)."
  (interactive)
  (org-roam-todo-capture (org-roam-todo--infer-project)))

;;;; Git Worktree Operations

(defun org-roam-todo--pre-trust-worktree (worktree-path)
  "Pre-trust WORKTREE-PATH in Claude's global config to skip trust dialog.
Calls the pretrust-directory.py script to add an entry to ~/.claude.json."
  (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name
                                               (locate-library "todo"))))
         (script-path (expand-file-name "scripts/pretrust-directory.py" script-dir))
         (expanded-path (expand-file-name worktree-path)))
    (if (file-exists-p script-path)
        (let ((result (call-process "uv" nil nil nil
                                    "run" script-path expanded-path)))
          (if (= result 0)
              (message "Pre-trusted worktree: %s" expanded-path)
            (message "Warning: Failed to pre-trust worktree (exit %d)" result)))
      (message "Warning: pretrust-directory.py not found at %s" script-path))))

(defun org-roam-todo--worktree-path (project-root branch-name)
  "Calculate worktree path for PROJECT-ROOT and BRANCH-NAME."
  (let* ((project-name (org-roam-todo--project-name project-root))
         (branch-slug (org-roam-todo--slugify branch-name)))
    (expand-file-name
     (concat project-name "/" branch-slug)
     org-roam-todo-worktree-directory)))

(defun org-roam-todo--worktree-exists-p (worktree-path)
  "Return non-nil if WORKTREE-PATH exists and is a git worktree."
  (and (file-directory-p worktree-path)
       (file-exists-p (expand-file-name ".git" worktree-path))))

(defun org-roam-todo--branch-exists-p (project-root branch-name)
  "Return non-nil if BRANCH-NAME exists in PROJECT-ROOT."
  (let ((default-directory project-root))
    (= 0 (call-process "git" nil nil nil "rev-parse" "--verify" branch-name))))

(defun org-roam-todo--create-worktree (project-root branch-name worktree-path)
  "Create a git worktree at WORKTREE-PATH for BRANCH-NAME from PROJECT-ROOT.
Creates the branch if it doesn't exist."
  (let ((default-directory project-root))
    ;; Ensure parent directory exists
    (make-directory (file-name-directory worktree-path) t)
    ;; Create worktree (with new branch if needed)
    (if (org-roam-todo--branch-exists-p project-root branch-name)
        ;; Branch exists, just create worktree
        (let ((result (call-process "git" nil "*org-roam-todo-worktree-output*" nil
                                    "worktree" "add" worktree-path branch-name)))
          (unless (= 0 result)
            (error "Failed to create worktree: see *org-roam-todo-worktree-output*")))
      ;; Create new branch with worktree
      (let ((result (call-process "git" nil "*org-roam-todo-worktree-output*" nil
                                  "worktree" "add" "-b" branch-name worktree-path)))
        (unless (= 0 result)
          (error "Failed to create worktree with new branch: see *org-roam-todo-worktree-output*"))))))

;;;; TODO Query & Selection

(defconst org-roam-todo-status-order
  '("draft" "active" "done" "rejected")
  "Order of TODO statuses for sorting.")

(defun org-roam-todo--query-todos (&optional project-filter)
  "Query all TODO nodes from org-roam, optionally filtered by PROJECT-FILTER.
Returns a list of plists with :id, :title, :project, :status, :file, :created."
  (let* ((todos '())
         ;; Query org-roam for file-level nodes only (level = 0)
         ;; This excludes sub-nodes within the file like "Progress Log"
         (nodes (org-roam-db-query
                 [:select [nodes:id nodes:file nodes:title]
                  :from nodes
                  :where (and (like nodes:file "%/todo-%.org")
                              (= nodes:level 0))])))
    (dolist (row nodes)
      (let* ((id (nth 0 row))
             (file (nth 1 row))
             (title (nth 2 row)))
        ;; Read properties from the file
        (when (file-exists-p file)
          (with-temp-buffer
            (insert-file-contents file nil 0 2000) ; Just read header
            (let ((project (when (re-search-forward "^:PROJECT_NAME:\\s-*\\(.+\\)$" nil t)
                             (match-string 1)))
                  (project-root (progn
                                  (goto-char (point-min))
                                  (when (re-search-forward "^:PROJECT_ROOT:\\s-*\\(.+\\)$" nil t)
                                    (match-string 1))))
                  (status (progn
                            (goto-char (point-min))
                            (when (re-search-forward "^:STATUS:\\s-*\\(.+\\)$" nil t)
                              (match-string 1))))
                  (created (progn
                             (goto-char (point-min))
                             (when (re-search-forward "^:CREATED:\\s-*\\(.+\\)$" nil t)
                               (match-string 1))))
                  (worktree-path (progn
                                   (goto-char (point-min))
                                   (when (re-search-forward "^:WORKTREE_PATH:\\s-*\\(.+\\)$" nil t)
                                     (match-string 1)))))
              (when (and project
                         (or (null project-filter)
                             (string= project project-filter)
                             ;; Normalize paths: expand ~ and remove trailing slashes
                             (string= (directory-file-name (expand-file-name project-root))
                                      (directory-file-name (expand-file-name project-filter)))))
                (push (list :id id
                            :title title
                            :project project
                            :project-root project-root
                            :status (or status "draft")
                            :file file
                            :created (or created "")
                            :worktree-path worktree-path)
                      todos)))))))
    ;; Sort by status order, then by created date (newest first)
    (sort todos
          (lambda (a b)
            (let ((status-a (org-roam-todo--status-sort-key (plist-get a :status)))
                  (status-b (org-roam-todo--status-sort-key (plist-get b :status))))
              (if (= status-a status-b)
                  (string> (plist-get a :created) (plist-get b :created))
                (< status-a status-b)))))))

(defun org-roam-todo--status-sort-key (status)
  "Return sort key for STATUS (lower = first)."
  (or (cl-position (or status "draft") org-roam-todo-status-order :test #'string=) 99))

(defun org-roam-todo--completing-read (&optional project-filter prompt)
  "Select a TODO using completion.
PROJECT-FILTER limits to a specific project.
PROMPT is the prompt string.
Returns the TODO plist."
  (let* ((todos (org-roam-todo--query-todos project-filter))
         (candidates (mapcar (lambda (todo)
                               (cons (format "[%s] %s - %s"
                                            (plist-get todo :status)
                                            (plist-get todo :title)
                                            (plist-get todo :project))
                                     todo))
                             todos))
         (selected (completing-read (or prompt "TODO: ") candidates nil t)))
    (cdr (assoc selected candidates))))

(defun org-roam-todo--get-full-content (file)
  "Get the full content of a TODO FILE after the filetags line."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      ;; Skip to after filetags line
      (when (re-search-forward "^#\\+filetags:" nil t)
        (forward-line 1)
        (string-trim (buffer-substring-no-properties (point) (point-max)))))))

;;;; Send to Main Session

(defun org-roam-todo--normalize-path (path)
  "Normalize PATH by expanding, resolving symlinks, and removing trailing slash."
  (directory-file-name (file-truename (expand-file-name path))))

(defun org-roam-todo--find-main-session (project-root)
  "Find the main Claude buffer for PROJECT-ROOT."
  (let ((normalized-root (org-roam-todo--normalize-path project-root)))
    (cl-find-if
     (lambda (buf)
       (and (string-match-p "^\\*claude" (buffer-name buf))
            ;; Exclude named agents (buffers with :agent-name suffix)
            (not (string-match-p "^\\*claude[^:]*:[^:]+:[^*]+\\*$" (buffer-name buf)))
            (with-current-buffer buf
              (and (boundp 'claude-agent--work-dir)
                   claude-agent--work-dir
                   (string= (org-roam-todo--normalize-path claude-agent--work-dir)
                            normalized-root)))))
     (buffer-list))))

(defun org-roam-todo--get-node-content ()
  "Get the content of the current TODO node for sending to Claude."
  (save-excursion
    (goto-char (point-min))
    ;; Skip to after filetags line
    (when (re-search-forward "^#\\+filetags:" nil t)
      (forward-line 1)
      (string-trim (buffer-substring-no-properties (point) (point-max))))))

;;;###autoload
(defun org-roam-todo-send-to-main ()
  "Send the current TODO to the main Claude session for its project.
Use this for quick tasks that don't need worktree isolation."
  (interactive)
  (unless (org-roam-todo--node-p)
    (user-error "Not in an org-roam TODO node"))
  (let* ((project-root (org-roam-todo--get-property "PROJECT_ROOT"))
         (todo-id (org-roam-todo--get-property "ID"))
         (title (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\+title: \\(.+\\)$" nil t)
                    (match-string 1))))
         (content (org-roam-todo--get-node-content))
         (claude-buffer (org-roam-todo--find-main-session project-root)))
    (unless project-root
      (user-error "No PROJECT_ROOT property found"))
    (unless claude-buffer
      (user-error "No Claude session found for project: %s\nStart one with M-x claude in that project" project-root))
    ;; Send to Claude using the new claude-agent process mechanism
    (with-current-buffer claude-buffer
      (when (and (boundp 'claude-agent--process)
                 claude-agent--process
                 (process-live-p claude-agent--process))
        (let ((msg (format "[TODO: %s] %s

%s

---
TODO Management:
- Use `todo_acceptance_criteria` to see checklist items
- Use `todo_check_acceptance` to mark items complete
- Use `todo_add_progress` to log progress updates
- Use `todo_update_status` with 'done' when finished"
                           (or todo-id "unknown") (or title "Task") content)))
          (process-send-string
           claude-agent--process
           (concat (json-encode `((type . "message") (text . ,msg))) "\n")))))
    ;; Update status
    (org-roam-todo--set-property "STATUS" "active")
    (save-buffer)
    (message "Sent TODO to main Claude session")))

;;;; Create Worktree

(defun org-roam-todo--send-task-to-buffer (buffer-name content worktree-path &optional delay)
  "Send task CONTENT to BUFFER-NAME after optional DELAY seconds.
WORKTREE-PATH is included in the message for context."
  (let ((send-fn (lambda (buf-name task-content wpath)
                   (message "Sending task to %s..." buf-name)
                   (let ((buffer (get-buffer buf-name)))
                     (if (not buffer)
                         (message "ERROR: Buffer %s not found" buf-name)
                       (with-current-buffer buffer
                         (if (not (and (boundp 'eat-terminal) eat-terminal))
                             (message "ERROR: eat-terminal not ready in %s" buf-name)
                           (let ((msg (format "[WORKTREE TASK]\n\n%s\n\nWorktree: %s\nPlease help me with this task."
                                              task-content wpath)))
                             (eat-term-send-string eat-terminal "\C-u")
                             (eat-term-send-string eat-terminal msg)
                             (sit-for 0.1)
                             (eat-term-send-string eat-terminal "\r")
                             (message "Task sent to %s" buf-name)))))))))
    (if delay
        (run-with-timer delay nil send-fn buffer-name content worktree-path)
      (funcall send-fn buffer-name content worktree-path))))

;;;###autoload
(defun org-roam-todo-create-worktree ()
  "Create a worktree for the current TODO and spawn a Claude session.
Use this for feature work that benefits from isolation.
If the worktree and session already exist, sends the task to the existing session."
  (interactive)
  (unless (org-roam-todo--node-p)
    (user-error "Not in an org-roam TODO node"))
  (require 'claude)
  (let* ((project-root (org-roam-todo--get-property "PROJECT_ROOT"))
         (existing-worktree (org-roam-todo--get-property "WORKTREE_PATH"))
         (title (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\+title: \\(.+\\)$" nil t)
                    (match-string 1))))
         (default-branch (org-roam-todo--default-branch-name (or title "feature")))
         (branch-name (or (org-roam-todo--get-property "WORKTREE_BRANCH")
                          (read-string "Branch name: " default-branch)))
         (worktree-path (or existing-worktree
                            (org-roam-todo--worktree-path project-root branch-name)))
         (content (org-roam-todo--get-node-content))
         ;; Calculate expected buffer name
         (expanded-path (expand-file-name worktree-path))
         (expected-buffer-name (format "*claude:%s:%s*" expanded-path branch-name))
         (existing-buffer (get-buffer expected-buffer-name)))
    (unless project-root
      (user-error "No PROJECT_ROOT property found"))
    ;; Create worktree if needed
    (unless (org-roam-todo--worktree-exists-p worktree-path)
      (message "Creating worktree at %s..." worktree-path)
      (org-roam-todo--create-worktree project-root branch-name worktree-path)
      ;; Store worktree info in node
      (org-roam-todo--set-property "WORKTREE_PATH" worktree-path)
      (org-roam-todo--set-property "WORKTREE_BRANCH" branch-name))
    ;; Update status
    (org-roam-todo--set-property "STATUS" "active")
    (save-buffer)
    ;; Check if session already exists
    (if existing-buffer
        (progn
          ;; Session exists - send task immediately (no delay needed)
          (org-roam-todo--send-task-to-buffer expected-buffer-name content worktree-path)
          (pop-to-buffer existing-buffer)
          (message "Sent task to existing session: %s" expected-buffer-name))
      ;; New session - pre-trust and spawn
      (org-roam-todo--pre-trust-worktree worktree-path)
      (let ((buffer-name (claude-spawn-agent worktree-path branch-name)))
        ;; Store buffer name in node
        (org-roam-todo--set-property "CLAUDE_AGENT_BUFFER" buffer-name)
        (save-buffer)
        ;; Wait for session to be ready (5 seconds for Claude to initialize)
        (org-roam-todo--send-task-to-buffer buffer-name content worktree-path 5)
        (message "Created worktree and spawned Claude session: %s" buffer-name)))))

;;;; Select TODO → Create Worktree

;;;###autoload
(defun org-roam-todo-select-worktree (&optional project-filter)
  "Select a TODO and create/open its worktree with a Claude session.
Optional PROJECT-FILTER limits selection to a specific project."
  (interactive)
  (let* ((todo (org-roam-todo--completing-read project-filter "Create worktree for TODO: "))
         (file (plist-get todo :file)))
    (unless todo
      (user-error "No TODO selected"))
    ;; Open the TODO file and run create-worktree
    (find-file file)
    (org-roam-todo-create-worktree)))

;;;###autoload
(defun org-roam-todo-select-worktree-project ()
  "Select a TODO from current project and create/open its worktree."
  (interactive)
  (org-roam-todo-select-worktree (org-roam-todo--infer-project)))

;;;; Start Claude on TODO

(defun org-roam-todo--send-initial-message (buffer-name todo content)
  "Send initial task message to Claude agent in BUFFER-NAME.
TODO is the todo plist, CONTENT is the full TODO content."
  (when-let ((buffer (get-buffer buffer-name)))
    (with-current-buffer buffer
      (when (and (boundp 'eat-terminal) eat-terminal)
        (let ((msg (format "You are working on a TODO task.

## Task: %s

%s

## Instructions
1. Use `emacs_todo_current` to retrieve full task details
2. Use `emacs_todo_acceptance_criteria` to see what needs to be done
3. As you make progress, use `emacs_todo_add_progress` to log updates
4. Use `emacs_todo_check_acceptance` to mark criteria as complete
5. When finished, use `emacs_todo_update_status` to set status to 'done'

Please start by reviewing the acceptance criteria and creating a plan."
                           (plist-get todo :title)
                           content)))
          (eat-term-send-string eat-terminal msg)
          (sit-for 0.1)
          (eat-term-send-string eat-terminal "\r"))))))

;;;###autoload
(defun org-roam-todo-start-claude (&optional project-filter)
  "Select a TODO and start a Claude agent to work on it.
Optional PROJECT-FILTER limits selection to a specific project.
If TODO has a worktree, starts agent there; otherwise uses project root."
  (interactive)
  (require 'claude)
  (let* ((todo (org-roam-todo--completing-read project-filter "Start Claude on TODO: "))
         (worktree (plist-get todo :worktree-path))
         (project-root (plist-get todo :project-root))
         ;; Use worktree if it exists, otherwise use project root
         (work-dir (if (and worktree (file-directory-p worktree))
                       worktree
                     project-root))
         (title (plist-get todo :title))
         (content (org-roam-todo--get-full-content (plist-get todo :file)))
         (agent-name (org-roam-todo--slugify title))
         (buffer-name (claude-spawn-agent work-dir agent-name)))
    (unless todo
      (user-error "No TODO selected"))
    ;; Update status in the TODO file
    (with-current-buffer (find-file-noselect (plist-get todo :file))
      (org-roam-todo--set-property "STATUS" "active")
      (org-roam-todo--set-property "CLAUDE_AGENT_BUFFER" buffer-name)
      (save-buffer))
    ;; Send task message after delay for Claude to initialize
    (run-with-timer 5 nil
                    #'org-roam-todo--send-initial-message
                    buffer-name todo content)
    (message "Started Claude agent for: %s" title)))

;;;###autoload
(defun org-roam-todo-start-claude-project ()
  "Select a TODO from current project and start a Claude agent."
  (interactive)
  (org-roam-todo-start-claude (org-roam-todo--infer-project)))

;;;; Resend Task

;;;###autoload
(defun org-roam-todo-resend ()
  "Resend the current TODO content to its associated Claude session.
Works for both main session TODOs and worktree TODOs."
  (interactive)
  (unless (org-roam-todo--node-p)
    (user-error "Not in an org-roam TODO node"))
  (let* ((worktree-path (org-roam-todo--get-property "WORKTREE_PATH"))
         (project-root (org-roam-todo--get-property "PROJECT_ROOT"))
         (title (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\+title: \\(.+\\)$" nil t)
                    (match-string 1))))
         (content (org-roam-todo--get-node-content))
         (claude-buffer (if worktree-path
                            ;; Find worktree session
                            (cl-find-if
                             (lambda (buf)
                               (and (string-match-p "^\\*claude" (buffer-name buf))
                                    (with-current-buffer buf
                                      (and (boundp 'claude-agent--work-dir)
                                           claude-agent--work-dir
                                           (string= (org-roam-todo--normalize-path claude-agent--work-dir)
                                                    (org-roam-todo--normalize-path worktree-path))))))
                             (buffer-list))
                          ;; Find main session
                          (org-roam-todo--find-main-session project-root))))
    (unless claude-buffer
      (user-error "No Claude session found. Use C-c c t or C-c c w first"))
    (with-current-buffer claude-buffer
      (when (and (boundp 'claude-agent--process)
                 claude-agent--process
                 (process-live-p claude-agent--process))
        (let ((msg (if worktree-path
                       (format "[WORKTREE TASK]\n\n%s\n\nWorktree: %s\nPlease help me with this task."
                               content worktree-path)
                     (format "[TODO] %s\n\n%s" (or title "Task") content))))
          (process-send-string
           claude-agent--process
           (concat (json-encode `((type . "message") (text . ,msg))) "\n")))))
    (message "Resent TODO to Claude session")))

;;;; TODO List Buffer

(defun org-roam-todo-list-buffer-name (&optional project)
  "Generate buffer name for TODO list, optionally for PROJECT."
  (if project
      (format "*todo-list:%s*" project)
    "*todo-list*"))

;;;; Faces for TODO List

(defface org-roam-todo-status-draft
  '((t :foreground "#888888" :weight normal))
  "Face for draft status."
  :group 'org-roam-todo)

(defface org-roam-todo-status-active
  '((t :foreground "#ffaa00" :weight bold))
  "Face for active status."
  :group 'org-roam-todo)

(defface org-roam-todo-status-done
  '((t :foreground "#00ff00" :weight bold))
  "Face for done status."
  :group 'org-roam-todo)

(defface org-roam-todo-status-rejected
  '((t :foreground "#ff4444" :weight normal :strike-through t))
  "Face for rejected status."
  :group 'org-roam-todo)

(defface org-roam-todo-title
  '((t :foreground "#aaccff"))
  "Face for TODO title."
  :group 'org-roam-todo)

(defface org-roam-todo-project
  '((t :foreground "#88aaff"))
  "Face for project name."
  :group 'org-roam-todo)

(defun org-roam-todo--status-face (status)
  "Return the face for STATUS."
  (pcase status
    ("draft" 'org-roam-todo-status-draft)
    ("active" 'org-roam-todo-status-active)
    ("done" 'org-roam-todo-status-done)
    ("rejected" 'org-roam-todo-status-rejected)
    (_ 'default)))

(defun org-roam-todo--format-status (status)
  "Format STATUS with appropriate face."
  (propertize (or status "draft") 'face (org-roam-todo--status-face status)))

(defvar-local org-roam-todo-list--project-filter nil
  "Current project filter for the TODO list buffer.")

(defun org-roam-todo-list--get-entries ()
  "Get tabulated list entries for TODOs."
  (mapcar
   (lambda (todo)
     (let ((id (plist-get todo :id))
           (title (plist-get todo :title))
           (project (plist-get todo :project))
           (status (plist-get todo :status))
           (created (plist-get todo :created))
           (file (plist-get todo :file)))
       (list file
             (vector
              (org-roam-todo--format-status status)
              (propertize (or title "Untitled") 'face 'org-roam-todo-title)
              (propertize (or project "") 'face 'org-roam-todo-project)
              (or created "")))))
   (org-roam-todo--query-todos org-roam-todo-list--project-filter)))

(defun org-roam-todo-list-refresh ()
  "Refresh the TODO list buffer."
  (interactive)
  (tabulated-list-revert))

(defun org-roam-todo-list-open ()
  "Open the TODO at point."
  (interactive)
  (when-let ((file (tabulated-list-get-id)))
    (find-file file)))

(defun org-roam-todo-list-set-status (new-status)
  "Set the status of the TODO at point to NEW-STATUS."
  (when-let ((file (tabulated-list-get-id)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^:STATUS:\\s-*.+$" nil t)
            (replace-match (format ":STATUS: %s" new-status))
          ;; Add STATUS property if it doesn't exist
          (when (re-search-forward "^:PROPERTIES:" nil t)
            (forward-line 1)
            (insert (format ":STATUS: %s\n" new-status))))
        (save-buffer)))
    (org-roam-todo-list-refresh)
    (message "Set status to: %s" new-status)))

(defun org-roam-todo-list-mark-done ()
  "Mark the TODO at point as done."
  (interactive)
  (org-roam-todo-list-set-status "done"))

(defun org-roam-todo-list-mark-rejected ()
  "Mark the TODO at point as rejected."
  (interactive)
  (org-roam-todo-list-set-status "rejected"))

(defun org-roam-todo-list-mark-active ()
  "Mark the TODO at point as active."
  (interactive)
  (org-roam-todo-list-set-status "active"))

(defun org-roam-todo-list-mark-draft ()
  "Mark the TODO at point as draft."
  (interactive)
  (org-roam-todo-list-set-status "draft"))

(defun org-roam-todo-list-cycle-status ()
  "Cycle the status of the TODO at point."
  (interactive)
  (when-let ((file (tabulated-list-get-id)))
    (let* ((current-status
            (with-temp-buffer
              (insert-file-contents file nil 0 1000)
              (when (re-search-forward "^:STATUS:\\s-*\\(.+\\)$" nil t)
                (match-string 1))))
           (current-idx (or (cl-position (or current-status "draft")
                                         org-roam-todo-status-order :test #'string=) 0))
           (next-idx (mod (1+ current-idx) (length org-roam-todo-status-order)))
           (next-status (nth next-idx org-roam-todo-status-order)))
      (org-roam-todo-list-set-status next-status))))

(defvar org-roam-todo-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-roam-todo-list-open)
    (define-key map (kbd "o") #'org-roam-todo-list-open)
    (define-key map (kbd "g") #'org-roam-todo-list-refresh)
    (define-key map (kbd "d") #'org-roam-todo-list-mark-done)
    (define-key map (kbd "r") #'org-roam-todo-list-mark-rejected)
    (define-key map (kbd "a") #'org-roam-todo-list-mark-active)
    (define-key map (kbd "u") #'org-roam-todo-list-mark-draft)
    (define-key map (kbd "TAB") #'org-roam-todo-list-cycle-status)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `org-roam-todo-list-mode'.")

;; Evil mode support
(with-eval-after-load 'evil
  (evil-define-key 'normal org-roam-todo-list-mode-map
    (kbd "RET") #'org-roam-todo-list-open
    (kbd "o") #'org-roam-todo-list-open
    (kbd "gr") #'org-roam-todo-list-refresh
    (kbd "d") #'org-roam-todo-list-mark-done
    (kbd "r") #'org-roam-todo-list-mark-rejected
    (kbd "a") #'org-roam-todo-list-mark-active
    (kbd "u") #'org-roam-todo-list-mark-draft
    (kbd "TAB") #'org-roam-todo-list-cycle-status
    (kbd "q") #'quit-window))

(define-derived-mode org-roam-todo-list-mode tabulated-list-mode "Org-Roam-TODOs"
  "Major mode for viewing and managing org-roam project TODOs.

\\{org-roam-todo-list-mode-map}"
  (setq tabulated-list-format
        [("Status" 12 (lambda (a b)
                        (< (org-roam-todo--status-sort-key (aref (cadr a) 0))
                           (org-roam-todo--status-sort-key (aref (cadr b) 0)))))
         ("Title" 50 t)
         ("Project" 20 t)
         ("Created" 12 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Status" . nil))
  (setq tabulated-list-entries #'org-roam-todo-list--get-entries)
  (tabulated-list-init-header))

;;;###autoload
(defun org-roam-todo-list ()
  "Display a buffer listing all project TODOs."
  (interactive)
  (let ((buffer (get-buffer-create (org-roam-todo-list-buffer-name))))
    (with-current-buffer buffer
      (org-roam-todo-list-mode)
      (setq-local org-roam-todo-list--project-filter nil)
      (tabulated-list-print))
    (pop-to-buffer buffer)))

;;;###autoload
(defun org-roam-todo-list-project (&optional prompt)
  "Display a buffer listing TODOs for the current project.
Auto-infers project from context (including worktree detection).
With prefix arg PROMPT, prompts for project selection."
  (interactive "P")
  (let* ((project (if prompt
                      (org-roam-todo--select-project)
                    (org-roam-todo--infer-project)))
         (project-name (org-roam-todo--project-name project))
         (buffer (get-buffer-create (org-roam-todo-list-buffer-name project-name))))
    (with-current-buffer buffer
      (org-roam-todo-list-mode)
      (setq-local org-roam-todo-list--project-filter project)
      (tabulated-list-print))
    (pop-to-buffer buffer)))

;;;; Minor Mode

(defvar org-roam-todo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c t") #'org-roam-todo-send-to-main)
    (define-key map (kbd "C-c c w") #'org-roam-todo-create-worktree)
    (define-key map (kbd "C-c c s") #'org-roam-todo-resend)
    map)
  "Keymap for `org-roam-todo-mode'.

Key bindings:
  C-c c t   Send TODO to main Claude session
  C-c c w   Create worktree and spawn new Claude session
  C-c c s   Resend TODO content to associated session")

;;;###autoload
(define-minor-mode org-roam-todo-mode
  "Minor mode for org-roam TODO nodes.

\\{org-roam-todo-mode-map}"
  :lighter " OrgTODO"
  :keymap org-roam-todo-mode-map)

(defun org-roam-todo--maybe-enable-mode ()
  "Enable `org-roam-todo-mode' if this is an org-roam TODO node."
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name)
             ;; Check if filename matches todo pattern in org-roam projects dir
             (string-match-p "/projects/[^/]+/todo-" (buffer-file-name)))
    ;; Double-check by reading the PROJECT_ROOT property
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^:PROJECT_ROOT:" nil t)
        (org-roam-todo-mode 1)))))

;; Auto-enable in TODO nodes
(add-hook 'find-file-hook #'org-roam-todo--maybe-enable-mode)

;;;; MCP Tool Functions

(defun org-roam-todo-mcp--resolve-todo (todo-id)
  "Resolve TODO-ID to a file path.
If TODO-ID is nil, tries to find the current TODO from the worktree.
If TODO-ID is a file path that exists, returns it.
Otherwise searches by title."
  (cond
   ;; No ID - try to infer from current worktree
   ((null todo-id)
    (let* ((cwd (or (bound-and-true-p claude-session-cwd)
                    (bound-and-true-p claude--cwd)
                    default-directory))
           (expanded-cwd (expand-file-name cwd))
           (todos (org-roam-todo--query-todos)))
      (plist-get
       (cl-find-if
        (lambda (todo)
          (let ((wpath (plist-get todo :worktree-path)))
            (and wpath
                 (string= (expand-file-name wpath) expanded-cwd))))
        todos)
       :file)))
   ;; File path that exists
   ((and (stringp todo-id) (file-exists-p todo-id))
    todo-id)
   ;; Search by title
   (t
    (let ((todos (org-roam-todo--query-todos)))
      (plist-get
       (cl-find-if (lambda (todo) (string= (plist-get todo :title) todo-id))
                   todos)
       :file)))))

(defun org-roam-todo-mcp-get-current ()
  "Get the TODO assigned to the current worktree session.
Returns JSON with the TODO details or null if not in a worktree."
  (let ((file (org-roam-todo-mcp--resolve-todo nil)))
    (if file
        (let ((todos (org-roam-todo--query-todos)))
          (let ((todo (cl-find-if (lambda (t) (string= (plist-get t :file) file)) todos)))
            (json-encode
             `((id . ,(plist-get todo :id))
               (title . ,(plist-get todo :title))
               (project . ,(plist-get todo :project))
               (project_root . ,(plist-get todo :project-root))
               (status . ,(plist-get todo :status))
               (file . ,(plist-get todo :file))
               (created . ,(plist-get todo :created))
               (content . ,(org-roam-todo--get-full-content file))))))
      "null")))

(defun org-roam-todo-mcp-list (&optional project)
  "List all project TODOs for MCP.
Returns JSON with all TODOs, optionally filtered by PROJECT."
  (let ((todos (org-roam-todo--query-todos project)))
    (json-encode
     (mapcar (lambda (todo)
               `((id . ,(plist-get todo :id))
                 (title . ,(plist-get todo :title))
                 (project . ,(plist-get todo :project))
                 (project_root . ,(plist-get todo :project-root))
                 (status . ,(plist-get todo :status))
                 (file . ,(plist-get todo :file))
                 (created . ,(plist-get todo :created))))
             todos))))

(defun org-roam-todo-mcp-get-acceptance-criteria (&optional todo-id)
  "Get all acceptance criteria items from a TODO.
TODO-ID can be a file path or title (defaults to current TODO).
Returns JSON array of {text, checked} objects."
  (let ((file (org-roam-todo-mcp--resolve-todo todo-id)))
    (unless file
      (error "TODO not found: %s" (or todo-id "current")))
    (let ((criteria '()))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Find the Acceptance Criteria section
        (when (re-search-forward "^\\*\\* Acceptance Criteria" nil t)
          (let ((section-end (save-excursion
                               (if (re-search-forward "^\\*\\* " nil t)
                                   (point)
                                 (point-max)))))
            (while (re-search-forward "^- \\[\\([ X]\\)\\] \\(.+\\)$" section-end t)
              (push `((text . ,(match-string 2))
                      (checked . ,(if (string= (match-string 1) "X") t :json-false)))
                    criteria)))))
      (json-encode (nreverse criteria)))))

(defun org-roam-todo-mcp-add-progress (message &optional todo-id)
  "Add a timestamped progress entry to a TODO.
MESSAGE is the progress text to add.
TODO-ID can be a file path or title (defaults to current TODO)."
  (let ((file (org-roam-todo-mcp--resolve-todo todo-id)))
    (unless file
      (error "TODO not found: %s" (or todo-id "current")))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        ;; Find the Progress Log section
        (if (re-search-forward "^\\*\\* Progress Log" nil t)
            (progn
              (forward-line 1)
              ;; Skip any property drawer
              (when (looking-at ":PROPERTIES:")
                (re-search-forward ":END:" nil t)
                (forward-line 1))
              ;; Insert the progress entry
              (insert (format "\n- [%s] %s\n"
                              (format-time-string "%Y-%m-%d %H:%M")
                              message)))
          ;; No Progress Log section, create one at the end
          (goto-char (point-max))
          (insert (format "\n** Progress Log\n\n- [%s] %s\n"
                          (format-time-string "%Y-%m-%d %H:%M")
                          message)))
        (save-buffer)))
    (format "Added progress entry to %s" (file-name-nondirectory file))))

(defun org-roam-todo-mcp-update-status (status &optional todo-id)
  "Update the status of a TODO.
STATUS should be one of: draft, active, done, rejected.
TODO-ID can be a file path or title (defaults to current TODO)."
  (let ((file (org-roam-todo-mcp--resolve-todo todo-id)))
    (unless file
      (error "TODO not found: %s" (or todo-id "current")))
    (unless (member status org-roam-todo-status-order)
      (error "Invalid status: %s. Must be one of: %s"
             status (string-join org-roam-todo-status-order ", ")))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^:STATUS:\\s-*.+$" nil t)
            (replace-match (format ":STATUS: %s" status))
          ;; Add STATUS property if it doesn't exist
          (when (re-search-forward "^:PROPERTIES:" nil t)
            (forward-line 1)
            (insert (format ":STATUS: %s\n" status))))
        (save-buffer)))
    (format "Updated status to: %s" status)))

(defun org-roam-todo-mcp-check-acceptance (item-text &optional checked todo-id)
  "Check or uncheck an acceptance criteria item in a TODO.
ITEM-TEXT is the text of the checkbox item to find.
CHECKED if non-nil, check the item; otherwise uncheck.
TODO-ID can be a file path or title (defaults to current TODO)."
  (let ((file (org-roam-todo-mcp--resolve-todo todo-id)))
    (unless file
      (error "TODO not found: %s" (or todo-id "current")))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        ;; Find the Acceptance Criteria section
        (unless (re-search-forward "^\\*\\* Acceptance Criteria" nil t)
          (error "No Acceptance Criteria section found"))
        ;; Find the matching item
        (let ((section-end (save-excursion
                             (if (re-search-forward "^\\*\\* " nil t)
                                 (point)
                               (point-max))))
              (found nil))
          (while (and (not found)
                      (re-search-forward "^- \\[[ X]\\] \\(.+\\)$" section-end t))
            (when (string-match-p (regexp-quote item-text) (match-string 1))
              (setq found t)
              (goto-char (match-beginning 0))
              (if checked
                  (progn
                    (re-search-forward "\\[ \\]" (line-end-position) t)
                    (replace-match "[X]"))
                (re-search-forward "\\[X\\]" (line-end-position) t)
                (replace-match "[ ]"))))
          (unless found
            (error "Acceptance criteria item not found: %s" item-text))
          (save-buffer))))
    (format "%s: %s" (if checked "Checked" "Unchecked") item-text)))

(defun org-roam-todo-mcp-create (project-root title &optional description acceptance-criteria)
  "Create a new TODO programmatically.
PROJECT-ROOT is the path to the project.
TITLE is the TODO title.
DESCRIPTION is optional task description text.
ACCEPTANCE-CRITERIA is an optional list of criteria strings.
Returns JSON with the created TODO's file path and ID."
  (unless project-root
    (error "project_root is required"))
  (unless title
    (error "title is required"))
  (let* ((project-name (org-roam-todo--project-name project-root))
         (project-dir (expand-file-name (concat "projects/" project-name) org-roam-directory))
         (slug (org-roam-todo--slugify title))
         (id-timestamp (format "%s%04x" (format-time-string "%Y%m%dT%H%M%S") (random 65536)))
         (date-stamp (format-time-string "%Y-%m-%d"))
         (file-path (expand-file-name (format "todo-%s.org" slug) project-dir))
         ;; Format acceptance criteria as org checkboxes
         (criteria-text (if acceptance-criteria
                            (mapconcat (lambda (c) (format "- [ ] %s" c))
                                       acceptance-criteria "\n")
                          "- [ ] ")))
    ;; Ensure project directory exists
    (unless (file-directory-p project-dir)
      (make-directory project-dir t))
    ;; Check if file already exists
    (when (file-exists-p file-path)
      (error "TODO already exists: %s" file-path))
    ;; Create the TODO file
    (with-temp-file file-path
      (insert (format ":PROPERTIES:
:ID: %s
:PROJECT_NAME: %s
:PROJECT_ROOT: %s
:STATUS: draft
:CREATED: %s
:END:
#+title: %s
#+filetags: :todo:%s:

** Task Description
%s

** Acceptance Criteria
%s

** Progress Log

"
                      id-timestamp
                      project-name
                      (expand-file-name project-root)
                      date-stamp
                      title
                      project-name
                      (or description "")
                      criteria-text)))
    ;; Update org-roam database
    (when (fboundp 'org-roam-db-update-file)
      (org-roam-db-update-file file-path))
    ;; Return JSON with file info
    (json-encode
     `((file . ,file-path)
       (id . ,id-timestamp)
       (title . ,title)
       (project . ,project-name)
       (status . "draft")))))

(defun org-roam-todo-mcp-update-acceptance (criteria &optional todo-id)
  "Update or add acceptance criteria items.
CRITERIA is a list of (text . checked) pairs.
TODO-ID can be a file path or title (defaults to current TODO)."
  (let ((file (org-roam-todo-mcp--resolve-todo todo-id)))
    (unless file
      (error "TODO not found: %s" (or todo-id "current")))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        ;; Find the Acceptance Criteria section
        (if (re-search-forward "^\\*\\* Acceptance Criteria" nil t)
            (let ((section-start (point))
                  (section-end (save-excursion
                                 (if (re-search-forward "^\\*\\* " nil t)
                                     (match-beginning 0)
                                   (point-max)))))
              ;; Delete existing criteria
              (forward-line 1)
              (delete-region (point) section-end)
              ;; Insert new criteria
              (dolist (item criteria)
                (let ((text (cdr (assoc 'text item)))
                      (checked (cdr (assoc 'checked item))))
                  (insert (format "- [%s] %s\n"
                                  (if (and checked (not (eq checked :json-false))) "X" " ")
                                  text))))
              (insert "\n"))
          ;; Create section if it doesn't exist
          (goto-char (point-max))
          (insert "\n** Acceptance Criteria\n")
          (dolist (item criteria)
            (let ((text (cdr (assoc 'text item)))
                  (checked (cdr (assoc 'checked item))))
              (insert (format "- [%s] %s\n"
                              (if (and checked (not (eq checked :json-false))) "X" " ")
                              text))))
          (insert "\n"))
        (save-buffer)))
    "Updated acceptance criteria"))

;;;; MCP Tool Registrations

;; These are registered when claude-mcp is loaded
(with-eval-after-load 'claude-mcp
  ;; Read-only tools (safe)
  (claude-mcp-deftool todo-current
    "Get the TODO assigned to the current session/worktree. Returns full task details including content."
    :function #'org-roam-todo-mcp-get-current
    :safe t
    :args ())

  (claude-mcp-deftool todo-acceptance-criteria
    "Get acceptance criteria for a TODO. Returns array of {text, checked} objects."
    :function #'org-roam-todo-mcp-get-acceptance-criteria
    :safe t
    :args ((todo-id string "TODO identifier (file path or title). Defaults to current TODO.")))

  (claude-mcp-deftool todo-list
    "List all TODOs, optionally filtered by project."
    :function #'org-roam-todo-mcp-list
    :safe t
    :args ((project string "Optional project name to filter by")))

  ;; Mutating tools (unsafe)
  (claude-mcp-deftool todo-add-progress
    "Add a timestamped progress entry to a TODO's Progress Log section."
    :function #'org-roam-todo-mcp-add-progress
    :safe nil
    :args ((message string :required "Progress message to add")
           (todo-id string "TODO identifier (file path or title). Defaults to current TODO.")))

  (claude-mcp-deftool todo-update-status
    "Update the status of a TODO."
    :function #'org-roam-todo-mcp-update-status
    :safe nil
    :args ((status string :required "New status: draft, active, done, or rejected")
           (todo-id string "TODO identifier (file path or title). Defaults to current TODO.")))

  (claude-mcp-deftool todo-check-acceptance
    "Check or uncheck an acceptance criteria item."
    :function #'org-roam-todo-mcp-check-acceptance
    :safe nil
    :args ((item-text string :required "Text of the acceptance criteria item to find")
           (checked boolean "Whether to check (true) or uncheck (false). Defaults to true.")
           (todo-id string "TODO identifier (file path or title). Defaults to current TODO.")))

  (claude-mcp-deftool todo-update-acceptance
    "Replace all acceptance criteria with new items."
    :function #'org-roam-todo-mcp-update-acceptance
    :safe nil
    :args ((criteria array :required "Array of {text: string, checked: boolean} objects")
           (todo-id string "TODO identifier (file path or title). Defaults to current TODO.")))

  (claude-mcp-deftool todo-create
    "Create a new TODO for a project. Returns the created TODO's file path and metadata."
    :function #'org-roam-todo-mcp-create
    :safe nil
    :args ((project-root string :required "Path to the project root directory")
           (title string :required "Title of the TODO")
           (description string "Optional task description")
           (acceptance-criteria array "Optional array of acceptance criteria strings"))))

;;;; Global Keybindings

;; Define prefix keymaps for notes commands
(defvar org-roam-todo-global-map (make-sparse-keymap)
  "Keymap for global TODO commands (C-c n t).")

(defvar org-roam-todo-project-map (make-sparse-keymap)
  "Keymap for project-scoped TODO commands (C-c n p).")

;; Set up the global TODO keymap (C-c n t):
;; C-c n t t -> capture TODO
;; C-c n t l -> list all TODOs
;; C-c n t w -> select TODO, create worktree
;; C-c n t c -> select TODO, start Claude
(define-key org-roam-todo-global-map (kbd "t") #'org-roam-todo-capture)
(define-key org-roam-todo-global-map (kbd "l") #'org-roam-todo-list)
(define-key org-roam-todo-global-map (kbd "w") #'org-roam-todo-select-worktree)
(define-key org-roam-todo-global-map (kbd "c") #'org-roam-todo-start-claude)

;; Set up the project-scoped keymap (C-c n p):
;; C-c n p t -> capture TODO (infers project)
;; C-c n p l -> list project TODOs
;; C-c n p w -> select project TODO, create worktree
;; C-c n p c -> select project TODO, start Claude
(define-key org-roam-todo-project-map (kbd "t") #'org-roam-todo-capture-project)
(define-key org-roam-todo-project-map (kbd "l") #'org-roam-todo-list-project)
(define-key org-roam-todo-project-map (kbd "w") #'org-roam-todo-select-worktree-project)
(define-key org-roam-todo-project-map (kbd "c") #'org-roam-todo-start-claude-project)

;;;###autoload
(defun org-roam-todo-setup-global-keybindings ()
  "Set up global keybindings for TODO management.
Binds:
  C-c n t t - Capture a new TODO
  C-c n t l - List all TODOs
  C-c n t w - Select TODO, create/open worktree
  C-c n t c - Select TODO, start Claude agent
  C-c n p t - Capture TODO (project inferred)
  C-c n p l - List project TODOs
  C-c n p w - Select project TODO, create worktree
  C-c n p c - Select project TODO, start Claude"
  (interactive)
  ;; Create C-c n prefix if it doesn't exist
  (unless (keymapp (lookup-key global-map (kbd "C-c n")))
    (define-key global-map (kbd "C-c n") (make-sparse-keymap)))
  ;; Bind C-c n t to global TODO map
  (define-key global-map (kbd "C-c n t") org-roam-todo-global-map)
  ;; Bind C-c n p to project map
  (define-key global-map (kbd "C-c n p") org-roam-todo-project-map)
  (message "TODO keybindings set up: C-c n t (global), C-c n p (project)"))

;; Auto-setup keybindings when loaded
(with-eval-after-load 'todo
  (org-roam-todo-setup-global-keybindings))

(provide 'todo)
;;; todo.el ends here
