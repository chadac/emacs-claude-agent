;;; todo.el --- Org-roam TODO management with Claude integration -*- lexical-binding: t; -*-
;; Author: chadac <chad@cacrawford.org>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.0"))
;; Keywords: org-roam todo ai emacs llm tools
;; URL: https://github.com/chadac/emacs-claude-agent;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Org-roam based TODO management with optional Claude integration.
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
;;   :WORKTREE_MODEL: opus | sonnet | haiku (model for worktree agent)
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
(declare-function claude-agent-run "claude-agent")
(declare-function claude-mcp-deftool "claude-mcp")
(declare-function claude-sessions--get-all-sessions "claude-sessions")
(declare-function claude-sessions--get-session-status "claude-sessions")
(declare-function claude-sessions--format-status "claude-sessions")
(declare-function magit-status "magit-status")
(declare-function magit-status-setup-buffer "magit-status")
(declare-function magit-get-mode-buffer "magit-mode")
(declare-function org-roam-todo-merge-run "todo-merge")
(declare-function org-roam-todo-merge--detect-main-branch "todo-merge")
;;;; Customization

(defgroup org-roam-todo nil
  "Org-roam TODO management with Claude integration."
  :group 'org-roam
  :group 'claude-agent)

(defcustom org-roam-todo-worktree-directory
  (expand-file-name "worktrees" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "Base directory for storing worktrees.
Worktrees are created as {this-dir}/{project-name}/{branch-slug}/"
  :type 'directory
  :group 'org-roam-todo)

(defcustom org-roam-todo-done-hook nil
  "Hook run when marking a TODO as done via `org-roam-todo-mark-done'.
Functions are called with no arguments in the context of the TODO buffer.
Use this for project-specific cleanup, notifications, or GitHub issue closing."
  :type 'hook
  :group 'org-roam-todo)

(defcustom org-roam-todo-auto-commit t
  "Whether to automatically commit changes when marking a TODO as done.
When non-nil, `org-roam-todo-mark-done' will commit all changes in the
worktree before closing it."
  :type 'boolean
  :group 'org-roam-todo)

(defcustom org-roam-todo-auto-push t
  "Whether to automatically push after committing when marking a TODO as done.
Only has effect when `org-roam-todo-auto-commit' is also non-nil."
  :type 'boolean
  :group 'org-roam-todo)

(defcustom org-roam-todo-branch-prefix "feat"
  "Prefix for generated branch names.
Branch names are formatted as {prefix}/{slug}.
For example, with prefix \"chadac\", a TODO titled \"Add feature\"
would create branch \"chadac/add_feature\"."
  :type 'string
  :group 'org-roam-todo)

(defcustom org-roam-todo-worktree-base-branch nil
  "Base branch/ref for new worktree branches.
When non-nil, new branches are created from this ref (e.g. \"origin/main\").
When nil, branches from the current HEAD.
Can be set per-project via .dir-locals.el."
  :type '(choice (const :tag "Current HEAD" nil) string)
  :group 'org-roam-todo)

(defcustom org-roam-todo-worktree-fetch-before-create t
  "Whether to run `git fetch' before creating a worktree.
When non-nil, fetches from the remote before creating the worktree
to ensure the base branch is up-to-date.
Can be set per-project via .dir-locals.el."
  :type 'boolean
  :group 'org-roam-todo)
(defcustom org-roam-todo-agent-allowed-tools
  '("Read(**)"
    "Glob(**)"
    "Grep(**)"
    "Bash(git *)"
    "Bash(npm *)"
    "Bash(npx *)"
    "Bash(yarn *)"
    "Bash(pnpm *)"
    "Bash(make *)"
    "Bash(cargo *)"
    "Bash(uv *)"
    "Bash(pytest *)"
    "Bash(python *)"
    "Bash(ruff *)"
    "Bash(ls *)"
    "Bash(find *)"
    "Bash(cat *)"
    "Bash(head *)"
    "Bash(tail *)"
    "Bash(grep *)"
    "Bash(rg *)"
    "Bash(wc *)"
    "Bash(diff *)"
    "Bash(tree *)"
    "mcp__emacs__read_file"
    "mcp__emacs__read_buffer"
    "mcp__emacs__magit_status"
    "mcp__emacs__magit_diff"
    "mcp__emacs__magit_log"
    "mcp__emacs__magit_stage"
    "mcp__emacs__magit_commit_propose"
    "mcp__emacs__todo_current"
    "mcp__emacs__todo_add_progress"
    "mcp__emacs__todo_check_acceptance"
    "mcp__emacs__todo_update_status"
    "mcp__emacs__kb_search"
    "mcp__emacs__kb_get"
    "mcp__emacs__edit"
    "mcp__emacs__unlock"
    "mcp__emacs__request_attention")
  "Base list of tools to pre-authorize for TODO worktree agents.
These are combined with `org-roam-todo-agent-allowed-tools-extra' at runtime.
These tools will be allowed without permission prompts, enabling
more autonomous operation.  Uses Claude Code permission pattern syntax:
- ToolName(**) for recursive file access
- Bash(pattern*) for specific bash commands
- mcp__server__tool for MCP tools
Note: mcp__emacs__lock is NOT in this list because it is added
dynamically with path-scoping at worktree dispatch time.
Edit/Write are also excluded since agents must use lock/edit/unlock."
  :type '(repeat string)
  :group 'org-roam-todo)

(defcustom org-roam-todo-agent-allowed-tools-extra '()
  "Additional allowed tools, appended to `org-roam-todo-agent-allowed-tools'.
Intended for use in .dir-locals.el so worktree-specific tools can be
added without overwriting the base list.
Same format as `org-roam-todo-agent-allowed-tools'."
  :type '(repeat string)
  :safe #'listp
  :group 'org-roam-todo)

(defun org-roam-todo--effective-agent-allowed-tools ()
  "Return the effective allowed tools list (base + extra).
Combines `org-roam-todo-agent-allowed-tools' and
`org-roam-todo-agent-allowed-tools-extra'."
  (append org-roam-todo-agent-allowed-tools
          org-roam-todo-agent-allowed-tools-extra))

(defcustom org-roam-todo-worktree-copy-patterns
  '(".claude/settings.local.json" ".dir-locals.el" ".envrc")
  "List of file paths (relative to project root) to copy to new worktrees.
These files are copied after worktree creation to preserve permissions
and settings.  Supports glob patterns like \".claude/*.json\".

Common files to copy:
- .claude/settings.local.json - Claude Code local permissions"
  :type '(repeat string)
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

(defun org-roam-todo--reorder-with-default (items default)
  "Reorder ITEMS to put DEFAULT at the front if present.
Uses `file-truename' for path comparison to handle symlinks."
  (if (and default items)
      (let ((normalized-default (file-truename (expand-file-name default))))
        (cons default
              (seq-remove (lambda (item)
                            (string= (file-truename (expand-file-name item))
                                     normalized-default))
                          items)))
    items))

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
        (let ((ordered-projects (org-roam-todo--reorder-with-default projects inferred)))
          (completing-read "Project: " ordered-projects nil t nil nil inferred))
      (read-directory-name "Git project root: " inferred))))

(defun org-roam-todo--project-name (project-root)
  "Get short project name from PROJECT-ROOT."
  (file-name-nondirectory (directory-file-name project-root)))

(defun org-roam-todo--org-roam-projects-dir ()
  "Return the expanded path to the org-roam projects directory."
  (expand-file-name "projects" org-roam-directory))

(defun org-roam-todo--is-org-roam-projects-path-p (path)
  "Return non-nil if PATH is inside the org-roam projects directory.
This detects paths like ~/org-roam/projects/my-project/ which are
org-roam storage directories, not actual git repositories."
  (let ((expanded (directory-file-name (expand-file-name path)))
        (projects-dir (directory-file-name (org-roam-todo--org-roam-projects-dir))))
    (string-prefix-p (concat projects-dir "/") (concat expanded "/"))))

(defun org-roam-todo--resolve-project-root (project-root)
  "Resolve PROJECT-ROOT to an actual git repository path.
If PROJECT-ROOT points to an org-roam projects directory (e.g.
~/org-roam/projects/my-project/), attempt to find the real git
repository root by:
1. Checking existing TODOs with the same project name for a valid root
2. Searching projectile known projects for a name match
3. Falling back to PROJECT-ROOT if no resolution is found.

Always returns an expanded, resolved path."
  (let* ((expanded (expand-file-name project-root))
         (is-org-roam-path (org-roam-todo--is-org-roam-projects-path-p expanded)))
    (if (and is-org-roam-path
             (not (file-directory-p (expand-file-name ".git" expanded))))
        ;; This is an org-roam projects dir, not a real git repo -- resolve it
        (let* ((project-name (file-name-nondirectory (directory-file-name expanded)))
               (resolved (or
                          ;; Strategy 1: Look through existing TODOs for the same
                          ;; project with a valid PROJECT_ROOT
                          (org-roam-todo--find-project-root-from-todos project-name)
                          ;; Strategy 2: Search projectile known projects
                          (org-roam-todo--find-project-root-from-projectile project-name))))
          (if resolved
              (progn
                (message "Resolved project root: %s -> %s" expanded resolved)
                resolved)
            (display-warning 'org-roam-todo
                             (format "Could not resolve org-roam project path %s to a real git repo"
                                     expanded)
                             :warning)
            expanded))
      expanded)))

(defun org-roam-todo--find-project-root-from-todos (project-name)
  "Search existing TODOs for PROJECT-NAME and return a valid PROJECT_ROOT.
Returns nil if no valid root is found."
  (let ((nodes (org-roam-db-query
                [:select [nodes:file]
                 :from nodes
                 :where (and (like nodes:file "%/todo-%.org")
                             (= nodes:level 0))])))
    (cl-loop for (file) in nodes
             when (file-exists-p file)
             do (with-temp-buffer
                  (insert-file-contents file nil 0 2000)
                  (let ((name (when (re-search-forward "^:PROJECT_NAME:\\s-*\\(.+\\)$" nil t)
                                (string-trim (match-string 1))))
                        (root (progn
                                (goto-char (point-min))
                                (when (re-search-forward "^:PROJECT_ROOT:\\s-*\\(.+\\)$" nil t)
                                  (string-trim (match-string 1))))))
                    (when (and name root
                               (string= name project-name)
                               (not (org-roam-todo--is-org-roam-projects-path-p root))
                               (file-directory-p (expand-file-name ".git" root)))
                      (cl-return (expand-file-name root))))))))

(defun org-roam-todo--find-project-root-from-projectile (project-name)
  "Search projectile known projects for one matching PROJECT-NAME.
Returns the project root or nil."
  (when (and (fboundp 'projectile-known-projects)
             (projectile-known-projects))
    (cl-loop for proj in (projectile-known-projects)
             when (and (string= (file-name-nondirectory (directory-file-name proj))
                                project-name)
                       (file-directory-p (expand-file-name ".git" proj)))
             return (expand-file-name proj))))

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
    (format "%s/%s" org-roam-todo-branch-prefix slug)))

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
  (let* ((project-root (org-roam-todo--resolve-project-root
                        (or project-root (org-roam-todo--select-project))))
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
:WORKTREE_MODEL: opus
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
  (let* ((script-dir (or (and (fboundp 'claude--package-root) (claude--package-root))
                         (file-name-directory (or load-file-name buffer-file-name
                                                  (locate-library "claude-agent")))))
         (script-path (expand-file-name "scripts/pretrust-directory.py" script-dir))
         (expanded-path (expand-file-name worktree-path)))
    (if (file-exists-p script-path)
        (let ((result (call-process "uv" nil nil nil
                                    "run" "--directory" script-dir
                                    script-path expanded-path)))
          (if (= result 0)
              (message "Pre-trusted worktree: %s" expanded-path)
            (message "Warning: Failed to pre-trust worktree (exit %d)" result)))
      (message "Warning: pretrust-directory.py not found at %s" script-path))))

(defun org-roam-todo--translate-worktree-settings (project-root worktree-path)
  "Translate .claude/settings.local.json paths for WORKTREE-PATH.
Reads settings from PROJECT-ROOT, rewrites permission patterns that
reference PROJECT-ROOT to use WORKTREE-PATH instead, and writes the
result to the worktree.  This is a no-op if the source file doesn't
exist."
  (let* ((script-dir (or (and (fboundp 'claude--package-root) (claude--package-root))
                         (file-name-directory (or load-file-name buffer-file-name
                                                  (locate-library "claude-agent")))))
         (script-path (expand-file-name "scripts/translate-settings.py" script-dir))
         (expanded-root (directory-file-name (expand-file-name project-root)))
         (expanded-wt (directory-file-name (expand-file-name worktree-path))))
    (if (file-exists-p script-path)
        (let ((result (call-process "uv" nil "*org-roam-todo-worktree-output*" nil
                                    "run" "--directory" script-dir
                                    script-path expanded-root expanded-wt)))
          (if (= result 0)
              (message "Translated settings.local.json for worktree: %s" expanded-wt)
            (message "Warning: Failed to translate settings (exit %d)" result)))
      (message "Warning: translate-settings.py not found at %s" script-path))))

(defun org-roam-todo--expand-glob-pattern (pattern directory)
  "Expand glob PATTERN in DIRECTORY, returning list of matching files.
If PATTERN contains no glob characters, returns a list with just that path
if the file exists."
  (let ((full-pattern (expand-file-name pattern directory)))
    (if (string-match-p "[*?\\[]" pattern)
        ;; Has glob characters - use file-expand-wildcards
        (file-expand-wildcards full-pattern t)
      ;; No glob - just check if file exists
      (if (file-exists-p full-pattern)
          (list full-pattern)
        nil))))

(defun org-roam-todo--copy-files-to-worktree (project-root worktree-path)
  "Copy configured permission files from PROJECT-ROOT to WORKTREE-PATH.
Files are specified in `org-roam-todo-worktree-copy-patterns'.
Missing source files are silently skipped."
  (let ((copied-count 0))
    (dolist (pattern org-roam-todo-worktree-copy-patterns)
      (let ((matching-files (org-roam-todo--expand-glob-pattern pattern project-root)))
        (dolist (src matching-files)
          (let* ((relative-path (file-relative-name src project-root))
                 (dst (expand-file-name relative-path worktree-path)))
            (condition-case err
                (progn
                  ;; Ensure destination directory exists
                  (make-directory (file-name-directory dst) t)
                  ;; Copy the file (overwrite if exists)
                  (copy-file src dst t)
                  (cl-incf copied-count)
                  (message "Copied %s to worktree" relative-path))
              (error
               (message "Warning: Failed to copy %s: %s" relative-path (error-message-string err))))))))
    (when (> copied-count 0)
      (message "Copied %d permission file(s) to worktree" copied-count))))

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

(defun org-roam-todo--write-worktree-dir-locals (worktree-path project-root)
  "Write .dir-locals.el in WORKTREE-PATH to confine agents.
Sets auto-reject rules to prevent editing files in PROJECT-ROOT
and adds a system prompt reminding agents to stay in the worktree.
If a .dir-locals.el already exists (e.g., copied from main repo),
it is merged with the new settings."
  (let* ((expanded-wt (directory-file-name (expand-file-name worktree-path)))
         (expanded-pr (directory-file-name (expand-file-name project-root)))
         (dir-locals-file (expand-file-name ".dir-locals.el" worktree-path))
         (system-prompt
          (format "CRITICAL: You are working in a git worktree at %s/\n\
All file edits MUST be made within this worktree directory.\n\
Do NOT edit, read, or reference files in the main repository at %s/.\n\
If you see paths pointing to the main repo, translate them to the worktree equivalent."
                  expanded-wt expanded-pr))
         (reject-rules
          `((:path-prefix ,(concat expanded-pr "/")
             :message ,(format "REJECTED: This file is in the main repository (%s/). You must edit files in the worktree at %s/ instead. Translate the path accordingly."
                               expanded-pr expanded-wt))))
         (new-settings
          `((nil . ((claude-agent-extra-system-prompt . ,system-prompt)
                    (claude-agent-auto-reject-rules-extra . ,reject-rules)
                    (claude-agent-system-hooks
                     . ((:name "todo-reminder"
                         :trigger "every_n"
                         :interval 10
                         :elisp-fn "(claude-agent--todo-acceptance-reminder)")))))))
         ;; Load existing .dir-locals.el if present
         (existing-settings
          (when (file-exists-p dir-locals-file)
            (with-temp-buffer
              (insert-file-contents dir-locals-file)
              (condition-case nil
                  (read (current-buffer))
                (error nil)))))
         ;; Merge: append new nil-mode settings to existing
         (merged-settings
          (if existing-settings
              (let ((existing-nil-alist (cdr (assq nil existing-settings)))
                    (new-nil-alist (cdr (assq nil new-settings)))
                    (other-entries (assq-delete-all nil (copy-alist existing-settings))))
                ;; Combine nil-mode entries (new overrides existing for same keys)
                (let ((combined nil))
                  (dolist (pair new-nil-alist)
                    (push pair combined))
                  (dolist (pair existing-nil-alist)
                    (unless (assq (car pair) combined)
                      (push pair combined)))
                  (append other-entries `((nil . ,combined)))))
            new-settings)))
    (with-temp-file dir-locals-file
      (insert ";;; Directory Local Variables for worktree agent confinement  -*- no-byte-compile: t; -*-\n")
      (insert ";;; Auto-generated by org-roam-todo--write-worktree-dir-locals\n")
      (insert (format "%S\n" merged-settings)))))

(defun org-roam-todo--create-worktree (project-root branch-name worktree-path)
  "Create a git worktree at WORKTREE-PATH for BRANCH-NAME from PROJECT-ROOT.
Creates the branch if it doesn't exist.  Optionally fetches first
if `org-roam-todo-worktree-fetch-before-create' is non-nil, and uses
`org-roam-todo-worktree-base-branch' as the start point for new branches.
Also copies permission files configured in
`org-roam-todo-worktree-copy-patterns' and translates any hardcoded
paths in .claude/settings.local.json."
  (let ((default-directory project-root))
    ;; Fetch from remote if configured
    (when org-roam-todo-worktree-fetch-before-create
      (message "Fetching from remote...")
      (let ((result (call-process "git" nil "*org-roam-todo-worktree-output*" nil
                                  "fetch")))
        (unless (= 0 result)
          (message "Warning: git fetch failed (see *org-roam-todo-worktree-output*)"))))
    ;; Ensure parent directory exists
    (make-directory (file-name-directory worktree-path) t)
    ;; Create worktree (with new branch if needed)
    (if (org-roam-todo--branch-exists-p project-root branch-name)
        ;; Branch exists, just create worktree
        (let ((result (call-process "git" nil "*org-roam-todo-worktree-output*" nil
                                    "worktree" "add" worktree-path branch-name)))
          (unless (= 0 result)
            (error "Failed to create worktree: see *org-roam-todo-worktree-output*")))
      ;; Create new branch with worktree, optionally from base branch
      (let* ((args (if org-roam-todo-worktree-base-branch
                       (list "worktree" "add" "-b" branch-name
                             worktree-path org-roam-todo-worktree-base-branch)
                     (list "worktree" "add" "-b" branch-name worktree-path)))
             (result (apply #'call-process "git" nil "*org-roam-todo-worktree-output*" nil args)))
        (unless (= 0 result)
          (error "Failed to create worktree with new branch: see *org-roam-todo-worktree-output*"))))
    ;; Copy permission files to the new worktree
    (org-roam-todo--copy-files-to-worktree project-root worktree-path)
    ;; Translate paths in .claude/settings.local.json for the worktree
    (org-roam-todo--translate-worktree-settings project-root worktree-path)))

(defun org-roam-todo--ensure-worktree-from-plist (todo)
  "Ensure a worktree exists for TODO plist, creating one if needed.
Returns the worktree path.  If the worktree already exists, returns
its path immediately.  If not, prompts for branch name and creates it.
Updates the TODO file properties accordingly."
  (let* ((project-root (org-roam-todo--resolve-project-root
                        (plist-get todo :project-root)))
         (file (plist-get todo :file))
         (existing-worktree (plist-get todo :worktree-path))
         (title (plist-get todo :title))
         (default-branch (org-roam-todo--default-branch-name
                          (or title "feature")))
         (branch-name (or (plist-get todo :worktree-branch)
                          (read-string "Branch name: " default-branch)))
         (worktree-path (or existing-worktree
                            (org-roam-todo--worktree-path
                             project-root branch-name))))
    (unless project-root
      (user-error "No PROJECT_ROOT property found"))
    ;; Create worktree if needed
    (unless (org-roam-todo--worktree-exists-p worktree-path)
      (message "Creating worktree at %s..." worktree-path)
      (let ((org-roam-todo-worktree-base-branch
             org-roam-todo-worktree-base-branch)
            (org-roam-todo-worktree-fetch-before-create
             org-roam-todo-worktree-fetch-before-create))
        (with-temp-buffer
          (setq default-directory (file-name-as-directory project-root))
          (hack-dir-local-variables-non-file-buffer)
          (when (local-variable-p 'org-roam-todo-worktree-base-branch)
            (setq org-roam-todo-worktree-base-branch
                  (buffer-local-value
                   'org-roam-todo-worktree-base-branch
                   (current-buffer))))
          (when (local-variable-p
                 'org-roam-todo-worktree-fetch-before-create)
            (setq org-roam-todo-worktree-fetch-before-create
                  (buffer-local-value
                   'org-roam-todo-worktree-fetch-before-create
                   (current-buffer)))))
        (org-roam-todo--create-worktree
         project-root branch-name worktree-path))
      ;; Write .dir-locals.el for worktree agent confinement
      (org-roam-todo--write-worktree-dir-locals worktree-path project-root)
      ;; Store worktree info in the TODO file
      (with-current-buffer (find-file-noselect file)
        (org-roam-todo--set-property "WORKTREE_PATH" worktree-path)
        (org-roam-todo--set-property "WORKTREE_BRANCH" branch-name)
        (unless (member (org-roam-todo--get-property "STATUS")
                        '("active" "review"))
          (org-roam-todo--set-property "STATUS" "active"))
        (save-buffer)))
    worktree-path))

(defun org-roam-todo--find-agent-buffer (worktree-path)
  "Find an existing Claude agent buffer for WORKTREE-PATH.
Returns the buffer or nil."
  (let ((expanded-path (expand-file-name worktree-path)))
    (cl-find-if
     (lambda (buf)
       (with-current-buffer buf
         (and (boundp 'claude-agent--work-dir)
              claude-agent--work-dir
              (string= (expand-file-name claude-agent--work-dir)
                       expanded-path))))
     (buffer-list))))

;;;; TODO Query & Selection

(defconst org-roam-todo-status-order
  '("draft" "active" "review" "done" "rejected")
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
            (insert-file-contents file nil 0 3000) ; Read header + properties
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
                                     (match-string 1))))
                  (worktree-branch (progn
                                     (goto-char (point-min))
                                     (when (re-search-forward "^:WORKTREE_BRANCH:\\s-*\\(.+\\)$" nil t)
                                       (match-string 1))))
                  (worktree-model (progn
                                    (goto-char (point-min))
                                    (when (re-search-forward "^:WORKTREE_MODEL:\\s-*\\(.+\\)$" nil t)
                                      (match-string 1))))
                  )
              (when (and project
                         (or (null project-filter)
                             (string= project project-filter)
                             ;; Also match filter's directory basename against project name
                             ;; (handles case where filter is a path like ~/org-roam/projects/foo/)
                             (string= project
                                      (file-name-nondirectory
                                       (directory-file-name (expand-file-name project-filter))))
                             ;; Normalize paths: expand ~ and remove trailing slashes
                             (and project-root
                                  (string= (directory-file-name (expand-file-name project-root))
                                           (directory-file-name (expand-file-name project-filter))))))
                (push (list :id id
                            :title title
                            :project project
                            :project-root project-root
                            :status (or status "draft")
                            :file file
                            :created (or created "")
                            :worktree-path worktree-path
                            :worktree-branch worktree-branch
                            :worktree-model worktree-model)
                      todos)))))))
    ;; Sort by status order, then by created date (newest first)
    (sort todos
          (lambda (a b)
            (let ((status-a (org-roam-todo--status-sort-key (plist-get a :status)))
                  (status-b (org-roam-todo--status-sort-key (plist-get b :status))))
              (if (= status-a status-b)
                  (string> (plist-get a :created) (plist-get b :created))
                (< status-a status-b)))))))

(defun org-roam-todo--read-commit-message (file)
  "Read the commit message from TODO FILE's Commit Message section.
Returns the content between #+begin_src and #+end_src, or nil if not found."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* Commit Message" nil t)
        (when (re-search-forward "^#\\+begin_src" nil t)
          (forward-line 1)
          (let ((start (point)))
            (when (re-search-forward "^#\\+end_src" nil t)
              (string-trim (buffer-substring-no-properties start (line-beginning-position))))))))))
(defun org-roam-todo--status-sort-key (status)
  "Return sort key for STATUS (lower = first)."
  (or (cl-position (or status "draft") org-roam-todo-status-order :test #'string=) 99))

(defun org-roam-todo--completing-read (&optional project-filter prompt status-filter)
  "Select a TODO using completion.
PROJECT-FILTER limits to a specific project.
PROMPT is the prompt string.
STATUS-FILTER limits to specific status(es) - can be a string or list of strings.
Returns the TODO plist."
  (let* ((todos (org-roam-todo--query-todos project-filter))
         ;; Apply status filter if provided
         (todos (if status-filter
                    (let ((statuses (if (listp status-filter) status-filter (list status-filter))))
                      (cl-remove-if-not (lambda (todo)
                                          (member (plist-get todo :status) statuses))
                                        todos))
                  todos))
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

(defun org-roam-todo--send-task-to-buffer (buffer-name content worktree-path &optional _delay)
  "Send task CONTENT to BUFFER-NAME by queuing it for the agent.
WORKTREE-PATH is included in the message for context.
_DELAY is ignored (kept for API compatibility); messages are queued
and sent automatically when the agent is ready."
  (let ((buffer (get-buffer buffer-name)))
    (if (not buffer)
        (message "ERROR: Buffer %s not found" buffer-name)
      (with-current-buffer buffer
        (let ((msg (format "[WORKTREE TASK]\n\n%s\n\nWorktree: %s\nPlease help me with this task."
                           content worktree-path)))
          ;; Use the agent's message queue so it sends when ready
          (push msg claude-agent--message-queue)
          ;; If already ready, send immediately
          (when (and claude-agent--process
                     (process-live-p claude-agent--process)
                     (not (claude-agent--is-busy-p)))
            (claude-agent--send-next-queued))
          (message "Task queued for %s" buffer-name))))))
;;;###autoload
(defun org-roam-todo-create-worktree ()
  "Create a worktree for the current TODO and spawn a Claude session.
Use this for feature work that benefits from isolation.
If the worktree and session already exist, sends the task to the existing session."
  (interactive)
  (unless (org-roam-todo--node-p)
    (user-error "Not in an org-roam TODO node"))
  (require 'claude-agent)
  (let* ((project-root (org-roam-todo--resolve-project-root
                        (org-roam-todo--get-property "PROJECT_ROOT")))
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
         ;; Check for existing claude-agent buffer for this worktree
         (expanded-path (expand-file-name worktree-path))
         (existing-buffer (cl-find-if
                           (lambda (buf)
                             (with-current-buffer buf
                               (and (boundp 'claude-agent--work-dir)
                                    claude-agent--work-dir
                                    (string= (expand-file-name claude-agent--work-dir)
                                             expanded-path))))
                           (buffer-list))))
    (unless project-root
      (user-error "No PROJECT_ROOT property found"))
    ;; Create worktree if needed
    (unless (org-roam-todo--worktree-exists-p worktree-path)
      (message "Creating worktree at %s..." worktree-path)
      ;; Load project's .dir-locals.el so per-project defcustoms
      ;; (e.g. org-roam-todo-worktree-base-branch) take effect
      (let ((org-roam-todo-worktree-base-branch org-roam-todo-worktree-base-branch)
            (org-roam-todo-worktree-fetch-before-create org-roam-todo-worktree-fetch-before-create))
        (with-temp-buffer
          (setq default-directory (file-name-as-directory project-root))
          (hack-dir-local-variables-non-file-buffer)
          (when (local-variable-p 'org-roam-todo-worktree-base-branch)
            (setq org-roam-todo-worktree-base-branch
                  (buffer-local-value 'org-roam-todo-worktree-base-branch (current-buffer))))
          (when (local-variable-p 'org-roam-todo-worktree-fetch-before-create)
            (setq org-roam-todo-worktree-fetch-before-create
                  (buffer-local-value 'org-roam-todo-worktree-fetch-before-create (current-buffer)))))
        (org-roam-todo--create-worktree project-root branch-name worktree-path))
      ;; Write .dir-locals.el for worktree agent confinement
      (org-roam-todo--write-worktree-dir-locals worktree-path project-root)
      ;; Store worktree info in node
      (org-roam-todo--set-property "WORKTREE_PATH" worktree-path)
      (org-roam-todo--set-property "WORKTREE_BRANCH" branch-name))
    ;; Update status
    (org-roam-todo--set-property "STATUS" "active")
    (save-buffer)
    ;; Check if session already exists
    (if existing-buffer
        (let ((buffer-name (buffer-name existing-buffer)))
          ;; Session exists - send task immediately (no delay needed)
          (org-roam-todo--send-task-to-buffer buffer-name content worktree-path)
          (pop-to-buffer existing-buffer)
          (message "Sent task to existing session: %s" buffer-name))
      ;; New session - pre-trust and spawn with TODO-specific allowed tools
      ;; Add path-scoped mcp__emacs__lock for worktree directory
      (org-roam-todo--pre-trust-worktree worktree-path)
      (let* ((lock-pattern (format "mcp__emacs__lock(%s*)" (expand-file-name worktree-path)))
             (all-tools (append (org-roam-todo--effective-agent-allowed-tools) (list lock-pattern)))
             (worktree-model (org-roam-todo--get-property "WORKTREE_MODEL"))
             (buf (claude-agent-run worktree-path nil nil nil worktree-model all-tools))
             (buffer-name (buffer-name buf)))
        ;; Queue task - will be sent when agent emits "ready"
        (org-roam-todo--send-task-to-buffer buffer-name content worktree-path)
        (message "Created worktree and spawned Claude session: %s" buffer-name)))))
;;;; Select TODO → Create Worktree

;;;###autoload
(defun org-roam-todo-select-worktree (&optional project-filter)
  "Select a draft TODO and create/open its worktree with a Claude session.
Optional PROJECT-FILTER limits selection to a specific project.
Only shows TODOs with status 'draft' since active/done/rejected don't need worktrees."
  (interactive)
  (let* ((todo (org-roam-todo--completing-read project-filter
                                               "Create worktree for TODO: "
                                               "draft"))
         (file (plist-get todo :file)))
    (unless todo
      (user-error "No draft TODOs found"))
    ;; Open the TODO file and run create-worktree
    (find-file file)
    (org-roam-todo-create-worktree)))

;;;###autoload
(defun org-roam-todo-select-worktree-project ()
  "Select a TODO from current project and create/open its worktree."
  (interactive)
  (org-roam-todo-select-worktree (org-roam-todo--infer-project)))

;;;; Close Worktree

(defun org-roam-todo--kill-worktree-buffers (worktree-path)
  "Kill all buffers visiting files in WORKTREE-PATH."
  (let ((expanded-path (expand-file-name worktree-path))
        (killed 0))
    (dolist (buf (buffer-list))
      (when-let ((file (buffer-file-name buf)))
        (when (string-prefix-p expanded-path (expand-file-name file))
          (kill-buffer buf)
          (cl-incf killed))))
    killed))

(defun org-roam-todo--kill-magit-buffers (worktree-path)
  "Kill all magit-related buffers for WORKTREE-PATH.
Returns the number of buffers killed."
  (let ((expanded-path (file-name-as-directory (expand-file-name worktree-path)))
        (killed 0))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (derived-mode-p 'magit-mode)
                     default-directory
                     (string-prefix-p expanded-path
                                      (file-name-as-directory
                                       (expand-file-name default-directory))))
            (kill-buffer buf)
            (cl-incf killed)))))
    killed))

(defun org-roam-todo--kill-todo-buffer (file)
  "Kill the buffer visiting the org TODO FILE, if any.
Returns non-nil if a buffer was killed."
  (when file
    (let ((buf (find-buffer-visiting file)))
      (when buf
        (with-current-buffer buf
          ;; Save before killing to avoid losing changes
          (when (buffer-modified-p) (save-buffer)))
        (kill-buffer buf)
        t))))

(defun org-roam-todo--kill-claude-session (worktree-path)
  "Kill any Claude agent session associated with WORKTREE-PATH.
Returns non-nil if a session was found and killed."
  (let ((expanded-path (expand-file-name worktree-path))
        (found nil))
    (dolist (buf (buffer-list) found)
      (when (and (buffer-live-p buf) (not found))
        (with-current-buffer buf
          (when (and (boundp 'claude-agent--work-dir)
                     claude-agent--work-dir
                     (string= (expand-file-name claude-agent--work-dir)
                              expanded-path)
                     (boundp 'claude-agent--process)
                     claude-agent--process)
            (when (process-live-p claude-agent--process)
              (delete-process claude-agent--process))
            (kill-buffer buf)
            (setq found t)))))))

(defun org-roam-todo--remove-worktree (project-root worktree-path &optional force)
  "Remove worktree at WORKTREE-PATH from PROJECT-ROOT.
If FORCE is non-nil, use --force flag."
  (let ((default-directory project-root)
        (args (if force
                  (list "worktree" "remove" "--force" worktree-path)
                (list "worktree" "remove" worktree-path))))
    (apply #'call-process "git" nil "*org-roam-todo-worktree-output*" nil args)))

(defun org-roam-todo--delete-branch (project-root branch-name &optional force)
  "Delete BRANCH-NAME from PROJECT-ROOT.
If FORCE is non-nil, use -D instead of -d."
  (let ((default-directory project-root)
        (flag (if force "-D" "-d")))
    (call-process "git" nil "*org-roam-todo-worktree-output*" nil
                  "branch" flag branch-name)))

(defun org-roam-todo--git-has-changes-p (directory)
  "Return non-nil if DIRECTORY has uncommitted git changes."
  (let ((default-directory directory))
    (not (string-empty-p
          (string-trim
           (shell-command-to-string "git status --porcelain"))))))

(defun org-roam-todo--git-commit-all (directory message)
  "Stage all changes in DIRECTORY and commit with MESSAGE.
Returns t on success, nil on failure."
  (let ((default-directory directory))
    (and (= 0 (call-process "git" nil "*org-roam-todo-git-output*" nil
                            "add" "-A"))
         (= 0 (call-process "git" nil "*org-roam-todo-git-output*" nil
                            "commit" "-m" message)))))

(defun org-roam-todo--git-push (directory)
  "Push current branch in DIRECTORY to origin.
Returns t on success, nil on failure."
  (let ((default-directory directory))
    (= 0 (call-process "git" nil "*org-roam-todo-git-output*" nil
                       "push" "-u" "origin" "HEAD"))))

(defun org-roam-todo--close-worktree-plist (todo &optional force)
  "Close the worktree described by TODO plist.
TODO should have :worktree-path, :project-root, :file, :title, and :status.
FORCE if non-nil, force-removes worktree without prompting.
Returns a list of result strings describing what was done.
Continues through all steps even if individual steps fail."
  (let* ((worktree-path (plist-get todo :worktree-path))
         (project-root (plist-get todo :project-root))
         (file (plist-get todo :file))
         (title (plist-get todo :title))
         (current-status (plist-get todo :status))
         (branch-name (when file
                        (with-temp-buffer
                          (insert-file-contents file nil 0 2000)
                          (when (re-search-forward "^:WORKTREE_BRANCH:\\s-*\\(.+\\)$" nil t)
                            (match-string 1)))))
         (results '()))
    ;; Kill Claude session
    (condition-case err
        (when (org-roam-todo--kill-claude-session worktree-path)
          (push "killed Claude session" results))
      (error (push (format "FAILED to kill Claude session: %s" (error-message-string err)) results)))
    ;; Kill file buffers
    (condition-case err
        (let ((killed (org-roam-todo--kill-worktree-buffers worktree-path)))
          (when (> killed 0)
            (push (format "killed %d buffer(s)" killed) results)))
      (error (push (format "FAILED to kill buffers: %s" (error-message-string err)) results)))
    ;; Kill magit buffers for the worktree
    (condition-case err
        (let ((killed (org-roam-todo--kill-magit-buffers worktree-path)))
          (when (> killed 0)
            (push (format "killed %d magit buffer(s)" killed) results)))
      (error (push (format "FAILED to kill magit buffers: %s" (error-message-string err)) results)))
    ;; Remove worktree if it exists
    (condition-case err
        (if (org-roam-todo--worktree-exists-p worktree-path)
            (let ((result (org-roam-todo--remove-worktree project-root worktree-path force)))
              (if (= 0 result)
                  (push "removed worktree" results)
                ;; Try force removal
                (if (or force
                        (yes-or-no-p "Worktree has uncommitted changes. Force remove? "))
                    (let ((force-result (org-roam-todo--remove-worktree project-root worktree-path t)))
                      (if (= 0 force-result)
                          (push "force-removed worktree" results)
                        (push "FAILED to remove worktree" results)))
                  (push "skipped worktree removal" results))))
          (push "worktree already gone" results))
      (error (push (format "FAILED worktree removal: %s" (error-message-string err)) results)))
    ;; Delete branch if it exists
    (condition-case err
        (when (and branch-name
                   project-root
                   (org-roam-todo--branch-exists-p project-root branch-name))
          (let ((result (org-roam-todo--delete-branch project-root branch-name)))
            (if (= 0 result)
                (push (format "deleted branch '%s'" branch-name) results)
              ;; Unmerged - prompt for force delete
              (if (or force
                      (yes-or-no-p (format "Branch '%s' is not fully merged. Force delete? " branch-name)))
                  (progn
                    (org-roam-todo--delete-branch project-root branch-name t)
                    (push (format "force-deleted branch '%s'" branch-name) results))
                (push (format "skipped branch deletion '%s'" branch-name) results)))))
      (error (push (format "FAILED branch deletion: %s" (error-message-string err)) results)))
    ;; Clear worktree properties and mark done in the file
    (condition-case err
        (when file
          (with-current-buffer (find-file-noselect file)
            (save-excursion
              (goto-char (point-min))
              ;; Delete WORKTREE_PATH line
              (when (re-search-forward "^:WORKTREE_PATH:\\s-*.+\n" nil t)
                (replace-match ""))
              (goto-char (point-min))
              ;; Delete WORKTREE_BRANCH line
              (when (re-search-forward "^:WORKTREE_BRANCH:\\s-*.+\n" nil t)
                (replace-match ""))
              (goto-char (point-min))
              ;; Mark as done (unless already done/rejected)
              (unless (member current-status '("done" "rejected"))
                (when (re-search-forward "^:STATUS:\\s-*.+$" nil t)
                  (replace-match ":STATUS: done"))))
            (save-buffer)))
      (error (push (format "FAILED to update TODO file: %s" (error-message-string err)) results)))
    ;; Close the org TODO buffer (save first, then kill)
    (condition-case err
        (when (org-roam-todo--kill-todo-buffer file)
          (push "closed TODO buffer" results))
      (error (push (format "FAILED to close TODO buffer: %s" (error-message-string err)) results)))
    (nreverse results)))

;;;###autoload
(defun org-roam-todo-close-worktree (&optional force)
  "Close the worktree associated with the current TODO.
Removes the worktree, kills associated buffers and Claude session.
With prefix arg FORCE, force removal even with uncommitted changes.
Also deletes the branch (prompting if unmerged) and marks TODO as done."
  (interactive "P")
  (unless (org-roam-todo--node-p)
    (user-error "Not in an org-roam TODO node"))
  (let* ((project-root (org-roam-todo--get-property "PROJECT_ROOT"))
         (worktree-path (org-roam-todo--get-property "WORKTREE_PATH"))
         (current-status (org-roam-todo--get-property "STATUS"))
         (title (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\+title: \\(.+\\)$" nil t)
                    (match-string 1)))))
    (unless worktree-path
      (user-error "No worktree associated with this TODO"))
    (let* ((todo (list :worktree-path worktree-path
                       :project-root project-root
                       :file (buffer-file-name)
                       :title title
                       :status current-status))
           (results (org-roam-todo--close-worktree-plist todo force)))
      (message "Closed worktree for '%s': %s"
               (or title "TODO") (string-join results ", ")))))

;;;###autoload
(defun org-roam-todo-mark-done (&optional skip-commit)
  "Mark the current TODO as done, with optional worktree cleanup.
Runs `org-roam-todo-done-hook' first.
If a worktree exists:
- Commits changes if `org-roam-todo-auto-commit' is non-nil
- Pushes if `org-roam-todo-auto-push' is non-nil
- Prompts to delete worktree and close related buffers
With prefix arg SKIP-COMMIT, skip the auto-commit/push step."
  (interactive "P")
  (unless (org-roam-todo--node-p)
    (user-error "Not in an org-roam TODO node"))
  (let* ((worktree-path (org-roam-todo--get-property "WORKTREE_PATH"))
         (project-root (org-roam-todo--get-property "PROJECT_ROOT"))
         (current-status (org-roam-todo--get-property "STATUS"))
         (title (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\+title: \\(.+\\)$" nil t)
                    (match-string 1)))))
    ;; Run done hooks first
    (run-hooks 'org-roam-todo-done-hook)
    ;; Handle worktree - commit/push first, then cleanup
    (when (and worktree-path
               (org-roam-todo--worktree-exists-p worktree-path))
      ;; Auto-commit if enabled and not skipped
      (when (and org-roam-todo-auto-commit
                 (not skip-commit)
                 (org-roam-todo--git-has-changes-p worktree-path))
        (let ((commit-msg (format "Complete TODO: %s\n\n🤖 Generated with Claude Code"
                                  (or title "task"))))
          (if (org-roam-todo--git-commit-all worktree-path commit-msg)
              (progn
                (message "Committed changes in worktree")
                ;; Auto-push if enabled
                (when org-roam-todo-auto-push
                  (message "Pushing to origin...")
                  (if (org-roam-todo--git-push worktree-path)
                      (message "Pushed to origin successfully")
                    (message "Warning: Push failed - you may need to push manually"))))
            (message "Warning: Commit failed - changes not committed"))))
      ;; Now handle worktree cleanup
      (when (yes-or-no-p "Delete associated worktree and close buffers? ")
        (let* ((todo (list :worktree-path worktree-path
                           :project-root project-root
                           :file (buffer-file-name)
                           :title title
                           :status current-status))
               (results (org-roam-todo--close-worktree-plist todo)))
          (message "Worktree cleanup: %s" (string-join results ", ")))))
    ;; If no worktree, just mark as done
    (unless (and worktree-path
                 (org-roam-todo--worktree-exists-p worktree-path))
      (unless (member current-status '("done" "rejected"))
        (org-roam-todo--set-property "STATUS" "done"))
      (save-buffer))
    (message "Marked TODO as done")))

;;;###autoload
(defun org-roam-todo-select-close-worktree (&optional project-filter)
  "Select a TODO with a worktree and close it.
Optional PROJECT-FILTER limits selection to a specific project.
Uses TODO data directly without needing to visit the org file."
  (interactive)
  (let* ((todos (cl-remove-if-not
                 (lambda (todo)
                   (plist-get todo :worktree-path))
                 (org-roam-todo--query-todos project-filter)))
         (todo (when todos
                 (let* ((candidates (mapcar (lambda (td)
                                              (cons (format "[%s] %s"
                                                            (plist-get td :project)
                                                            (plist-get td :title))
                                                    td))
                                            todos))
                        (choice (completing-read "Close worktree for: "
                                                 (mapcar #'car candidates)
                                                 nil t)))
                   (cdr (assoc choice candidates))))))
    (unless todo
      (user-error "No TODOs with worktrees found"))
    (unless (yes-or-no-p (format "Close worktree for '%s'? " (plist-get todo :title)))
      (user-error "Cancelled"))
    (let ((results (org-roam-todo--close-worktree-plist todo)))
      (message "Closed worktree for '%s': %s"
               (plist-get todo :title) (string-join results ", ")))))

;;;; Start Claude on TODO

(defun org-roam-todo--send-initial-message (buffer-name todo content)
  "Send initial task message to Claude agent in BUFFER-NAME.
TODO is the todo plist, CONTENT is the full TODO content."
  (when-let ((buffer (get-buffer buffer-name)))
    (with-current-buffer buffer
      (when (and (boundp 'claude-agent--process)
                 claude-agent--process
                 (process-live-p claude-agent--process))
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
          (process-send-string
           claude-agent--process
           (concat (json-encode `((type . "message") (text . ,msg))) "\n")))))))

;;;###autoload
(defun org-roam-todo-start-claude (&optional project-filter)
  "Select a TODO and start a Claude agent to work on it.
Optional PROJECT-FILTER limits selection to a specific project.
If TODO has a worktree, starts agent there; otherwise uses project root."
  (interactive)
  (require 'claude-agent)
  (let* ((todo (org-roam-todo--completing-read project-filter "Start Claude on TODO: "))
         (worktree (plist-get todo :worktree-path))
         (project-root (plist-get todo :project-root))
         ;; Use worktree if it exists, otherwise use project root
         (work-dir (if (and worktree (file-directory-p worktree))
                       worktree
                     project-root))
         (title (plist-get todo :title))
         (content (org-roam-todo--get-full-content (plist-get todo :file))))
    (unless todo
      (user-error "No TODO selected"))
    (let* ((buf (claude-agent-run work-dir))
           (buffer-name (buffer-name buf)))
      ;; Update status in the TODO file
      (with-current-buffer (find-file-noselect (plist-get todo :file))
        (org-roam-todo--set-property "STATUS" "active")
        (save-buffer))
      ;; Send task message after delay for Claude to initialize
      (run-with-timer 5 nil
                      #'org-roam-todo--send-initial-message
                      buffer-name todo content)
      (message "Started Claude agent for: %s" title))))

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

(defface org-roam-todo-status-review
  '((t :foreground "#4488ff" :weight bold))
  "Face for review status (agent completed, awaiting user review)."
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
    ("review" 'org-roam-todo-status-review)
    ("done" 'org-roam-todo-status-done)
    ("rejected" 'org-roam-todo-status-rejected)
    (_ 'default)))

(defun org-roam-todo--format-status (status)
  "Format STATUS with appropriate face."
  (propertize (or status "draft") 'face (org-roam-todo--status-face status)))

(defvar-local org-roam-todo-list--project-filter nil
  "Current project filter for the TODO list buffer.")

(defun org-roam-todo-list--claude-status (todo)
  "Get the Claude agent status string for TODO.
Returns a propertized string: ready, thinking, waiting, typing, dead, or empty."
  (let ((worktree-path (plist-get todo :worktree-path)))
    (cond
     ;; Check for live agent session matching this worktree
     (worktree-path
      (let ((expanded-wt (expand-file-name worktree-path))
            (status nil))
        (when (fboundp 'claude-sessions--get-all-sessions)
          (dolist (session (claude-sessions--get-all-sessions))
            (when (string= (expand-file-name
                            (plist-get session :real-directory))
                           (directory-file-name expanded-wt))
              (setq status (plist-get session :status)))))
        (if status
            (claude-sessions--format-status status)
          "—")))
     ;; No worktree
     (t ""))))

(defun org-roam-todo-list--format-worktree (worktree-path)
  "Format worktree indicator for WORKTREE-PATH.
Shows a marker if the worktree exists on disk, a warning if path is set
but directory is missing, or empty if no worktree."
  (cond
   ((and worktree-path (org-roam-todo--worktree-exists-p worktree-path))
    (propertize "✓ exists" 'face 'success))
   (worktree-path
    (propertize "✗ missing" 'face 'warning))
   (t "")))

(defun org-roam-todo-list--get-entries ()
  "Get tabulated list entries for TODOs.
Columns: Status, Title, Claude, Project, Worktree, Created."
  (mapcar
   (lambda (todo)
     (let ((title (plist-get todo :title))
           (project (plist-get todo :project))
           (status (plist-get todo :status))
           (created (plist-get todo :created))
           (file (plist-get todo :file))
           (worktree-path (plist-get todo :worktree-path)))
       (list file
             (vector
              (org-roam-todo--format-status status)
              (propertize (or title "Untitled") 'face 'org-roam-todo-title)
              (org-roam-todo-list--claude-status todo)
              (propertize (or project "") 'face 'org-roam-todo-project)
              (org-roam-todo-list--format-worktree worktree-path)
              (or created "")))))
   (org-roam-todo--query-todos org-roam-todo-list--project-filter)))

(defun org-roam-todo-list-refresh ()
  "Refresh the TODO list buffer."
  (interactive)
  (tabulated-list-revert))

(defun org-roam-todo-list--todo-at-point ()
  "Get the TODO plist for the entry at point in the TODO list.
Returns nil if no entry at point."
  (when-let ((file (tabulated-list-get-id)))
    (cl-find-if (lambda (todo) (string= (plist-get todo :file) file))
                (org-roam-todo--query-todos org-roam-todo-list--project-filter))))

(defun org-roam-todo-list-open ()
  "Open the TODO at point.
If the TODO has a worktree, open magit-status for it.
Otherwise, open the TODO org file."
  (interactive)
  (when-let ((todo (org-roam-todo-list--todo-at-point)))
    (let ((worktree-path (plist-get todo :worktree-path))
          (file (plist-get todo :file)))
      (if (and worktree-path
               (org-roam-todo--worktree-exists-p worktree-path))
          (magit-status worktree-path)
        (find-file file)))))

(defun org-roam-todo-list-open-org-file ()
  "Open the TODO org file at point (always opens the file, not magit)."
  (interactive)
  (when-let ((file (tabulated-list-get-id)))
    (find-file file)))

(defun org-roam-todo-list-magit-status ()
  "Open magit-status for the TODO's worktree at point in other-window."
  (interactive)
  (when-let ((todo (org-roam-todo-list--todo-at-point)))
    (let ((worktree-path (plist-get todo :worktree-path)))
      (if (and worktree-path
               (org-roam-todo--worktree-exists-p worktree-path))
          (let ((default-directory worktree-path))
            (magit-status-setup-buffer worktree-path)
            (display-buffer (magit-get-mode-buffer 'magit-status-mode)))
        (message "No worktree for this TODO")))))

(defun org-roam-todo-list-merge ()
  "Run the configured merge workflow for the TODO at point.
Uses the per-project workflow from `org-roam-todo-merge-workflows'
or `org-roam-todo-merge-workflow'."
  (interactive)
  (require 'todo-merge)
  (when-let ((todo (org-roam-todo-list--todo-at-point)))
    (let ((worktree-path (plist-get todo :worktree-path)))
      (unless worktree-path
        (user-error "This TODO has no worktree"))
      (org-roam-todo-merge-run todo))))

(defun org-roam-todo-list-open-worktree ()
  "Create a worktree for the TODO at point (if needed) and open magit-status.
If the worktree already exists, opens magit-status directly."
  (interactive)
  (when-let ((todo (org-roam-todo-list--todo-at-point)))
    (let ((worktree-path (org-roam-todo--ensure-worktree-from-plist todo)))
      (magit-status worktree-path)
      (org-roam-todo-list-refresh))))

(defun org-roam-todo-list-spawn-agent ()
  "Create a worktree for the TODO at point (if needed) and spawn an agent.
If the worktree already exists, reuses it.  If an agent session already
exists for the worktree, switches to it instead of spawning a new one."
  (interactive)
  (require 'claude-agent)
  (when-let ((todo (org-roam-todo-list--todo-at-point)))
    (let* ((worktree-path (org-roam-todo--ensure-worktree-from-plist todo))
           (content (org-roam-todo--get-full-content (plist-get todo :file)))
           (existing-buffer (org-roam-todo--find-agent-buffer worktree-path)))
      (if existing-buffer
          ;; Agent exists - switch to it
          (progn
            (pop-to-buffer existing-buffer)
            (message "Switched to existing agent: %s"
                     (buffer-name existing-buffer)))
        ;; Spawn new agent
        (org-roam-todo--pre-trust-worktree worktree-path)
        (let* ((lock-pattern (format "mcp__emacs__lock(%s*)"
                                     (expand-file-name worktree-path)))
               (all-tools (append (org-roam-todo--effective-agent-allowed-tools)
                                  (list lock-pattern)))
               (worktree-model (plist-get todo :worktree-model))
               (buf (claude-agent-run worktree-path nil nil nil worktree-model
                                      all-tools))
               (buffer-name (buffer-name buf)))
          ;; Queue task - will be sent when agent emits "ready"
          (org-roam-todo--send-task-to-buffer
           buffer-name content worktree-path)
          (message "Spawned agent: %s" buffer-name)))
      (org-roam-todo-list-refresh))))

(defun org-roam-todo-list-help ()
  "Show available keybindings in a transient-style popup.
The popup captures input: pressing a command key dismisses the popup
and executes the command in the TODO list buffer.  C-g just closes."
  (interactive)
  (let* ((todo-buf (current-buffer))
         (help-buf (get-buffer-create "*todo-list-help*"))
         ;; Commands that can be dispatched from the help popup
         (dispatch-keys '(("RET" . org-roam-todo-list-open-org-file)
                          ("w" . org-roam-todo-list-open-worktree)
                          ("c" . org-roam-todo-list-spawn-agent)
                          ("M" . org-roam-todo-list-magit-status)
                          ("m" . org-roam-todo-list-merge)
                          ("d" . org-roam-todo-list-mark-done)
                          ("a" . org-roam-todo-list-mark-active)
                          ("r" . org-roam-todo-list-mark-rejected)
                          ("u" . org-roam-todo-list-mark-draft)
                          ("g" . org-roam-todo-list-refresh)
                          ("q" . quit-window))))
    (with-current-buffer help-buf
      (let ((inhibit-read-only t)
            (map (make-sparse-keymap)))
        (erase-buffer)
        (insert
         (propertize "TODO List Commands\n" 'face 'bold)
         (propertize "──────────────────\n\n" 'face 'shadow)
         (propertize "Navigation & Actions\n" 'face '(:weight bold :underline t))
         "  "
         (propertize "RET" 'face 'help-key-binding)
         "   Open TODO org file\n"
         "  "
         (propertize "w" 'face 'help-key-binding)
         "     Create/open worktree (magit-status)\n"
         "  "
         (propertize "c" 'face 'help-key-binding)
         "     Create/open worktree agent\n"
         "  "
         (propertize "M" 'face 'help-key-binding)
         "     Open magit-status for worktree\n"
         "  "
         (propertize "m" 'face 'help-key-binding)
         "     Run merge/approval workflow\n"
         "\n"
         (propertize "Status\n" 'face '(:weight bold :underline t))
         "  "
         (propertize "d" 'face 'help-key-binding)
         "     Mark as done\n"
         "  "
         (propertize "a" 'face 'help-key-binding)
         "     Mark as active\n"
         "  "
         (propertize "r" 'face 'help-key-binding)
         "     Mark as rejected\n"
         "  "
         (propertize "u" 'face 'help-key-binding)
         "     Mark as draft\n"
         "\n"
         (propertize "Other\n" 'face '(:weight bold :underline t))
         "  "
         (propertize "g" 'face 'help-key-binding)
         "     Refresh list\n"
         "  "
         (propertize "?" 'face 'help-key-binding)
         "     Show this help\n"
         "  "
         (propertize "q" 'face 'help-key-binding)
         "     Quit\n")
        (goto-char (point-min))
        ;; Start with suppress-keymap to block all self-insert characters
        (suppress-keymap map t)
        ;; Create a close function for reuse
        (let ((close-fn (lambda ()
                          (interactive)
                          (let ((win (get-buffer-window help-buf)))
                            (when win (delete-window win)))
                          (kill-buffer help-buf)
                          (when (buffer-live-p todo-buf)
                            (pop-to-buffer todo-buf)))))
          ;; Build a keymap that dispatches commands back to the todo-list buffer
          (dolist (binding dispatch-keys)
            (let ((key (car binding))
                  (cmd (cdr binding)))
              (define-key map (kbd key)
                (let ((command cmd)
                      (source-buf todo-buf))
                  (lambda ()
                    (interactive)
                    (let ((win (get-buffer-window help-buf)))
                      (when win (delete-window win)))
                    (kill-buffer help-buf)
                    (when (buffer-live-p source-buf)
                      (pop-to-buffer source-buf)
                      (call-interactively command)))))))
          ;; C-g, ?, and Escape all just close
          (define-key map (kbd "C-g") close-fn)
          (define-key map (kbd "?") close-fn)
          (define-key map (kbd "<escape>") close-fn))
        ;; Suppress navigation keys, arrow keys, scrolling, etc.
        ;; This ensures ONLY our explicitly bound keys work.
        (dolist (key '("<up>" "<down>" "<left>" "<right>"
                       "C-n" "C-p" "C-f" "C-b" "C-a" "C-e"
                       "C-v" "M-v" "C-l"
                       "C-x" "C-c" "M-x"
                       "<prior>" "<next>" "<home>" "<end>"
                       "<C-up>" "<C-down>" "<C-left>" "<C-right>"
                       "<M-up>" "<M-down>" "<M-left>" "<M-right>"
                       "j" "k" "h" "l"  ; vim-style nav (not bound as commands)
                       "n" "p"           ; common nav keys
                       "C-x o" "C-x b" "C-x k"))
          ;; Only suppress if not already bound to a dispatch command
          (unless (lookup-key map (kbd key))
            (define-key map (kbd key) 'ignore)))
        ;; Catch-all for any remaining undefined keys
        (define-key map [t] 'ignore)
        (use-local-map map)
        (setq-local mode-line-format
                    (propertize " TODO List Help — press a key or C-g to close"
                                'face 'mode-line-emphasis))
        (setq buffer-read-only t
              cursor-type nil)))
    ;; Display and select the help window
    (let ((win (display-buffer help-buf
                               '((display-buffer-in-side-window)
                                 (side . bottom)
                                 (window-height . fit-window-to-buffer)
                                 (dedicated . t)))))
      (select-window win)
      ;; Prevent switching away
      (set-window-dedicated-p win t))))

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

;;;; TODO List Auto-Refresh (for live Claude status)

(defvar org-roam-todo-list--refresh-timer nil
  "Timer for automatic refresh of the TODO list buffer.")

(defcustom org-roam-todo-list-refresh-interval 3.0
  "Interval in seconds between automatic refreshes of the TODO list buffer."
  :type 'number
  :group 'org-roam-todo)

(defun org-roam-todo-list--start-auto-refresh ()
  "Start the auto-refresh timer for the TODO list buffer."
  (org-roam-todo-list--stop-auto-refresh)
  (setq org-roam-todo-list--refresh-timer
        (run-with-timer org-roam-todo-list-refresh-interval
                        org-roam-todo-list-refresh-interval
                        #'org-roam-todo-list--auto-refresh)))

(defun org-roam-todo-list--stop-auto-refresh ()
  "Stop the auto-refresh timer."
  (when org-roam-todo-list--refresh-timer
    (cancel-timer org-roam-todo-list--refresh-timer)
    (setq org-roam-todo-list--refresh-timer nil)))

(defconst org-roam-todo-list--column-format
  [("Status" 12 (lambda (a b)
                  (< (org-roam-todo--status-sort-key (aref (cadr a) 0))
                     (org-roam-todo--status-sort-key (aref (cadr b) 0)))))
   ("Title" 50 t)
   ("Claude" 10 t)
   ("Created" 12 t)
   ("Project" 20 t)]
  "Column format for TODO list buffers.")

(defun org-roam-todo-list--safe-revert ()
  "Revert the current TODO list buffer, re-initializing format if stale.
Must be called with the TODO list buffer current."
  (let ((pos (point)))
    ;; Re-set the format in case columns changed since buffer was created
    (unless (equal tabulated-list-format org-roam-todo-list--column-format)
      (setq tabulated-list-format org-roam-todo-list--column-format)
      (tabulated-list-init-header))
    (tabulated-list-revert)
    (goto-char (min pos (point-max)))))

(defun org-roam-todo-list-refresh-all ()
  "Refresh all TODO list buffers regardless of visibility.
Call this after operations that change TODO state (e.g., merge, cleanup)."
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (derived-mode-p 'org-roam-todo-list-mode)))
      (with-current-buffer buffer
        (org-roam-todo-list--safe-revert)))))

(defun org-roam-todo-list--auto-refresh ()
  "Auto-refresh callback that only refreshes if a TODO list buffer is visible."
  (let ((found nil))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (derived-mode-p 'org-roam-todo-list-mode))
                 (get-buffer-window buffer 'visible))
        (setq found t)
        (with-current-buffer buffer
          (org-roam-todo-list--safe-revert))))
    (unless found
      (org-roam-todo-list--stop-auto-refresh))))

(defun org-roam-todo-list-close-worktree ()
  "Close the worktree for the TODO at point.
Removes the worktree, kills associated buffers and Claude session,
deletes the branch, and marks the TODO as done.
Works directly from the TODO list view without needing to visit the file."
  (interactive)
  (when-let ((file (tabulated-list-get-id)))
    (let* ((todos (org-roam-todo--query-todos org-roam-todo-list--project-filter))
           (todo (cl-find-if (lambda (td) (string= (plist-get td :file) file)) todos))
           (worktree-path (plist-get todo :worktree-path))
           (title (plist-get todo :title)))
      (unless worktree-path
        (user-error "No worktree associated with this TODO"))
      (unless (yes-or-no-p (format "Close worktree for '%s'? " title))
        (user-error "Cancelled"))
      (let ((results (org-roam-todo--close-worktree-plist todo)))
        ;; Refresh the list
        (org-roam-todo-list-refresh)
        (message "Closed worktree for '%s': %s"
                 title (string-join results ", "))))))

;;;; TODO List Keybindings

(defvar org-roam-todo-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-roam-todo-list-open-org-file)
    (define-key map (kbd "w") #'org-roam-todo-list-open-worktree)
    (define-key map (kbd "c") #'org-roam-todo-list-spawn-agent)
    (define-key map (kbd "M") #'org-roam-todo-list-magit-status)
    (define-key map (kbd "m") #'org-roam-todo-list-merge)
    (define-key map (kbd "g") #'org-roam-todo-list-refresh)
    (define-key map (kbd "d") #'org-roam-todo-list-mark-done)
    (define-key map (kbd "r") #'org-roam-todo-list-mark-rejected)
    (define-key map (kbd "a") #'org-roam-todo-list-mark-active)
    (define-key map (kbd "u") #'org-roam-todo-list-mark-draft)
    (define-key map (kbd "x") #'org-roam-todo-list-close-worktree)
    (define-key map (kbd "TAB") #'org-roam-todo-list-cycle-status)
    (define-key map (kbd "?") #'org-roam-todo-list-help)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `org-roam-todo-list-mode'.")

;; Evil mode support
(with-eval-after-load 'evil
  (evil-define-key 'normal org-roam-todo-list-mode-map
    (kbd "RET") #'org-roam-todo-list-open-org-file
    (kbd "w") #'org-roam-todo-list-open-worktree
    (kbd "c") #'org-roam-todo-list-spawn-agent
    (kbd "M") #'org-roam-todo-list-magit-status
    (kbd "m") #'org-roam-todo-list-merge
    (kbd "gr") #'org-roam-todo-list-refresh
    (kbd "d") #'org-roam-todo-list-mark-done
    (kbd "r") #'org-roam-todo-list-mark-rejected
    (kbd "a") #'org-roam-todo-list-mark-active
    (kbd "u") #'org-roam-todo-list-mark-draft
    (kbd "x") #'org-roam-todo-list-close-worktree
    (kbd "TAB") #'org-roam-todo-list-cycle-status
    (kbd "?") #'org-roam-todo-list-help
    (kbd "q") #'quit-window))

(define-derived-mode org-roam-todo-list-mode tabulated-list-mode "Org-Roam-TODOs"
  "Major mode for viewing and managing org-roam project TODOs.
\\{org-roam-todo-list-mode-map}"
  (setq tabulated-list-format org-roam-todo-list--column-format)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Status" . nil))
  (setq tabulated-list-entries #'org-roam-todo-list--get-entries)
  (tabulated-list-init-header)
  ;; Auto-refresh for live Claude status updates
  (add-hook 'kill-buffer-hook #'org-roam-todo-list--stop-auto-refresh nil t)
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (if (get-buffer-window (current-buffer) 'visible)
                  (org-roam-todo-list--start-auto-refresh)
                (org-roam-todo-list--stop-auto-refresh)))
            nil t))

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

;;;; Minor Mode & Transient Integration

;; Forward declaration for claude-menu
(declare-function claude-menu "claude-transient")

(defvar org-roam-todo-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Bind C-c c to the unified Claude menu
    (define-key map (kbd "C-c c") #'claude-menu)
    ;; Bind C-c C-d to mark TODO as done
    (define-key map (kbd "C-c C-d") #'org-roam-todo-mark-done)
    map)
  "Keymap for `org-roam-todo-mode'.

Press C-c c to show the Claude menu.
Press C-c C-d to mark the TODO as done.")

;;;###autoload
(define-minor-mode org-roam-todo-mode
  "Minor mode for org-roam TODO nodes.

\\{org-roam-todo-mode-map}"
  :lighter " OrgTODO"
  :keymap org-roam-todo-mode-map
  (if org-roam-todo-mode
      ;; Mode enabled - add C-c C-c handler buffer-locally
      (add-hook 'org-ctrl-c-ctrl-c-hook #'org-roam-todo--ctrl-c-ctrl-c-handler nil t)
    ;; Mode disabled - remove handler
    (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-roam-todo--ctrl-c-ctrl-c-handler t)))

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

(defun org-roam-todo--ctrl-c-ctrl-c-handler ()
  "Handle C-c C-c in org-roam TODO buffers.
If the buffer has :WORKTREE: t property, create a worktree.
Returns non-nil if handled, nil otherwise."
  (when (and (derived-mode-p 'org-mode)
             org-roam-todo-mode
             (org-roam-todo--node-p))
    (let ((worktree-prop (org-roam-todo--get-property "WORKTREE")))
      (when (and worktree-prop
                 (string= (downcase worktree-prop) "t"))
        ;; Only trigger if no worktree exists yet
        (unless (org-roam-todo--get-property "WORKTREE_PATH")
          (require 'claude-agent)
          (org-roam-todo-create-worktree)
          t)))))  ; Return non-nil to indicate we handled it

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
           (expanded-cwd (directory-file-name (expand-file-name cwd)))
           (todos (org-roam-todo--query-todos)))
      (plist-get
       (cl-find-if
        (lambda (todo)
          (let ((wpath (plist-get todo :worktree-path)))
            (and wpath
                 (string= (directory-file-name (expand-file-name wpath))
                           expanded-cwd))))
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
          (let ((todo (cl-find-if (lambda (td) (string= (plist-get td :file) file)) todos)))
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
STATUS should be one of: draft, active, review, done, rejected.
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
CHECKED defaults to t (check the item).  Pass :json-false or nil to uncheck.
TODO-ID can be a file path or title (defaults to current TODO)."
  ;; Handle the checked parameter:
  ;; - nil (not provided) -> default to t (check the item)
  ;; - :json-false (MCP sends this for false) -> uncheck
  ;; - t or any truthy value -> check
  (let ((should-check (cond
                       ((null checked) t)  ; Default to checking
                       ((eq checked :json-false) nil)  ; MCP false
                       (t checked)))  ; Use provided value
        (file (org-roam-todo-mcp--resolve-todo todo-id)))
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
                      (re-search-forward "^- \\[\\([ X]\\)\\] \\(.+\\)$" section-end t))
            (when (string-match-p (regexp-quote item-text) (match-string 2))
              (setq found t)
              ;; Save the full line info before modifying
              (let ((line-start (line-beginning-position))
                    (current-state (match-string 1)))
                (goto-char line-start)
                ;; Only modify if state actually needs to change
                (if should-check
                    (when (string= current-state " ")
                      (when (re-search-forward "\\[ \\]" (line-end-position) t)
                        (replace-match "[X]")))
                  (when (string= current-state "X")
                    (when (re-search-forward "\\[X\\]" (line-end-position) t)
                      (replace-match "[ ]")))))))
          (unless found
            (error "Acceptance criteria item not found: %s" item-text))
          (save-buffer))))
    (format "%s: %s" (if should-check "Checked" "Unchecked") item-text)))

(defun org-roam-todo-mcp-create (project-root title &optional description acceptance-criteria model)
  "Create a new TODO programmatically.
PROJECT-ROOT is the path to the project.
TITLE is the TODO title.
DESCRIPTION is optional task description text.
ACCEPTANCE-CRITERIA is an optional list of criteria strings.
MODEL is the worktree model to use (defaults to \"sonnet\").
Returns JSON with the created TODO's file path and ID."
  (unless project-root
    (error "project_root is required"))
  (unless title
    (error "title is required"))
  ;; Resolve org-roam project paths to actual git repo roots
  (setq project-root (org-roam-todo--resolve-project-root project-root))
  (let* ((project-name (org-roam-todo--project-name project-root))
         (project-dir (expand-file-name (concat "projects/" project-name) org-roam-directory))
         (slug (org-roam-todo--slugify title))
         (id-timestamp (format "%s%04x" (format-time-string "%Y%m%dT%H%M%S") (random 65536)))
         (date-stamp (format-time-string "%Y-%m-%d"))
         (file-path (expand-file-name (format "todo-%s.org" slug) project-dir))
         (worktree-model (or model "sonnet"))
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
:WORKTREE_MODEL: %s
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
                      worktree-model
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

(defun org-roam-todo-mcp-complete (&optional summary commit-message unsafe-ignore-unstaged)
  "Mark the current TODO as ready for review.
SUMMARY is a description of what was accomplished.
COMMIT-MESSAGE is the proposed commit message for the merge workflow.
UNSAFE-IGNORE-UNSTAGED if non-nil, skips the unstaged changes check.
This will:
1. Verify there are staged changes (agent must stage files manually)
2. Fail if there are unstaged changes (unless UNSAFE-IGNORE-UNSTAGED)
3. Rebase onto the upstream branch (e.g. main)
4. Run pre-commit hooks if they exist (fail if hooks fail)
5. Propose a commit via magit for user approval
6. Set TODO status to 'review'
7. Store the COMMIT-MESSAGE on the TODO for the merge workflow
The user will then review the commit and run the merge workflow."
  (let* ((cwd (or (bound-and-true-p claude-session-cwd)
                  (bound-and-true-p claude--cwd)
                  default-directory))
         (expanded-cwd (directory-file-name (expand-file-name cwd)))
         (todos (org-roam-todo--query-todos))
         (todo (cl-find-if
                (lambda (td)
                  (let ((wpath (plist-get td :worktree-path)))
                    (and wpath
                         (string= (directory-file-name (expand-file-name wpath))
                                  expanded-cwd))))
                todos))
         (file (plist-get todo :file))
         (title (plist-get todo :title))
         (project-root (plist-get todo :project-root)))
    (unless todo
      (error "No TODO found for current worktree: %s" cwd))
    (unless commit-message
      (setq commit-message (format "Complete TODO: %s" (or title "task"))))
    ;; Check for staged changes
    (let ((default-directory cwd))
      (message "[todo-complete] Step 1: Checking staged changes...")
      (let ((staged (string-trim (shell-command-to-string "git diff --cached --name-only"))))
        (message "[todo-complete] Staged files: %s" (if (string-empty-p staged) "(none)" staged))
        (when (string-empty-p staged)
          (error "No files staged for commit.  Stage your changes with magit_stage first")))
      ;; Check for unstaged changes (unless explicitly ignored)
      (message "[todo-complete] Step 2: Checking unstaged changes...")
      (let ((unstaged-now (string-trim (shell-command-to-string "git diff --name-only"))))
        (message "[todo-complete] Unstaged files: %s" (if (string-empty-p unstaged-now) "(none)" unstaged-now))
        (unless (or unsafe-ignore-unstaged (string-empty-p unstaged-now))
          (error "Unstaged changes detected in:\n%s\n\nEither stage these files with magit_stage, or discard them if unintended.\nIf you truly want to leave them unstaged, pass unsafe_ignore_unstaged: true"
                 unstaged-now)))
      ;; Rebase onto upstream branch
      (message "[todo-complete] Step 3: Rebasing onto upstream...")
      (let ((main-branch (org-roam-todo-merge--detect-main-branch
                          (or project-root cwd))))
        (message "[todo-complete] Rebasing onto %s..." main-branch)
        (let ((result (call-process "git" nil "*org-roam-todo-rebase-output*" nil
                                    "rebase" "--autostash" main-branch)))
          (message "[todo-complete] Rebase result: %d" result)
          (unless (= 0 result)
            (call-process "git" nil nil nil "rebase" "--abort")
            (error "Rebase onto %s failed (exit %d).  Resolve conflicts and try again"
                   main-branch result)))
        ;; Re-stage any files that autostash unstashed (it doesn't preserve staging)
        (let ((unstashed (string-trim (shell-command-to-string "git diff --name-only"))))
          (message "[todo-complete] Post-rebase unstaged (from autostash): %s"
                   (if (string-empty-p unstashed) "(none)" unstashed))
          (unless (string-empty-p unstashed)
            (message "[todo-complete] Re-staging autostashed files...")
            (apply #'call-process "git" nil nil nil "add" "--"
                   (split-string unstashed "\n" t)))))
      (message "[todo-complete] Post-rebase staged: %s"
               (string-trim (shell-command-to-string "git diff --cached --name-only")))
      ;; Run pre-commit hooks if they exist
      (message "[todo-complete] Step 4: Running pre-commit hooks...")
      (let ((hook-path (expand-file-name ".git/hooks/pre-commit" cwd)))
        ;; Also check for worktree hooks via core.hooksPath
        (unless (file-exists-p hook-path)
          (let ((hooks-path (string-trim (shell-command-to-string "git config core.hooksPath 2>/dev/null"))))
            (unless (string-empty-p hooks-path)
              (setq hook-path (expand-file-name "pre-commit" hooks-path)))))
        (message "[todo-complete] Hook path: %s (exists: %s)" hook-path (file-exists-p hook-path))
        (when (file-exists-p hook-path)
          (let ((result (call-process "git" nil "*org-roam-todo-hook-output*" nil
                                      "hook" "run" "pre-commit")))
            (message "[todo-complete] Pre-commit hook result: %d" result)
            (unless (= 0 result)
              (error "Pre-commit hook failed (exit %d).  Fix the issues and try again" result)))
          ;; Check if the hooks modified any files (e.g. linter auto-formatting)
          (let ((modified (string-trim (shell-command-to-string "git diff --name-only"))))
            (message "[todo-complete] Post-hook unstaged: %s" (if (string-empty-p modified) "(none)" modified))
            (unless (string-empty-p modified)
              (error "Pre-commit hooks modified the following files:\n%s\n\nReview the changes, then stage them with magit_stage and run todo_complete again"
                     modified)))))
      ;; Propose commit via magit
      (message "[todo-complete] Step 5: Proposing commit via magit...")
      (when (fboundp 'claude-mcp-magit-commit-propose)
        (claude-mcp-magit-commit-propose commit-message cwd))
      (message "[todo-complete] Step 5 complete. Checking final git status...")
      (message "[todo-complete] Final unstaged: %s"
               (string-trim (shell-command-to-string "git diff --name-only")))
      (message "[todo-complete] Final staged: %s"
               (string-trim (shell-command-to-string "git diff --cached --name-only"))))
    ;; Set status to "review" and store commit message as org section
    (with-current-buffer (find-file-noselect file)
      (org-roam-todo--set-property "STATUS" "review")
      (save-excursion
        (goto-char (point-min))
        ;; Replace existing Commit Message section or create new one
        (if (re-search-forward "^\\*\\* Commit Message" nil t)
            (let ((section-start (point))
                  (section-end (save-excursion
                                 (if (re-search-forward "^\\*\\* " nil t)
                                     (match-beginning 0)
                                   (point-max)))))
              (forward-line 1)
              (delete-region (point) section-end)
              (insert "#+begin_src\n" commit-message "\n#+end_src\n\n"))
          ;; Insert before Progress Log if it exists, otherwise at end
          (goto-char (point-min))
          (if (re-search-forward "^\\*\\* Progress Log" nil t)
              (progn (beginning-of-line) (insert "** Commit Message\n#+begin_src\n" commit-message "\n#+end_src\n\n"))
            (goto-char (point-max))
            (insert "\n** Commit Message\n#+begin_src\n" commit-message "\n#+end_src\n"))))
      (save-buffer))
    (org-roam-todo-mcp-add-progress
     (format "Marked for review: %s" (or summary "task completed")))
    (format "TODO marked for review. Commit proposed for user approval. The user will review and run the merge workflow from the TODO list (m key).")))

(defun org-roam-todo-mcp--alist-get (key alist)
  "Get value for KEY from ALIST, trying both symbol and string keys.
MCP sends alists with string keys (e.g. (\"text\" . \"value\")) but elisp
code often uses symbol keys (e.g. (text . \"value\")).  This handles both."
  (or (cdr (assoc key alist))
      (cdr (assoc (if (symbolp key)
                      (symbol-name key)
                    (intern key))
                  alist))))

(defun org-roam-todo-mcp--insert-criteria (criteria)
  "Insert acceptance CRITERIA as org checkbox items.
Each item in CRITERIA should be an alist with `text' and `checked' keys.
Items with nil or empty text are skipped to avoid writing nils."
  (dolist (item criteria)
    (let ((text (org-roam-todo-mcp--alist-get 'text item))
          (checked (org-roam-todo-mcp--alist-get 'checked item)))
      (when (and text (stringp text) (not (string-empty-p (string-trim text))))
        (insert (format "- [%s] %s\n"
                        (if (and checked (not (eq checked :json-false))) "X" " ")
                        text))))))

(defun org-roam-todo-mcp-update-acceptance (criteria &optional todo-id)
  "Update or add acceptance criteria items.
CRITERIA is a list of alists with `text' and `checked' keys.
Keys may be symbols or strings (both are handled).
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
              (org-roam-todo-mcp--insert-criteria criteria)
              (insert "\n"))
          ;; Create section if it doesn't exist
          (goto-char (point-max))
          (insert "\n** Acceptance Criteria\n")
          (org-roam-todo-mcp--insert-criteria criteria)
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
    :needs-session-cwd t
    :args ())

  (claude-mcp-deftool todo-acceptance-criteria
    "Get acceptance criteria for a TODO. Returns array of {text, checked} objects."
    :function #'org-roam-todo-mcp-get-acceptance-criteria
    :safe t
    :needs-session-cwd t
    :args ((todo-id string "TODO identifier (file path or title). Defaults to current TODO.")))

  (claude-mcp-deftool todo-list
    "List all TODOs, optionally filtered by project."
    :function #'org-roam-todo-mcp-list
    :safe t
    :needs-session-cwd t
    :args ((project string "Optional project name to filter by")))

  ;; Mutating tools (safe - todo operations are low-risk)
  (claude-mcp-deftool todo-add-progress
    "Add a timestamped progress entry to a TODO's Progress Log section."
    :function #'org-roam-todo-mcp-add-progress
    :safe t
    :needs-session-cwd t
    :args ((message string :required "Progress message to add")
           (todo-id string "TODO identifier (file path or title). Defaults to current TODO.")))

  (claude-mcp-deftool todo-update-status
    "Update the status of a TODO."
    :function #'org-roam-todo-mcp-update-status
    :safe t
    :needs-session-cwd t
    :args ((status string :required "New status: draft, active, review, done, or rejected")
           (todo-id string "TODO identifier (file path or title). Defaults to current TODO.")))

  (claude-mcp-deftool todo-check-acceptance
    "Check or uncheck an acceptance criteria item."
    :function #'org-roam-todo-mcp-check-acceptance
    :safe t
    :needs-session-cwd t
    :args ((item-text string :required "Text of the acceptance criteria item to find")
           (checked boolean "Whether to check (true) or uncheck (false). Defaults to true.")
           (todo-id string "TODO identifier (file path or title). Defaults to current TODO.")))

  (claude-mcp-deftool todo-update-acceptance
    "Replace all acceptance criteria with new items."
    :function #'org-roam-todo-mcp-update-acceptance
    :safe t
    :needs-session-cwd t
    :args ((criteria array :required "Array of {text: string, checked: boolean} objects")
           (todo-id string "TODO identifier (file path or title). Defaults to current TODO.")))

  (claude-mcp-deftool todo-create
    "Create a new TODO for a project. Returns the created TODO's file path and metadata.
IMPORTANT: When creating TODOs, include as much detail as possible in the
description - full context, requirements, constraints, examples, and acceptance
criteria. The more detail you provide, the better the worktree agent can
autonomously implement the task without needing clarification."
    :function #'org-roam-todo-mcp-create
    :safe t
    :needs-session-cwd t
    :args ((project-root string :required "Path to the project root directory")
           (title string :required "Title of the TODO")
           (description string "Task description - be as detailed as possible with full context, requirements, and constraints")
           (acceptance-criteria array "Array of acceptance criteria strings - be specific and testable")
           (model string "Worktree model override: 'opus', 'sonnet', or 'haiku'. Defaults to 'sonnet'. Use 'opus' for complex architectural tasks.")))

  (claude-mcp-deftool todo-complete
    "Signal that your work is done and ready for user review. This will:
1. Verify files are staged (you must stage files yourself via magit_stage)
2. Fail if there are unstaged changes (you must stage or discard them)
3. Rebase onto the upstream branch (e.g. main) to ensure clean history
4. Run pre-commit hooks if they exist (fails if hooks fail OR if hooks modify files)
5. Propose a commit via magit for user approval
6. Set the TODO status to 'review'
7. Store your proposed commit message for the merge workflow
Call this when you have finished all the work for a TODO.
You MUST stage your changed files first, then provide a commit-message.
Write it as you would a real git commit message: a short summary line,
then a blank line, then bullet points describing what changed and why.
NOTE: If pre-commit hooks auto-format files, this will fail and tell you which
files were modified. Review the changes, re-stage them, and call todo_complete again."
    :function #'org-roam-todo-mcp-complete
    :safe t  ; Safe - only proposes commit, user must approve
    :needs-session-cwd t
    :args ((summary string "Brief summary of what was accomplished")
           (commit-message string :required "Proposed commit message for the merge. Write a proper git commit message: short summary line, blank line, then detailed bullet points.")
           (unsafe-ignore-unstaged boolean "If true, skip the unstaged changes check. Only use this if you intentionally want to leave files unstaged."))))
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
;; C-c n t x -> select TODO, close worktree
;; C-c n t c -> select TODO, start Claude
(define-key org-roam-todo-global-map (kbd "t") #'org-roam-todo-capture)
(define-key org-roam-todo-global-map (kbd "l") #'org-roam-todo-list)
(define-key org-roam-todo-global-map (kbd "w") #'org-roam-todo-select-worktree)
(define-key org-roam-todo-global-map (kbd "x") #'org-roam-todo-select-close-worktree)
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
  C-c n t x - Select TODO, close worktree
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
