;;; claude-agent-expert.el --- Ask-the-Expert system for Claude -*- lexical-binding: t; -*-

;; Author: Claude + Chad Crawford
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: claude-agent ai emacs llm tools expert
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides an "Ask the Expert" system for Claude agents.
;;
;; The expert is a dedicated Claude agent per-project that:
;; - Answers queries from other agents about the project
;; - Maintains a project-specific knowledge base (project-kb.org)
;; - Has restricted permissions (read-only + KB editing only)
;;
;; Architecture:
;; - Caller agents use `ask-the-expert` to send questions
;; - Expert sessions are named *claude:PROJECT:expert*
;; - Experts use `expert-respond` to send answers back
;; - Experts use `expert-kb` to manage their knowledge base
;;
;; Permission Model:
;; Expert sessions can only use:
;; - Read-only tools: read_file, grep, glob, search_buffer, etc.
;; - KB tools: lock/edit/unlock (scoped to project-kb.org only)
;; - Expert-specific: expert_respond, expert_kb

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'claude-mcp-registry)
(require 'claude-mcp-messaging)

;; Forward declarations
(declare-function claude-agent-run "claude-agent-repl")
(declare-function claude-mcp-send-message "claude-mcp-messaging")
(declare-function claude-mcp-send-and-wait "claude-mcp-messaging")

;;;; Customization

(defgroup claude-agent-expert nil
  "Ask-the-Expert system for Claude agents."
  :group 'claude-agent)

(defcustom claude-agent-expert-kb-directory
  (expand-file-name "claude-agent/expert-kb" user-emacs-directory)
  "Directory where expert knowledge bases are stored.
Each project gets a PROJECT-NAME.org file in this directory."
  :type 'directory
  :group 'claude-agent-expert)

(defcustom claude-agent-expert-timeout 300
  "Default timeout in seconds for expert responses."
  :type 'integer
  :group 'claude-agent-expert)

(defcustom claude-agent-expert-model "sonnet"
  "Model to use for expert agents."
  :type 'string
  :group 'claude-agent-expert)

;;;; Expert Session State

(defvar claude-agent-expert--pending-queries (make-hash-table :test 'equal)
  "Hash table mapping expert buffer names to pending query info.
Each entry is a plist with :caller, :query, :timestamp.")

(defvar claude-agent-expert--sessions (make-hash-table :test 'equal)
  "Hash table mapping project names to expert buffer names.")

;;;; Permission System

(defconst claude-agent-expert-allowed-tools
  '(;; Read-only tools
    "read_file"
    "grep"
    "glob"
    "search_buffer"
    "get_buffer_content"
    "buffer_info"
    "list_buffers"
    "read_buffer"
    "magit_status"
    "magit_log"
    "magit_diff"
    "magit_branch"
    "magit_show"
    "magit_blame"
    "magit_file_log"
    "magit_remote"
    "magit_tags"
    "magit_stash_list"
    "magit_rev_parse"
    ;; KB tools (scoped to KB file only - enforced separately)
    "lock_file"
    "lock_buffer"
    "lock"
    "locks"
    "edit"
    "edits"
    "unlock"
    "unlocks"
    "save_buffer"
    ;; Expert-specific tools
    "expert_respond"
    "expert_kb"
    ;; Utility tools
    "whoami"
    "list_agents"
    "send_message")
  "List of tools that expert sessions are allowed to use.")

;;;; Worktree Detection

(defun claude-agent-expert--worktree-main-repo (dir)
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

(defun claude-agent-expert--resolve-project-path (path)
  "Resolve PATH to the canonical project path.
If PATH is a git worktree, returns the main repository path.
Otherwise returns PATH expanded."
  (let* ((expanded (expand-file-name path))
         (main-repo (claude-agent-expert--worktree-main-repo expanded)))
    (or main-repo expanded)))

;;;; Session Detection

(defun claude-agent-expert--is-expert-session-p (buffer-name)
  "Return non-nil if BUFFER-NAME is an expert session."
  (and buffer-name
       (string-match-p "\\*claude:[^:]+:expert\\*" buffer-name)))

(defun claude-agent-expert--get-project-from-buffer (buffer-name)
  "Extract project name from expert BUFFER-NAME.
Returns nil if not an expert buffer."
  (when (string-match "\\*claude:\\([^:]+\\):expert\\*" buffer-name)
    (match-string 1 buffer-name)))

(defun claude-agent-expert--kb-file (project-name)
  "Return the knowledge base file path for PROJECT-NAME."
  (unless (file-directory-p claude-agent-expert-kb-directory)
    (make-directory claude-agent-expert-kb-directory t))
  (expand-file-name (format "%s.org" project-name)
                    claude-agent-expert-kb-directory))

(defun claude-agent-expert--is-kb-file-p (file-path project-name)
  "Return non-nil if FILE-PATH is the KB file for PROJECT-NAME."
  (when (and file-path project-name)
    (string= (expand-file-name file-path)
             (expand-file-name (claude-agent-expert--kb-file project-name)))))

(defun claude-agent-expert--check-permission (tool-name tool-args buffer-name)
  "Check if TOOL-NAME with TOOL-ARGS is allowed for expert BUFFER-NAME.
Returns nil if allowed, or an error message string if denied."
  (unless (claude-agent-expert--is-expert-session-p buffer-name)
    ;; Not an expert session, allow everything
    (cl-return-from claude-agent-expert--check-permission nil))

  (let ((project-name (claude-agent-expert--get-project-from-buffer buffer-name)))
    ;; Check if tool is in allowlist
    (unless (member tool-name claude-agent-expert-allowed-tools)
      (cl-return-from claude-agent-expert--check-permission
        (format "Permission denied: Expert sessions cannot use '%s'. Use expert_respond to answer queries or expert_kb to manage knowledge."
                tool-name)))

    ;; For edit tools, check that they're only targeting the KB file
    (when (member tool-name '("lock_file" "lock_buffer" "lock" "locks"
                              "edit" "edits" "unlock" "unlocks" "save_buffer"))
      (let ((file-path (or (cdr (assq 'file_path tool-args))
                           ;; For buffer-based operations, try to resolve
                           (when-let ((buf-name (cdr (assq 'buffer_name tool-args))))
                             (buffer-file-name (get-buffer buf-name))))))
        (when (and file-path
                   (not (claude-agent-expert--is-kb-file-p file-path project-name)))
          (cl-return-from claude-agent-expert--check-permission
            (format "Permission denied: Expert sessions can only edit the knowledge base file (%s), not '%s'."
                    (claude-agent-expert--kb-file project-name)
                    file-path)))))

    ;; All checks passed
    nil))

;;;; Expert Session Management

(defun claude-agent-expert--buffer-name (project-name)
  "Return the expert buffer name for PROJECT-NAME."
  (format "*claude:%s:expert*" project-name))

(defun claude-agent-expert--get-or-create (project-path)
  "Get or create an expert session for PROJECT-PATH.
If PROJECT-PATH is a git worktree, the expert is created for the main repository.
Returns the buffer name of the expert."
  (let* ((resolved-path (claude-agent-expert--resolve-project-path project-path))
         (project-name (file-name-nondirectory
                        (directory-file-name resolved-path)))
         (buffer-name (claude-agent-expert--buffer-name project-name))
         (existing-buffer (get-buffer buffer-name)))

    (if (and existing-buffer
             (with-current-buffer existing-buffer
               (and (boundp 'claude-agent--process)
                    claude-agent--process
                    (process-live-p claude-agent--process))))
        ;; Expert already exists and is running
        buffer-name
      ;; Create new expert session
      (claude-agent-expert--spawn resolved-path project-name)
      buffer-name)))

(defun claude-agent-expert--spawn (project-path project-name)
  "Spawn a new expert session for PROJECT-PATH with PROJECT-NAME."
  (let* ((expanded-path (expand-file-name project-path))
         (buffer-name (claude-agent-expert--buffer-name project-name))
         (kb-file (claude-agent-expert--kb-file project-name))
         (system-prompt (claude-agent-expert--system-prompt project-name kb-file))
         ;; Don't switch to the expert buffer
         (claude-agent-switch-to-buffer nil))

    ;; Ensure KB file exists with basic structure
    (claude-agent-expert--ensure-kb-file kb-file project-name)

    ;; Store mapping
    (puthash project-name buffer-name claude-agent-expert--sessions)

    ;; Spawn the expert agent
    ;; We use claude-agent-run with a custom slug and system prompt
    (require 'claude-agent-repl)
    (let ((claude-agent-extra-system-prompt system-prompt))
      (claude-agent-run expanded-path nil nil "expert" claude-agent-expert-model
                        claude-agent-expert-allowed-tools))

    ;; Set up permission policy for the expert buffer
    (when-let ((buf (get-buffer buffer-name)))
      (with-current-buffer buf
        ;; Set buffer-local permission policy
        (setq-local claude-agent-permission-policy :rules)
        (setq-local claude-agent-permission-rules-local
                    `(;; Allow all read-only tools
                      (:match (:tool-regex "read_file\\|grep\\|glob\\|search_buffer\\|get_buffer_content\\|buffer_info\\|list_buffers\\|read_buffer\\|magit_")
                       :action :allow
                       :scope :session)
                      ;; Allow expert-specific tools
                      (:match (:tool-regex "expert_respond\\|expert_kb\\|whoami\\|list_agents\\|send_message")
                       :action :allow
                       :scope :session)
                      ;; Allow edit tools only for KB file
                      (:match (:and (:tool-regex "lock\\|edit\\|unlock\\|save_buffer")
                                    (:path-prefix ,kb-file))
                       :action :allow
                       :scope :session)
                      ;; Deny edit tools for other files
                      (:match (:tool-regex "lock\\|edit\\|unlock\\|save_buffer")
                       :action :deny
                       :message "Expert sessions can only edit the knowledge base file")
                      ;; Deny all other tools
                      (:match (:tool-regex ".*")
                       :action :deny
                       :message "Expert sessions have restricted permissions")))))

    buffer-name))

(defun claude-agent-expert--ensure-kb-file (kb-file project-name)
  "Ensure KB-FILE exists with basic structure for PROJECT-NAME."
  (unless (file-exists-p kb-file)
    (with-temp-file kb-file
      (insert (format "#+TITLE: %s Knowledge Base\n" project-name))
      (insert "#+STARTUP: overview\n\n")
      (insert "* Overview\n\n")
      (insert "This knowledge base contains information about the project.\n\n")
      (insert "* Architecture\n\n")
      (insert "* Patterns\n\n")
      (insert "* Gotchas\n\n")
      (insert "* FAQ\n\n"))))

(defun claude-agent-expert--system-prompt (project-name kb-file)
  "Generate the system prompt for an expert session.
PROJECT-NAME is the project name.
KB-FILE is the path to the knowledge base file."
  (format "You are a project expert for '%s'. Your role is to:

1. ANSWER QUERIES: When you receive a query from another agent, analyze it carefully
   and provide a helpful, accurate response using the `expert_respond` tool.

2. MAINTAIN KNOWLEDGE: Use the `expert_kb` tool to:
   - Search existing knowledge before answering
   - Create new entries for important discoveries
   - Update entries with new learnings

3. STAY FOCUSED: You have restricted permissions:
   - You can READ any file in the project
   - You can only EDIT the knowledge base file: %s
   - Use `expert_respond` to send answers (not send_message)

KNOWLEDGE BASE LOCATION: %s

When answering queries:
1. First search the KB with `expert_kb` action=\"search\"
2. Read relevant project files if needed
3. Formulate your answer
4. Save important learnings to KB with `expert_kb` action=\"create\" or action=\"update\"
5. Respond using `expert_respond`

IMPORTANT: Always use `expert_respond` to send your answer back to the caller.
Do not use `send_message` directly - the system tracks pending queries.

KB Entry Types:
- architecture: Design patterns, system structure, module relationships
- pattern: Reusable code patterns, idioms specific to this codebase
- gotcha: Bugs, edge cases, non-obvious behaviors, things that don't work as expected
- faq: Frequently asked questions and their answers"
          project-name kb-file kb-file))

;;;; MCP Tool: ask-the-expert

(defun claude-agent-expert-mcp-ask (project-path query)
  "Ask the expert for PROJECT-PATH a QUERY.
Spawns an expert if one doesn't exist, sends the query, and waits for response."
  (unless project-path (error "project_path is required"))
  (unless query (error "query is required"))

  (let* ((expert-buffer (claude-agent-expert--get-or-create project-path))
         (caller-buffer (or (bound-and-true-p claude-session-buffer-name)
                            "unknown"))
         (project-name (claude-agent-expert--get-project-from-buffer expert-buffer)))

    ;; Store pending query info
    (puthash expert-buffer
             (list :caller caller-buffer
                   :query query
                   :timestamp (current-time))
             claude-agent-expert--pending-queries)

    ;; Format message for expert
    (let ((expert-message
           (format "QUERY FROM %s:\n\n%s\n\nPlease research this and respond using the `expert_respond` tool."
                   caller-buffer query)))

      ;; Send to expert (fire-and-forget, response comes via expert_respond)
      ;; Return immediately - the expert will respond asynchronously
      (claude-mcp-send-message expert-buffer expert-message caller-buffer)

      (json-encode
       `((status . "query_sent")
         (expert_buffer . ,expert-buffer)
         (message . ,(format "Query sent to expert for %s. The expert will respond when ready."
                             project-name)))))))

;;;; MCP Tool: expert-respond

(defun claude-agent-expert-mcp-respond (response)
  "Respond to the pending query with RESPONSE.
This tool is only available to expert sessions."
  (unless response (error "response is required"))

  ;; Get current buffer name (the expert)
  (let* ((expert-buffer (or (bound-and-true-p claude-session-buffer-name)
                            "unknown"))
         (pending (gethash expert-buffer claude-agent-expert--pending-queries)))

    (unless pending
      (error "No pending query to respond to"))

    (let ((caller (plist-get pending :caller))
          (original-query (plist-get pending :query)))

      ;; Clear pending query
      (remhash expert-buffer claude-agent-expert--pending-queries)

      ;; Send response back to caller
      (let ((formatted-response
             (format "EXPERT RESPONSE:\n\n%s\n\n---\nOriginal query: %s"
                     response
                     (truncate-string-to-width original-query 200 nil nil "..."))))
        (claude-mcp-send-message caller formatted-response expert-buffer))

      (json-encode
       `((status . "response_sent")
         (caller . ,caller)
         (message . "Response sent to caller"))))))

;;;; MCP Tool: expert-kb

(defun claude-agent-expert-mcp-kb (action &optional title kb-type content heading query)
  "Manage the expert's knowledge base.
ACTION is one of: search, get, create, update, list.
TITLE is the entry title (for create/update/get).
KB-TYPE is the entry type: architecture, pattern, gotcha, faq.
CONTENT is the entry content (for create/update).
HEADING is the parent heading to add under (for create).
QUERY is the search query (for search)."
  (unless action (error "action is required"))

  ;; Get current session info
  (let* ((expert-buffer (or (bound-and-true-p claude-session-buffer-name)
                            "unknown"))
         (project-name (claude-agent-expert--get-project-from-buffer expert-buffer)))

    (unless project-name
      (error "expert_kb can only be used by expert sessions"))

    (let ((kb-file (claude-agent-expert--kb-file project-name)))
      (pcase action
        ("search"
         (claude-agent-expert--kb-search kb-file query))

        ("get"
         (unless title (error "title is required for get action"))
         (claude-agent-expert--kb-get kb-file title))

        ("create"
         (unless title (error "title is required for create action"))
         (unless content (error "content is required for create action"))
         (claude-agent-expert--kb-create kb-file title kb-type content heading))

        ("update"
         (unless title (error "title is required for update action"))
         (unless content (error "content is required for update action"))
         (claude-agent-expert--kb-update kb-file title content))

        ("list"
         (claude-agent-expert--kb-list kb-file))

        (_
         (error "Unknown action: %s. Use: search, get, create, update, list" action))))))

;;;; MCP Tool: list-experts

(defun claude-agent-expert-mcp-list (&optional include-inactive)
  "List available expert sessions.
If INCLUDE-INACTIVE is non-nil, also list projects with KB files but no running expert.
Returns JSON with running experts and available projects."
  (let ((running-experts '())
        (available-projects '()))
    
    ;; Find running expert sessions
    (dolist (buffer (buffer-list))
      (let ((name (buffer-name buffer)))
        (when (claude-agent-expert--is-expert-session-p name)
          (let ((project (claude-agent-expert--get-project-from-buffer name))
                (alive (with-current-buffer buffer
                         (and (boundp 'claude-agent--process)
                              claude-agent--process
                              (process-live-p claude-agent--process)))))
            (push `((project . ,project)
                    (buffer . ,name)
                    (status . ,(if alive "running" "stopped"))
                    (kb_file . ,(claude-agent-expert--kb-file project)))
                  running-experts)))))
    
    ;; Optionally find projects with KB files but no running expert
    (when include-inactive
      (let ((kb-dir claude-agent-expert-kb-directory))
        (when (file-directory-p kb-dir)
          (dolist (file (directory-files kb-dir t "\\.org$"))
            (let* ((project (file-name-sans-extension (file-name-nondirectory file)))
                   (already-listed (cl-find project running-experts
                                            :key (lambda (e) (cdr (assq 'project e)))
                                            :test #'string=)))
              (unless already-listed
                (push `((project . ,project)
                        (buffer . nil)
                        (status . "available")
                        (kb_file . ,file))
                      available-projects)))))))
    
    (json-encode
     `((running . ,(vconcat (nreverse running-experts)))
       (available . ,(vconcat (nreverse available-projects)))
       (count . ,(+ (length running-experts) (length available-projects)))))))

;;;; KB Operations

(defun claude-agent-expert--kb-search (kb-file query)
  "Search KB-FILE for QUERY."
  (if (not (file-exists-p kb-file))
      (json-encode '((results . []) (message . "Knowledge base is empty")))
    (with-temp-buffer
      (insert-file-contents kb-file)
      (let ((results '())
            (query-lower (downcase (or query ""))))
        (goto-char (point-min))
        ;; Find all headings
        (while (re-search-forward "^\\(\\*+\\)\\s-+\\(.+\\)$" nil t)
          (let* ((level (length (match-string 1)))
                 (heading (match-string 2))
                 (heading-lower (downcase heading))
                 (start (point))
                 (end (save-excursion
                        (if (re-search-forward (format "^\\*\\{1,%d\\}\\s-" level) nil t)
                            (match-beginning 0)
                          (point-max))))
                 (content (buffer-substring-no-properties start (min end (+ start 500)))))
            ;; Match if query is empty or matches heading/content
            (when (or (string-empty-p query-lower)
                      (string-match-p (regexp-quote query-lower) heading-lower)
                      (string-match-p (regexp-quote query-lower) (downcase content)))
              (push `((heading . ,heading)
                      (level . ,level)
                      (preview . ,(truncate-string-to-width
                                   (string-trim content) 200 nil nil "...")))
                    results))))
        (json-encode `((results . ,(vconcat (nreverse results)))
                       (count . ,(length results))))))))

(defun claude-agent-expert--kb-get (kb-file title)
  "Get entry with TITLE from KB-FILE."
  (if (not (file-exists-p kb-file))
      (json-encode '((found . :json-false) (message . "Knowledge base not found")))
    (with-temp-buffer
      (insert-file-contents kb-file)
      (goto-char (point-min))
      (if (re-search-forward (format "^\\(\\*+\\)\\s-+%s\\s-*$"
                                     (regexp-quote title))
                             nil t)
          (let* ((level (length (match-string 1)))
                 (start (point))
                 (end (save-excursion
                        (if (re-search-forward (format "^\\*\\{1,%d\\}\\s-" level) nil t)
                            (match-beginning 0)
                          (point-max))))
                 (content (string-trim (buffer-substring-no-properties start end))))
            (json-encode `((found . t)
                           (title . ,title)
                           (level . ,level)
                           (content . ,content))))
        (json-encode '((found . :json-false)
                       (message . "Entry not found")))))))

(defun claude-agent-expert--kb-create (kb-file title kb-type content &optional heading)
  "Create entry in KB-FILE with TITLE, KB-TYPE, CONTENT under HEADING."
  (let ((parent-heading (or heading
                            (pcase kb-type
                              ("architecture" "Architecture")
                              ("pattern" "Patterns")
                              ("gotcha" "Gotchas")
                              ("faq" "FAQ")
                              (_ "Overview")))))
    (with-current-buffer (find-file-noselect kb-file)
      (goto-char (point-min))
      ;; Find or create parent heading
      (if (re-search-forward (format "^\\*\\s-+%s\\s-*$"
                                     (regexp-quote parent-heading))
                             nil t)
          (progn
            ;; Go to end of this section
            (let ((end (save-excursion
                        (if (re-search-forward "^\\*\\s-" nil t)
                            (match-beginning 0)
                          (point-max)))))
              (goto-char end)))
        ;; Create parent heading at end
        (goto-char (point-max))
        (insert (format "\n* %s\n" parent-heading)))

      ;; Insert new entry
      (insert (format "\n** %s\n\n%s\n" title content))
      (save-buffer)
      (json-encode `((created . t)
                     (title . ,title)
                     (parent . ,parent-heading)
                     (message . ,(format "Created entry '%s' under '%s'"
                                         title parent-heading)))))))

(defun claude-agent-expert--kb-update (kb-file title content)
  "Update or append to entry with TITLE in KB-FILE with CONTENT."
  (with-current-buffer (find-file-noselect kb-file)
    (goto-char (point-min))
    (if (re-search-forward (format "^\\(\\*+\\)\\s-+%s\\s-*$"
                                   (regexp-quote title))
                           nil t)
        (let* ((level (length (match-string 1)))
               (end (save-excursion
                      (if (re-search-forward (format "^\\*\\{1,%d\\}\\s-" level) nil t)
                          (match-beginning 0)
                        (point-max)))))
          ;; Go to end of entry, before next heading
          (goto-char end)
          (skip-chars-backward "\n")
          ;; Append content
          (insert (format "\n\n%s" content))
          (save-buffer)
          (json-encode `((updated . t)
                         (title . ,title)
                         (message . ,(format "Appended content to '%s'" title)))))
      (json-encode `((updated . :json-false)
                     (message . ,(format "Entry '%s' not found. Use create action instead."
                                         title)))))))

(defun claude-agent-expert--kb-list (kb-file)
  "List all entries in KB-FILE."
  (if (not (file-exists-p kb-file))
      (json-encode '((entries . []) (message . "Knowledge base is empty")))
    (with-temp-buffer
      (insert-file-contents kb-file)
      (let ((entries '()))
        (goto-char (point-min))
        (while (re-search-forward "^\\(\\*+\\)\\s-+\\(.+\\)$" nil t)
          (let ((level (length (match-string 1)))
                (heading (match-string 2)))
            (push `((heading . ,heading)
                    (level . ,level))
                  entries)))
        (json-encode `((entries . ,(vconcat (nreverse entries)))
                       (count . ,(length entries))))))))

;;;; MCP Tool Registration

(with-eval-after-load 'claude-mcp-registry
  ;; ask_the_expert - Main entry point for querying experts
  (claude-mcp-deftool ask-the-expert
    "Ask a project expert a question. The expert is a dedicated Claude agent
that maintains knowledge about the project and can answer questions.
The expert will research your query and respond asynchronously.

Use this when you need:
- Deep knowledge about project architecture or patterns
- Help understanding complex code interactions
- Historical context about why things are implemented a certain way

The expert maintains a knowledge base and can search existing knowledge
before answering."
    :function #'claude-agent-expert-mcp-ask
    :safe t
    :needs-session-cwd nil
    :args ((project-path string :required "Root directory of the project")
           (query string :required "The question to ask the expert")))

  ;; expert_respond - For experts to send responses
  (claude-mcp-deftool expert-respond
    "Respond to a pending query. This tool is only available to expert sessions.
Use this to send your answer back to the agent that asked the question.

IMPORTANT: Always use this tool to respond to queries, not send_message.
The system tracks pending queries and this ensures proper routing."
    :function #'claude-agent-expert-mcp-respond
    :safe nil
    :needs-session-cwd nil
    :args ((response string :required "Your response to the query")))

  ;; expert_kb - For experts to manage their knowledge base
  (claude-mcp-deftool expert-kb
    "Manage the expert's project knowledge base.

Actions:
- search: Search for entries matching a query
- get: Get the full content of an entry by title
- create: Create a new entry under a heading
- update: Append content to an existing entry
- list: List all entries in the KB

KB entry types (for create action):
- architecture: Design patterns, system structure
- pattern: Reusable code patterns
- gotcha: Bugs, edge cases, non-obvious behaviors
- faq: Frequently asked questions

The KB is stored as an org file and persists across sessions."
    :function #'claude-agent-expert-mcp-kb
    :safe nil
    :needs-session-cwd nil
    :args ((action string :required "Action: search, get, create, update, list")
           (title string "Entry title (for get/create/update)")
           (kb-type string "Entry type: architecture, pattern, gotcha, faq (for create)")
           (content string "Entry content (for create/update)")
           (heading string "Parent heading to create under (optional, defaults based on type)")
           (query string "Search query (for search action)")))

  ;; list_experts - List available expert sessions
  (claude-mcp-deftool list-experts
    "List available project experts. Shows running expert sessions and
optionally projects that have knowledge bases but no active expert.

Use this to discover which experts are available before using ask_the_expert.
Each expert maintains knowledge about a specific project."
    :function #'claude-agent-expert-mcp-list
    :safe t
    :needs-session-cwd nil
    :args ((include-inactive boolean "If true, also list projects with KB files but no running expert"))))

;;;; Hook into permission system (optional, for defense-in-depth)

(defun claude-agent-expert--permission-advice (orig-fn tool-name tool-input)
  "Advice to check expert permissions before normal permission handling.
ORIG-FN is the original permission handler.
TOOL-NAME and TOOL-INPUT are the tool call details."
  (let ((buffer-name (or (bound-and-true-p claude-session-buffer-name)
                         (buffer-name))))
    (if-let ((error-msg (claude-agent-expert--check-permission
                         tool-name tool-input buffer-name)))
        ;; Permission denied by expert rules
        (list :deny :message error-msg)
      ;; Pass to original handler
      (funcall orig-fn tool-name tool-input))))

;; Note: This advice can be enabled if needed for defense-in-depth,
;; but the primary enforcement is via the tool allowlist passed to claude-agent-run
;; and the buffer-local permission rules set up in claude-agent-expert--spawn.

(provide 'claude-agent-expert)
;;; claude-agent-expert.el ends here
