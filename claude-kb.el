;;; claude-kb.el --- Knowledge Base for Claude AI -*- lexical-binding: t; -*-
;; Author: Claude + Chad Crawford
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.0"))
;; Keywords: claudemacs ai emacs llm tools knowledge-base
;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:
;; Knowledge base system integrated with org-roam for Claude AI.
;;
;; This system allows Claude to:
;; - Store learnings about files, modules, and concepts
;; - Retrieve relevant knowledge when working on related code
;; - Build institutional memory across sessions
;;
;; KB entries are stored as org-roam nodes in org-roam-directory/kb/{project}/
;; with properties for:
;; - KB_PROJECT: Project name for scoped queries
;; - KB_TYPE: gotcha, architecture, pattern, or reference
;; - KB_FILES: Associated file paths
;; - KB_MODULES: Related module names
;; - KB_CONCEPTS: Concept keywords for searching
;;
;; MCP Tools provided:
;; - kb_create: Create a new KB entry
;; - kb_search: Search KB entries by text, file, module, or concept
;; - kb_get: Get full KB entry content with optional related entries
;; - kb_update: Update or append to existing KB entries
;; - kb_list: List all KB entries with optional filtering

;;; Code:

(require 'org)
(require 'org-roam)
(require 'org-id)
(require 'json)
(require 'cl-lib)

;; Forward declarations
(declare-function claude-mcp-deftool "claude-mcp")

;;;; Customization

(defgroup claude-kb nil
  "Knowledge base for Claude AI."
  :group 'org-roam
  :group 'claude-agent)

(defcustom claude-kb-types
  '("gotcha" "architecture" "pattern" "reference")
  "Valid KB entry types.
- gotcha: Bugs, edge cases, things that don't work as expected
- architecture: How systems are designed, why certain patterns are used
- pattern: Reusable code patterns in this codebase
- reference: API docs, external resources, configuration details"
  :type '(repeat string)
  :group 'claude-kb)

;;;; Helper Functions

(defun claude-kb--directory ()
  "Return the KB storage directory in org-roam.
Creates it if it doesn't exist."
  (let ((dir (expand-file-name "kb" org-roam-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun claude-kb--project-directory (project)
  "Return the KB directory for PROJECT.
Creates it if it doesn't exist."
  (let ((dir (expand-file-name project (claude-kb--directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun claude-kb--slugify (text)
  "Convert TEXT to a filename-safe slug."
  (let* ((slug (downcase text))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
         (slug (replace-regexp-in-string "^-\\|-$" "" slug)))
    ;; Limit slug length
    (if (> (length slug) 60)
        (substring slug 0 60)
      slug)))

(defun claude-kb--worktree-main-repo (dir)
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

(defvar claudemacs-session-cwd nil
  "The working directory for the current claudemacs session.
Set by the MCP server via a let binding to provide session context.")

(defun claude-kb--get-current-project ()
  "Get the current project name from context.
Uses the session cwd, with worktree detection to find the main repo."
  (let* ((cwd (or (bound-and-true-p claudemacs-session-cwd)
                  (bound-and-true-p claude-session-cwd)
                  (bound-and-true-p claude--cwd)
                  default-directory))
         (expanded-cwd (expand-file-name cwd))
         ;; Check if we're in a worktree and get main repo
         (main-repo (claude-kb--worktree-main-repo expanded-cwd))
         (project-dir (or main-repo expanded-cwd)))
    (file-name-nondirectory (directory-file-name project-dir))))

(defun claude-kb--generate-id ()
  "Generate a unique ID for a KB entry."
  (format "%s%04x" (format-time-string "%Y%m%dT%H%M%S") (random 65536)))

(defun claude-kb--format-files-property (files)
  "Format FILES list for storage in property.
FILES is a list of file paths."
  (if files
      (mapconcat #'identity files " ")
    ""))

(defun claude-kb--parse-files-property (value)
  "Parse FILES property VALUE back to a list."
  (if (and value (not (string-empty-p value)))
      (split-string value " " t)
    nil))

(defun claude-kb--format-list-property (items)
  "Format ITEMS list for storage in property."
  (if items
      (mapconcat #'identity items " ")
    ""))

(defun claude-kb--parse-list-property (value)
  "Parse list property VALUE back to a list."
  (if (and value (not (string-empty-p value)))
      (split-string value " " t)
    nil))

;;;; Node Creation

(defun claude-kb--create-node (title kb-type summary details project
                                     &optional files modules concepts related-ids)
  "Create a new KB entry node.
TITLE is the entry title.
KB-TYPE is one of: gotcha, architecture, pattern, reference.
SUMMARY is a one-paragraph summary.
DETAILS is the detailed explanation (optional).
PROJECT is the project name.
FILES is a list of related file paths (optional).
MODULES is a list of module names (optional).
CONCEPTS is a list of concept keywords (optional).
RELATED-IDS is a list of KB entry IDs to link (optional).
Returns the file path of the created node."
  (unless (member kb-type claude-kb-types)
    (error "Invalid KB type: %s. Must be one of: %s"
           kb-type (string-join claude-kb-types ", ")))
  (let* ((project-dir (claude-kb--project-directory project))
         (slug (claude-kb--slugify title))
         (id (claude-kb--generate-id))
         (date-stamp (format-time-string "%Y-%m-%d"))
         (filename (format "kb-%s-%s.org" kb-type slug))
         (file-path (expand-file-name filename project-dir))
         ;; Format properties
         (files-str (claude-kb--format-files-property files))
         (modules-str (claude-kb--format-list-property modules))
         (concepts-str (claude-kb--format-list-property concepts))
         ;; Format related links
         (related-links (when related-ids
                          (mapconcat
                           (lambda (rid) (format "- [[id:%s]]" rid))
                           related-ids
                           "\n"))))
    ;; Check if file already exists
    (when (file-exists-p file-path)
      (error "KB entry already exists: %s" file-path))
    ;; Create the node file
    (with-temp-file file-path
      (insert (format ":PROPERTIES:
:ID: %s
:ROAM_TAGS: kb
:KB_PROJECT: %s
:KB_TYPE: %s
:KB_FILES: %s
:KB_MODULES: %s
:KB_CONCEPTS: %s
:CREATED: %s
:UPDATED: %s
:END:
#+title: %s
#+filetags: :kb:%s:%s:

** Summary
%s

** Details
%s
%s"
                      id
                      project
                      kb-type
                      files-str
                      modules-str
                      concepts-str
                      date-stamp
                      date-stamp
                      title
                      kb-type
                      project
                      (or summary "")
                      (or details "")
                      (if related-links
                          (format "\n** Related\n%s\n" related-links)
                        ""))))
    ;; Update org-roam database
    (when (fboundp 'org-roam-db-update-file)
      (org-roam-db-update-file file-path))
    file-path))

;;;; Query Functions

(defun claude-kb--query-all (&optional project-filter)
  "Query all KB entries from org-roam.
If PROJECT-FILTER is provided, only return entries for that project.
If PROJECT-FILTER is \"*\", return entries for all projects.
Returns a list of plists with KB entry info."
  (let ((kb-entries '())
        ;; Query org-roam for KB nodes
        (nodes (org-roam-db-query
                [:select [nodes:id nodes:file nodes:title]
                 :from nodes
                 :where (like nodes:file "%/kb/%")])))
    (dolist (row nodes)
      (let* ((id (nth 0 row))
             (file (nth 1 row))
             (title (nth 2 row)))
        ;; Read properties from the file
        (when (file-exists-p file)
          (with-temp-buffer
            (insert-file-contents file nil 0 3000) ; Read header
            (let ((project (when (re-search-forward "^:KB_PROJECT:[ \t]*\\(.*\\)$" nil t)
                             (string-trim (match-string 1))))
                  (kb-type (progn
                             (goto-char (point-min))
                             (when (re-search-forward "^:KB_TYPE:[ \t]*\\(.*\\)$" nil t)
                               (string-trim (match-string 1)))))
                  (files (progn
                           (goto-char (point-min))
                           (when (re-search-forward "^:KB_FILES:[ \t]*\\(.*\\)$" nil t)
                             (claude-kb--parse-files-property (match-string 1)))))
                  (modules (progn
                             (goto-char (point-min))
                             (when (re-search-forward "^:KB_MODULES:[ \t]*\\(.*\\)$" nil t)
                               (claude-kb--parse-list-property (match-string 1)))))
                  (concepts (progn
                              (goto-char (point-min))
                              (when (re-search-forward "^:KB_CONCEPTS:[ \t]*\\(.*\\)$" nil t)
                                (claude-kb--parse-list-property (match-string 1)))))
                  (created (progn
                             (goto-char (point-min))
                             (when (re-search-forward "^:CREATED:[ \t]*\\(.*\\)$" nil t)
                               (match-string 1))))
                  (updated (progn
                             (goto-char (point-min))
                             (when (re-search-forward "^:UPDATED:[ \t]*\\(.*\\)$" nil t)
                               (match-string 1)))))
              ;; Apply project filter
              (when (or (null project-filter)
                        (string= project-filter "*")
                        (string= project project-filter))
                (push (list :id id
                            :title title
                            :project project
                            :kb-type kb-type
                            :files files
                            :modules modules
                            :concepts concepts
                            :file file
                            :created created
                            :updated updated)
                      kb-entries)))))))
    (nreverse kb-entries)))

(defun claude-kb--search-by-text (query &optional project entries)
  "Search KB entries for QUERY in title and content.
PROJECT limits search to a specific project (nil for current, \"*\" for all).
ENTRIES can be pre-queried list of entries to search."
  (let* ((project (or project (claude-kb--get-current-project)))
         (entries (or entries (claude-kb--query-all project)))
         (query-lower (downcase query)))
    (cl-remove-if-not
     (lambda (entry)
       (let ((title (downcase (or (plist-get entry :title) "")))
             (file (plist-get entry :file)))
         (or (string-match-p (regexp-quote query-lower) title)
             ;; Check file content if title doesn't match
             (when (file-exists-p file)
               (with-temp-buffer
                 (insert-file-contents file)
                 (string-match-p (regexp-quote query-lower)
                                 (downcase (buffer-string))))))))
     entries)))

(defun claude-kb--search-by-file (file-path &optional project entries)
  "Find KB entries related to FILE-PATH.
PROJECT limits search to a specific project.
ENTRIES can be pre-queried list of entries to search."
  (let* ((project (or project (claude-kb--get-current-project)))
         (entries (or entries (claude-kb--query-all project)))
         (file-base (file-name-nondirectory file-path)))
    (cl-remove-if-not
     (lambda (entry)
       (let ((files (plist-get entry :files)))
         (cl-some (lambda (f)
                    (or (string= f file-path)
                        (string= (file-name-nondirectory f) file-base)))
                  files)))
     entries)))

(defun claude-kb--search-by-module (module &optional project entries)
  "Find KB entries related to MODULE.
PROJECT limits search to a specific project.
ENTRIES can be pre-queried list of entries to search."
  (let* ((project (or project (claude-kb--get-current-project)))
         (entries (or entries (claude-kb--query-all project)))
         (module-lower (downcase module)))
    (cl-remove-if-not
     (lambda (entry)
       (let ((modules (plist-get entry :modules)))
         (cl-some (lambda (m) (string= (downcase m) module-lower)) modules)))
     entries)))

(defun claude-kb--search-by-concept (concept &optional project entries)
  "Find KB entries related to CONCEPT.
PROJECT limits search to a specific project.
ENTRIES can be pre-queried list of entries to search."
  (let* ((project (or project (claude-kb--get-current-project)))
         (entries (or entries (claude-kb--query-all project)))
         (concept-lower (downcase concept)))
    (cl-remove-if-not
     (lambda (entry)
       (let ((concepts (plist-get entry :concepts)))
         (cl-some (lambda (c) (string= (downcase c) concept-lower)) concepts)))
     entries)))

(defun claude-kb--get-by-id (kb-id)
  "Get KB entry by ID.
KB-ID can be the node ID or a title to search for.
Returns the entry plist or nil if not found."
  (let ((entries (claude-kb--query-all "*")))
    (or
     ;; Try exact ID match
     (cl-find-if (lambda (e) (string= (plist-get e :id) kb-id)) entries)
     ;; Try title match
     (cl-find-if (lambda (e) (string= (plist-get e :title) kb-id)) entries))))

(defun claude-kb--get-full-content (file)
  "Get the full content of a KB entry FILE.
Returns a plist with :summary, :details, and :related."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((summary "")
            (details "")
            (related nil))
        ;; Extract summary
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\* Summary\n" nil t)
          (let ((start (point))
                (end (save-excursion
                       (if (re-search-forward "^\\*\\* " nil t)
                           (match-beginning 0)
                         (point-max)))))
            (setq summary (string-trim (buffer-substring-no-properties start end)))))
        ;; Extract details
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\* Details\n" nil t)
          (let ((start (point))
                (end (save-excursion
                       (if (re-search-forward "^\\*\\* " nil t)
                           (match-beginning 0)
                         (point-max)))))
            (setq details (string-trim (buffer-substring-no-properties start end)))))
        ;; Extract related IDs
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\* Related\n" nil t)
          (let ((end (save-excursion
                       (if (re-search-forward "^\\*\\* " nil t)
                           (match-beginning 0)
                         (point-max)))))
            (while (re-search-forward "\\[\\[id:\\([^]]+\\)\\]" end t)
              (push (match-string 1) related))))
        (list :summary summary
              :details details
              :related (nreverse related))))))

(defun claude-kb--get-related-summaries (related-ids)
  "Get summaries of entries with RELATED-IDS.
Returns a list of plists with :id, :title, :summary, :type."
  (mapcar
   (lambda (rid)
     (let ((entry (claude-kb--get-by-id rid)))
       (when entry
         (let* ((file (plist-get entry :file))
                (content (claude-kb--get-full-content file)))
           (list :id rid
                 :title (plist-get entry :title)
                 :summary (plist-get content :summary)
                 :type (plist-get entry :kb-type))))))
   related-ids))

;;;; Update Functions

(defun claude-kb--append-details (file text)
  "Append TEXT to the Details section of KB entry FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^\\*\\* Details\n" nil t)
          (let ((end (save-excursion
                       (if (re-search-forward "^\\*\\* " nil t)
                           (match-beginning 0)
                         (point-max)))))
            (goto-char end)
            ;; Move back before any trailing newlines
            (skip-chars-backward "\n")
            (insert "\n\n" text))
        ;; No Details section, create one
        (goto-char (point-max))
        (insert "\n** Details\n" text "\n"))
      ;; Update the UPDATED property
      (goto-char (point-min))
      (when (re-search-forward "^:UPDATED:[ \t]*.+$" nil t)
        (replace-match (format ":UPDATED: %s" (format-time-string "%Y-%m-%d"))))
      (save-buffer))))

(defun claude-kb--add-files (file new-files)
  "Add NEW-FILES to the KB_FILES property in FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^:KB_FILES:[ \t]*\\(.*\\)$" nil t)
        (let* ((current (claude-kb--parse-files-property (match-string 1)))
               (merged (cl-remove-duplicates (append current new-files) :test #'string=))
               (new-value (claude-kb--format-files-property merged)))
          (replace-match (format ":KB_FILES: %s" new-value))))
      ;; Update UPDATED property
      (goto-char (point-min))
      (when (re-search-forward "^:UPDATED:[ \t]*.+$" nil t)
        (replace-match (format ":UPDATED: %s" (format-time-string "%Y-%m-%d"))))
      (save-buffer))))

(defun claude-kb--add-modules (file new-modules)
  "Add NEW-MODULES to the KB_MODULES property in FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^:KB_MODULES:[ \t]*\\(.*\\)$" nil t)
        (let* ((current (claude-kb--parse-list-property (match-string 1)))
               (merged (cl-remove-duplicates (append current new-modules) :test #'string=))
               (new-value (claude-kb--format-list-property merged)))
          (replace-match (format ":KB_MODULES: %s" new-value))))
      ;; Update UPDATED property
      (goto-char (point-min))
      (when (re-search-forward "^:UPDATED:[ \t]*.+$" nil t)
        (replace-match (format ":UPDATED: %s" (format-time-string "%Y-%m-%d"))))
      (save-buffer))))

(defun claude-kb--add-concepts (file new-concepts)
  "Add NEW-CONCEPTS to the KB_CONCEPTS property in FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^:KB_CONCEPTS:[ \t]*\\(.*\\)$" nil t)
        (let* ((current (claude-kb--parse-list-property (match-string 1)))
               (merged (cl-remove-duplicates (append current new-concepts) :test #'string=))
               (new-value (claude-kb--format-list-property merged)))
          (replace-match (format ":KB_CONCEPTS: %s" new-value))))
      ;; Update UPDATED property
      (goto-char (point-min))
      (when (re-search-forward "^:UPDATED:[ \t]*.+$" nil t)
        (replace-match (format ":UPDATED: %s" (format-time-string "%Y-%m-%d"))))
      (save-buffer))))

(defun claude-kb--add-related (file new-related-ids)
  "Add NEW-RELATED-IDS to the Related section of FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^\\*\\* Related\n" nil t)
          ;; Section exists, append to it
          (let ((end (save-excursion
                       (if (re-search-forward "^\\*\\* " nil t)
                           (match-beginning 0)
                         (point-max)))))
            (goto-char end)
            (skip-chars-backward "\n")
            (dolist (rid new-related-ids)
              (insert (format "\n- [[id:%s]]" rid))))
        ;; Create Related section
        (goto-char (point-max))
        (insert "\n** Related\n")
        (dolist (rid new-related-ids)
          (insert (format "- [[id:%s]]\n" rid))))
      ;; Update UPDATED property
      (goto-char (point-min))
      (when (re-search-forward "^:UPDATED:[ \t]*.+$" nil t)
        (replace-match (format ":UPDATED: %s" (format-time-string "%Y-%m-%d"))))
      (save-buffer))))

;;;; MCP Tool Functions

(defun claude-kb-mcp-create (title kb-type summary &optional details files modules concepts project related-ids)
  "Create a new KB entry.
TITLE is the entry title.
KB-TYPE is one of: gotcha, architecture, pattern, reference.
SUMMARY is a one-paragraph summary.
DETAILS is optional detailed explanation.
FILES is optional array of file paths.
MODULES is optional array of module names.
CONCEPTS is optional array of concept keywords.
PROJECT is optional project name (defaults to current).
RELATED-IDS is optional array of KB entry IDs to link.
Returns JSON with the created entry info."
  (unless title (error "title is required"))
  (unless kb-type (error "kb_type is required"))
  (unless summary (error "summary is required"))
  (let* ((project (or project (claude-kb--get-current-project)))
         (file-path (claude-kb--create-node title kb-type summary details project
                                            files modules concepts related-ids))
         ;; Get the ID from the created file
         (id (with-temp-buffer
               (insert-file-contents file-path nil 0 500)
               (when (re-search-forward "^:ID:[ \t]*\\(.+\\)$" nil t)
                 (match-string 1)))))
    (json-encode
     `((id . ,id)
       (title . ,title)
       (type . ,kb-type)
       (project . ,project)
       (file . ,file-path)
       (message . ,(format "Created KB entry: %s" title))))))

(defun claude-kb-mcp-search (&optional query file module concept kb-type project limit)
  "Search KB entries.
QUERY is optional text to search in title/content.
FILE is optional file path to find related entries.
MODULE is optional module name to search.
CONCEPT is optional concept to search.
KB-TYPE is optional type filter (gotcha, architecture, pattern, reference).
PROJECT is optional project filter (defaults to current, \"*\" for all).
LIMIT is optional max entries to return (default 10).
Returns JSON array of matching entries."
  (let* ((project (or project (claude-kb--get-current-project)))
         (limit (or limit 10))
         (entries (claude-kb--query-all project))
         (results entries))
    ;; Apply filters
    (when query
      (setq results (claude-kb--search-by-text query project results)))
    (when file
      (setq results (claude-kb--search-by-file file project results)))
    (when module
      (setq results (claude-kb--search-by-module module project results)))
    (when concept
      (setq results (claude-kb--search-by-concept concept project results)))
    (when kb-type
      (setq results (cl-remove-if-not
                     (lambda (e) (string= (plist-get e :kb-type) kb-type))
                     results)))
    ;; Apply limit
    (when (> (length results) limit)
      (setq results (cl-subseq results 0 limit)))
    ;; Format response
    (json-encode
     (mapcar (lambda (e)
               `((id . ,(plist-get e :id))
                 (title . ,(plist-get e :title))
                 (type . ,(plist-get e :kb-type))
                 (project . ,(plist-get e :project))
                 (files . ,(plist-get e :files))
                 (modules . ,(plist-get e :modules))
                 (concepts . ,(plist-get e :concepts))
                 (created . ,(plist-get e :created))
                 (updated . ,(plist-get e :updated))))
             results))))

(defun claude-kb-mcp-get (kb-id &optional include-related)
  "Get full content of a KB entry.
KB-ID is the entry ID or title.
INCLUDE-RELATED if true, also returns summaries of linked entries.
Returns JSON with the entry content."
  (unless kb-id (error "kb_id is required"))
  (let ((entry (claude-kb--get-by-id kb-id)))
    (unless entry
      (error "KB entry not found: %s" kb-id))
    (let* ((file (plist-get entry :file))
           (content (claude-kb--get-full-content file))
           (response `((id . ,(plist-get entry :id))
                       (title . ,(plist-get entry :title))
                       (type . ,(plist-get entry :kb-type))
                       (project . ,(plist-get entry :project))
                       (files . ,(plist-get entry :files))
                       (modules . ,(plist-get entry :modules))
                       (concepts . ,(plist-get entry :concepts))
                       (created . ,(plist-get entry :created))
                       (updated . ,(plist-get entry :updated))
                       (summary . ,(plist-get content :summary))
                       (details . ,(plist-get content :details)))))
      ;; Add related entries if requested
      (when (and include-related (not (eq include-related :json-false)))
        (let ((related-ids (plist-get content :related)))
          (when related-ids
            (setq response
                  (append response
                          `((related . ,(vconcat
                                         (cl-remove-if
                                          #'null
                                          (claude-kb--get-related-summaries related-ids))))))))))
      (json-encode response))))

(defun claude-kb-mcp-update (kb-id &optional append-details add-files add-modules add-concepts add-related-ids)
  "Update a KB entry.
KB-ID is the entry ID or title.
APPEND-DETAILS is optional text to append to Details section.
ADD-FILES is optional array of file paths to associate.
ADD-MODULES is optional array of modules to associate.
ADD-CONCEPTS is optional array of concepts to associate.
ADD-RELATED-IDS is optional array of KB entry IDs to link.
Returns JSON with update confirmation."
  (unless kb-id (error "kb_id is required"))
  (let ((entry (claude-kb--get-by-id kb-id)))
    (unless entry
      (error "KB entry not found: %s" kb-id))
    (let ((file (plist-get entry :file))
          (changes '()))
      ;; Apply updates
      (when (and append-details (not (string-empty-p append-details)))
        (claude-kb--append-details file append-details)
        (push "details" changes))
      (when add-files
        (claude-kb--add-files file add-files)
        (push "files" changes))
      (when add-modules
        (claude-kb--add-modules file add-modules)
        (push "modules" changes))
      (when add-concepts
        (claude-kb--add-concepts file add-concepts)
        (push "concepts" changes))
      (when add-related-ids
        (claude-kb--add-related file add-related-ids)
        (push "related" changes))
      ;; Update org-roam database
      (when (fboundp 'org-roam-db-update-file)
        (org-roam-db-update-file file))
      (json-encode
       `((id . ,(plist-get entry :id))
         (title . ,(plist-get entry :title))
         (updated . ,(nreverse changes))
         (message . ,(format "Updated KB entry: %s" (plist-get entry :title))))))))

(defun claude-kb-mcp-list (&optional kb-type project limit)
  "List all KB entries.
KB-TYPE is optional type filter.
PROJECT is optional project filter (defaults to current, \"*\" for all).
LIMIT is optional max entries to return (default 20).
Returns JSON array of entries."
  (let* ((project (or project (claude-kb--get-current-project)))
         (limit (or limit 20))
         (entries (claude-kb--query-all project)))
    ;; Apply type filter
    (when kb-type
      (setq entries (cl-remove-if-not
                     (lambda (e) (string= (plist-get e :kb-type) kb-type))
                     entries)))
    ;; Apply limit
    (when (> (length entries) limit)
      (setq entries (cl-subseq entries 0 limit)))
    ;; Format response
    (json-encode
     (mapcar (lambda (e)
               `((id . ,(plist-get e :id))
                 (title . ,(plist-get e :title))
                 (type . ,(plist-get e :kb-type))
                 (project . ,(plist-get e :project))
                 (files . ,(plist-get e :files))
                 (modules . ,(plist-get e :modules))
                 (concepts . ,(plist-get e :concepts))
                 (created . ,(plist-get e :created))
                 (updated . ,(plist-get e :updated))))
             entries))))

;;;; MCP Tool Registration

(with-eval-after-load 'claude-mcp
  ;; kb_create - Create a new KB entry
  (claude-mcp-deftool kb-create
    "Create a new knowledge base entry.
KB entries store learnings about files, modules, and concepts.
Types: gotcha (bugs/edge cases), architecture (design patterns),
pattern (reusable code patterns), reference (docs/resources)."
    :function #'claude-kb-mcp-create
    :safe nil
    :needs-session-cwd t
    :args ((title string :required "Brief descriptive title")
           (kb-type string :required "Type: gotcha, architecture, pattern, or reference")
           (summary string :required "One-paragraph summary")
           (details string "Detailed explanation with code examples")
           (files array "Array of file paths this knowledge relates to")
           (modules array "Array of module names")
           (concepts array "Array of concept keywords")
           (project string "Project name (defaults to current session's project)")
           (related-ids array "Array of KB entry IDs to link in Related section")))

  ;; kb_search - Search KB entries
  (claude-mcp-deftool kb-search
    "Search knowledge base entries by text, file, module, or concept.
Searches are scoped to the current project by default.
Use this before modifying files to find relevant gotchas and patterns."
    :function #'claude-kb-mcp-search
    :safe t
    :needs-session-cwd t
    :args ((query string "Text to search for in title/content")
           (file string "Find entries related to this file path")
           (module string "Find entries about this module")
           (concept string "Find entries about this concept")
           (kb-type string "Filter by type: gotcha, architecture, pattern, reference")
           (project string "Project to search (defaults to current, use '*' for all)")
           (limit integer "Max entries to return (default 10)")))

  ;; kb_get - Get full KB entry content
  (claude-mcp-deftool kb-get
    "Get the full content of a KB entry by ID or title.
Use include_related=true to also fetch summaries of linked entries (1-hop)."
    :function #'claude-kb-mcp-get
    :safe t
    :needs-session-cwd nil  ; kb_get doesn't need project context - it looks up by ID
    :args ((kb-id string :required "KB entry ID or title")
           (include-related boolean "If true, also return summaries of entries linked in Related section")))

  ;; kb_update - Update existing KB entry
  (claude-mcp-deftool kb-update
    "Update a KB entry - append to details or update metadata.
Use this to add new learnings to existing entries."
    :function #'claude-kb-mcp-update
    :safe nil
    :needs-session-cwd nil  ; kb_update doesn't need project context - it looks up by ID
    :args ((kb-id string :required "KB entry ID or title")
           (append-details string "Text to append to Details section")
           (add-files array "Additional file paths to associate")
           (add-modules array "Additional modules to associate")
           (add-concepts array "Additional concepts to associate")
           (add-related-ids array "Additional KB entry IDs to link")))

  ;; kb_list - List all KB entries
  (claude-mcp-deftool kb-list
    "List all KB entries, optionally filtered.
Scoped to current project by default."
    :function #'claude-kb-mcp-list
    :safe t
    :needs-session-cwd t
    :args ((kb-type string "Filter by type")
           (project string "Project to list (defaults to current, use '*' for all)")
           (limit integer "Max entries to return (default 20)"))))

(provide 'claude-kb)
;;; claude-kb.el ends here
