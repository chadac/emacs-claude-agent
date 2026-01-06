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
       (let ((tool-def `((description . ,(or (plist-get def :description) ""))
                         (function . ,(symbol-name (plist-get def :function)))
                         (safe . ,(if (plist-get def :safe) t :json-false))
                         (args . ,(claude-mcp--convert-args (plist-get def :args))))))
         ;; Add context hint if specified
         (when-let ((context (plist-get def :context)))
           (push (cons 'context context) tool-def))
         (puthash name tool-def tools)))
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

;;;; Rich File Reading with IDE Context

(defun claude-mcp--get-flycheck-errors ()
  "Get flycheck errors for the current buffer as a list of plists."
  (when (and (bound-and-true-p flycheck-mode)
             (bound-and-true-p flycheck-current-errors))
    (mapcar (lambda (err)
              (list :line (flycheck-error-line err)
                    :column (flycheck-error-column err)
                    :level (symbol-name (flycheck-error-level err))
                    :message (flycheck-error-message err)
                    :checker (symbol-name (flycheck-error-checker err))))
            flycheck-current-errors)))

(defun claude-mcp--get-flymake-diagnostics ()
  "Get flymake diagnostics for the current buffer as a list of plists."
  (when (bound-and-true-p flymake-mode)
    (mapcar (lambda (diag)
              (list :line (line-number-at-pos (flymake-diagnostic-beg diag))
                    :end-line (line-number-at-pos (flymake-diagnostic-end diag))
                    :type (symbol-name (flymake-diagnostic-type diag))
                    :message (flymake-diagnostic-text diag)))
            (flymake-diagnostics))))

(defun claude-mcp--get-lsp-diagnostics ()
  "Get LSP diagnostics directly for the current buffer as a list of plists.
This extracts diagnostics directly from LSP when flycheck/flymake haven't picked them up."
  (when (and (bound-and-true-p lsp-mode)
             (fboundp 'lsp--get-buffer-diagnostics))
    (mapcar (lambda (diag)
              (let* ((range (gethash "range" diag))
                     (start (gethash "start" range))
                     (severity (gethash "severity" diag))
                     (level (pcase severity
                              (1 "error")
                              (2 "warning")
                              (3 "info")
                              (4 "hint")
                              (_ "unknown"))))
                (list :line (1+ (gethash "line" start))
                      :column (1+ (gethash "character" start))
                      :level level
                      :message (gethash "message" diag))))
            (lsp--get-buffer-diagnostics))))

(defun claude-mcp--format-diagnostics (diagnostics source)
  "Format DIAGNOSTICS from SOURCE (flycheck/flymake) as a string."
  (if (null diagnostics)
      nil
    (concat
     (format "── %s ──\n" source)
     (mapconcat
      (lambda (diag)
        (let ((line (plist-get diag :line))
              (col (plist-get diag :column))
              (level (or (plist-get diag :level) (plist-get diag :type)))
              (msg (plist-get diag :message)))
          (if col
              (format "%d:%d [%s] %s" line col level msg)
            (format "%d [%s] %s" line level msg))))
      diagnostics
      "\n"))))

(defun claude-mcp-read-file-with-context (file-path &optional offset limit)
  "Read FILE-PATH and return rich context including IDE diagnostics.
Opens the file in a buffer if not already open to get live diagnostics.
OFFSET is the line number to start reading from (1-indexed, default 1).
LIMIT is the number of lines to read (default: all remaining lines).
Returns a formatted string with metadata, diagnostics, and content."
  (let* ((file-path (expand-file-name file-path))
         (existing-buffer (get-file-buffer file-path))
         ;; Suppress interactive prompts when opening files
         (lsp-auto-guess-root t)  ; Auto-guess LSP root instead of prompting
         (lsp-ask-to-select-first-project nil)  ; Don't ask to select project
         (enable-local-variables :safe)  ; Only use safe local vars, don't prompt
         (buffer (or existing-buffer
                     (find-file-noselect file-path)))
         (offset (or offset 1))
         result)
    (unwind-protect
        (with-current-buffer buffer
          ;; Give flycheck/flymake a moment to run if buffer was just opened
          (unless existing-buffer
            (run-hooks 'find-file-hook)
            ;; Small delay for async checkers
            (sit-for 0.5))

          (let* ((flycheck-errors (claude-mcp--get-flycheck-errors))
                 (flymake-diags (claude-mcp--get-flymake-diagnostics))
                 (lsp-diags (claude-mcp--get-lsp-diagnostics))
                 (has-diagnostics (or flycheck-errors flymake-diags lsp-diags))
                 (total-lines (count-lines (point-min) (point-max)))
                 ;; Calculate the range of lines to include
                 (start-line (max 1 offset))
                 (end-line (if limit
                               (min total-lines (+ start-line limit -1))
                             total-lines))
                 ;; Get content for the specified range
                 (content (save-excursion
                            (goto-char (point-min))
                            (forward-line (1- start-line))
                            (let ((start-pos (point)))
                              (forward-line (1+ (- end-line start-line)))
                              (buffer-substring-no-properties start-pos (point)))))
                 ;; Calculate width needed for line numbers
                 (max-line-width (length (number-to-string end-line))))

            ;; Build the result string - compact format like Read tool
            (setq result
                  (concat
                   ;; Diagnostics section (filter to visible range if limited)
                   (when has-diagnostics
                     (let* ((filter-to-range
                             (lambda (diags)
                               (if limit
                                   (seq-filter (lambda (d)
                                                 (let ((line (plist-get d :line)))
                                                   (and (>= line start-line)
                                                        (<= line end-line))))
                                               diags)
                                 diags)))
                            (visible-flycheck (funcall filter-to-range flycheck-errors))
                            (visible-flymake (funcall filter-to-range flymake-diags))
                            (visible-lsp (funcall filter-to-range lsp-diags))
                            (has-visible (or visible-flycheck visible-flymake visible-lsp)))
                       (when has-visible
                         (concat
                          (or (claude-mcp--format-diagnostics visible-flycheck "Flycheck") "")
                          (when (and visible-flycheck (or visible-flymake visible-lsp)) "\n")
                          (or (claude-mcp--format-diagnostics visible-flymake "Flymake") "")
                          (when (and visible-flymake visible-lsp) "\n")
                          (or (claude-mcp--format-diagnostics visible-lsp "LSP") "")
                          "\n\n"))))

                   ;; Content with line numbers - compact format matching Read tool
                   (let ((lines (split-string content "\n" t))
                         (line-num start-line)
                         (numbered-lines '()))
                     (dolist (line lines)
                       (push (format (format "%%%dd→%%s" max-line-width) line-num line)
                             numbered-lines)
                       (setq line-num (1+ line-num)))
                     (string-join (nreverse numbered-lines) "\n"))))))

      ;; Clean up: kill buffer if we opened it
      (unless existing-buffer
        (kill-buffer buffer)))

    result))

(claude-mcp-deftool read-file
  "Read a file with IDE diagnostics (flycheck/flymake errors). Use this for reading files."
  :function #'claude-mcp-read-file-with-context
  :safe t
  :args ((file-path string :required "Path to the file to read")
         (offset integer "Line number to start reading from (1-indexed, default: 1)")
         (limit integer "Number of lines to read (default: all remaining lines)")))

(defun claude-mcp--wait-for-diagnostics (buffer timeout)
  "Wait up to TIMEOUT seconds for diagnostics to update in BUFFER."
  (let ((start-time (float-time))
        (done nil))
    (while (and (not done)
                (< (- (float-time) start-time) timeout))
      (with-current-buffer buffer
        (when (and (bound-and-true-p flycheck-mode)
                   (not (eq flycheck-last-status-change 'running)))
          (setq done t))
        (when (and (bound-and-true-p flymake-mode)
                   (not (flymake-running-backends)))
          (setq done t)))
      (unless done
        (sit-for 0.1)))))

(defun claude-mcp-edit-file (file-path old-string new-string &optional replace-all)
  "Edit FILE-PATH by replacing OLD-STRING with NEW-STRING.
If REPLACE-ALL is non-nil, replace all occurrences.
Returns a result with the edit status and any diagnostics from affected lines."
  (let* ((file-path (expand-file-name file-path))
         (existing-buffer (get-file-buffer file-path))
         (lsp-auto-guess-root t)
         (lsp-ask-to-select-first-project nil)
         (enable-local-variables :safe)
         (buffer (or existing-buffer (find-file-noselect file-path)))
         (content (with-current-buffer buffer
                    (buffer-substring-no-properties (point-min) (point-max))))
         (occurrences (let ((positions '())
                            (start 0)
                            pos)
                        (while (setq pos (cl-search old-string content :start2 start))
                          (push pos positions)
                          (setq start (1+ pos)))
                        (nreverse positions)))
         result edit-start-line edit-end-line replacements-made)

    (cond
     ;; No matches found
     ((null occurrences)
      (setq result (format "Error: old_string not found in %s" file-path)))

     ;; Multiple matches but replace-all not set
     ((and (> (length occurrences) 1) (not replace-all))
      (setq result (format "Error: old_string found %d times. Use replace_all=true or provide more context."
                           (length occurrences))))

     ;; Perform the replacement
     (t
      (let* ((first-pos (car occurrences))
             (last-pos (car (last occurrences)))
             (new-content (if replace-all
                              (let ((res content) (offset 0))
                                (dolist (pos occurrences res)
                                  (let ((adj (+ pos offset)))
                                    (setq res (concat (substring res 0 adj)
                                                      new-string
                                                      (substring res (+ adj (length old-string)))))
                                    (setq offset (+ offset (- (length new-string) (length old-string)))))))
                            (concat (substring content 0 first-pos)
                                    new-string
                                    (substring content (+ first-pos (length old-string)))))))

        ;; Calculate line numbers
        (setq edit-start-line (1+ (cl-count ?\n (substring content 0 first-pos))))
        (setq edit-end-line (+ edit-start-line (cl-count ?\n new-string)))
        (setq replacements-made (if replace-all (length occurrences) 1))

        ;; Apply edit
        (with-current-buffer buffer
          (erase-buffer)
          (insert new-content)
          (save-buffer)

          ;; Trigger syntax checking and LSP refresh
          (when (bound-and-true-p flycheck-mode) (flycheck-buffer))
          (when (bound-and-true-p flymake-mode) (flymake-start))

          ;; Wait for diagnostics (longer wait for LSP)
          (sit-for 1.0)  ; Give LSP time to process changes
          (claude-mcp--wait-for-diagnostics buffer 3.0)

          ;; Collect diagnostics and show result
          (let* ((check-start (max 1 (- edit-start-line 3)))
                 (check-end (+ edit-end-line 3))
                 (flycheck-errs (claude-mcp--get-flycheck-errors))
                 (flymake-diags (claude-mcp--get-flymake-diagnostics))
                 (lsp-diags (claude-mcp--get-lsp-diagnostics))
                 (in-range (lambda (d)
                             (let ((line (plist-get d :line)))
                               (and (>= line check-start) (<= line check-end)))))
                 (rel-flycheck (seq-filter in-range flycheck-errs))
                 (rel-flymake (seq-filter in-range flymake-diags))
                 (rel-lsp (seq-filter in-range lsp-diags))
                 (has-diags (or rel-flycheck rel-flymake rel-lsp))
                 ;; Build diff showing old and new
                 (diff-output
                  (let ((old-lines (split-string old-string "\n"))
                        (new-lines (split-string new-string "\n")))
                    (concat
                     (mapconcat (lambda (l) (concat "- " l)) old-lines "\n")
                     "\n"
                     (mapconcat (lambda (l) (concat "+ " l)) new-lines "\n")))))

            (setq result
                  (concat
                   (if replace-all
                       (format "Replaced %d occurrence(s) in %s (lines %d-%d)"
                               replacements-made file-path edit-start-line edit-end-line)
                     (format "Edited %s (lines %d-%d)" file-path edit-start-line edit-end-line))
                   "\n\n" diff-output
                   (when has-diags
                     (concat "\n\n"
                             (or (claude-mcp--format-diagnostics rel-flycheck "Flycheck") "")
                             (when (and rel-flycheck (or rel-flymake rel-lsp)) "\n")
                             (or (claude-mcp--format-diagnostics rel-flymake "Flymake") "")
                             (when (and rel-flymake rel-lsp) "\n")
                             (or (claude-mcp--format-diagnostics rel-lsp "LSP") ""))))))))))
    result))

(claude-mcp-deftool edit-file
  "Edit a file by replacing old_string with new_string. Returns colored diff and diagnostics."
  :function #'claude-mcp-edit-file
  :args ((file-path string :required "Path to the file to edit")
         (old_string string :required "The text to replace")
         (new_string string :required "The replacement text")
         (replace_all boolean "Replace all occurrences (default: false)")))

;;;; Buffer Operations (read/edit/write buffers)
;;
;; These tools operate on Emacs buffers directly, complementing the file tools.

(defun claude-mcp-read-buffer (buffer-name &optional offset limit)
  "Read content from BUFFER-NAME with optional OFFSET and LIMIT.
OFFSET is the line number to start reading from (1-indexed, default: 1).
LIMIT is the number of lines to read (default: all remaining lines).
Returns content with line numbers in the same format as read-file."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))
  (with-current-buffer buffer-name
    (let* ((offset (or offset 1))
           (total-lines (count-lines (point-min) (point-max)))
           (start-line (max 1 offset))
           (end-line (if limit
                         (min total-lines (+ start-line limit -1))
                       total-lines))
           (content (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- start-line))
                      (let ((start-pos (point)))
                        (forward-line (1+ (- end-line start-line)))
                        (buffer-substring-no-properties start-pos (point)))))
           (max-line-width (length (number-to-string end-line))))
      ;; Format with line numbers like read-file
      (let ((lines (split-string content "\n" t))
            (line-num start-line)
            (numbered-lines '()))
        (dolist (line lines)
          (push (format (format "%%%dd→%%s" max-line-width) line-num line)
                numbered-lines)
          (setq line-num (1+ line-num)))
        (string-join (nreverse numbered-lines) "\n")))))

(claude-mcp-deftool read-buffer
  "Read content from an Emacs buffer. Similar to read-file but for buffers.
Returns content with line numbers. Use this for reading buffer content that
may not be saved to disk yet, or for special buffers like *scratch*."
  :function #'claude-mcp-read-buffer
  :safe t
  :args ((buffer-name string :required "Name of the buffer to read")
         (offset integer "Line number to start reading from (1-indexed, default: 1)")
         (limit integer "Number of lines to read (default: all remaining lines)")))

(defun claude-mcp-edit-buffer (buffer-name old-string new-string &optional replace-all)
  "Edit BUFFER-NAME by replacing OLD-STRING with NEW-STRING.
If REPLACE-ALL is non-nil, replace all occurrences.
Returns a result with the edit status and diff."
  (unless (get-buffer buffer-name)
    (error "Buffer '%s' does not exist" buffer-name))
  (with-current-buffer buffer-name
    (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
           (occurrences (let ((positions '())
                              (start 0)
                              pos)
                          (while (setq pos (cl-search old-string content :start2 start))
                            (push pos positions)
                            (setq start (1+ pos)))
                          (nreverse positions))))
      (cond
       ;; No matches found
       ((null occurrences)
        (format "Error: old_string not found in buffer %s" buffer-name))
       ;; Multiple matches but replace-all not set
       ((and (> (length occurrences) 1) (not replace-all))
        (format "Error: old_string found %d times. Use replace_all=true or provide more context."
                (length occurrences)))
       ;; Perform the replacement
       (t
        (let* ((first-pos (car occurrences))
               (replacements-made (if replace-all (length occurrences) 1))
               (edit-start-line (1+ (cl-count ?\n (substring content 0 first-pos)))))
          ;; Perform the edit
          (if replace-all
              (progn
                (goto-char (point-min))
                (while (search-forward old-string nil t)
                  (replace-match new-string t t)))
            (goto-char (point-min))
            (search-forward old-string)
            (replace-match new-string t t))
          ;; Build diff showing old and new
          (let* ((old-lines (split-string old-string "\n"))
                 (new-lines (split-string new-string "\n"))
                 (diff-output (concat
                               (mapconcat (lambda (l) (concat "- " l)) old-lines "\n")
                               "\n"
                               (mapconcat (lambda (l) (concat "+ " l)) new-lines "\n"))))
            (format "%s in buffer %s (line %d)\n\n%s"
                    (if replace-all
                        (format "Replaced %d occurrence(s)" replacements-made)
                      "Edited")
                    buffer-name edit-start-line diff-output))))))))

(claude-mcp-deftool edit-buffer
  "Edit a buffer by replacing old_string with new_string. Similar to edit-file but for buffers.
Use this for editing buffer content that may not be saved to disk yet."
  :function #'claude-mcp-edit-buffer
  :args ((buffer-name string :required "Name of the buffer to edit")
         (old_string string :required "The text to replace")
         (new_string string :required "The replacement text")
         (replace_all boolean "Replace all occurrences (default: false)")))

(defun claude-mcp-write-buffer (buffer-name content &optional mode)
  "Create a new buffer BUFFER-NAME with CONTENT.
If the buffer already exists, it will be replaced.
Optional MODE is a major mode to apply (e.g., 'python-mode', 'org-mode').
Returns the buffer name."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert content)
      ;; Apply major mode if specified
      (when mode
        (let ((mode-fn (intern mode)))
          (when (fboundp mode-fn)
            (funcall mode-fn))))
      (goto-char (point-min)))
    ;; Display the buffer
    (display-buffer buf)
    (format "Created buffer '%s' with %d characters" buffer-name (length content))))

(claude-mcp-deftool write-buffer
  "Create a new buffer with the given content. If the buffer already exists, it will be replaced.
Use this for creating scratch buffers, preview content, or temporary work areas."
  :function #'claude-mcp-write-buffer
  :context "none"
  :args ((buffer-name string :required "Name for the new buffer")
         (content string :required "Content to put in the buffer")
         (mode string "Optional major mode to apply (e.g., 'python-mode', 'org-mode')")))

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
    ;; Set CLAUDE_AGENT_SOCKET using the actual server-socket-dir and server-name
    (when (and (boundp 'server-socket-dir)
               server-socket-dir
               (boundp 'server-name)
               server-name
               (server-running-p))
      (let ((socket-file (expand-file-name server-name server-socket-dir)))
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

;;;; Interactive UX Primitives
;;
;; These functions provide structured interaction patterns between
;; the agent and user, making agent interactions more intuitive.
;;
;; Design: These use a popup buffer with keybindings rather than
;; blocking minibuffer prompts, so the MCP call can return immediately
;; while the user makes their selection.

(defvar-local claude-mcp--prompt-id nil
  "Unique ID for the current prompt.")

(defvar claude-mcp--prompt-results (make-hash-table :test 'equal)
  "Hash table storing prompt results by ID.")

(defvar claude-mcp--prompt-options nil
  "Options for the current choice prompt.")

(defvar claude-mcp--prompt-include-other nil
  "Whether to include 'Other' option.")

(defvar claude-mcp--prompt-selection 0
  "Currently selected option index.")

(defface claude-mcp-prompt-title-face
  '((t :foreground "#61afef" :weight bold :height 1.2))
  "Face for prompt titles."
  :group 'claude-mcp)

(defface claude-mcp-prompt-option-face
  '((t :foreground "#abb2bf"))
  "Face for unselected options."
  :group 'claude-mcp)

(defface claude-mcp-prompt-selected-face
  '((t :foreground "#282c34" :background "#98c379" :weight bold))
  "Face for selected option."
  :group 'claude-mcp)

(defface claude-mcp-prompt-hint-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for hint text."
  :group 'claude-mcp)

(defvar claude-mcp-choice-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "1") (lambda () (interactive) (claude-mcp--select-option 0)))
    (define-key map (kbd "2") (lambda () (interactive) (claude-mcp--select-option 1)))
    (define-key map (kbd "3") (lambda () (interactive) (claude-mcp--select-option 2)))
    (define-key map (kbd "4") (lambda () (interactive) (claude-mcp--select-option 3)))
    (define-key map (kbd "5") (lambda () (interactive) (claude-mcp--select-option 4)))
    (define-key map (kbd "6") (lambda () (interactive) (claude-mcp--select-option 5)))
    (define-key map (kbd "7") (lambda () (interactive) (claude-mcp--select-option 6)))
    (define-key map (kbd "8") (lambda () (interactive) (claude-mcp--select-option 7)))
    (define-key map (kbd "9") (lambda () (interactive) (claude-mcp--select-option 8)))
    (define-key map (kbd "j") #'claude-mcp--next-option)
    (define-key map (kbd "k") #'claude-mcp--prev-option)
    (define-key map (kbd "n") #'claude-mcp--next-option)
    (define-key map (kbd "p") #'claude-mcp--prev-option)
    (define-key map (kbd "<down>") #'claude-mcp--next-option)
    (define-key map (kbd "<up>") #'claude-mcp--prev-option)
    (define-key map (kbd "RET") #'claude-mcp--confirm-selection)
    (define-key map (kbd "o") #'claude-mcp--enter-other)
    (define-key map (kbd "q") #'claude-mcp--cancel-prompt)
    (define-key map (kbd "C-g") #'claude-mcp--cancel-prompt)
    map)
  "Keymap for choice prompt buffer.")

(define-derived-mode claude-mcp-choice-mode special-mode "Choice"
  "Mode for displaying choice prompts from Claude."
  :group 'claude-mcp
  (setq-local cursor-type nil)
  (setq-local truncate-lines t))

;; Set evil to use emacs state for this mode so our keymap works
(with-eval-after-load 'evil
  (evil-set-initial-state 'claude-mcp-choice-mode 'emacs))

(defun claude-mcp--select-option (n)
  "Select option N (0-indexed) and confirm."
  (when (< n (length claude-mcp--prompt-options))
    (setq claude-mcp--prompt-selection n)
    (claude-mcp--confirm-selection)))

(defun claude-mcp--next-option ()
  "Move to next option."
  (interactive)
  (setq claude-mcp--prompt-selection
        (min (1- (length claude-mcp--prompt-options))
             (1+ claude-mcp--prompt-selection)))
  (claude-mcp--redraw-choices))

(defun claude-mcp--prev-option ()
  "Move to previous option."
  (interactive)
  (setq claude-mcp--prompt-selection
        (max 0 (1- claude-mcp--prompt-selection)))
  (claude-mcp--redraw-choices))

(defun claude-mcp--redraw-choices ()
  "Redraw the choice list with current selection."
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)
    (claude-mcp--insert-choice-content)
    (goto-char (min pos (point-max)))))

(defun claude-mcp--insert-choice-content ()
  "Insert the choice prompt content."
  (insert (propertize claude-mcp--prompt-title 'face 'claude-mcp-prompt-title-face))
  (insert "\n\n")
  (dotimes (i (length claude-mcp--prompt-options))
    (let ((opt (nth i claude-mcp--prompt-options))
          (selected (= i claude-mcp--prompt-selection)))
      (insert (propertize (format "  %d. %s\n"
                                  (1+ i)
                                  opt)
                          'face (if selected
                                    'claude-mcp-prompt-selected-face
                                  'claude-mcp-prompt-option-face)))))
  (insert "\n")
  (insert (propertize "↑↓/jk navigate, RET confirm, 1-9 direct select"
                      'face 'claude-mcp-prompt-hint-face))
  (when claude-mcp--prompt-include-other
    (insert (propertize ", o for other" 'face 'claude-mcp-prompt-hint-face)))
  (insert (propertize ", q cancel" 'face 'claude-mcp-prompt-hint-face)))

(defun claude-mcp--confirm-selection ()
  "Confirm the current selection."
  (interactive)
  (let ((result (nth claude-mcp--prompt-selection claude-mcp--prompt-options))
        (id claude-mcp--prompt-id))
    (puthash id result claude-mcp--prompt-results)
    (quit-window t)
    (exit-recursive-edit)))

(defun claude-mcp--enter-other ()
  "Enter a custom response."
  (interactive)
  (if claude-mcp--prompt-include-other
      (let ((response (read-string "Enter your response: "))
            (id claude-mcp--prompt-id))
        (puthash id response claude-mcp--prompt-results)
        (quit-window t)
        (exit-recursive-edit))
    (message "Other option not enabled for this prompt")))

(defun claude-mcp--cancel-prompt ()
  "Cancel the prompt."
  (interactive)
  (let ((id claude-mcp--prompt-id))
    (puthash id 'cancelled claude-mcp--prompt-results)
    (quit-window t)
    (exit-recursive-edit)))

(defvar claude-mcp--prompt-title nil
  "Title for current prompt.")

(defun claude-mcp-prompt-choice (prompt options &optional include-other)
  "Present a numbered list of OPTIONS with PROMPT.
If INCLUDE-OTHER is non-nil, allow free-form input with 'o' key.
Blocks until user makes a selection. Returns the selected option or 'cancelled'."
  (let* ((id (format "choice-%s" (format-time-string "%s%N")))
         (options-list (if (stringp options)
                           (split-string options "\n" t)
                         (if (listp options) options (list options))))
         (buf (get-buffer-create "*claude-prompt*")))
    (with-current-buffer buf
      (claude-mcp-choice-mode)
      (setq-local claude-mcp--prompt-id id)
      (setq-local claude-mcp--prompt-title prompt)
      (setq-local claude-mcp--prompt-options options-list)
      (setq-local claude-mcp--prompt-include-other include-other)
      (setq-local claude-mcp--prompt-selection 0)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (claude-mcp--insert-choice-content))
      (goto-char (point-min)))
    (pop-to-buffer buf '((display-buffer-below-selected)
                         (window-height . fit-window-to-buffer)))
    ;; Block until user makes selection
    (recursive-edit)
    ;; Return the result
    (let ((result (gethash id claude-mcp--prompt-results)))
      (remhash id claude-mcp--prompt-results)
      (if (eq result 'cancelled)
          "cancelled"
        (or result "cancelled")))))

(defun claude-mcp-get-prompt-result (prompt-id)
  "Get the result for PROMPT-ID. Returns nil if not yet answered."
  (let ((result (gethash prompt-id claude-mcp--prompt-results)))
    (cond
     ((null result) "pending")
     ((eq result 'cancelled) "cancelled")
     (t result))))

(claude-mcp-deftool prompt-choice
  "Present a numbered list of choices to the user and wait for selection. Blocks until user picks an option. User can navigate with j/k or arrows, confirm with RET or 1-9, press 'o' for custom input if enabled. Returns the selected option text or 'cancelled'."
  :function #'claude-mcp-prompt-choice
  :safe t
  :args ((prompt string :required "The prompt/question to display to the user")
         (options array :required "Array of option strings to present as numbered choices")
         (include-other boolean "If true, user can press 'o' to enter custom response")))

;;;; Proposal Buffer
;;
;; A buffer for reviewing/editing proposals before accepting or rejecting.

(defvar claude-mcp--proposal-result nil
  "Result of the proposal: 'accepted, 'rejected, or 'cancelled.")

(defvar claude-mcp--proposal-original nil
  "Original content of the proposal for diff generation.")

(defvar claude-mcp--proposal-title nil
  "Title of the current proposal.")

(defvar claude-mcp-proposal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claude-mcp--accept-proposal)
    (define-key map (kbd "C-c C-k") #'claude-mcp--reject-proposal)
    map)
  "Keymap for proposal buffer.")

(define-derived-mode claude-mcp-proposal-mode text-mode "Proposal"
  "Mode for reviewing and editing proposals from Claude.
\\<claude-mcp-proposal-mode-map>
\\[claude-mcp--accept-proposal] - Accept the proposal (with any edits)
\\[claude-mcp--reject-proposal] - Reject the proposal"
  :group 'claude-mcp
  (setq header-line-format
        (propertize " C-c C-c accept | C-c C-k reject | Edit freely "
                    'face '(:foreground "#98c379" :weight bold))))

;; Evil emacs state for proposal mode too
(with-eval-after-load 'evil
  (evil-set-initial-state 'claude-mcp-proposal-mode 'insert))

(defun claude-mcp--accept-proposal ()
  "Accept the proposal with any user modifications."
  (interactive)
  (let ((content (if claude-mcp--proposal-content-marker
                     (buffer-substring-no-properties claude-mcp--proposal-content-marker (point-max))
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (setq claude-mcp--proposal-result (list 'accepted content)))
  (quit-window t)
  (exit-recursive-edit))

(defun claude-mcp--reject-proposal ()
  "Reject the proposal, returning a diff of any modifications."
  (interactive)
  (let* ((modified (if claude-mcp--proposal-content-marker
                       (buffer-substring-no-properties claude-mcp--proposal-content-marker (point-max))
                     (buffer-substring-no-properties (point-min) (point-max))))
         (diff (if (string= modified claude-mcp--proposal-original)
                   nil
                 (claude-mcp--generate-diff claude-mcp--proposal-original modified))))
    (setq claude-mcp--proposal-result (list 'rejected diff))
    (quit-window t)
    (exit-recursive-edit)))

(defun claude-mcp--generate-diff (original modified)
  "Generate a simple diff between ORIGINAL and MODIFIED text."
  (let ((orig-file (make-temp-file "proposal-orig"))
        (mod-file (make-temp-file "proposal-mod")))
    (unwind-protect
        (progn
          (with-temp-file orig-file (insert original))
          (with-temp-file mod-file (insert modified))
          (with-temp-buffer
            (call-process "diff" nil t nil "-u" orig-file mod-file)
            (buffer-string)))
      (delete-file orig-file)
      (delete-file mod-file))))

(defface claude-mcp-proposal-header-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for proposal header instructions."
  :group 'claude-mcp)

(defface claude-mcp-proposal-title-face
  '((t :foreground "#61afef" :weight bold :height 1.1))
  "Face for proposal title."
  :group 'claude-mcp)

(defface claude-mcp-proposal-separator-face
  '((t :foreground "#3e4451"))
  "Face for proposal separator line."
  :group 'claude-mcp)

(defvar-local claude-mcp--proposal-content-marker nil
  "Marker for the start of the actual proposal content.")

(defun claude-mcp--insert-proposal-header (title)
  "Insert the proposal header with TITLE and instructions."
  (let ((inhibit-read-only t))
    ;; Title
    (insert (propertize (concat "  " title "\n")
                        'face 'claude-mcp-proposal-title-face
                        'read-only t
                        'front-sticky t
                        'rear-nonsticky t))
    ;; Instructions
    (insert (propertize "  Review and edit this proposal, then:\n"
                        'face 'claude-mcp-proposal-header-face
                        'read-only t))
    (insert (propertize "    C-c C-c  "
                        'face '(:foreground "#98c379" :weight bold)
                        'read-only t))
    (insert (propertize "Accept proposal (with your edits)\n"
                        'face 'claude-mcp-proposal-header-face
                        'read-only t))
    (insert (propertize "    C-c C-k  "
                        'face '(:foreground "#e06c75" :weight bold)
                        'read-only t))
    (insert (propertize "Reject proposal (sends your edits as feedback)\n"
                        'face 'claude-mcp-proposal-header-face
                        'read-only t))
    ;; Separator
    (insert (propertize (concat "  " (make-string 60 ?─) "\n\n")
                        'face 'claude-mcp-proposal-separator-face
                        'read-only t
                        'rear-nonsticky t))))

(defun claude-mcp-show-proposal (title content &optional mode)
  "Show a proposal buffer with TITLE and CONTENT for user review.
Optional MODE specifies the major mode for syntax highlighting (e.g., 'python-mode).
Blocks until user accepts (C-c C-c) or rejects (C-c C-k).
Returns a list: (status result) where:
- For accepted: ('accepted final-content)
- For rejected with edits: ('rejected diff-string)
- For rejected without edits: ('rejected nil)"
  (let ((buf (get-buffer-create "*claude-proposal*"))
        (content-start nil))
    (setq claude-mcp--proposal-original content)
    (setq claude-mcp--proposal-result nil)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Insert header with instructions
        (claude-mcp--insert-proposal-header title)
        ;; Remember where content starts
        (setq content-start (point))
        ;; Insert the actual content
        (insert content))
      ;; Apply syntax highlighting mode if specified
      (when mode
        (let ((mode-fn (intern mode)))
          (when (fboundp mode-fn)
            (funcall mode-fn))))
      ;; Then enable our mode for keybindings
      (claude-mcp-proposal-mode)
      (setq-local claude-mcp--proposal-title title)
      ;; Set marker after mode is enabled (mode might reset buffer-locals)
      (setq-local claude-mcp--proposal-content-marker (copy-marker content-start))
      ;; Go to start of editable content
      (goto-char content-start))
    (pop-to-buffer buf '((display-buffer-same-window)))
    ;; Block until user responds
    (recursive-edit)
    ;; Return result
    (let ((result claude-mcp--proposal-result))
      (setq claude-mcp--proposal-result nil)
      (setq claude-mcp--proposal-original nil)
      (pcase result
        (`(accepted ,content) (format "ACCEPTED\n%s" content))
        (`(rejected nil) "REJECTED")
        (`(rejected ,diff) (format "REJECTED_WITH_CHANGES\n%s" diff))
        (_ "CANCELLED")))))

(claude-mcp-deftool show-proposal
  "Show a proposal buffer for user to review, edit, and accept/reject. The user can freely edit the content. Blocks until user presses C-c C-c (accept) or C-c C-k (reject). Returns 'ACCEPTED\\n<content>' if accepted, 'REJECTED' if rejected without changes, or 'REJECTED_WITH_CHANGES\\n<diff>' if rejected after making edits."
  :function #'claude-mcp-show-proposal
  :safe t
  :args ((title string :required "Title for the proposal (shown in header)")
         (content string :required "The proposal content to display")
         (mode string "Optional major mode for syntax highlighting (e.g., 'python-mode', 'org-mode')")))

;;;; Confirmation Prompt
;;
;; Simple yes/no confirmation dialog using popup buffer.

(defvar claude-mcp--confirm-result nil
  "Stores the result of the current confirmation prompt.")

(defvar-local claude-mcp--confirm-id nil
  "Unique ID for the current confirmation prompt buffer.")

(defvar claude-mcp--confirm-results (make-hash-table :test 'equal)
  "Hash table storing confirmation results by ID.")

(defvar claude-mcp-confirm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "y") #'claude-mcp-confirm--yes)
    (define-key map (kbd "Y") #'claude-mcp-confirm--yes)
    (define-key map (kbd "n") #'claude-mcp-confirm--no)
    (define-key map (kbd "N") #'claude-mcp-confirm--no)
    (define-key map (kbd "RET") #'claude-mcp-confirm--yes)
    (define-key map (kbd "q") #'claude-mcp-confirm--no)
    (define-key map (kbd "C-g") #'claude-mcp-confirm--cancel)
    map)
  "Keymap for confirmation prompt buffer.")

(define-derived-mode claude-mcp-confirm-mode special-mode "Confirm"
  "Major mode for confirmation prompts."
  :interactive nil
  (setq buffer-read-only t
        truncate-lines t))

(with-eval-after-load 'evil
  (evil-set-initial-state 'claude-mcp-confirm-mode 'emacs))

(defun claude-mcp-confirm--yes ()
  "Accept the confirmation."
  (interactive)
  (puthash claude-mcp--confirm-id "yes" claude-mcp--confirm-results)
  (quit-window t)
  (exit-recursive-edit))

(defun claude-mcp-confirm--no ()
  "Reject the confirmation."
  (interactive)
  (puthash claude-mcp--confirm-id "no" claude-mcp--confirm-results)
  (quit-window t)
  (exit-recursive-edit))

(defun claude-mcp-confirm--cancel ()
  "Cancel the confirmation."
  (interactive)
  (puthash claude-mcp--confirm-id "cancelled" claude-mcp--confirm-results)
  (quit-window t)
  (exit-recursive-edit))

(defun claude-mcp-confirm (prompt)
  "Ask user for yes/no confirmation with PROMPT.
Returns \"yes\", \"no\", or \"cancelled\"."
  (let* ((id (format "confirm-%s" (format-time-string "%s%N")))
         (buf (get-buffer-create "*claude-confirm*")))
    (with-current-buffer buf
      (claude-mcp-confirm-mode)
      (setq claude-mcp--confirm-id id)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n")
        (insert (propertize "  Confirmation Required\n"
                            'face '(:foreground "#e5c07b" :weight bold :height 1.3)))
        (insert (propertize (concat "  " (make-string 50 ?─) "\n\n")
                            'face '(:foreground "#5c6370")))
        (insert (propertize (concat "  " prompt "\n\n")
                            'face '(:foreground "#abb2bf" :height 1.1)))
        (insert (propertize "  " 'face 'default))
        (insert (propertize "[y]" 'face '(:foreground "#98c379" :weight bold)))
        (insert (propertize " Yes    " 'face '(:foreground "#abb2bf")))
        (insert (propertize "[n]" 'face '(:foreground "#e06c75" :weight bold)))
        (insert (propertize " No\n" 'face '(:foreground "#abb2bf")))
        (goto-char (point-min))))
    (pop-to-buffer buf '((display-buffer-below-selected)
                         (window-height . fit-window-to-buffer)))
    (recursive-edit)
    ;; Return the result
    (let ((result (gethash id claude-mcp--confirm-results)))
      (remhash id claude-mcp--confirm-results)
      (or result "cancelled"))))

(claude-mcp-deftool confirm
  "Ask the user for a simple yes/no confirmation. Displays a popup with [y] Yes / [n] No options. Returns 'yes', 'no', or 'cancelled'."
  :function #'claude-mcp-confirm
  :safe t
  :args ((prompt string :required "The confirmation prompt to display")))

;;;; Multi-Select Prompt
;;
;; Checkbox-style selection from a list of options.

(defvar-local claude-mcp--multiselect-id nil
  "Unique ID for the current multi-select buffer.")

(defvar-local claude-mcp--multiselect-options nil
  "List of options for the current multi-select.")

(defvar-local claude-mcp--multiselect-selected nil
  "Hash table of selected option indices.")

(defvar-local claude-mcp--multiselect-first-line nil
  "Line number of the first option.")

(defvar claude-mcp--multiselect-results (make-hash-table :test 'equal)
  "Hash table storing multi-select results by ID.")

(defvar claude-mcp-multiselect-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'claude-mcp-multiselect--toggle)
    (define-key map (kbd "x") #'claude-mcp-multiselect--toggle)
    (define-key map (kbd "RET") #'claude-mcp-multiselect--confirm)
    (define-key map (kbd "C-c C-c") #'claude-mcp-multiselect--confirm)
    (define-key map (kbd "q") #'claude-mcp-multiselect--cancel)
    (define-key map (kbd "C-g") #'claude-mcp-multiselect--cancel)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "j") #'next-line)
    (define-key map (kbd "k") #'previous-line)
    (define-key map (kbd "a") #'claude-mcp-multiselect--select-all)
    (define-key map (kbd "u") #'claude-mcp-multiselect--unselect-all)
    map)
  "Keymap for multi-select buffer.")

(define-derived-mode claude-mcp-multiselect-mode special-mode "MultiSelect"
  "Major mode for multi-select prompts."
  :interactive nil
  (setq buffer-read-only t
        truncate-lines t))

(with-eval-after-load 'evil
  (evil-set-initial-state 'claude-mcp-multiselect-mode 'emacs))

(defun claude-mcp-multiselect--get-option-index ()
  "Get the option index at the current line."
  (let ((line (line-number-at-pos)))
    (when (>= line claude-mcp--multiselect-first-line)
      (- line claude-mcp--multiselect-first-line))))

(defun claude-mcp-multiselect--refresh ()
  "Refresh the display of checkboxes."
  (let ((inhibit-read-only t)
        (pos (point)))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- claude-mcp--multiselect-first-line))
      (dotimes (i (length claude-mcp--multiselect-options))
        (let ((option (nth i claude-mcp--multiselect-options))
              (selected (gethash i claude-mcp--multiselect-selected)))
          (delete-region (line-beginning-position) (line-end-position))
          (insert (propertize (if selected "  [x] " "  [ ] ")
                              'face (if selected
                                        '(:foreground "#98c379" :weight bold)
                                      '(:foreground "#5c6370"))))
          (insert (propertize option
                              'face (if selected
                                        '(:foreground "#98c379")
                                      '(:foreground "#abb2bf"))))
          (forward-line 1))))
    (goto-char pos)))

(defun claude-mcp-multiselect--toggle ()
  "Toggle selection of the current option."
  (interactive)
  (let ((idx (claude-mcp-multiselect--get-option-index)))
    (when (and idx (< idx (length claude-mcp--multiselect-options)))
      (puthash idx (not (gethash idx claude-mcp--multiselect-selected))
               claude-mcp--multiselect-selected)
      (claude-mcp-multiselect--refresh)
      (forward-line 1))))

(defun claude-mcp-multiselect--select-all ()
  "Select all options."
  (interactive)
  (dotimes (i (length claude-mcp--multiselect-options))
    (puthash i t claude-mcp--multiselect-selected))
  (claude-mcp-multiselect--refresh))

(defun claude-mcp-multiselect--unselect-all ()
  "Unselect all options."
  (interactive)
  (clrhash claude-mcp--multiselect-selected)
  (claude-mcp-multiselect--refresh))

(defun claude-mcp-multiselect--confirm ()
  "Confirm the selection."
  (interactive)
  (let ((selected-items '()))
    (dotimes (i (length claude-mcp--multiselect-options))
      (when (gethash i claude-mcp--multiselect-selected)
        (push (nth i claude-mcp--multiselect-options) selected-items)))
    (puthash claude-mcp--multiselect-id (nreverse selected-items)
             claude-mcp--multiselect-results)
    (quit-window t)
    (exit-recursive-edit)))

(defun claude-mcp-multiselect--cancel ()
  "Cancel the multi-select."
  (interactive)
  (puthash claude-mcp--multiselect-id 'cancelled claude-mcp--multiselect-results)
  (quit-window t)
  (exit-recursive-edit))

(defun claude-mcp-multiselect (prompt options)
  "Present OPTIONS with checkboxes under PROMPT.
OPTIONS can be a newline-separated string or a list.
Returns a list of selected items or \"cancelled\"."
  (let* ((id (format "multiselect-%s" (format-time-string "%s%N")))
         (options-list (if (stringp options)
                           (split-string options "\n" t)
                         (if (listp options) options (list options))))
         (buf (get-buffer-create "*claude-multiselect*")))
    (with-current-buffer buf
      (claude-mcp-multiselect-mode)
      (setq claude-mcp--multiselect-id id
            claude-mcp--multiselect-options options-list
            claude-mcp--multiselect-selected (make-hash-table :test 'equal))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n")
        (insert (propertize (concat "  " prompt "\n")
                            'face '(:foreground "#61afef" :weight bold :height 1.2)))
        (insert (propertize (concat "  " (make-string 50 ?─) "\n")
                            'face '(:foreground "#5c6370")))
        (insert (propertize "  SPC/x toggle  |  a select all  |  u unselect all  |  RET confirm  |  q cancel\n\n"
                            'face '(:foreground "#5c6370" :slant italic)))
        (setq claude-mcp--multiselect-first-line (line-number-at-pos))
        (let ((start-pos (point)))
          (dolist (option options-list)
            (insert (propertize "  [ ] " 'face '(:foreground "#5c6370")))
            (insert (propertize (concat option "\n") 'face '(:foreground "#abb2bf"))))
          (goto-char start-pos))))
    (pop-to-buffer buf '((display-buffer-below-selected)
                         (window-height . fit-window-to-buffer)))
    (recursive-edit)
    ;; Return the result
    (let ((result (gethash id claude-mcp--multiselect-results)))
      (remhash id claude-mcp--multiselect-results)
      (if (eq result 'cancelled)
          "cancelled"
        (or result '())))))

(claude-mcp-deftool multiselect
  "Present a list of options with checkboxes for multi-selection. User can toggle items with SPC/x, select all with 'a', unselect all with 'u'. Returns a list of selected items or 'cancelled'."
  :function #'claude-mcp-multiselect
  :safe t
  :args ((prompt string :required "The prompt to display above the options")
         (options string :required "Newline-separated list of options")))

;;;; File Picker
;;
;; Let user select a file from the project using popup buffer.

(defvar-local claude-mcp--picker-id nil
  "Unique ID for the current file picker buffer.")

(defvar-local claude-mcp--picker-files nil
  "List of files for the current picker.")

(defvar-local claude-mcp--picker-directory nil
  "Base directory for the current picker.")

(defvar claude-mcp--picker-results (make-hash-table :test 'equal)
  "Hash table storing file picker results by ID.")

(defvar claude-mcp-file-picker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-mcp-picker--select)
    (define-key map (kbd "C-g") #'claude-mcp-picker--cancel)
    (define-key map (kbd "q") #'claude-mcp-picker--cancel)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "j") #'next-line)
    (define-key map (kbd "k") #'previous-line)
    map)
  "Keymap for file picker buffer.")

(define-derived-mode claude-mcp-file-picker-mode special-mode "FilePicker"
  "Major mode for file picker prompts."
  :interactive nil
  (setq buffer-read-only t
        truncate-lines t))

(with-eval-after-load 'evil
  (evil-set-initial-state 'claude-mcp-file-picker-mode 'emacs))

(defvar-local claude-mcp--picker-first-file-line nil
  "Line number of the first file in the picker.")

(defun claude-mcp-picker--select ()
  "Select the file at point."
  (interactive)
  (let* ((current-line (line-number-at-pos))
         (file-index (- current-line claude-mcp--picker-first-file-line))
         (file (when (>= file-index 0)
                 (nth file-index claude-mcp--picker-files))))
    (if file
        (progn
          (puthash claude-mcp--picker-id
                   (expand-file-name file claude-mcp--picker-directory)
                   claude-mcp--picker-results)
          (quit-window t)
          (exit-recursive-edit))
      (message "No file selected"))))

(defun claude-mcp-picker--cancel ()
  "Cancel the file picker."
  (interactive)
  (puthash claude-mcp--picker-id "cancelled" claude-mcp--picker-results)
  (quit-window t)
  (exit-recursive-edit))

(defun claude-mcp-pick-file (&optional prompt directory)
  "Let user pick a file with completion.
PROMPT is the prompt to display (default: \"Select file: \").
DIRECTORY is the starting directory (default: current project root or default-directory).
Returns the selected file path or \"cancelled\" if user cancels."
  (let* ((id (format "picker-%s" (format-time-string "%s%N")))
         (prompt (or prompt "Select file"))
         (dir (or directory
                  (when (fboundp 'project-root)
                    (when-let ((proj (project-current)))
                      (project-root proj)))
                  default-directory))
         ;; Get project files if in a project, otherwise list directory
         (files (if (and (fboundp 'project-current) (project-current))
                    (mapcar (lambda (f) (file-relative-name f dir))
                            (project-files (project-current)))
                  (directory-files dir nil "^[^.]")))
         (buf (get-buffer-create "*claude-file-picker*")))
    (with-current-buffer buf
      (claude-mcp-file-picker-mode)
      (setq claude-mcp--picker-id id
            claude-mcp--picker-files files
            claude-mcp--picker-directory dir)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (concat "  " prompt "\n")
                            'face '(:foreground "#61afef" :weight bold :height 1.2)))
        (insert (propertize (concat "  " dir "\n")
                            'face '(:foreground "#5c6370")))
        (insert (propertize (concat "  " (make-string 50 ?─) "\n")
                            'face '(:foreground "#5c6370")))
        (insert (propertize "  Use j/k or n/p to navigate, RET to select, q to cancel\n\n"
                            'face '(:foreground "#5c6370" :slant italic)))
        ;; Store where file list starts (line number)
        (setq claude-mcp--picker-first-file-line (line-number-at-pos))
        (let ((file-start (point)))
          (dolist (file files)
            (insert (propertize (concat "  " file "\n")
                                'face '(:foreground "#abb2bf"))))
          (goto-char file-start))))
    (pop-to-buffer buf '((display-buffer-below-selected)
                         (window-height . 20)))
    (recursive-edit)
    ;; Return the result
    (let ((result (gethash id claude-mcp--picker-results)))
      (remhash id claude-mcp--picker-results)
      (or result "cancelled"))))

(claude-mcp-deftool pick-file
  "Show a file picker for user to select a file from the project. Lists project files in a popup buffer. Returns the selected file path or 'cancelled'."
  :function #'claude-mcp-pick-file
  :safe t
  :args ((prompt string "Prompt to display (default: 'Select file')")
         (directory string "Starting directory (default: project root)")))

;;;; Directory Picker
;;
;; Let user select a directory using popup buffer.

(defun claude-mcp-pick-directory (&optional prompt directory)
  "Let user pick a directory with completion.
PROMPT is the prompt to display (default: \"Select directory\").
DIRECTORY is the starting directory (default: current project root or default-directory).
Returns the selected directory path or \"cancelled\" if user cancels."
  (let* ((id (format "picker-%s" (format-time-string "%s%N")))
         (prompt (or prompt "Select directory"))
         (dir (or directory
                  (when (fboundp 'project-root)
                    (when-let ((proj (project-current)))
                      (project-root proj)))
                  default-directory))
         ;; Get only directories
         (dirs (cl-remove-if-not
                (lambda (f) (file-directory-p (expand-file-name f dir)))
                (directory-files dir nil "^[^.]")))
         (buf (get-buffer-create "*claude-file-picker*")))
    (with-current-buffer buf
      (claude-mcp-file-picker-mode)
      (setq claude-mcp--picker-id id
            claude-mcp--picker-files dirs
            claude-mcp--picker-directory dir)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (concat "  " prompt "\n")
                            'face '(:foreground "#61afef" :weight bold :height 1.2)))
        (insert (propertize (concat "  " dir "\n")
                            'face '(:foreground "#5c6370")))
        (insert (propertize (concat "  " (make-string 50 ?─) "\n")
                            'face '(:foreground "#5c6370")))
        (insert (propertize "  Use j/k or n/p to navigate, RET to select, q to cancel\n\n"
                            'face '(:foreground "#5c6370" :slant italic)))
        ;; Track where directories start (including ./ option)
        (setq claude-mcp--picker-first-file-line (line-number-at-pos))
        ;; Add option to select current directory
        (let ((current-dir-start (point)))
          (insert (propertize "  ./ (current directory)\n"
                              'face '(:foreground "#98c379")))
          (dolist (d dirs)
            (insert (propertize (concat "  " d "/\n")
                                'face '(:foreground "#abb2bf"))))
          ;; Store dirs with ./ as first option
          (setq claude-mcp--picker-files (cons "./" dirs))
          (goto-char current-dir-start))))
    (pop-to-buffer buf '((display-buffer-below-selected)
                         (window-height . 15)))
    (recursive-edit)
    ;; Return the result
    (let ((result (gethash id claude-mcp--picker-results)))
      (remhash id claude-mcp--picker-results)
      (or result "cancelled"))))

(claude-mcp-deftool pick-directory
  "Show a directory picker for user to select a directory. Lists subdirectories in a popup buffer. Returns the selected directory path or 'cancelled'."
  :function #'claude-mcp-pick-directory
  :safe t
  :args ((prompt string "Prompt to display (default: 'Select directory')")
         (directory string "Starting directory (default: project root)")))

;;;; Progress Indicator
;;
;; Updatable progress messages for long operations.

(defun claude-mcp--find-claude-buffer ()
  "Find the Claude agent buffer for the current session.
Uses CLAUDE_AGENT_BUFFER_NAME env var if set, otherwise searches for *claude:* buffers."
  (or (when-let ((name (getenv "CLAUDE_AGENT_BUFFER_NAME")))
        (get-buffer name))
      ;; Fallback: find any claude buffer
      (cl-find-if (lambda (buf)
                    (string-match-p "^\\*claude:" (buffer-name buf)))
                  (buffer-list))))

(defun claude-mcp-progress-start (message &optional id percent)
  "Start a progress indicator with MESSAGE as label in the Claude buffer.
Returns the progress ID which can be used to update or stop it.
Optional ID allows specifying a custom identifier.
Optional PERCENT sets initial progress (default 0)."
  (if-let ((buf (claude-mcp--find-claude-buffer)))
      (with-current-buffer buf
        (claude-agent-progress-start message id percent))
    ;; Fallback to echo area if no Claude buffer
    (let* ((progress-id (or id (format "progress-%s" (format-time-string "%s%N"))))
           (pct (or percent 0))
           (filled (round (* 10 (/ (min pct 100.0) 100.0))))
           (empty (- 10 filled)))
      (message "[▐%s%s▌] %s (%d%%)"
               (make-string filled ?█)
               (make-string empty ?░)
               message
               (round pct))
      progress-id)))

(defun claude-mcp-progress-update (id message &optional percent)
  "Update progress indicator ID with new MESSAGE and optional PERCENT (0-100)."
  (if-let ((buf (claude-mcp--find-claude-buffer)))
      (with-current-buffer buf
        (claude-agent-progress-update id message percent))
    ;; Fallback to echo area
    (let* ((pct (or percent 0))
           (filled (round (* 10 (/ (min pct 100.0) 100.0))))
           (empty (- 10 filled)))
      (message "[▐%s%s▌] %s (%d%%)"
               (make-string filled ?█)
               (make-string empty ?░)
               message
               (round pct))))
  id)

(defun claude-mcp-progress-stop (id &optional final-message)
  "Stop progress indicator ID.
Optional FINAL-MESSAGE is displayed briefly."
  (if-let ((buf (claude-mcp--find-claude-buffer)))
      (with-current-buffer buf
        (claude-agent-progress-stop id final-message))
    ;; Fallback to echo area
    (when final-message
      (message "✓ %s" final-message)))
  "stopped")

(claude-mcp-deftool progress-start
  "Start a progress indicator with an animated bar. Returns a progress ID that can be used to update or stop the indicator. Use this for long-running operations to provide feedback to the user."
  :function #'claude-mcp-progress-start
  :safe t
  :args ((message string :required "The progress message/label to display")
         (id string "Optional custom identifier for the progress indicator")))

(claude-mcp-deftool progress-update
  "Update an existing progress indicator with a new message and optional percentage."
  :function #'claude-mcp-progress-update
  :safe t
  :args ((id string :required "The progress indicator ID returned by progress-start")
         (message string :required "The new progress message/label")
         (percent number "Optional progress percentage 0-100. If provided, shows a filled bar.")))

(claude-mcp-deftool progress-stop
  "Stop a progress indicator. Optionally display a final completion message."
  :function #'claude-mcp-progress-stop
  :safe t
  :args ((id string :required "The progress indicator ID to stop")
         (final_message string "Optional final message to display (prefixed with ✓)")))

;;;; Session Control

(defun claude-mcp-restart-session ()
  "Restart the Claude session to reload the MCP server and Python agent.
The conversation will be continued from where it left off."
  (if-let ((buf (claude-mcp--find-claude-buffer)))
      (with-current-buffer buf
        (claude-agent-restart)
        "Session restarting... MCP server will be reloaded.")
    "No Claude buffer found"))

(claude-mcp-deftool restart-session
  "Restart the Claude session to reload the MCP server and Python agent. Use this after making changes to elisp files that need to be picked up by the MCP server. The conversation will be continued from where it left off."
  :function #'claude-mcp-restart-session
  :safe t
  :args ())

;;;; Magit Integration Tools
;;
;; These tools provide git operations via magit with a commit approval workflow.
;; The agent can stage files and propose commits, but the user must approve.

(claude-mcp-deftool magit-status
  "Get current git status including staged, unstaged, and untracked files. Returns branch name and file lists."
  :function #'claude-mcp-magit-status
  :safe t
  :args ((directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-stage
  "Stage files for commit. Takes a list of file paths relative to the repository root."
  :function #'claude-mcp-magit-stage
  :safe nil
  :args ((files array :required "Array of file paths to stage")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-unstage
  "Unstage files (remove from staging area). Takes a list of file paths."
  :function #'claude-mcp-magit-unstage
  :safe nil
  :args ((files array :required "Array of file paths to unstage")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-diff
  "Get git diff output. Can get diff for a specific file or all changes. Use staged=true for staged changes."
  :function #'claude-mcp-magit-diff
  :safe t
  :args ((file string "Specific file to diff (default: all files)")
         (directory string "Git repository directory (default: session working directory)")
         (staged boolean "If true, show staged diff; otherwise show unstaged diff")))

(claude-mcp-deftool magit-log
  "Get recent git commit log entries."
  :function #'claude-mcp-magit-log
  :safe t
  :args ((count integer "Number of log entries to return (default: 5)")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-commit-propose
  "Propose a commit for user approval. The user must call magit-commit-approve to actually create the commit. This allows the user to review and sign the commit with their GPG key."
  :function #'claude-mcp-magit-commit-propose
  :safe nil
  :args ((message string :required "The commit message to propose")
         (directory string "Git repository directory (default: session working directory)")))

(claude-mcp-deftool magit-commit-status
  "Check if there's a pending commit proposal awaiting user approval."
  :function #'claude-mcp-magit-commit-status
  :safe t
  :args ())

;;;; Safe Tools Query

(defun claude-mcp-get-safe-tools ()
  "Return list of tool names marked as safe.
These tools can be pre-authorized via --allowedTools."
  (let (safe-tools)
    (maphash
     (lambda (name def)
       (when (plist-get def :safe)
         (push name safe-tools)))
     claude-mcp-tools)
    (sort safe-tools #'string<)))

(defconst claude-mcp-native-safe-tools
  '("watch_buffer" "watch_for_pattern" "watch_for_change"
    "spawn_agent" "list_agents" "message_agent" "check_messages"
    "message_board_summary" "whoami")
  "Native Python MCP tools that are safe (defined in server.py).")

(defun claude-mcp-get-safe-tools-for-cli ()
  "Return safe MCP tool names formatted for Claude CLI --allowedTools.
Format: mcp__emacs__toolname
Includes both elisp-defined tools and native Python tools."
  (let ((elisp-tools (claude-mcp-get-safe-tools))
        (native-tools claude-mcp-native-safe-tools))
    (mapcar (lambda (name)
              (format "mcp__emacs__%s" name))
            (append elisp-tools native-tools))))

(provide 'claude-mcp)
;;; claude-mcp.el ends here
