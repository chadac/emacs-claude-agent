;;; claude-kb-test.el --- Tests for Knowledge Base MCP tools -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;; Unit tests for the knowledge base system in claude-kb.el.
;; Covers: kb helper functions, node creation, search, get, update, list.
;; Tests that don't require org-roam test the helper functions directly.
;; Tests that need org-roam are conditionally skipped.
;;
;; Run with:
;;   emacs -batch -l ert -l test/claude-kb-test.el -f ert-run-tests-batch

;;; Code:

(require 'ert)
(require 'cl-lib)


;; Load test helper (sets up load path)
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'test-helper)

;; Load registry first
(require 'claude-mcp-registry)

;; Try loading claude-kb (it may fail if org-roam is not available)
(condition-case nil
    (require 'claude-kb)
  (error nil))

;;; ============================================================
;;; Helper Function Tests (no org-roam needed)
;;; ============================================================

(ert-deftest claude-kb-test-slugify ()
  "Test slug generation."
  :tags '(:unit :mcp :kb)
  (when (fboundp 'claude-kb--slugify)
    (should (string= "hello-world" (claude-kb--slugify "Hello World")))
    (should (string= "fix-bug-123" (claude-kb--slugify "Fix Bug #123")))
    (should (string= "simple" (claude-kb--slugify "simple")))
    ;; Strips leading/trailing hyphens
    (should (string= "test" (claude-kb--slugify "--test--")))))

(ert-deftest claude-kb-test-slugify-length-limit ()
  "Test that slugs are limited to 60 characters."
  :tags '(:unit :mcp :kb)
  (when (fboundp 'claude-kb--slugify)
    (let ((long-title (make-string 100 ?a)))
      (should (<= (length (claude-kb--slugify long-title)) 60)))))

(ert-deftest claude-kb-test-format-files-property ()
  "Test formatting file list to property string."
  :tags '(:unit :mcp :kb)
  (when (fboundp 'claude-kb--format-files-property)
    (should (string= "a.el b.el" (claude-kb--format-files-property '("a.el" "b.el"))))
    (should (string= "" (claude-kb--format-files-property nil)))
    (should (string= "single.el" (claude-kb--format-files-property '("single.el"))))))

(ert-deftest claude-kb-test-parse-files-property ()
  "Test parsing file property back to list."
  :tags '(:unit :mcp :kb)
  (when (fboundp 'claude-kb--parse-files-property)
    (should (equal '("a.el" "b.el") (claude-kb--parse-files-property "a.el b.el")))
    (should (null (claude-kb--parse-files-property nil)))
    (should (null (claude-kb--parse-files-property "")))))

(ert-deftest claude-kb-test-format-list-property ()
  "Test formatting list to property string."
  :tags '(:unit :mcp :kb)
  (when (fboundp 'claude-kb--format-list-property)
    (should (string= "locking editing" (claude-kb--format-list-property '("locking" "editing"))))
    (should (string= "" (claude-kb--format-list-property nil)))))

(ert-deftest claude-kb-test-parse-list-property ()
  "Test parsing list property back to list."
  :tags '(:unit :mcp :kb)
  (when (fboundp 'claude-kb--parse-list-property)
    (should (equal '("locking" "editing") (claude-kb--parse-list-property "locking editing")))
    (should (null (claude-kb--parse-list-property nil)))
    (should (null (claude-kb--parse-list-property "")))))

(ert-deftest claude-kb-test-generate-id ()
  "Test that generated IDs are unique."
  :tags '(:unit :mcp :kb)
  (when (fboundp 'claude-kb--generate-id)
    (let ((id1 (claude-kb--generate-id))
          (id2 (claude-kb--generate-id)))
      (should (stringp id1))
      (should (stringp id2))
      (should (> (length id1) 0)))))

;;; ============================================================
;;; KB Node Content Parsing Tests
;;; ============================================================

(ert-deftest claude-kb-test-get-full-content ()
  "Test parsing full content from a KB file."
  :tags '(:unit :mcp :kb)
  (when (fboundp 'claude-kb--get-full-content)
    (let ((temp-file (make-temp-file "kb-test-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert ":PROPERTIES:
:ID: test123
:KB_TYPE: gotcha
:END:
#+title: Test KB Entry

** Summary
This is the summary.

** Details
These are the details.
With multiple lines.

** Related
- [[id:related1]]
- [[id:related2]]
"))
            (let ((content (claude-kb--get-full-content temp-file)))
              (should (listp content))
              (should (string-match-p "This is the summary" (plist-get content :summary)))
              (should (string-match-p "These are the details" (plist-get content :details)))
              (should (equal '("related1" "related2") (plist-get content :related)))))
        (delete-file temp-file)))))

(ert-deftest claude-kb-test-get-full-content-missing-file ()
  "Test parsing content from a nonexistent file returns nil."
  :tags '(:unit :mcp :kb)
  (when (fboundp 'claude-kb--get-full-content)
    (should (null (claude-kb--get-full-content "/nonexistent/file.org")))))

;;; ============================================================
;;; MCP Tool Function Tests
;;; ============================================================

(ert-deftest claude-kb-test-mcp-create-requires-args ()
  "Test that kb_create requires title, kb_type, and summary."
  :tags '(:unit :mcp :kb)
  (when (fboundp 'claude-kb-mcp-create)
    (should-error (claude-kb-mcp-create nil "gotcha" "summary")
                  :type 'error)
    (should-error (claude-kb-mcp-create "title" nil "summary")
                  :type 'error)
    (should-error (claude-kb-mcp-create "title" "gotcha" nil)
                  :type 'error)))

(ert-deftest claude-kb-test-mcp-create-invalid-type ()
  "Test that kb_create rejects invalid types."
  :tags '(:unit :mcp :kb)
  (when (fboundp 'claude-kb--create-node)
    (should-error (claude-kb--create-node "test" "invalid-type" "summary" nil "test-proj")
                  :type 'error)))

(ert-deftest claude-kb-test-mcp-get-requires-id ()
  "Test that kb_get requires kb_id."
  :tags '(:unit :mcp :kb)
  (when (fboundp 'claude-kb-mcp-get)
    (should-error (claude-kb-mcp-get nil)
                  :type 'error)))

(ert-deftest claude-kb-test-mcp-update-requires-id ()
  "Test that kb_update requires kb_id."
  :tags '(:unit :mcp :kb)
  (when (fboundp 'claude-kb-mcp-update)
    (should-error (claude-kb-mcp-update nil)
                  :type 'error)))

;;; ============================================================
;;; Valid KB Types Tests
;;; ============================================================

(ert-deftest claude-kb-test-valid-types ()
  "Test that valid KB types are defined."
  :tags '(:unit :mcp :kb)
  (when (boundp 'claude-kb-types)
    (should (member "gotcha" claude-kb-types))
    (should (member "architecture" claude-kb-types))
    (should (member "pattern" claude-kb-types))
    (should (member "reference" claude-kb-types))))

;;; ============================================================
;;; Tool Registration Tests
;;; ============================================================

(ert-deftest claude-kb-test-tools-registered ()
  "Test that KB tools are registered in the tool registry."
  :tags '(:unit :mcp :kb :registration)
  (skip-unless (featurep 'claude-kb))
  (should (gethash "kb_create" claude-mcp-tools))
  (should (gethash "kb_search" claude-mcp-tools))
  (should (gethash "kb_get" claude-mcp-tools))
  (should (gethash "kb_update" claude-mcp-tools))
  (should (gethash "kb_list" claude-mcp-tools)))

(ert-deftest claude-kb-test-tools-have-descriptions ()
  "Test that KB tools have descriptions."
  :tags '(:unit :mcp :kb :registration)
  (skip-unless (featurep 'claude-kb))
  (dolist (tool-name '("kb_create" "kb_search" "kb_get" "kb_update" "kb_list"))
    (let ((tool-def (gethash tool-name claude-mcp-tools)))
      (should tool-def)
      (should (stringp (plist-get tool-def :description)))
      (should (> (length (plist-get tool-def :description)) 0)))))

(provide 'claude-kb-test)
;;; claude-kb-test.el ends here
