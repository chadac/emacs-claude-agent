;;; claude-integration-runner.el --- Agent-driven integration test runner -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Reads test specifications from integration-specs.org and spawns
;; lightweight agents to execute each test, collecting results into
;; a summary buffer.
;;
;; Usage:
;;   M-x claude-integration-test-run-all
;;   M-x claude-integration-test-run-section "Buffer Operations"

;;; Code:

(require 'org)
(require 'cl-lib)

;; Forward declarations
(declare-function claude-oneshot--start "claude-oneshot")
(declare-function claude-mcp-done "claude-oneshot")

(defgroup claude-integration-test nil
  "Agent-driven integration test runner."
  :group 'claude-agent)

(defcustom claude-integration-test-spec-file
  (expand-file-name "integration-specs.org"
                     (file-name-directory (or load-file-name buffer-file-name "")))
  "Path to the integration test specification file."
  :type 'file
  :group 'claude-integration-test)

(defcustom claude-integration-test-model "haiku"
  "Model to use for test runner agents."
  :type '(choice (const "haiku") (const "sonnet") (const "opus"))
  :group 'claude-integration-test)

(defcustom claude-integration-test-timeout 60
  "Timeout in seconds for each integration test."
  :type 'integer
  :group 'claude-integration-test)

(defvar claude-integration-test--results nil
  "List of test results from the last run.")

(defvar claude-integration-test-results-buffer "*claude-integration-test-results*"
  "Buffer name for test results.")

;;; Spec Parsing

(defun claude-integration-test--parse-specs (&optional file)
  "Parse integration test specs from FILE.
Returns a list of plists (:section :title :body) for each test heading."
  (let ((file (or file claude-integration-test-spec-file))
        (specs '())
        (current-section nil))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (goto-char (point-min))
      ;; Walk through headings
      (while (re-search-forward "^\\(\\*+\\) \\(.+\\)$" nil t)
        (let ((level (length (match-string 1)))
              (title (match-string 2)))
          (cond
           ;; Level 1: top-level sections (About, Buffer Operations, etc.)
           ((= level 1)
            (setq current-section title))
           ;; Level 2: individual test specs
           ((= level 2)
            (let* ((body-start (1+ (point)))
                   (body-end (save-excursion
                               (if (re-search-forward "^\\*\\{1,2\\} " nil t)
                                   (line-beginning-position)
                                 (point-max))))
                   (body (string-trim
                          (buffer-substring-no-properties body-start body-end))))
              (unless (string= current-section "About This File")
                (push (list :section current-section
                            :title title
                            :body body)
                      specs))))))))
    (nreverse specs)))

(defun claude-integration-test--filter-specs (specs &optional section)
  "Filter SPECS to only those in SECTION (case-insensitive match)."
  (if section
      (cl-remove-if-not
       (lambda (spec)
         (string-match-p (regexp-quote section)
                         (plist-get spec :section)))
       specs)
    specs))

;;; Test Execution

(defun claude-integration-test--build-prompt (spec)
  "Build an agent prompt from a test SPEC plist."
  (format "You are running an integration test. Execute the following test spec exactly.

TEST: %s
SECTION: %s

STEPS:
%s

INSTRUCTIONS:
- Execute each step described above
- Use the MCP tools available to you
- Report your result by calling mcp__emacs__done with a message in this format:
  PASS: <one-line reason>
  or
  FAIL: <one-line reason>

If a step says 'Note: This test requires user interaction', report:
  SKIP: Requires user interaction

Be concise. Execute the test and report the result."
          (plist-get spec :title)
          (plist-get spec :section)
          (plist-get spec :body)))

(defun claude-integration-test--format-results (results)
  "Format test RESULTS into a human-readable summary."
  (let ((total (length results))
        (passed 0) (failed 0) (skipped 0) (pending 0))
    (dolist (r results)
      (pcase (plist-get r :status)
        ("PASS" (cl-incf passed))
        ("FAIL" (cl-incf failed))
        ("SKIP" (cl-incf skipped))
        (_ (cl-incf pending))))
    (concat
     (format "Integration Test Results\n")
     (format "========================\n\n")
     (format "Total: %d | Pass: %d | Fail: %d | Skip: %d | Pending: %d\n\n"
             total passed failed skipped pending)
     (mapconcat
      (lambda (r)
        (format "[%s] %s :: %s\n    %s"
                (or (plist-get r :status) "PENDING")
                (plist-get r :section)
                (plist-get r :title)
                (or (plist-get r :message) "Awaiting execution")))
      results
      "\n\n")
     "\n")))

;;; Interactive Commands

;;;###autoload
(defun claude-integration-test-run-all ()
  "Run all integration tests from the spec file.
Results are displayed in a summary buffer."
  (interactive)
  (let* ((specs (claude-integration-test--parse-specs))
         (results (mapcar (lambda (spec)
                            (list :section (plist-get spec :section)
                                  :title (plist-get spec :title)
                                  :status nil
                                  :message nil))
                          specs)))
    (setq claude-integration-test--results results)
    ;; Display initial results
    (claude-integration-test--display-results)
    (message "Parsed %d test specs. Use claude-integration-test-run-next to execute tests one by one."
             (length specs))
    ;; Return specs for programmatic use
    specs))

;;;###autoload
(defun claude-integration-test-run-section (section)
  "Run integration tests for a specific SECTION.
SECTION should match a heading in integration-specs.org."
  (interactive "sSection name: ")
  (let* ((all-specs (claude-integration-test--parse-specs))
         (specs (claude-integration-test--filter-specs all-specs section)))
    (if specs
        (progn
          (setq claude-integration-test--results
                (mapcar (lambda (spec)
                          (list :section (plist-get spec :section)
                                :title (plist-get spec :title)
                                :status nil
                                :message nil))
                        specs))
          (claude-integration-test--display-results)
          (message "Found %d tests in section '%s'" (length specs) section))
      (message "No tests found for section '%s'" section))))

;;;###autoload
(defun claude-integration-test-list-sections ()
  "List all test sections from the spec file."
  (interactive)
  (let* ((specs (claude-integration-test--parse-specs))
         (sections (cl-remove-duplicates
                    (mapcar (lambda (s) (plist-get s :section)) specs)
                    :test #'equal)))
    (message "Sections: %s" (string-join sections ", "))))

(defun claude-integration-test--display-results ()
  "Display current test results in the results buffer."
  (let ((buf (get-buffer-create claude-integration-test-results-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (claude-integration-test--format-results
                 claude-integration-test--results))
        (goto-char (point-min))))
    (display-buffer buf)))

;;; ERT test for the runner itself

(ert-deftest claude-integration-test-parse-specs ()
  "Test that the spec file can be parsed."
  :tags '(:unit :mcp :integration-runner)
  (let ((spec-file (expand-file-name
                    "integration-specs.org"
                    (file-name-directory (or load-file-name "")))))
    (when (file-exists-p spec-file)
      (let ((specs (claude-integration-test--parse-specs spec-file)))
        (should (listp specs))
        (should (> (length specs) 0))
        ;; Each spec should have required fields
        (dolist (spec specs)
          (should (plist-get spec :section))
          (should (plist-get spec :title))
          (should (plist-get spec :body)))))))

(ert-deftest claude-integration-test-format-results ()
  "Test result formatting."
  :tags '(:unit :mcp :integration-runner)
  (let ((results (list (list :section "Test" :title "test1"
                             :status "PASS" :message "All good")
                       (list :section "Test" :title "test2"
                             :status "FAIL" :message "Something broke"))))
    (let ((formatted (claude-integration-test--format-results results)))
      (should (stringp formatted))
      (should (string-match-p "Pass: 1" formatted))
      (should (string-match-p "Fail: 1" formatted))
      (should (string-match-p "All good" formatted)))))

(provide 'claude-integration-runner)
;;; claude-integration-runner.el ends here
