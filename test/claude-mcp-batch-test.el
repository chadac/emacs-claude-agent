;;; claude-mcp-batch-test.el --- Tests for batch lock/edit/unlock -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;; Unit tests for batch lock/edit/unlock MCP tools.
;; These tools allow locking, editing, and unlocking multiple regions
;; in a single buffer with one call each, reducing round-trips.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load test helper (handles circular dependency between claude-mcp and sub-modules)
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'test-helper)

;;; Test Utilities

(defmacro claude-mcp-batch-test-with-buffer (content &rest body)
  "Execute BODY in a temp buffer with CONTENT and lock infrastructure ready."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (let ((inhibit-message t))
       (setq claude-mcp--locked-regions (make-hash-table :test 'equal))
       ,@body)))

;;; Tests for claude-mcp--alist-get

(ert-deftest claude-mcp-batch-test-alist-get-dot-pair ()
  "Test alist-get with dot pair (KEY . VALUE) form."
  :tags '(:unit :batch)
  (should (= 10 (claude-mcp--alist-get "start_line" '(("start_line" . 10) ("end_line" . 20))))))

(ert-deftest claude-mcp-batch-test-alist-get-list-pair ()
  "Test alist-get with list pair (KEY VALUE) form."
  :tags '(:unit :batch)
  (should (= 10 (claude-mcp--alist-get "start_line" '(("start_line" 10) ("end_line" 20))))))

(ert-deftest claude-mcp-batch-test-alist-get-missing-key ()
  "Test alist-get returns nil for missing key."
  :tags '(:unit :batch)
  (should-not (claude-mcp--alist-get "missing" '(("start_line" . 10)))))

;;; Tests for claude-mcp-lock-regions (batch lock)

(ert-deftest claude-mcp-batch-test-lock-two-regions ()
  "Test locking two non-overlapping regions in one call."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "line1\nline2\nline3\nline4\nline5\n"
    (let* ((buf-name (buffer-name))
           (regions '((("start_line" . 1) ("end_line" . 2))
                      (("start_line" . 4) ("end_line" . 5))))
           (result (claude-mcp-lock-regions regions "TestAgent" nil buf-name)))
      ;; Should have locked 2 regions
      (should (string-match-p "Locked 2 regions" result))
      ;; Should have 2 entries in the hash table
      (should (= 2 (hash-table-count claude-mcp--locked-regions))))))

(ert-deftest claude-mcp-batch-test-lock-single-region ()
  "Test locking a single region via batch API."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "line1\nline2\nline3\n"
    (let* ((buf-name (buffer-name))
           (regions '((("start_line" . 2) ("end_line" . 2))))
           (result (claude-mcp-lock-regions regions "TestAgent" nil buf-name)))
      (should (string-match-p "Locked 1 regions" result))
      (should (= 1 (hash-table-count claude-mcp--locked-regions))))))

(ert-deftest claude-mcp-batch-test-lock-returns-content ()
  "Test that batch lock returns the content of locked regions."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "alpha\nbeta\ngamma\ndelta\n"
    (let* ((buf-name (buffer-name))
           (regions '((("start_line" . 1) ("end_line" . 1))
                      (("start_line" . 3) ("end_line" . 3))))
           (result (claude-mcp-lock-regions regions "TestAgent" nil buf-name)))
      (should (string-match-p "alpha" result))
      (should (string-match-p "gamma" result)))))

(ert-deftest claude-mcp-batch-test-lock-overlapping-regions-error ()
  "Test that overlapping regions in a batch are rejected atomically."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "line1\nline2\nline3\nline4\n"
    (let* ((buf-name (buffer-name))
           (regions '((("start_line" . 1) ("end_line" . 3))
                      (("start_line" . 2) ("end_line" . 4)))))
      (should-error (claude-mcp-lock-regions regions "TestAgent" nil buf-name)
                    :type 'error)
      ;; No locks should have been created (atomic)
      (should (= 0 (hash-table-count claude-mcp--locked-regions))))))

(ert-deftest claude-mcp-batch-test-lock-overlaps-existing-error ()
  "Test that batch lock rejects regions overlapping with existing locks."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "line1\nline2\nline3\nline4\n"
    (let* ((buf-name (buffer-name)))
      ;; First, lock line 2 with single lock
      (claude-mcp-lock-region buf-name 2 2 "TestAgent")
      (should (= 1 (hash-table-count claude-mcp--locked-regions)))
      ;; Now try batch lock that overlaps line 2
      (let ((regions '((("start_line" . 1) ("end_line" . 2)))))
        (should-error (claude-mcp-lock-regions regions "TestAgent" nil buf-name)
                      :type 'error)
        ;; Should still have just the original lock
        (should (= 1 (hash-table-count claude-mcp--locked-regions)))))))

(ert-deftest claude-mcp-batch-test-lock-invalid-lines-error ()
  "Test that invalid line numbers are rejected before any locks are created."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "line1\nline2\nline3\n"
    (let* ((buf-name (buffer-name))
           ;; First region is valid, second has end_line > total lines
           (regions '((("start_line" . 1) ("end_line" . 1))
                      (("start_line" . 2) ("end_line" . 99)))))
      (should-error (claude-mcp-lock-regions regions "TestAgent" nil buf-name)
                    :type 'error)
      ;; No locks should have been created (atomic)
      (should (= 0 (hash-table-count claude-mcp--locked-regions))))))

(ert-deftest claude-mcp-batch-test-lock-missing-fields-error ()
  "Test that regions missing required fields are rejected."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "line1\nline2\n"
    (let* ((buf-name (buffer-name))
           (regions '((("start_line" . 1)))))  ; missing end_line
      (should-error (claude-mcp-lock-regions regions "TestAgent" nil buf-name)
                    :type 'error))))

(ert-deftest claude-mcp-batch-test-lock-creates-overlays ()
  "Test that batch lock creates visible overlays for each region."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "line1\nline2\nline3\nline4\n"
    (let* ((buf-name (buffer-name))
           (regions '((("start_line" . 1) ("end_line" . 1))
                      (("start_line" . 3) ("end_line" . 4)))))
      (claude-mcp-lock-regions regions "TestAgent" nil buf-name)
      ;; Check that overlays with claude-mcp-lock property exist
      (let ((lock-overlays (cl-remove-if-not
                            (lambda (ov) (overlay-get ov 'claude-mcp-lock))
                            (overlays-in (point-min) (point-max)))))
        (should (= 2 (length lock-overlays)))))))

;;; Tests for claude-mcp-edit-regions (batch edit)

(ert-deftest claude-mcp-batch-test-edit-two-regions ()
  "Test editing two locked regions in one call."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "old1\nkeep\nold2\n"
    (let* ((buf-name (buffer-name))
           ;; Lock two regions
           (lock-result (claude-mcp-lock-regions
                         '((("start_line" . 1) ("end_line" . 1))
                           (("start_line" . 3) ("end_line" . 3)))
                         "TestAgent" nil buf-name))
           ;; Collect lock IDs
           (lock-ids nil))
      (maphash (lambda (id _) (push id lock-ids)) claude-mcp--locked-regions)
      (should (= 2 (length lock-ids)))
      ;; Build edits
      (let* ((edits (mapcar (lambda (id)
                              (let* ((info (gethash id claude-mcp--locked-regions))
                                     (start-line (plist-get info :start-line)))
                                `(("lock_id" . ,id)
                                  ("content" . ,(format "new%d" start-line)))))
                            lock-ids))
             (result (claude-mcp-edit-regions edits nil buf-name)))
        (should (string-match-p "Edited 2 regions" result))
        ;; All locks should be consumed
        (should (= 0 (hash-table-count claude-mcp--locked-regions)))
        ;; Buffer should have new content
        (let ((content (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "new1" content))
          (should (string-match-p "keep" content))
          (should (string-match-p "new3" content)))))))

(ert-deftest claude-mcp-batch-test-edit-preserves-order ()
  "Test that edits are applied bottom-up to preserve positions."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "aaa\nbbb\nccc\nddd\neee\n"
    (let* ((buf-name (buffer-name)))
      ;; Lock lines 1 and 5
      (claude-mcp-lock-regions
       '((("start_line" . 1) ("end_line" . 1))
         (("start_line" . 5) ("end_line" . 5)))
       "TestAgent" nil buf-name)
      ;; Collect lock IDs, sorted by start-line
      (let ((lock-pairs nil))
        (maphash (lambda (id info)
                   (push (cons (plist-get info :start-line) id) lock-pairs))
                 claude-mcp--locked-regions)
        (setq lock-pairs (sort lock-pairs (lambda (a b) (< (car a) (car b)))))
        ;; Edit: replace line 1 with multi-line content (shifts positions)
        ;; and replace line 5 with different content
        (let* ((id-line1 (cdr (nth 0 lock-pairs)))
               (id-line5 (cdr (nth 1 lock-pairs)))
               (edits `((("lock_id" . ,id-line1)
                         ("content" . "AAA-expanded\nAAA-line2"))
                        (("lock_id" . ,id-line5)
                         ("content" . "EEE-replaced"))))
               (result (claude-mcp-edit-regions edits nil buf-name)))
          (should (string-match-p "Edited 2 regions" result))
          (let ((content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "AAA-expanded" content))
            (should (string-match-p "AAA-line2" content))
            (should (string-match-p "EEE-replaced" content))
            (should (string-match-p "bbb" content))))))))

(ert-deftest claude-mcp-batch-test-edit-invalid-lock-id-error ()
  "Test that edits with invalid lock IDs are rejected atomically."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "line1\nline2\n"
    (let* ((buf-name (buffer-name)))
      ;; Lock one region
      (claude-mcp-lock-regions
       '((("start_line" . 1) ("end_line" . 1)))
       "TestAgent" nil buf-name)
      (let ((valid-id nil))
        (maphash (lambda (id _) (setq valid-id id)) claude-mcp--locked-regions)
        ;; Try to edit with one valid and one invalid lock ID
        (let ((edits `((("lock_id" . ,valid-id)
                        ("content" . "new1"))
                       (("lock_id" . "NONEXISTENT")
                        ("content" . "new2")))))
          (should-error (claude-mcp-edit-regions edits nil buf-name)
                        :type 'error)
          ;; Lock should still exist (atomic - no edits applied)
          (should (= 1 (hash-table-count claude-mcp--locked-regions)))
          ;; Buffer should be unchanged
          (should (string= "line1\n"
                           (buffer-substring-no-properties
                            (point-min) (+ (point-min) 6)))))))))

(ert-deftest claude-mcp-batch-test-edit-duplicate-lock-id-error ()
  "Test that duplicate lock_ids in batch are rejected."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "line1\nline2\n"
    (let* ((buf-name (buffer-name)))
      (claude-mcp-lock-regions
       '((("start_line" . 1) ("end_line" . 1)))
       "TestAgent" nil buf-name)
      (let ((lock-id nil))
        (maphash (lambda (id _) (setq lock-id id)) claude-mcp--locked-regions)
        (let ((edits `((("lock_id" . ,lock-id) ("content" . "new1"))
                       (("lock_id" . ,lock-id) ("content" . "new2")))))
          (should-error (claude-mcp-edit-regions edits nil buf-name)
                        :type 'error))))))

(ert-deftest claude-mcp-batch-test-edit-missing-content-error ()
  "Test that edits missing content field are rejected."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "line1\n"
    (let* ((buf-name (buffer-name)))
      (claude-mcp-lock-regions
       '((("start_line" . 1) ("end_line" . 1)))
       "TestAgent" nil buf-name)
      (let ((lock-id nil))
        (maphash (lambda (id _) (setq lock-id id)) claude-mcp--locked-regions)
        (let ((edits `((("lock_id" . ,lock-id)))))  ; missing content
          (should-error (claude-mcp-edit-regions edits nil buf-name)
                        :type 'error))))))

(ert-deftest claude-mcp-batch-test-edit-returns-diff ()
  "Test that batch edit returns diff output for each region."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "old-content\n"
    (let* ((buf-name (buffer-name)))
      (claude-mcp-lock-regions
       '((("start_line" . 1) ("end_line" . 1)))
       "TestAgent" nil buf-name)
      (let ((lock-id nil))
        (maphash (lambda (id _) (setq lock-id id)) claude-mcp--locked-regions)
        (let* ((edits `((("lock_id" . ,lock-id) ("content" . "new-content"))))
               (result (claude-mcp-edit-regions edits nil buf-name)))
          (should (string-match-p "- old-content" result))
          (should (string-match-p "\\+ new-content" result)))))))

;;; Tests for claude-mcp-unlock-regions (batch unlock)

(ert-deftest claude-mcp-batch-test-unlock-two-regions ()
  "Test unlocking two regions in one call."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "line1\nline2\nline3\nline4\n"
    (let* ((buf-name (buffer-name)))
      ;; Lock two regions
      (claude-mcp-lock-regions
       '((("start_line" . 1) ("end_line" . 2))
         (("start_line" . 3) ("end_line" . 4)))
       "TestAgent" nil buf-name)
      (should (= 2 (hash-table-count claude-mcp--locked-regions)))
      ;; Collect lock IDs
      (let ((lock-ids nil))
        (maphash (lambda (id _) (push id lock-ids)) claude-mcp--locked-regions)
        ;; Unlock both
        (let ((result (claude-mcp-unlock-regions lock-ids nil buf-name)))
          (should (string-match-p "Unlocked 2 regions" result))
          ;; All locks should be removed
          (should (= 0 (hash-table-count claude-mcp--locked-regions)))
          ;; Buffer content should be unchanged
          (should (string= "line1\nline2\nline3\nline4\n"
                           (buffer-substring-no-properties (point-min) (point-max)))))))))

(ert-deftest claude-mcp-batch-test-unlock-removes-overlays ()
  "Test that batch unlock removes all overlays."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "line1\nline2\nline3\n"
    (let* ((buf-name (buffer-name)))
      (claude-mcp-lock-regions
       '((("start_line" . 1) ("end_line" . 1))
         (("start_line" . 3) ("end_line" . 3)))
       "TestAgent" nil buf-name)
      ;; Verify overlays exist
      (let ((lock-overlays (cl-remove-if-not
                            (lambda (ov) (overlay-get ov 'claude-mcp-lock))
                            (overlays-in (point-min) (point-max)))))
        (should (= 2 (length lock-overlays))))
      ;; Unlock all
      (let ((lock-ids nil))
        (maphash (lambda (id _) (push id lock-ids)) claude-mcp--locked-regions)
        (claude-mcp-unlock-regions lock-ids nil buf-name))
      ;; Verify overlays are gone
      (let ((lock-overlays (cl-remove-if-not
                            (lambda (ov) (overlay-get ov 'claude-mcp-lock))
                            (overlays-in (point-min) (point-max)))))
        (should (= 0 (length lock-overlays)))))))

(ert-deftest claude-mcp-batch-test-unlock-invalid-id-error ()
  "Test that batch unlock with invalid ID is rejected atomically."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "line1\nline2\n"
    (let* ((buf-name (buffer-name)))
      ;; Lock one region
      (claude-mcp-lock-regions
       '((("start_line" . 1) ("end_line" . 1)))
       "TestAgent" nil buf-name)
      (let ((valid-id nil))
        (maphash (lambda (id _) (setq valid-id id)) claude-mcp--locked-regions)
        ;; Try to unlock with one valid and one invalid ID
        (should-error (claude-mcp-unlock-regions
                       (list valid-id "NONEXISTENT") nil buf-name)
                      :type 'error)
        ;; Lock should still exist (atomic)
        (should (= 1 (hash-table-count claude-mcp--locked-regions)))))))

;;; Tests for full workflow: batch lock -> batch edit -> verify

(ert-deftest claude-mcp-batch-test-full-rename-workflow ()
  "Test a full rename workflow: lock 3 call sites, edit all, verify."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer
      "  (old-function arg1)\n  (other-stuff)\n  (old-function arg2)\n  (more-stuff)\n  (old-function arg3)\n"
    (let* ((buf-name (buffer-name)))
      ;; Lock all three call sites
      (claude-mcp-lock-regions
       '((("start_line" . 1) ("end_line" . 1))
         (("start_line" . 3) ("end_line" . 3))
         (("start_line" . 5) ("end_line" . 5)))
       "TestAgent" nil buf-name)
      (should (= 3 (hash-table-count claude-mcp--locked-regions)))
      ;; Collect lock IDs sorted by position
      (let ((lock-pairs nil))
        (maphash (lambda (id info)
                   (push (cons (plist-get info :start-line) id) lock-pairs))
                 claude-mcp--locked-regions)
        (setq lock-pairs (sort lock-pairs (lambda (a b) (< (car a) (car b)))))
        ;; Build edits: rename old-function to new-function
        (let* ((edits (mapcar
                       (lambda (pair)
                         (let* ((id (cdr pair))
                                (info (gethash id claude-mcp--locked-regions))
                                (start (plist-get info :start-pos))
                                (end (plist-get info :end-pos))
                                (old (buffer-substring-no-properties start end))
                                (new (replace-regexp-in-string "old-function" "new-function" old)))
                           `(("lock_id" . ,id)
                             ("content" . ,(string-trim-right new "\n")))))
                       lock-pairs))
               (result (claude-mcp-edit-regions edits nil buf-name)))
          (should (string-match-p "Edited 3 regions" result))
          ;; Verify final buffer content
          (let ((content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "new-function arg1" content))
            (should (string-match-p "new-function arg2" content))
            (should (string-match-p "new-function arg3" content))
            (should-not (string-match-p "old-function" content))
            (should (string-match-p "other-stuff" content))
            (should (string-match-p "more-stuff" content))))))))

(ert-deftest claude-mcp-batch-test-lock-then-partial-unlock ()
  "Test locking 3 regions, unlocking 2, then editing the remaining one."
  :tags '(:unit :batch)
  (claude-mcp-batch-test-with-buffer "line1\nline2\nline3\n"
    (let* ((buf-name (buffer-name)))
      ;; Lock all three lines
      (claude-mcp-lock-regions
       '((("start_line" . 1) ("end_line" . 1))
         (("start_line" . 2) ("end_line" . 2))
         (("start_line" . 3) ("end_line" . 3)))
       "TestAgent" nil buf-name)
      (should (= 3 (hash-table-count claude-mcp--locked-regions)))
      ;; Find lock IDs by line number
      (let ((id-for-line (make-hash-table :test 'equal)))
        (maphash (lambda (id info)
                   (puthash (plist-get info :start-line) id id-for-line))
                 claude-mcp--locked-regions)
        ;; Unlock lines 1 and 3
        (claude-mcp-unlock-regions
         (list (gethash 1 id-for-line) (gethash 3 id-for-line))
         nil buf-name)
        (should (= 1 (hash-table-count claude-mcp--locked-regions)))
        ;; Edit the remaining lock (line 2)
        (let* ((remaining-id (gethash 2 id-for-line))
               (edits `((("lock_id" . ,remaining-id)
                         ("content" . "EDITED"))))
               (result (claude-mcp-edit-regions edits nil buf-name)))
          (should (string-match-p "Edited 1 regions" result))
          (let ((content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "line1" content))
            (should (string-match-p "EDITED" content))
            (should (string-match-p "line3" content))))))))

;;; Tests for tool registration

(ert-deftest claude-mcp-batch-test-tools-registered ()
  "Test that batch tools are registered in the tool registry."
  :tags '(:unit :batch)
  (should (gethash "locks" claude-mcp-tools))
  (should (gethash "edits" claude-mcp-tools))
  (should (gethash "unlocks" claude-mcp-tools)))

(ert-deftest claude-mcp-batch-test-tools-have-descriptions ()
  "Test that batch tools have proper descriptions."
  :tags '(:unit :batch)
  (let ((locks-def (gethash "locks" claude-mcp-tools))
        (edits-def (gethash "edits" claude-mcp-tools))
        (unlocks-def (gethash "unlocks" claude-mcp-tools)))
    (should (stringp (plist-get locks-def :description)))
    (should (stringp (plist-get edits-def :description)))
    (should (stringp (plist-get unlocks-def :description)))
    ;; Should mention "multiple" or "batch" nature
    (should (string-match-p "multiple\\|batch" (plist-get locks-def :description)))
    (should (string-match-p "multiple\\|batch" (plist-get edits-def :description)))
    (should (string-match-p "multiple\\|batch" (plist-get unlocks-def :description)))))

(provide 'claude-mcp-batch-test)
;;; claude-mcp-batch-test.el ends here
