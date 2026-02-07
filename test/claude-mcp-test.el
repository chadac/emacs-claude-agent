;;; claude-mcp-test.el --- Tests for core MCP tools -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;; Unit tests for core MCP tool backing functions in claude-mcp.el.
;; Covers: single lock/unlock/edit, read_buffer, get_buffer_content,
;; search_buffer, get_region, buffer_info, list_buffers, eval, clear_buffer.
;;
;; Run with:
;;   emacs -batch -l ert -l test/claude-mcp-test.el -f ert-run-tests-batch

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load test helper (handles circular dependency between claude-mcp and sub-modules)
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'test-helper)

;;; Test Utilities

(defmacro claude-mcp-test-with-buffer (content &rest body)
  "Execute BODY in a temp buffer with CONTENT and lock infrastructure ready."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (let ((inhibit-message t))
       (setq claude-mcp--locked-regions (make-hash-table :test 'equal))
       ,@body)))

(defmacro claude-mcp-test-with-named-buffer (name content &rest body)
  "Execute BODY in a named buffer NAME with CONTENT.
Cleans up the buffer after BODY completes."
  (declare (indent 2))
  `(let ((buf (get-buffer-create ,name)))
     (unwind-protect
         (with-current-buffer buf
           (erase-buffer)
           (insert ,content)
           (goto-char (point-min))
           (let ((inhibit-message t))
             (setq claude-mcp--locked-regions (make-hash-table :test 'equal))
             ,@body))
       (kill-buffer buf))))

;;; ============================================================
;;; Single Lock Tests
;;; ============================================================

(ert-deftest claude-mcp-test-lock-region-happy-path ()
  "Test locking a region with required args."
  :tags '(:unit :mcp :lock)
  (claude-mcp-test-with-named-buffer " *test-lock*" "line1\nline2\nline3\n"
    (let ((result (claude-mcp-lock-region (buffer-name) 1 2 "TestAgent")))
      (should (stringp result))
      (should (string-match-p "Locked" result))
      (should (string-match-p "lines 1-2" result))
      (should (= 1 (hash-table-count claude-mcp--locked-regions))))))

(ert-deftest claude-mcp-test-lock-region-returns-content ()
  "Test that lock returns the content of the locked region."
  :tags '(:unit :mcp :lock)
  (claude-mcp-test-with-named-buffer " *test-lock-content*" "alpha\nbeta\ngamma\n"
    (let ((result (claude-mcp-lock-region (buffer-name) 2 2 "TestAgent")))
      (should (string-match-p "beta" result)))))

(ert-deftest claude-mcp-test-lock-region-creates-overlay ()
  "Test that lock creates an overlay."
  :tags '(:unit :mcp :lock)
  (claude-mcp-test-with-named-buffer " *test-lock-ov*" "line1\nline2\nline3\n"
    (claude-mcp-lock-region (buffer-name) 1 2 "TestAgent")
    (let ((lock-overlays (cl-remove-if-not
                          (lambda (ov) (overlay-get ov 'claude-mcp-lock))
                          (overlays-in (point-min) (point-max)))))
      (should (= 1 (length lock-overlays))))))

(ert-deftest claude-mcp-test-lock-region-overlap-error ()
  "Test that overlapping locks produce an error."
  :tags '(:unit :mcp :lock)
  (claude-mcp-test-with-named-buffer " *test-lock-overlap*" "line1\nline2\nline3\nline4\n"
    (claude-mcp-lock-region (buffer-name) 1 3 "TestAgent")
    (should-error (claude-mcp-lock-region (buffer-name) 2 4 "TestAgent")
                  :type 'error)
    ;; Only the first lock should exist
    (should (= 1 (hash-table-count claude-mcp--locked-regions)))))

(ert-deftest claude-mcp-test-lock-region-invalid-line-error ()
  "Test that invalid line numbers produce an error."
  :tags '(:unit :mcp :lock)
  (claude-mcp-test-with-named-buffer " *test-lock-invalid*" "line1\nline2\n"
    ;; end-line > total lines
    (should-error (claude-mcp-lock-region (buffer-name) 1 99 "TestAgent")
                  :type 'error)
    ;; start-line < 1
    (should-error (claude-mcp-lock-region (buffer-name) 0 1 "TestAgent")
                  :type 'error)
    ;; start > end
    (should-error (claude-mcp-lock-region (buffer-name) 3 1 "TestAgent")
                  :type 'error)))

(ert-deftest claude-mcp-test-lock-region-nonexistent-buffer-error ()
  "Test that locking a nonexistent buffer produces an error."
  :tags '(:unit :mcp :lock)
  (should-error (claude-mcp-lock-region "nonexistent-buffer-xyz" 1 1 "TestAgent")
                :type 'error))

;;; ============================================================
;;; Single Unlock Tests
;;; ============================================================

(ert-deftest claude-mcp-test-unlock-region-happy-path ()
  "Test unlocking a locked region."
  :tags '(:unit :mcp :unlock)
  (claude-mcp-test-with-named-buffer " *test-unlock*" "line1\nline2\nline3\n"
    (claude-mcp-lock-region (buffer-name) 1 2 "TestAgent")
    ;; Extract lock ID from hash table
    (let (lock-id)
      (maphash (lambda (id _) (setq lock-id id)) claude-mcp--locked-regions)
      (let ((result (claude-mcp-unlock-region (buffer-name) nil lock-id)))
        (should (string-match-p "Unlocked" result))
        (should (= 0 (hash-table-count claude-mcp--locked-regions)))))))

(ert-deftest claude-mcp-test-unlock-region-auto-resolve ()
  "Test unlocking auto-resolves when only one lock exists."
  :tags '(:unit :mcp :unlock)
  (claude-mcp-test-with-named-buffer " *test-unlock-auto*" "line1\nline2\n"
    (claude-mcp-lock-region (buffer-name) 1 1 "TestAgent")
    ;; No lock-id provided - should auto-resolve
    (let ((result (claude-mcp-unlock-region (buffer-name) nil nil)))
      (should (string-match-p "Unlocked" result))
      (should (= 0 (hash-table-count claude-mcp--locked-regions))))))

(ert-deftest claude-mcp-test-unlock-region-removes-overlay ()
  "Test that unlocking removes the overlay."
  :tags '(:unit :mcp :unlock)
  (claude-mcp-test-with-named-buffer " *test-unlock-ov*" "line1\nline2\n"
    (claude-mcp-lock-region (buffer-name) 1 1 "TestAgent")
    (let ((before-overlays (cl-remove-if-not
                            (lambda (ov) (overlay-get ov 'claude-mcp-lock))
                            (overlays-in (point-min) (point-max)))))
      (should (= 1 (length before-overlays))))
    (claude-mcp-unlock-region (buffer-name) nil nil)
    (let ((after-overlays (cl-remove-if-not
                           (lambda (ov) (overlay-get ov 'claude-mcp-lock))
                           (overlays-in (point-min) (point-max)))))
      (should (= 0 (length after-overlays))))))

(ert-deftest claude-mcp-test-unlock-region-invalid-id-error ()
  "Test that unlocking with an invalid ID errors."
  :tags '(:unit :mcp :unlock)
  (claude-mcp-test-with-named-buffer " *test-unlock-bad*" "line1\n"
    (claude-mcp-lock-region (buffer-name) 1 1 "TestAgent")
    (should-error (claude-mcp-unlock-region (buffer-name) nil "NONEXISTENT")
                  :type 'error)
    ;; Lock should still exist
    (should (= 1 (hash-table-count claude-mcp--locked-regions)))))

(ert-deftest claude-mcp-test-unlock-region-no-locks-error ()
  "Test that unlocking with no locks errors."
  :tags '(:unit :mcp :unlock)
  (claude-mcp-test-with-named-buffer " *test-unlock-empty*" "line1\n"
    (should-error (claude-mcp-unlock-region (buffer-name) nil nil)
                  :type 'error)))

;;; ============================================================
;;; Single Edit (write-region) Tests
;;; ============================================================

(ert-deftest claude-mcp-test-write-region-happy-path ()
  "Test editing a locked region."
  :tags '(:unit :mcp :edit)
  (claude-mcp-test-with-named-buffer " *test-edit*" "old-content\nkeep-this\n"
    (claude-mcp-lock-region (buffer-name) 1 1 "TestAgent")
    (let ((result (claude-mcp-write-region (buffer-name) "new-content" nil nil)))
      (should (stringp result))
      ;; Lock should be consumed
      (should (= 0 (hash-table-count claude-mcp--locked-regions)))
      ;; Buffer content should be updated
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "new-content" content))
        (should (string-match-p "keep-this" content))
        (should-not (string-match-p "old-content" content))))))

(ert-deftest claude-mcp-test-write-region-auto-resolve ()
  "Test editing auto-resolves lock when only one exists."
  :tags '(:unit :mcp :edit)
  (claude-mcp-test-with-named-buffer " *test-edit-auto*" "original\n"
    (claude-mcp-lock-region (buffer-name) 1 1 "TestAgent")
    (let ((result (claude-mcp-write-region (buffer-name) "replaced" nil nil)))
      (should (stringp result))
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "replaced" content))))))

(ert-deftest claude-mcp-test-write-region-no-lock-error ()
  "Test that editing without a lock errors."
  :tags '(:unit :mcp :edit)
  (claude-mcp-test-with-named-buffer " *test-edit-nolock*" "line1\n"
    (should-error (claude-mcp-write-region (buffer-name) "new" nil nil)
                  :type 'error)))

(ert-deftest claude-mcp-test-write-region-auto-detect-buffer ()
  "Test that edit auto-detects buffer when only one buffer has a lock."
  :tags '(:unit :mcp :edit)
  (claude-mcp-test-with-named-buffer " *test-edit-autodetect*" "line1\nline2\n"
    (claude-mcp-lock-region (buffer-name) 1 1 "TestAgent")
    ;; Call write-region with nil buffer-name - should auto-detect
    (let ((result (claude-mcp-write-region nil "auto-detected" nil nil)))
      (should (stringp result))
      (should (string-match-p "Replaced" result)))
    ;; Verify content changed
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "auto-detected" content)))))

(ert-deftest claude-mcp-test-write-region-no-buffer-no-lock-error ()
  "Test that editing without buffer_name/file_path and no locks gives helpful error."
  :tags '(:unit :mcp :edit)
  (claude-mcp-test-with-named-buffer " *test-edit-noargs*" "line1\n"
    ;; No lock, no buffer specified - should error with helpful message
    (let ((err (should-error (claude-mcp-write-region nil "new" nil nil)
                             :type 'error)))
      (should (string-match-p "auto-detect" (error-message-string err))))))
(ert-deftest claude-mcp-test-write-region-multiline-expansion ()
  "Test editing a single line to multiple lines."
  :tags '(:unit :mcp :edit)
  (claude-mcp-test-with-named-buffer " *test-edit-expand*" "line1\nline2\nline3\n"
    (claude-mcp-lock-region (buffer-name) 2 2 "TestAgent")
    (claude-mcp-write-region (buffer-name) "expanded-a\nexpanded-b\nexpanded-c" nil nil)
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "line1" content))
      (should (string-match-p "expanded-a" content))
      (should (string-match-p "expanded-b" content))
      (should (string-match-p "expanded-c" content))
      (should (string-match-p "line3" content)))))

;;; ============================================================
;;; Lock + Edit + Unlock Workflow Tests
;;; ============================================================

(ert-deftest claude-mcp-test-lock-edit-workflow ()
  "Test a full lock -> edit -> verify workflow."
  :tags '(:unit :mcp :workflow)
  (claude-mcp-test-with-named-buffer " *test-workflow*" "  (old-func arg1)\n  (other-stuff)\n  (old-func arg2)\n"
    ;; Lock the first call site
    (claude-mcp-lock-region (buffer-name) 1 1 "TestAgent")
    ;; Edit it
    (claude-mcp-write-region (buffer-name) "  (new-func arg1)" nil nil)
    ;; Lock the second call site (now line 3)
    (claude-mcp-lock-region (buffer-name) 3 3 "TestAgent")
    ;; Edit it
    (claude-mcp-write-region (buffer-name) "  (new-func arg2)" nil nil)
    ;; Verify final content
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "new-func arg1" content))
      (should (string-match-p "new-func arg2" content))
      (should (string-match-p "other-stuff" content))
      (should-not (string-match-p "old-func" content)))))

(ert-deftest claude-mcp-test-lock-unlock-no-change ()
  "Test that lock -> unlock doesn't modify the buffer."
  :tags '(:unit :mcp :workflow)
  (claude-mcp-test-with-named-buffer " *test-lock-unlock*" "line1\nline2\nline3\n"
    (let ((original (buffer-substring-no-properties (point-min) (point-max))))
      (claude-mcp-lock-region (buffer-name) 1 2 "TestAgent")
      (claude-mcp-unlock-region (buffer-name) nil nil)
      (should (string= original (buffer-substring-no-properties (point-min) (point-max)))))))

;;; ============================================================
;;; read_buffer Tests
;;; ============================================================

(ert-deftest claude-mcp-test-read-buffer-full ()
  "Test reading entire buffer."
  :tags '(:unit :mcp :read-buffer)
  (claude-mcp-test-with-named-buffer " *test-read*" "line1\nline2\nline3\n"
    (let ((result (claude-mcp-read-buffer (buffer-name))))
      (should (stringp result))
      (should (string-match-p "line1" result))
      (should (string-match-p "line2" result))
      (should (string-match-p "line3" result)))))

(ert-deftest claude-mcp-test-read-buffer-with-offset ()
  "Test reading buffer from an offset."
  :tags '(:unit :mcp :read-buffer)
  (claude-mcp-test-with-named-buffer " *test-read-off*" "line1\nline2\nline3\nline4\n"
    (let ((result (claude-mcp-read-buffer (buffer-name) 2)))
      (should (stringp result))
      (should-not (string-match-p "1.*line1" result))
      (should (string-match-p "line2" result))
      (should (string-match-p "line3" result)))))

(ert-deftest claude-mcp-test-read-buffer-with-limit ()
  "Test reading buffer with a line limit."
  :tags '(:unit :mcp :read-buffer)
  (claude-mcp-test-with-named-buffer " *test-read-lim*" "line1\nline2\nline3\nline4\n"
    (let ((result (claude-mcp-read-buffer (buffer-name) 1 2)))
      (should (stringp result))
      (should (string-match-p "line1" result))
      (should (string-match-p "line2" result))
      (should-not (string-match-p "line3" result)))))

(ert-deftest claude-mcp-test-read-buffer-with-offset-and-limit ()
  "Test reading buffer with both offset and limit."
  :tags '(:unit :mcp :read-buffer)
  (claude-mcp-test-with-named-buffer " *test-read-both*" "aaa\nbbb\nccc\nddd\neee\n"
    (let ((result (claude-mcp-read-buffer (buffer-name) 2 2)))
      (should (string-match-p "bbb" result))
      (should (string-match-p "ccc" result))
      (should-not (string-match-p "aaa" result))
      (should-not (string-match-p "ddd" result)))))

(ert-deftest claude-mcp-test-read-buffer-has-line-numbers ()
  "Test that read-buffer output includes line numbers."
  :tags '(:unit :mcp :read-buffer)
  (claude-mcp-test-with-named-buffer " *test-read-nums*" "alpha\nbeta\n"
    (let ((result (claude-mcp-read-buffer (buffer-name))))
      ;; Should have line numbers in the format N->content
      (should (string-match-p "[0-9]" result)))))

(ert-deftest claude-mcp-test-read-buffer-nonexistent-error ()
  "Test that reading a nonexistent buffer errors."
  :tags '(:unit :mcp :read-buffer)
  (should-error (claude-mcp-read-buffer "nonexistent-buffer-xyz")
                :type 'error))

;;; ============================================================
;;; get_buffer_content Tests
;;; ============================================================

(ert-deftest claude-mcp-test-get-buffer-content-full ()
  "Test getting full buffer content."
  :tags '(:unit :mcp :get-buffer-content)
  (claude-mcp-test-with-named-buffer " *test-gbc*" "line1\nline2\nline3\n"
    (let ((result (claude-mcp-get-buffer-content (buffer-name))))
      (should (string= result "line1\nline2\nline3\n")))))

(ert-deftest claude-mcp-test-get-buffer-content-head-lines ()
  "Test getting first N lines."
  :tags '(:unit :mcp :get-buffer-content)
  (claude-mcp-test-with-named-buffer " *test-gbc-head*" "line1\nline2\nline3\nline4\n"
    (let ((result (claude-mcp-get-buffer-content (buffer-name) nil 2)))
      (should (string-match-p "line1" result))
      (should (string-match-p "line2" result))
      (should-not (string-match-p "line3" result)))))

(ert-deftest claude-mcp-test-get-buffer-content-tail-lines ()
  "Test getting last N lines."
  :tags '(:unit :mcp :get-buffer-content)
  (claude-mcp-test-with-named-buffer " *test-gbc-tail*" "line1\nline2\nline3\nline4\n"
    (let ((result (claude-mcp-get-buffer-content (buffer-name) 2)))
      (should (string-match-p "line3" result))
      (should (string-match-p "line4" result))
      (should-not (string-match-p "line1" result)))))

(ert-deftest claude-mcp-test-get-buffer-content-line-range ()
  "Test getting a specific line range."
  :tags '(:unit :mcp :get-buffer-content)
  (claude-mcp-test-with-named-buffer " *test-gbc-range*" "aaa\nbbb\nccc\nddd\n"
    (let ((result (claude-mcp-get-buffer-content (buffer-name) nil nil 2 3)))
      (should (string-match-p "bbb" result))
      (should (string-match-p "ccc" result))
      (should-not (string-match-p "aaa" result))
      (should-not (string-match-p "ddd" result)))))

(ert-deftest claude-mcp-test-get-buffer-content-nonexistent-error ()
  "Test that nonexistent buffer errors."
  :tags '(:unit :mcp :get-buffer-content)
  (should-error (claude-mcp-get-buffer-content "nonexistent-xyz")
                :type 'error))

;;; ============================================================
;;; search_buffer Tests
;;; ============================================================

(ert-deftest claude-mcp-test-search-buffer-happy-path ()
  "Test searching a buffer for a pattern."
  :tags '(:unit :mcp :search-buffer)
  (claude-mcp-test-with-named-buffer " *test-search*" "foo bar\nbaz qux\nfoo again\n"
    (let ((result (claude-mcp-search-buffer (buffer-name) "foo")))
      (should (stringp result))
      (should (string-match-p "foo bar" result))
      (should (string-match-p "foo again" result)))))

(ert-deftest claude-mcp-test-search-buffer-with-context ()
  "Test searching with context lines."
  :tags '(:unit :mcp :search-buffer)
  (claude-mcp-test-with-named-buffer " *test-search-ctx*" "before\ntarget\nafter\n"
    (let ((result (claude-mcp-search-buffer (buffer-name) "target" 1 1)))
      (should (string-match-p "before" result))
      (should (string-match-p "target" result))
      (should (string-match-p "after" result)))))

(ert-deftest claude-mcp-test-search-buffer-case-insensitive ()
  "Test case-insensitive searching."
  :tags '(:unit :mcp :search-buffer)
  (claude-mcp-test-with-named-buffer " *test-search-ci*" "Hello World\nhello world\nHELLO WORLD\n"
    (let ((result (claude-mcp-search-buffer (buffer-name) "hello" nil nil t)))
      ;; Should find all three
      (should (string-match-p "Hello World" result))
      (should (string-match-p "hello world" result))
      (should (string-match-p "HELLO WORLD" result)))))

(ert-deftest claude-mcp-test-search-buffer-with-limit ()
  "Test searching with a match limit."
  :tags '(:unit :mcp :search-buffer)
  (claude-mcp-test-with-named-buffer " *test-search-lim*" "match1\nmatch2\nmatch3\n"
    (let ((result (claude-mcp-search-buffer (buffer-name) "match" nil nil nil 1)))
      (should (string-match-p "match1" result))
      (should-not (string-match-p "match2" result)))))

(ert-deftest claude-mcp-test-search-buffer-no-matches ()
  "Test searching when there are no matches."
  :tags '(:unit :mcp :search-buffer)
  (claude-mcp-test-with-named-buffer " *test-search-none*" "hello world\n"
    (let ((result (claude-mcp-search-buffer (buffer-name) "ZZZZZ")))
      (should (stringp result))
      (should (string= "" result)))))

(ert-deftest claude-mcp-test-search-buffer-nonexistent-error ()
  "Test that searching nonexistent buffer errors."
  :tags '(:unit :mcp :search-buffer)
  (should-error (claude-mcp-search-buffer "nonexistent-xyz" "pattern")
                :type 'error))

;;; ============================================================
;;; get_region Tests
;;; ============================================================

(ert-deftest claude-mcp-test-get-region-happy-path ()
  "Test getting a region by character positions (end is exclusive)."
  :tags '(:unit :mcp :get-region)
  (claude-mcp-test-with-named-buffer " *test-region*" "abcdefghij"
    (let ((result (claude-mcp-get-region (buffer-name) 1 6)))
      (should (string= "abcde" result)))))

(ert-deftest claude-mcp-test-get-region-nonexistent-error ()
  "Test that getting a region from nonexistent buffer errors."
  :tags '(:unit :mcp :get-region)
  (should-error (claude-mcp-get-region "nonexistent-xyz" 1 5)
                :type 'error))

;;; ============================================================
;;; buffer_info Tests
;;; ============================================================

(ert-deftest claude-mcp-test-buffer-info-happy-path ()
  "Test getting buffer info."
  :tags '(:unit :mcp :buffer-info)
  (claude-mcp-test-with-named-buffer " *test-info*" "hello world\n"
    (let ((result (claude-mcp-buffer-info (buffer-name))))
      (should (listp result))
      (should (string= (plist-get result :name) (buffer-name)))
      (should (numberp (plist-get result :size)))
      (should (> (plist-get result :size) 0))
      (should (numberp (plist-get result :point)))
      (should (symbolp (plist-get result :major-mode))))))

(ert-deftest claude-mcp-test-buffer-info-nonexistent-error ()
  "Test that buffer-info on nonexistent buffer errors."
  :tags '(:unit :mcp :buffer-info)
  (should-error (claude-mcp-buffer-info "nonexistent-xyz")
                :type 'error))

;;; ============================================================
;;; list_buffers Tests
;;; ============================================================

(ert-deftest claude-mcp-test-list-buffers ()
  "Test listing all buffers."
  :tags '(:unit :mcp :list-buffers)
  (let ((result (claude-mcp-list-buffers)))
    (should (listp result))
    (should (> (length result) 0))
    ;; Should contain strings
    (should (stringp (car result)))))

;;; ============================================================
;;; eval Tests
;;; ============================================================

(ert-deftest claude-mcp-test-eval-happy-path ()
  "Test evaluating a simple expression."
  :tags '(:unit :mcp :eval)
  (let ((result (claude-mcp-eval "(+ 1 2)")))
    (should (string= "3" result))))

(ert-deftest claude-mcp-test-eval-string-result ()
  "Test evaluating an expression that returns a string."
  :tags '(:unit :mcp :eval)
  (let ((result (claude-mcp-eval "(concat \"hello\" \" \" \"world\")")))
    (should (string= "\"hello world\"" result))))

(ert-deftest claude-mcp-test-eval-list-result ()
  "Test evaluating an expression that returns a list."
  :tags '(:unit :mcp :eval)
  (let ((result (claude-mcp-eval "'(1 2 3)")))
    (should (string= "(1 2 3)" result))))

(ert-deftest claude-mcp-test-eval-invalid-expression-error ()
  "Test that invalid expressions produce an error."
  :tags '(:unit :mcp :eval)
  (should-error (claude-mcp-eval "(this-is-not-a-real-function-xyz)")
                :type 'error))

;;; ============================================================
;;; Tool Registration Tests
;;; ============================================================

(ert-deftest claude-mcp-test-core-tools-registered ()
  "Test that core tools are registered in the tool registry."
  :tags '(:unit :mcp :registration)
  (should (gethash "lock" claude-mcp-tools))
  (should (gethash "unlock" claude-mcp-tools))
  (should (gethash "edit" claude-mcp-tools))
  (should (gethash "read_file" claude-mcp-tools))
  (should (gethash "read_buffer" claude-mcp-tools))
  (should (gethash "get_buffer_content" claude-mcp-tools))
  (should (gethash "list_buffers" claude-mcp-tools))
  (should (gethash "buffer_info" claude-mcp-tools))
  (should (gethash "search_buffer" claude-mcp-tools))
  (should (gethash "get_region" claude-mcp-tools))
  (should (gethash "clear_buffer" claude-mcp-tools))
  (should (gethash "eval" claude-mcp-tools)))

(ert-deftest claude-mcp-test-core-tools-have-descriptions ()
  "Test that core tools have descriptions."
  :tags '(:unit :mcp :registration)
  (dolist (tool-name '("lock" "unlock" "edit" "read_file" "read_buffer"
                       "get_buffer_content" "list_buffers" "buffer_info"
                       "search_buffer" "get_region" "clear_buffer" "eval"))
    (let ((tool-def (gethash tool-name claude-mcp-tools)))
      (should tool-def)
      (should (stringp (plist-get tool-def :description)))
      (should (> (length (plist-get tool-def :description)) 0)))))

(ert-deftest claude-mcp-test-deftool-macro ()
  "Test that the deftool macro registers tools correctly."
  :tags '(:unit :mcp :registration)
  ;; Test that a tool has the expected structure
  (let ((lock-def (gethash "lock" claude-mcp-tools)))
    (should lock-def)
    (should (plist-get lock-def :description))
    (should (plist-get lock-def :function))
    (should (plist-get lock-def :args))))

;;; ============================================================
;;; Large Buffer Performance Tests
;;; ============================================================

(ert-deftest claude-mcp-test-lock-edit-large-buffer ()
  "Test that lock and edit work correctly on large buffers (3000+ lines)."
  :tags '(:unit :mcp :performance)
  (claude-mcp-test-with-named-buffer " *test-large*"
      ;; Generate a buffer with 5000 lines
      (mapconcat (lambda (n) (format "line %d: content here" n))
                 (number-sequence 1 5000)
                 "\n")
    ;; Lock a region near the end of the buffer
    (let ((result (claude-mcp-lock-region (buffer-name) 4990 4995 "TestAgent")))
      (should (stringp result))
      (should (string-match-p "Locked" result))
      (should (string-match-p "lines 4990-4995" result)))
    ;; Edit the locked region
    (let ((result (claude-mcp-write-region (buffer-name) "replaced line 1\nreplaced line 2" nil nil)))
      (should (stringp result))
      (should (string-match-p "Replaced" result)))
    ;; Verify the content was changed
    (goto-char (point-min))
    (forward-line 4989)  ; Go to line 4990
    (should (looking-at "replaced line 1"))))

(ert-deftest claude-mcp-test-multiple-locks-large-buffer ()
  "Test multiple locks and edits on a large buffer."
  :tags '(:unit :mcp :performance)
  (claude-mcp-test-with-named-buffer " *test-large-multi*"
      ;; Generate a buffer with 3000 lines
      (mapconcat (lambda (n) (format "line %d" n))
                 (number-sequence 1 3000)
                 "\n")
    ;; Lock multiple regions (spread throughout the buffer)
    (claude-mcp-lock-region (buffer-name) 100 105 "TestAgent")
    (claude-mcp-lock-region (buffer-name) 1500 1505 "TestAgent")
    (claude-mcp-lock-region (buffer-name) 2900 2905 "TestAgent")
    (should (= 3 (hash-table-count claude-mcp--locked-regions)))
    ;; Get lock IDs in order
    (let ((lock-ids nil))
      (maphash (lambda (id _) (push id lock-ids)) claude-mcp--locked-regions)
      ;; Edit the first lock (should work without performance issues)
      (let ((first-id (car (sort lock-ids #'string<))))
        (claude-mcp-write-region (buffer-name) "edited region" nil first-id))
      ;; Two locks should remain
      (should (= 2 (hash-table-count claude-mcp--locked-regions))))))

(ert-deftest claude-mcp-test-lock-edit-performance-timing ()
  "Test that lock and edit complete in reasonable time on large buffers.
This test ensures operations complete within 2 seconds even on 5000 line buffers."
  :tags '(:unit :mcp :performance)
  (claude-mcp-test-with-named-buffer " *test-perf*"
      ;; Generate a buffer with 5000 lines
      (mapconcat (lambda (n) (format "line %d: some content to make lines longer" n))
                 (number-sequence 1 5000)
                 "\n")
    (let ((start-time (float-time)))
      ;; Lock a region at the end
      (claude-mcp-lock-region (buffer-name) 4500 4510 "TestAgent")
      ;; Edit it
      (claude-mcp-write-region (buffer-name) "new content" nil nil)
      (let ((elapsed (- (float-time) start-time)))
        ;; Should complete in under 2 seconds (generous limit)
        (should (< elapsed 2.0))))))

(provide 'claude-mcp-test)
;;; claude-mcp-test.el ends here
