;;; check-duplicate-defs.el --- Detect duplicate definitions across .el files -*- lexical-binding: t; -*-
;;
;; Usage:
;;   emacs -batch -l scripts/check-duplicate-defs.el -f check-duplicate-defs
;;
;; Uses Emacs's own reader to parse each .el file, so it handles
;; comments, strings, and nested forms correctly.
;;
;; Forward declarations — (defvar NAME) with no value — are excluded,
;; since they exist only to silence byte-compiler warnings.

(require 'cl-lib)
(require 'subr-x)

(defun check-duplicate-defs--forward-decl-p (form)
  "Return non-nil if FORM is a forward declaration like (defvar NAME)."
  (and (eq (car form) 'defvar)
       (= (length form) 2)))

(defun check-duplicate-defs--definition-p (form)
  "Return non-nil if FORM is a top-level definition form we care about."
  (memq (car-safe form) '(defun defcustom defvar defmacro)))

(defun check-duplicate-defs--collect-from-file (file)
  "Collect all top-level definitions from FILE.
Returns a list of (NAME TYPE LINE FILE) entries."
  (let ((defs '()))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (condition-case _err
            (let ((start-pos (point))
                  (form (read (current-buffer))))
              (when (and (listp form)
                         (check-duplicate-defs--definition-p form)
                         (not (check-duplicate-defs--forward-decl-p form)))
                (let ((name (cadr form))
                      (type (car form))
                      (line (line-number-at-pos start-pos)))
                  (push (list name type line file) defs))))
          (end-of-file (goto-char (point-max)))
          (error (forward-line 1)))))
    (nreverse defs)))

(defun check-duplicate-defs--find-el-files (directory)
  "Find all .el files in DIRECTORY, excluding test/ and test-* files."
  (let ((all-files (directory-files directory t "\\.el\\'")))
    (cl-remove-if
     (lambda (f)
       (let ((name (file-name-nondirectory f)))
         (or (string-prefix-p "test-" name)
             (string-prefix-p "." name))))
     all-files)))

(defun check-duplicate-defs ()
  "Check for duplicate definitions across all .el files.
Exits with code 1 if duplicates found, 0 otherwise."
  (let* ((dir (or (car command-line-args-left) default-directory))
         (files (check-duplicate-defs--find-el-files dir))
         (test-dir (expand-file-name "test" dir))
         (test-files (when (file-directory-p test-dir)
                       (directory-files test-dir t "\\.el\\'" t)))
         (all-files (append files test-files))
         (all-defs '())
         (has-duplicates nil))

    ;; Collect definitions from all files
    (dolist (file all-files)
      (when (file-exists-p file)
        (let ((file-defs (check-duplicate-defs--collect-from-file file)))
          (setq all-defs (append all-defs file-defs)))))

    ;; Group by name — find names with multiple definitions
    (let ((by-name (make-hash-table :test 'equal))
          (duplicates (make-hash-table :test 'equal)))
      (dolist (def all-defs)
        (let ((name (symbol-name (car def))))
          (puthash name (cons def (gethash name by-name)) by-name)))

      ;; Find duplicates
      (maphash
       (lambda (name entries)
         (when (> (length entries) 1)
           (setq has-duplicates t)
           (puthash name (reverse entries) duplicates)))
       by-name)

      (if (not has-duplicates)
          (progn
            (message "✓ No duplicate definitions found across %d files." (length all-files))
            (kill-emacs 0))

        (message "✗ Duplicate definitions found:\n")
        (let ((sorted-names (sort (hash-table-keys duplicates) #'string<)))
          (dolist (name sorted-names)
            (message "  %s:" name)
            (dolist (entry (gethash name duplicates))
              (let ((type (nth 1 entry))
                    (line (nth 2 entry))
                    (file (file-relative-name (nth 3 entry) dir)))
                (message "    %s:%d (%s)" file line type)))
            (message "")))
        (message "Each symbol should be defined in exactly one file.")
        (message "Use (defvar NAME) without a value for forward declarations,")
        (message "or (declare-function NAME ...) to silence byte-compiler warnings.")
        (kill-emacs 1)))))

(provide 'check-duplicate-defs)
;;; check-duplicate-defs.el ends here
