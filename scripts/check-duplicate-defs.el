;;; check-duplicate-defs.el --- Detect duplicate definitions across .el files -*- lexical-binding: t; -*-
;;
;; Usage:
;;   emacs -batch -l scripts/check-duplicate-defs.el -f check-duplicate-defs
;;
;; Uses Emacs's own reader to parse each .el file, so it handles
;; comments, strings, and nested forms correctly.
;;
;; Checked definition forms:
;;   defun, defmacro, defsubst, cl-defun, cl-defmacro, cl-defgeneric,
;;   defvar, defvar-local, defconst, defcustom, defalias,
;;   define-minor-mode, define-derived-mode, define-globalized-minor-mode
;;
;; Elisp is a Lisp-2: functions and variables occupy separate namespaces.
;; A defvar-local and defun with the same name is perfectly valid.
;; Duplicates are only flagged within the same namespace.
;;
;; Forward declarations — (defvar NAME) with no value — are excluded,
;; since they exist only to silence byte-compiler warnings.
;; Similarly, (declare-function ...) is not a definition and is ignored.

(require 'cl-lib)
(require 'subr-x)

;; All definition forms that introduce a named symbol into the global namespace.
;; cl-defmethod is intentionally excluded: multiple methods on the same generic
;; function with different specializers are normal, not duplicates.
(defconst check-duplicate-defs--forms
  '(defun defmacro defsubst cl-defun cl-defmacro cl-defgeneric
    defvar defvar-local defconst defcustom defalias
    define-minor-mode define-derived-mode define-globalized-minor-mode)
  "Definition forms that should not be duplicated across files.")

;; Forms that define into the function cell (Lisp-2 function namespace).
;; Everything else defines into the variable cell.
(defconst check-duplicate-defs--function-forms
  '(defun defmacro defsubst cl-defun cl-defmacro cl-defgeneric
    defalias define-minor-mode define-derived-mode
    define-globalized-minor-mode)
  "Definition forms that occupy the function namespace.")

(defun check-duplicate-defs--namespace (type)
  "Return the namespace keyword for definition TYPE.
Elisp is a Lisp-2: functions and variables can share a name."
  (if (memq type check-duplicate-defs--function-forms)
      'function
    'variable))

(defun check-duplicate-defs--forward-decl-p (form)
  "Return non-nil if FORM is a forward declaration like (defvar NAME)."
  (and (memq (car form) '(defvar defvar-local))
       (= (length form) 2)))

(defun check-duplicate-defs--definition-p (form)
  "Return non-nil if FORM is a top-level definition form we care about."
  (memq (car-safe form) check-duplicate-defs--forms))

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
Exits with code 1 if duplicates found, 0 otherwise.
Definitions are grouped by (name, namespace) since Elisp is a Lisp-2."
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

    ;; Group by (name, namespace) — find names with multiple definitions
    ;; in the same namespace.
    (let ((by-key (make-hash-table :test 'equal))
          (duplicates (make-hash-table :test 'equal)))
      (dolist (def all-defs)
        (let* ((name (symbol-name (car def)))
               (type (nth 1 def))
               (ns (check-duplicate-defs--namespace type))
               (key (format "%s [%s]" name ns)))
          (puthash key (cons def (gethash key by-key)) by-key)))

      ;; Find duplicates
      (maphash
       (lambda (key entries)
         (when (> (length entries) 1)
           (setq has-duplicates t)
           (puthash key (reverse entries) duplicates)))
       by-key)

      (if (not has-duplicates)
          (progn
            (message "No duplicate definitions found across %d files."
                     (length all-files))
            (kill-emacs 0))

        (message "Duplicate definitions found:\n")
        (let ((sorted-keys (sort (hash-table-keys duplicates) #'string<)))
          (dolist (key sorted-keys)
            (message "  %s:" key)
            (dolist (entry (gethash key duplicates))
              (let ((type (nth 1 entry))
                    (line (nth 2 entry))
                    (file (file-relative-name (nth 3 entry) dir)))
                (message "    %s:%d (%s)" file line type)))
            (message "")))
        (message "Each symbol should be defined in exactly one file per namespace.")
        (message "Use (defvar NAME) without a value for forward declarations,")
        (message "or (declare-function NAME ...) to silence byte-compiler warnings.")
        (kill-emacs 1)))))

(provide 'check-duplicate-defs)
;;; check-duplicate-defs.el ends here
