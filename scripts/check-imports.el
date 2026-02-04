;;; check-imports.el --- Verify all modules load cleanly in batch mode -*- lexical-binding: t; -*-
;;
;; Usage:
;;   emacs -batch -l scripts/check-imports.el -f check-imports
;;
;; Ensures that every .el file in the package can be loaded without
;; errors.  Initializes package.el first so that external dependencies
;; (org-roam, magit, transient, etc.) are available, then attempts to
;; require every module in the project.  Catches circular requires,
;; missing dependencies, and malformed defface/defcustom specs that
;; only surface in batch mode.

;;; Code:

(require 'cl-lib)

;; Capture load-file-name at load time (it's nil during --eval)
(defvar check-imports--this-file (or load-file-name buffer-file-name)
  "Path to this file, captured at load time.")

(defun check-imports--project-root ()
  "Return the project root directory (parent of scripts/)."
  (file-name-directory
   (directory-file-name
    (file-name-directory check-imports--this-file))))

(defun check-imports--find-el-files (directory)
  "Find all .el files in DIRECTORY, excluding test/ and scripts/."
  (let ((all-files (directory-files directory t "\\.el\\'")))
    (cl-remove-if
     (lambda (f)
       (let ((name (file-name-nondirectory f)))
         (or (string-prefix-p "." name)
             (string-prefix-p "test-" name))))
     all-files)))

(defun check-imports--init-packages ()
  "Initialize package.el so external dependencies are on `load-path'.
This makes org-roam, magit, etc. available in batch mode."
  (require 'package)
  (package-initialize))

(defun check-imports ()
  "Check that all modules can be loaded in batch mode.
Exits with code 1 if any required module fails to load."
  ;; Initialize external dependencies first
  (check-imports--init-packages)

  (let* ((root (check-imports--project-root))
         (files (check-imports--find-el-files root))
         (succeeded '())
         (failed '()))

    ;; Add project root to load path
    (add-to-list 'load-path root)

    ;; Try to load each module
    (dolist (file files)
      (let* ((feature (intern (file-name-sans-extension
                               (file-name-nondirectory file))))
             (result (condition-case err
                         (progn
                           (require feature)
                           'ok)
                       (error
                        (cons 'error err)))))
        (if (eq result 'ok)
            (progn
              (push (cons feature file) succeeded)
              (message "  ✓ %s" feature))
          (push (cons feature (error-message-string (cdr result))) failed)
          (message "  ✗ %s: %s" feature
                   (error-message-string (cdr result))))))

    ;; Report
    (message "\n━━━ Import Check Results ━━━")
    (message "  Succeeded: %d" (length succeeded))
    (message "  FAILED: %d" (length failed))

    (when failed
      (message "\nFAILED modules:")
      (dolist (f (nreverse failed))
        (message "  ✗ %s: %s" (car f) (cdr f))))

    (if failed
        (progn
          (message "\n✗ Import check FAILED — %d module(s) could not be loaded."
                   (length failed))
          (kill-emacs 1))
      (message "\n✓ Import check passed — all %d modules loaded cleanly."
               (length succeeded))
      (kill-emacs 0))))

(provide 'check-imports)
;;; check-imports.el ends here
