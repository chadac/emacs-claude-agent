;;; check-cyclic-requires.el --- Detect cyclic require dependencies -*- lexical-binding: t; -*-
;;
;; Usage:
;;   emacs -batch -l scripts/check-cyclic-requires.el -f check-cyclic-requires
;;
;; Parses (require 'FEATURE) forms from all .el files in the project
;; and builds a dependency graph.  Uses depth-first search to detect
;; cycles — both direct (A→B→A) and transitive (A→B→C→A).
;;
;; Only considers requires of project-local features (i.e., features
;; that correspond to .el files in the project directory).  External
;; packages (cl-lib, org-roam, etc.) are ignored.

;;; Code:

(require 'cl-lib)

;; Capture load-file-name at load time (it's nil during --eval)
(defvar check-cyclic-requires--this-file (or load-file-name buffer-file-name)
  "Path to this file, captured at load time.")

(defun check-cyclic-requires--project-root ()
  "Return the project root directory (parent of scripts/)."
  (file-name-directory
   (directory-file-name
    (file-name-directory check-cyclic-requires--this-file))))

(defun check-cyclic-requires--find-el-files (directory)
  "Find all .el files in DIRECTORY, excluding test/ and scripts/."
  (let ((all-files (directory-files directory t "\\.el\\'")))
    (cl-remove-if
     (lambda (f)
       (let ((name (file-name-nondirectory f)))
         (or (string-prefix-p "." name)
             (string-prefix-p "test-" name))))
     all-files)))

(defun check-cyclic-requires--collect-requires (file)
  "Collect all (require \\='FEATURE) forms from FILE.
Returns a list of feature symbols."
  (let ((requires '()))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (condition-case _err
            (let ((form (read (current-buffer))))
              (when (and (listp form)
                         (eq (car form) 'require)
                         (>= (length form) 2)
                         (listp (cadr form))
                         (eq (car (cadr form)) 'quote))
                (push (cadr (cadr form)) requires)))
          (end-of-file (goto-char (point-max)))
          (error (forward-line 1)))))
    (nreverse requires)))

(defun check-cyclic-requires--build-graph (files)
  "Build a dependency graph from FILES.
Returns (GRAPH . LOCAL-FEATURES) where GRAPH is a hash table
mapping feature symbols to lists of required feature symbols,
and LOCAL-FEATURES is the set of features defined by project files."
  (let ((graph (make-hash-table :test 'eq))
        (local-features (make-hash-table :test 'eq)))
    ;; First pass: determine which features are local to the project
    (dolist (file files)
      (let ((feature (intern (file-name-sans-extension
                              (file-name-nondirectory file)))))
        (puthash feature file local-features)))
    ;; Second pass: build the graph, filtering to local requires only
    (dolist (file files)
      (let* ((feature (intern (file-name-sans-extension
                               (file-name-nondirectory file))))
             (requires (check-cyclic-requires--collect-requires file))
             (local-requires (cl-remove-if-not
                              (lambda (r) (gethash r local-features))
                              requires)))
        (puthash feature local-requires graph)))
    (cons graph local-features)))

(defvar check-cyclic-requires--visited nil
  "Hash table tracking fully-visited nodes during DFS.")
(defvar check-cyclic-requires--in-stack nil
  "Hash table tracking nodes currently in the DFS recursion stack.")
(defvar check-cyclic-requires--cycles nil
  "Accumulator for detected cycles during DFS.")
(defvar check-cyclic-requires--graph nil
  "The dependency graph being searched.")

(defun check-cyclic-requires--dfs (node path)
  "Visit NODE during DFS cycle detection, with PATH as the current trail."
  (cond
   ((gethash node check-cyclic-requires--in-stack)
    ;; Found a cycle — extract just the cycle portion
    (let* ((cycle-start (cl-position node path))
           (cycle (append (nthcdr cycle-start path) (list node))))
      (push cycle check-cyclic-requires--cycles)))
   ((not (gethash node check-cyclic-requires--visited))
    (puthash node t check-cyclic-requires--visited)
    (puthash node t check-cyclic-requires--in-stack)
    (dolist (dep (gethash node check-cyclic-requires--graph))
      (check-cyclic-requires--dfs dep (append path (list node))))
    (puthash node nil check-cyclic-requires--in-stack))))

(defun check-cyclic-requires--find-cycles (graph local-features)
  "Find all cycles in GRAPH using DFS.
GRAPH is a hash table mapping feature -> list of required features.
LOCAL-FEATURES is a hash table of features defined in the project.
Returns a list of cycles, where each cycle is a list of feature symbols."
  (let ((check-cyclic-requires--visited (make-hash-table :test 'eq))
        (check-cyclic-requires--in-stack (make-hash-table :test 'eq))
        (check-cyclic-requires--cycles nil)
        (check-cyclic-requires--graph graph))
    (maphash
     (lambda (feature _file)
       (unless (gethash feature check-cyclic-requires--visited)
         (check-cyclic-requires--dfs feature nil)))
     local-features)
    check-cyclic-requires--cycles))

(defun check-cyclic-requires--normalize-cycle (cycle)
  "Normalize CYCLE so the smallest element comes first.
This makes cycle comparison stable regardless of DFS starting point."
  (let* ((without-close (butlast cycle))
         (min-elem (car (sort (mapcar #'symbol-name without-close) #'string<)))
         (pos (cl-position min-elem without-close
                           :test (lambda (a b)
                                   (string= a (symbol-name b)))))
         (rotated (append (nthcdr pos without-close)
                          (cl-subseq without-close 0 pos))))
    (append rotated (list (car rotated)))))

(defun check-cyclic-requires--deduplicate-cycles (cycles)
  "Remove duplicate cycles from CYCLES.
Two cycles are the same if they contain the same nodes in the same
order, regardless of which node the DFS started from."
  (let ((seen (make-hash-table :test 'equal))
        (unique '()))
    (dolist (cycle cycles)
      (let* ((normalized (check-cyclic-requires--normalize-cycle cycle))
             (key (mapcar #'symbol-name normalized)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push normalized unique))))
    (nreverse unique)))

(defun check-cyclic-requires--format-cycle (cycle)
  "Format CYCLE as a readable string like \"A → B → C → A\"."
  (mapconcat #'symbol-name cycle " → "))

(defun check-cyclic-requires ()
  "Check for cyclic require dependencies among project .el files.
Exits with code 1 if cycles found, 0 otherwise."
  (let* ((root (check-cyclic-requires--project-root))
         (files (check-cyclic-requires--find-el-files root))
         (result (check-cyclic-requires--build-graph files))
         (graph (car result))
         (local-features (cdr result))
         (raw-cycles (check-cyclic-requires--find-cycles graph local-features))
         (cycles (check-cyclic-requires--deduplicate-cycles raw-cycles)))
    (if (null cycles)
        (progn
          (message "No cyclic require dependencies found across %d files."
                   (hash-table-count local-features))
          (kill-emacs 0))
      (message "Cyclic require dependencies found:\n")
      (dolist (cycle cycles)
        (message "  %s" (check-cyclic-requires--format-cycle cycle)))
      (message "\nEach cycle means that loading one file will attempt to")
      (message "load another file that directly or transitively requires")
      (message "the first — causing either infinite recursion or missing")
      (message "definitions depending on load order.")
      (message "\nTo fix: break the cycle by removing one of the require")
      (message "forms, using autoload, or restructuring the code.")
      (kill-emacs 1))))

(provide 'check-cyclic-requires)
;;; check-cyclic-requires.el ends here
