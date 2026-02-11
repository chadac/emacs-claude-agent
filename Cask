;; -*- mode: emacs-lisp -*-
;; Cask file for claude-agent
;;
;; Install dependencies:
;;   cask install
;;
;; Run tests:
;;   cask exec emacs -batch -l test/test-helper.el -l test/TESTFILE.el -f ert-run-tests-batch

(source gnu)
(source melpa)

;; Package metadata
(package-file "claude-agent.el")

;; Runtime dependencies
(depends-on "emacs" "28.1")
(depends-on "dash")
(depends-on "markdown-mode")
(depends-on "transient")
(depends-on "magit")
(depends-on "org-roam")

;; Development dependencies
(development
 (depends-on "ert-runner")
 (depends-on "el-mock")
 (depends-on "undercover"))
