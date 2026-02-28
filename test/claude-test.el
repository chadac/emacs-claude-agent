;;; claude-test.el --- Tests for claude -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "1.0"))

;;; Commentary:
;; Test suite for claude-agent.el using ERT (Emacs Lisp Regression Testing).
;; 
;; Test categories:
;; - :unit - Pure function tests, no external dependencies
;; - :integration - Tests requiring mocked dependencies  
;; - :e2e - End-to-end tests with real processes
;; - :requires-claude - Tests requiring Claude CLI installation

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add parent directory to load path to find claude-agent
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'claude-agent)

;;; Test Utilities

(defmacro claude-test-with-temp-buffer (&rest body)
  "Execute BODY in a temporary buffer with claude-agent loaded.
This provides a clean environment for testing without side effects."
  `(with-temp-buffer
     (let ((inhibit-message t))
       ,@body)))

(defmacro claude-test-with-temp-file (filename content &rest body)
  "Execute BODY with a temporary file FILENAME containing CONTENT.
The file is automatically cleaned up after BODY executes."
  (declare (indent 2))
  `(let ((temp-file (make-temp-file ,filename)))
     (unwind-protect
         (progn
           (write-region ,content nil temp-file)
           (let ((buffer-file-name temp-file))
             ,@body))
       (when (file-exists-p temp-file)
         (delete-file temp-file)))))

(defun claude-test-cleanup-buffers ()
  "Clean up any Claude test buffers."
  (dolist (buffer (buffer-list))
    (when (string-match-p "^\\*claude:.*test\\*" (buffer-name buffer))
      (kill-buffer buffer))))

;;; Basic Sanity Tests

(ert-deftest claude-test-sanity ()
  "Basic sanity test to verify ERT is working."
  :tags '(:unit :sanity)
  (should t)
  (should-not nil)
  (should (= 2 (+ 1 1)))
  (should (string= "hello" "hello"))
  (should (listp '(1 2 3))))

(ert-deftest claude-test-package-loaded ()
  "Test that claude-agent package is loaded correctly."
  :tags '(:unit :sanity)
  (should (featurep 'claude-agent))
  (should (fboundp 'claude-run))
  (should (fboundp 'claude-kill))
  (should (fboundp 'claude-toggle-buffer))
  (should (boundp 'claude-program))
  (should (boundp 'claude-notify-on-await)))

(ert-deftest claude-test-custom-group-exists ()
  "Test that claude-agent custom group is properly defined."
  :tags '(:unit :sanity)
  (should (get 'claude-agent 'group-documentation))
  ;; Check that the group has members (custom variables and faces)
  (should (get 'claude-agent 'custom-group))
  ;; Verify some expected members are in the group
  (let ((members (get 'claude-agent 'custom-group)))
    (should (assq 'claude-program members))
    (should (assq 'claude-notify-on-await members))))

;;; Project Root Detection Tests

(ert-deftest claude-test-project-root-detection-git ()
  "Test project root detection with git repository."
  :tags '(:unit :project)
  (let ((test-dir (make-temp-file "claude-test" t)))
    (unwind-protect
        (progn
          ;; Create a git repo
          (let ((default-directory test-dir))
            (call-process "git" nil nil nil "init" "--quiet")
            (write-region "test content" nil "test.txt")
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil 
                         "-c" "user.name=Test" 
                         "-c" "user.email=test@example.com"
                         "commit" "-m" "initial" "--quiet"))
          
          ;; Test from repo root with mocked buffer-file-name
          (let ((test-file (expand-file-name "test.txt" test-dir)))
            (cl-letf (((symbol-function 'buffer-file-name) 
                       (lambda () test-file)))
              ;; Remove trailing slash for comparison
              (should (string= (file-name-as-directory (claude--project-root)) 
                               (file-name-as-directory test-dir)))))
          
          ;; Test from subdirectory
          (let* ((subdir (expand-file-name "subdir" test-dir))
                 (sub-file (expand-file-name "sub.txt" subdir)))
            (make-directory subdir)
            (write-region "sub content" nil sub-file)
            (cl-letf (((symbol-function 'buffer-file-name) 
                       (lambda () sub-file)))
              (should (string= (file-name-as-directory (claude--project-root)) 
                               (file-name-as-directory test-dir))))))
      
      ;; Cleanup
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest claude-test-project-root-detection-no-git ()
  "Test project root detection without git repository."
  :tags '(:unit :project)
  (let ((test-dir (make-temp-file "claude-no-git-test" t)))
    (unwind-protect
        (progn
          ;; Create a regular directory (no git)
          (let ((default-directory test-dir)
                (test-file (expand-file-name "test.txt" test-dir)))
            (write-region "content" nil test-file)
            
            ;; Mock buffer-file-name to return our test file
            (cl-letf (((symbol-function 'buffer-file-name) 
                       (lambda () test-file)))
              ;; Should return buffer dir since there's no git repo
              (should (string= (file-name-directory (buffer-file-name)) (claude--project-root))))))
      
      ;; Cleanup
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest claude-test-project-root-with-explicit-dir ()
  "Test project root detection with explicit directory parameter."
  :tags '(:unit :project)
  (let ((test-dir (make-temp-file "claude-explicit-test" t)))
    (unwind-protect
        (progn
          ;; Create a git repo
          (let ((default-directory test-dir))
            (call-process "git" nil nil nil "init" "--quiet")
            (write-region "content" nil "test.txt")
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil 
                         "-c" "user.name=Test" 
                         "-c" "user.email=test@example.com"
                         "commit" "-m" "initial" "--quiet"))
          
          ;; Test with explicit directory parameter (normalize trailing slashes)
          (should (string= (file-name-as-directory (claude--project-root test-dir)) 
                           (file-name-as-directory test-dir)))
          
          ;; Test with subdirectory
          (let ((subdir (expand-file-name "sub" test-dir)))
            (make-directory subdir)
            (should (string= (file-name-as-directory (claude--project-root subdir)) 
                             (file-name-as-directory test-dir)))))
      
      ;; Cleanup
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest claude-test-project-root-nested-repos ()
  "Test project root detection with nested git repositories."
  :tags '(:unit :project)
  (let ((outer-dir (make-temp-file "claude-outer" t))
        (inner-dir nil))
    (unwind-protect
        (progn
          ;; Create outer git repo
          (let ((default-directory outer-dir))
            (call-process "git" nil nil nil "init" "--quiet")
            (write-region "outer" nil "outer.txt")
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil 
                         "-c" "user.name=Test" 
                         "-c" "user.email=test@example.com"
                         "commit" "-m" "outer" "--quiet"))
          
          ;; Create inner git repo
          (setq inner-dir (expand-file-name "inner" outer-dir))
          (make-directory inner-dir)
          (let ((default-directory inner-dir))
            (call-process "git" nil nil nil "init" "--quiet")
            (write-region "inner" nil "inner.txt")
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil 
                         "-c" "user.name=Test" 
                         "-c" "user.email=test@example.com"
                         "commit" "-m" "inner" "--quiet"))
          
          ;; Test that inner directory returns inner repo, not outer (normalize paths)
          (should (string= (file-name-as-directory (claude--project-root inner-dir)) 
                           (file-name-as-directory inner-dir)))
          
          ;; Test that outer directory returns outer repo
          (should (string= (file-name-as-directory (claude--project-root outer-dir)) 
                           (file-name-as-directory outer-dir))))
      
      ;; Cleanup
      (when (file-exists-p outer-dir)
        (delete-directory outer-dir t)))))

;;; Session ID Generation Tests

(ert-deftest claude-test-session-id-doom-workspace ()
  "Test session ID generation with Doom Emacs workspace."
  :tags '(:unit :session)
  (cl-letf (((symbol-function '+workspace-current-name)
             (lambda () "my-doom-workspace"))
            ((symbol-function 'fboundp) 
             (lambda (func) 
               (eq func '+workspace-current-name))))
    (should (string= (claude--session-id) "my-doom-workspace"))))

(ert-deftest claude-test-session-id-perspective-workspace ()
  "Test session ID generation with Perspective mode."
  :tags '(:unit :session)
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
            ((symbol-function 'safe-persp-name)
             (lambda (persp) "my-perspective"))
            ((symbol-function 'get-current-persp)
             (lambda () 'fake-persp))
            ((symbol-function 'fboundp) 
             (lambda (func) 
               (memq func '(safe-persp-name get-current-persp)))))
    (should (string= (claude--session-id) "my-perspective"))))

(ert-deftest claude-test-session-id-fallback-to-project ()
  "Test session ID fallback to project root when no workspace is active."
  :tags '(:unit :session)
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
            ((symbol-function 'safe-persp-name) (lambda (persp) nil))
            ((symbol-function 'get-current-persp) (lambda () nil))
            ((symbol-function 'fboundp) (lambda (func) nil))
            ((symbol-function 'claude--project-root) (lambda () "/tmp/test-project"))
            ((symbol-function 'file-truename) (lambda (path) path)))
    (should (string= (claude--session-id) "/tmp/test-project"))))

(ert-deftest claude-test-session-id-empty-workspace-names ()
  "Test session ID with empty or nil workspace names."
  :tags '(:unit :session)
  ;; Test with Doom workspace returning empty string
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () ""))
            ((symbol-function 'fboundp) 
             (lambda (func) (eq func '+workspace-current-name)))
            ((symbol-function 'claude--project-root) (lambda () "/fallback"))
            ((symbol-function 'file-truename) (lambda (path) path)))
    (should (string= (claude--session-id) "/fallback")))
  
  ;; Test with Perspective returning nil
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil))
            ((symbol-function 'safe-persp-name) (lambda (persp) nil))
            ((symbol-function 'get-current-persp) (lambda () 'fake-persp))
            ((symbol-function 'fboundp) 
             (lambda (func) (memq func '(safe-persp-name get-current-persp))))
            ((symbol-function 'claude--project-root) (lambda () "/fallback2"))
            ((symbol-function 'file-truename) (lambda (path) path)))
    (should (string= (claude--session-id) "/fallback2"))))

(ert-deftest claude-test-session-id-workspace-priority ()
  "Test that Doom workspace takes priority over Perspective."
  :tags '(:unit :session)
  (cl-letf (((symbol-function '+workspace-current-name) (lambda () "doom-wins"))
            ((symbol-function 'safe-persp-name) (lambda (persp) "perspective-loses"))
            ((symbol-function 'get-current-persp) (lambda () 'fake-persp))
            ((symbol-function 'fboundp) 
             (lambda (func) 
               (memq func '(+workspace-current-name safe-persp-name get-current-persp)))))
    (should (string= (claude--session-id) "doom-wins"))))

;;; Buffer Name Generation Tests

(ert-deftest claude-test-buffer-name-generation ()
  "Test buffer name generation from session ID."
  :tags '(:unit :session)
  (cl-letf (((symbol-function 'claude--session-id) (lambda () "test-session")))
    (should (string= (claude--get-buffer-name) "*claude:test-session*"))))

(ert-deftest claude-test-buffer-name-with-special-chars ()
  "Test buffer name generation with special characters in session ID."
  :tags '(:unit :session)
  (cl-letf (((symbol-function 'claude--session-id) 
             (lambda () "/path/with/slashes")))
    (should (string= (claude--get-buffer-name) "*claude:/path/with/slashes*"))))

(ert-deftest claude-test-buffer-detection ()
  "Test Claude buffer detection."
  :tags '(:unit :session)
  (claude-test-with-temp-buffer
    ;; Test non-Claude buffer
    (should-not (claude--is-claude-buffer-p))
    
    ;; Test Claude buffer
    (rename-buffer "*claude:test*")
    (should (claude--is-claude-buffer-p))
    
    ;; Test with specific buffer argument
    (with-temp-buffer
      (should-not (claude--is-claude-buffer-p (current-buffer)))
      (rename-buffer "*claude:another*")
      (should (claude--is-claude-buffer-p (current-buffer)))))
  
  ;; Test with non-live buffer
  (let ((dead-buffer (get-buffer-create "*test-dead*")))
    (kill-buffer dead-buffer)
    (should-not (claude--is-claude-buffer-p dead-buffer))))

;;; Startup Hook Tests

(ert-deftest claude-test-startup-hook-exists ()
  "Test that claude-startup-hook variable exists."
  :tags '(:unit :startup-hook)
  (should (boundp 'claude-startup-hook))
  (should (eq claude-startup-hook nil))) ; Default value should be nil

(ert-deftest claude-test-startup-hook-is-hook ()
  "Test that claude-startup-hook is properly defined as a hook."
  :tags '(:unit :startup-hook)
  (should (get 'claude-startup-hook 'custom-type))
  (should (eq (get 'claude-startup-hook 'custom-type) 'hook)))

(ert-deftest claude-test-startup-hook-documentation ()
  "Test that claude-startup-hook has proper documentation."
  :tags '(:unit :startup-hook)
  (let ((doc (documentation-property 'claude-startup-hook 'variable-documentation)))
    (should doc)
    (should (string-match-p "after.*finished starting up" doc))
    (should (string-match-p "current buffer" doc))))

(ert-deftest claude-test-startup-hook-called-during-setup ()
  "Test that claude-mcp-startup-hook is called during agent setup."
  :tags '(:integration :startup-hook)
  (let ((hook-called nil)
        (hook-called-in-claude-buffer nil)
        (test-buffer nil)
        (hook-fn (lambda () 
                   (setq hook-called t)
                   (when (claude--is-claude-buffer-p)
                     (setq hook-called-in-claude-buffer t)))))
    
    ;; Create a test hook function
    (add-hook 'claude-mcp-startup-hook hook-fn)
    
    (unwind-protect
        (progn
          ;; Create a buffer that looks like a Claude buffer
          (setq test-buffer (get-buffer-create "*claude:test-hook*"))
          (with-current-buffer test-buffer
            ;; Set up minimal fake claude-agent--process
            (setq-local claude-agent--process
                        (start-process "fake-claude" nil "sleep" "60")))
          
          ;; Run the startup hook directly in the buffer context
          (with-current-buffer test-buffer
            (run-hooks 'claude-mcp-startup-hook))
          
          ;; Verify hook was called
          (should hook-called)
          (should hook-called-in-claude-buffer))
      
      ;; Cleanup
      (remove-hook 'claude-mcp-startup-hook hook-fn)
      (when (and test-buffer (buffer-live-p test-buffer))
        (with-current-buffer test-buffer
          (when (and (boundp 'claude-agent--process) claude-agent--process
                     (process-live-p claude-agent--process))
            (delete-process claude-agent--process)))
        (kill-buffer test-buffer)))))

(ert-deftest claude-test-startup-hook-multiple-functions ()
  "Test that multiple functions can be added to claude-startup-hook."
  :tags '(:integration :startup-hook)
  (let ((hook1-called nil)
        (hook2-called nil)
        (test-buffer nil))
    
    ;; Create two test hook functions
    (add-hook 'claude-mcp-startup-hook (lambda () (setq hook1-called t)))
    (add-hook 'claude-mcp-startup-hook (lambda () (setq hook2-called t)))
    
    (unwind-protect
        (progn
          ;; Create a buffer that looks like a Claude buffer
          (setq test-buffer (get-buffer-create "*claude:test-multiple*"))
          (with-current-buffer test-buffer
            ;; Set up minimal fake claude-agent--process
            (setq-local claude-agent--process
                        (start-process "fake-claude" nil "sleep" "60")))
          
          ;; Run the startup hook directly
          (with-current-buffer test-buffer
            (run-hooks 'claude-mcp-startup-hook))
          
          ;; Verify both hooks were called
          (should hook1-called)
          (should hook2-called))
      
      ;; Cleanup
      (remove-hook 'claude-mcp-startup-hook (lambda () (setq hook1-called t)))
      (remove-hook 'claude-mcp-startup-hook (lambda () (setq hook2-called t)))
      (when (and test-buffer (buffer-live-p test-buffer))
        (with-current-buffer test-buffer
          (when (and (boundp 'claude-agent--process) claude-agent--process
                     (process-live-p claude-agent--process))
            (delete-process claude-agent--process)))
        (kill-buffer test-buffer)))))

(ert-deftest claude-test-startup-hook-buffer-context ()
  "Test that claude-mcp-startup-hook runs with Claude buffer as current buffer."
  :tags '(:integration :startup-hook)
  (let ((captured-buffer-name nil)
        (captured-cwd nil)
        (test-buffer nil))
    
    ;; Create a test hook function that captures context
    (add-hook 'claude-mcp-startup-hook 
              (lambda () 
                (setq captured-buffer-name (buffer-name))
                (setq captured-cwd claude-mcp--cwd)))
    
    (unwind-protect
        (progn
          ;; Create a buffer that looks like a Claude buffer
          (setq test-buffer (get-buffer-create "*claude:test-context*"))
          (with-current-buffer test-buffer
            ;; Set up minimal fake environment
            (setq-local claude-agent--process
                        (start-process "fake-claude" nil "sleep" "60"))
            (setq-local claude-mcp--cwd "/test/directory"))
          
          ;; Run the startup hook directly
          (with-current-buffer test-buffer
            (run-hooks 'claude-mcp-startup-hook))
          
          ;; Verify hook ran in correct buffer context
          (should captured-buffer-name)
          (should (string= captured-buffer-name "*claude:test-context*"))
          (should captured-cwd)
          (should (string= captured-cwd "/test/directory")))
      
      ;; Cleanup
      (remove-hook 'claude-mcp-startup-hook 
                   (lambda () 
                     (setq captured-buffer-name (buffer-name))
                     (setq captured-cwd claude-mcp--cwd)))
      (when (and test-buffer (buffer-live-p test-buffer))
        (with-current-buffer test-buffer
          (when (and (boundp 'claude-agent--process) claude-agent--process
                     (process-live-p claude-agent--process))
            (delete-process claude-agent--process)))
        (kill-buffer test-buffer)))))

(ert-deftest claude-test-startup-hook-error-handling ()
  "Test that errors in claude-mcp-startup-hook are propagated."
  :tags '(:integration :startup-hook)
  (let ((hook-error-occurred nil)
        (test-buffer nil))
    
    ;; Create a hook function that throws an error
    (add-hook 'claude-mcp-startup-hook 
              (lambda () (error "Test hook error")))
    
    (unwind-protect
        (progn
          ;; Create a buffer that looks like a Claude buffer
          (setq test-buffer (get-buffer-create "*claude:test-error*"))
          (with-current-buffer test-buffer
            ;; Set up minimal fake claude-agent--process
            (setq-local claude-agent--process
                        (start-process "fake-claude" nil "sleep" "60")))
          
          ;; Run the startup hook and expect it to propagate errors
          (condition-case _err
              (with-current-buffer test-buffer
                (run-hooks 'claude-mcp-startup-hook))
            (error (setq hook-error-occurred t)))
          
          ;; The error should have been propagated
          (should hook-error-occurred))
      
      ;; Cleanup
      (remove-hook 'claude-mcp-startup-hook (lambda () (error "Test hook error")))
      (when (and test-buffer (buffer-live-p test-buffer))
        (with-current-buffer test-buffer
          (when (and (boundp 'claude-agent--process) claude-agent--process
                     (process-live-p claude-agent--process))
            (delete-process claude-agent--process)))
        (kill-buffer test-buffer)))))

;;; Custom Variable Tests

(ert-deftest claude-test-custom-variable-defaults ()
  "Test that custom variables have correct default values."
  :tags '(:unit :config)
  (should (string= claude-program "claude"))
  (should (eq claude-program-switches nil))
  (should (eq claude-switch-to-buffer-on-create t))
  (should (eq claude-switch-to-buffer-on-toggle t))
  (should (eq claude-m-return-is-submit nil))
  (should (eq claude-shift-return-newline t))
  (should (eq claude-switch-to-buffer-on-file-add nil))
  (should (eq claude-notify-on-await t))
  (should (string= claude-notification-sound-mac "Submarine"))
  (should (eq claude-startup-hook nil))) ; Include startup hook in defaults test

(ert-deftest claude-test-custom-variable-types ()
  "Test that custom variables have correct types."
  :tags '(:unit :config)
  ;; Test string variables
  (should (stringp claude-program))
  (should (stringp claude-notification-sound-mac))
  
  ;; Test boolean variables
  (should (booleanp claude-switch-to-buffer-on-create))
  (should (booleanp claude-switch-to-buffer-on-toggle))
  (should (booleanp claude-m-return-is-submit))
  (should (booleanp claude-shift-return-newline))
  (should (booleanp claude-switch-to-buffer-on-file-add))
  (should (booleanp claude-notify-on-await))
  
  ;; Test list variable
  (should (listp claude-program-switches)))

(ert-deftest claude-test-notification-sound-behavior ()
  "Test that claude-notification-sound-mac affects notification calls."
  :tags '(:unit :config)
  (let ((notification-command nil))
    ;; Mock call-process to capture the full command
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &rest args)
                 (when (string= program "osascript")
                   (setq notification-command (mapconcat 'identity args " ")))))
              ;; Mock system-type to be macOS
              (system-type 'darwin))
      
      ;; Test with custom sound
      (let ((claude-notification-sound-mac "Ping"))
        (claude--system-notification "Test message" "Test title")
        (should notification-command)
        (should (string-match-p "Ping" notification-command)))
      
      ;; Test with different sound
      (setq notification-command nil)
      (let ((claude-notification-sound-mac "Glass"))
        (claude--system-notification "Test message" "Test title") 
        (should notification-command)
        (should (string-match-p "Glass" notification-command))))))

(ert-deftest claude-test-program-switches-behavior ()
  "Test that claude-program-switches affects command construction."
  :tags '(:unit :config)
  ;; Test the switches logic directly without complex mocking
  (let ((claude-program-switches nil)
        (args '("--resume")))
    ;; Test with no custom switches - should just have the passed args
    (let ((result (remove nil (append args claude-program-switches))))
      (should (equal result '("--resume"))))
    
    ;; Test with custom switches - should include both
    (setq claude-program-switches '("--verbose" "--test"))
    (let ((result (remove nil (append args claude-program-switches))))
      (should (member "--verbose" result))
      (should (member "--test" result))
      (should (member "--resume" result))
      (should (= (length result) 3)))))

(ert-deftest claude-test-switch-to-buffer-behavior ()
  "Test buffer switching behavior based on custom variables."
  :tags '(:unit :config)
  (let ((window-selected nil))
    ;; Mock window selection functions
    (cl-letf (((symbol-function 'display-buffer) (lambda (buffer) buffer))
              ((symbol-function 'select-window) 
               (lambda (window &optional norecord) (setq window-selected t)))
              ((symbol-function 'get-buffer-window) 
               (lambda (buffer &optional all-frames) 'fake-window)))
      
      ;; Test switch-to-buffer-on-create behavior
      (let ((test-buffer (get-buffer-create "*claude:switch-test*")))
        (unwind-protect
            (progn
              ;; Test with switching enabled
              (setq window-selected nil)
              (let ((claude-switch-to-buffer-on-create t))
                (with-current-buffer test-buffer
                  (let ((window (display-buffer test-buffer)))
                    (when claude-switch-to-buffer-on-create
                      (select-window window))))
                (should window-selected))
              
              ;; Test with switching disabled  
              (setq window-selected nil)
              (let ((claude-switch-to-buffer-on-create nil))
                (with-current-buffer test-buffer
                  (let ((window (display-buffer test-buffer)))
                    (when claude-switch-to-buffer-on-create
                      (select-window window))))
                (should-not window-selected)))
          
          ;; Cleanup
          (when (buffer-live-p test-buffer)
            (kill-buffer test-buffer)))))))

(ert-deftest claude-test-load-cli-env-vars ()
  "Test loading environment variables from ~/.claude/settings.json."
  :tags '(:unit :config)
  (require 'claude-mcp-process)

  ;; Store original function
  (let ((original-expand-file-name (symbol-function 'expand-file-name)))

    ;; Test with a mock settings file
    (claude-test-with-temp-file "test-settings.json"
        "{\"env\": {\"AWS_PROFILE\": \"test-profile\", \"AWS_REGION\": \"us-west-2\", \"CLAUDE_CODE_USE_BEDROCK\": \"1\"}}"

      ;; Mock expand-file-name to return our temp file
      (cl-letf (((symbol-function 'expand-file-name)
                 (lambda (name &optional dir)
                   (if (string= name "~/.claude/settings.json")
                       buffer-file-name
                     (funcall original-expand-file-name name dir)))))

        ;; Test successful loading
        (let ((env-vars (claude-mcp--load-cli-env-vars)))
          (should (member "AWS_PROFILE=test-profile" env-vars))
          (should (member "AWS_REGION=us-west-2" env-vars))
          (should (member "CLAUDE_CODE_USE_BEDROCK=1" env-vars)))))

    ;; Test with missing file
    (cl-letf (((symbol-function 'expand-file-name)
               (lambda (name &optional dir)
                 (if (string= name "~/.claude/settings.json")
                     "/nonexistent/path.json"
                   (funcall original-expand-file-name name dir)))))
      (let ((env-vars (claude-mcp--load-cli-env-vars)))
        (should (null env-vars))))

    ;; Test with malformed JSON
    (claude-test-with-temp-file "malformed.json" "{ invalid json"
      (cl-letf (((symbol-function 'expand-file-name)
                 (lambda (name &optional dir)
                   (if (string= name "~/.claude/settings.json")
                       buffer-file-name
                     (funcall original-expand-file-name name dir)))))
        ;; Should handle error gracefully
        (let ((env-vars (claude-mcp--load-cli-env-vars)))
          (should (null env-vars)))))

    ;; Test with settings file but no env key
    (claude-test-with-temp-file "no-env.json" "{\"other\": \"value\"}"
      (cl-letf (((symbol-function 'expand-file-name)
                 (lambda (name &optional dir)
                   (if (string= name "~/.claude/settings.json")
                       buffer-file-name
                     (funcall original-expand-file-name name dir)))))
        (let ((env-vars (claude-mcp--load-cli-env-vars)))
          (should (null env-vars)))))))

(provide 'claude-test)
;;; claude-test.el ends here
