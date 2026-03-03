;;; claude-agent-permissions-test.el --- Tests for permission policy system -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the claude-agent-permissions module.

;;; Code:

(require 'ert)
(require 'claude-agent-permissions)

;;;; Path extraction tests

(ert-deftest claude-permission-extract-path-read ()
  "Test path extraction for Read tool."
  (should (equal "/home/user/file.txt"
                 (claude-agent-permission--extract-path
                  "Read"
                  '((file_path . "/home/user/file.txt"))))))

(ert-deftest claude-permission-extract-path-edit ()
  "Test path extraction for Edit tool."
  (should (equal "/etc/config.conf"
                 (claude-agent-permission--extract-path
                  "Edit"
                  '((file_path . "/etc/config.conf")
                    (old_string . "foo")
                    (new_string . "bar"))))))

(ert-deftest claude-permission-extract-path-write ()
  "Test path extraction for Write tool."
  (should (equal "/tmp/output.txt"
                 (claude-agent-permission--extract-path
                  "Write"
                  '((file_path . "/tmp/output.txt")
                    (content . "hello"))))))

(ert-deftest claude-permission-extract-path-mcp-emacs ()
  "Test path extraction for MCP emacs tools."
  (should (equal "/home/user/buffer.el"
                 (claude-agent-permission--extract-path
                  "mcp__emacs__read_file"
                  '((file_path . "/home/user/buffer.el"))))))

(ert-deftest claude-permission-extract-path-bash-cd ()
  "Test path extraction from bash cd command."
  (should (equal "/home/user/project"
                 (claude-agent-permission--extract-path-from-bash
                  "cd /home/user/project"))))

(ert-deftest claude-permission-extract-path-bash-cat ()
  "Test path extraction from bash cat command."
  (should (equal "/etc/passwd"
                 (claude-agent-permission--extract-path-from-bash
                  "cat /etc/passwd"))))

(ert-deftest claude-permission-extract-path-bash-rm ()
  "Test path extraction from bash rm command."
  (should (equal "/tmp/file.txt"
                 (claude-agent-permission--extract-path-from-bash
                  "rm -f /tmp/file.txt"))))

(ert-deftest claude-permission-extract-path-no-path ()
  "Test path extraction returns nil for tools without paths."
  (should (null (claude-agent-permission--extract-path
                 "Task"
                 '((description . "Do something"))))))

;;;; Rule matching tests

(ert-deftest claude-permission-match-tool-exact ()
  "Test exact tool name matching."
  (should (claude-agent-permission--match-rule
           '(:tool "Read")
           "Read"
           '((file_path . "/tmp/file.txt"))))
  (should-not (claude-agent-permission--match-rule
               '(:tool "Read")
               "Write"
               '((file_path . "/tmp/file.txt")))))

(ert-deftest claude-permission-match-tool-regex ()
  "Test tool name regex matching."
  (should (claude-agent-permission--match-rule
           '(:tool-regex "Read\\|Write")
           "Read"
           nil))
  (should (claude-agent-permission--match-rule
           '(:tool-regex "Read\\|Write")
           "Write"
           nil))
  (should-not (claude-agent-permission--match-rule
               '(:tool-regex "Read\\|Write")
               "Edit"
               nil)))

(ert-deftest claude-permission-match-path-prefix ()
  "Test path prefix matching."
  (should (claude-agent-permission--match-rule
           '(:path-prefix "/home/user/project")
           "Edit"
           '((file_path . "/home/user/project/src/file.el"))))
  (should-not (claude-agent-permission--match-rule
               '(:path-prefix "/home/user/project")
               "Edit"
               '((file_path . "/etc/passwd")))))

(ert-deftest claude-permission-match-path-regex ()
  "Test path regex matching."
  (should (claude-agent-permission--match-rule
           '(:path-regex "\\.el$")
           "Read"
           '((file_path . "/home/user/test.el"))))
  (should-not (claude-agent-permission--match-rule
               '(:path-regex "\\.el$")
               "Read"
               '((file_path . "/home/user/test.py")))))

(ert-deftest claude-permission-match-and ()
  "Test AND combinator."
  (should (claude-agent-permission--match-rule
           '(:and (:tool "Edit") (:path-prefix "/home/user"))
           "Edit"
           '((file_path . "/home/user/file.el"))))
  (should-not (claude-agent-permission--match-rule
               '(:and (:tool "Edit") (:path-prefix "/home/user"))
               "Read"
               '((file_path . "/home/user/file.el"))))
  (should-not (claude-agent-permission--match-rule
               '(:and (:tool "Edit") (:path-prefix "/home/user"))
               "Edit"
               '((file_path . "/etc/passwd")))))

(ert-deftest claude-permission-match-or ()
  "Test OR combinator."
  (should (claude-agent-permission--match-rule
           '(:or (:tool "Read") (:tool "Edit"))
           "Read"
           nil))
  (should (claude-agent-permission--match-rule
           '(:or (:tool "Read") (:tool "Edit"))
           "Edit"
           nil))
  (should-not (claude-agent-permission--match-rule
               '(:or (:tool "Read") (:tool "Edit"))
               "Write"
               nil)))

(ert-deftest claude-permission-match-not ()
  "Test NOT combinator."
  (should (claude-agent-permission--match-rule
           '(:not (:tool "Bash"))
           "Read"
           nil))
  (should-not (claude-agent-permission--match-rule
               '(:not (:tool "Bash"))
               "Bash"
               nil)))

(ert-deftest claude-permission-match-complex ()
  "Test complex nested matcher."
  ;; Match: (Edit OR Write) AND (path in /home) AND (NOT .conf files)
  (let ((matcher '(:and (:or (:tool "Edit") (:tool "Write"))
                        (:path-prefix "/home")
                        (:not (:path-regex "\\.conf$")))))
    (should (claude-agent-permission--match-rule
             matcher "Edit" '((file_path . "/home/user/file.el"))))
    (should-not (claude-agent-permission--match-rule
                 matcher "Edit" '((file_path . "/home/user/file.conf"))))
    (should-not (claude-agent-permission--match-rule
                 matcher "Read" '((file_path . "/home/user/file.el"))))
    (should-not (claude-agent-permission--match-rule
                 matcher "Edit" '((file_path . "/etc/file.el"))))))

;;;; Rule evaluation tests

(ert-deftest claude-permission-evaluate-rules-first-match ()
  "Test that first matching rule wins."
  (let ((rules '((:match (:tool "Read") :action :allow :scope :session)
                 (:match (:tool "Read") :action :deny :message "Should not reach"))))
    (let ((result (claude-agent-permission--evaluate-rules "Read" nil rules)))
      (should (eq :allow (plist-get result :action)))
      (should (eq :session (plist-get result :scope))))))

(ert-deftest claude-permission-evaluate-rules-no-match ()
  "Test that nil is returned when no rule matches."
  (let ((rules '((:match (:tool "Read") :action :allow))))
    (should (null (claude-agent-permission--evaluate-rules "Write" nil rules)))))

(ert-deftest claude-permission-evaluate-rules-deny-with-message ()
  "Test deny action with message."
  (let ((rules '((:match (:tool "Bash") :action :deny :message "No shell access"))))
    (let ((result (claude-agent-permission--evaluate-rules "Bash" nil rules)))
      (should (eq :deny (plist-get result :action)))
      (should (equal "No shell access" (plist-get result :message))))))

;;;; Policy handler tests

(ert-deftest claude-permission-policy-deny-all ()
  "Test :deny-all policy."
  (let ((claude-agent-permission-policy :deny-all)
        (claude-agent-permission-deny-reason "Sandboxed session"))
    (let ((result (claude-agent-permission-handle-request "Read" nil)))
      (should (eq :deny (car result)))
      (should (equal "Sandboxed session" (plist-get (cdr result) :message))))))

(ert-deftest claude-permission-policy-allow-all ()
  "Test :allow-all policy."
  (let ((claude-agent-permission-policy :allow-all))
    (let ((result (claude-agent-permission-handle-request "Edit" nil)))
      (should (eq :allow (car result)))
      (should (eq :session (plist-get (cdr result) :scope))))))

(ert-deftest claude-permission-policy-rules ()
  "Test :rules policy with local rules."
  (let ((claude-agent-permission-policy :rules)
        (claude-agent-permission-rules-local
         '((:match (:tool "Read") :action :allow :scope :always)))
        (claude-agent-permission-rules nil))
    (let ((result (claude-agent-permission-handle-request "Read" '((file_path . "/tmp/test")))))
      (should (eq :allow (car result)))
      (should (eq :always (plist-get (cdr result) :scope))))))

(ert-deftest claude-permission-policy-nil-with-global-rules ()
  "Test nil policy falls through to global rules."
  (let ((claude-agent-permission-policy nil)
        (claude-agent-permission-rules-local nil)
        (claude-agent-permission-rules
         '((:match (:path-prefix "/etc") :action :deny :message "System files"))))
    (let ((result (claude-agent-permission-handle-request
                   "Edit" '((file_path . "/etc/passwd")))))
      (should (eq :deny (car result))))))

(ert-deftest claude-permission-policy-nil-no-match ()
  "Test nil policy with no matching rules returns nil (show UI)."
  (let ((claude-agent-permission-policy nil)
        (claude-agent-permission-rules-local nil)
        (claude-agent-permission-rules nil))
    (should (null (claude-agent-permission-handle-request "Edit" nil)))))

(ert-deftest claude-permission-local-rules-precedence ()
  "Test that local rules take precedence over global rules."
  (let ((claude-agent-permission-policy :rules)
        (claude-agent-permission-rules-local
         '((:match (:tool "Read") :action :deny :message "Local deny")))
        (claude-agent-permission-rules
         '((:match (:tool "Read") :action :allow :scope :always))))
    (let ((result (claude-agent-permission-handle-request "Read" nil)))
      (should (eq :deny (car result)))
      (should (equal "Local deny" (plist-get (cdr result) :message))))))

;;;; Pattern generation tests

(ert-deftest claude-permission-generate-pattern-once ()
  "Test pattern generation for :once scope."
  (should (equal "Read(/tmp/file.txt)"
                 (claude-agent-permission--generate-pattern
                  "Read" '((file_path . "/tmp/file.txt")) :once))))

(ert-deftest claude-permission-generate-pattern-session ()
  "Test pattern generation for :session scope."
  (should (equal "Bash(git:*)"
                 (claude-agent-permission--generate-pattern
                  "Bash" '((command . "git status")) :session))))

(ert-deftest claude-permission-generate-pattern-always ()
  "Test pattern generation for :always scope."
  (should (equal "Edit(/home/user/*)"
                 (claude-agent-permission--generate-pattern
                  "Edit" '((file_path . "/home/user/file.el")) :always))))

;;;; Scope to action conversion tests

(ert-deftest claude-permission-scope-to-action ()
  "Test scope symbol to action string conversion."
  (should (equal "allow_once" (claude-agent-permission-scope-to-action :once)))
  (should (equal "allow_session" (claude-agent-permission-scope-to-action :session)))
  (should (equal "allow_always" (claude-agent-permission-scope-to-action :always)))
  (should (equal "allow_once" (claude-agent-permission-scope-to-action :unknown))))

;;;; Predicate matcher tests

(ert-deftest claude-permission-predicate-matcher ()
  "Test :predicate matcher with custom function."
  (let ((claude-agent-permission-policy :rules)
        (claude-agent-permission-rules
         `((:match (:predicate ,(lambda (tool input)
                                  (and (string= tool "Bash")
                                       (string-match-p "^rm " (alist-get 'command input)))))
            :action :deny
            :reason "rm command blocked")
           (:match t :action :allow :scope :session))))
    ;; rm command should be denied
    (let ((result (claude-agent-permission-handle-request
                   "Bash" '((command . "rm -rf /")))))
      (should (eq :deny (car result)))
      (should (equal "rm command blocked" (plist-get (cdr result) :reason))))
    ;; Other commands should be allowed
    (let ((result (claude-agent-permission-handle-request
                   "Bash" '((command . "ls -la")))))
      (should (eq :allow (car result))))))

(ert-deftest claude-permission-predicate-matcher-with-symbol ()
  "Test :predicate matcher with named function."
  (cl-flet ((my-test-predicate (tool input)
              (string= tool "Write")))
    (let ((claude-agent-permission-policy :rules)
          (claude-agent-permission-rules
           `((:match (:predicate ,#'my-test-predicate)
              :action :deny
              :reason "Write blocked"))))
      (let ((result (claude-agent-permission-handle-request "Write" nil)))
        (should (eq :deny (car result)))))))

;;;; Catch-all (t) matcher tests

(ert-deftest claude-permission-catch-all-matcher ()
  "Test t matcher as catch-all."
  (let ((claude-agent-permission-policy :rules)
        (claude-agent-permission-rules
         '((:match (:tool "Read") :action :allow :scope :session)
           (:match t :action :deny :reason "Everything else denied"))))
    ;; Read should be allowed
    (let ((result (claude-agent-permission-handle-request "Read" nil)))
      (should (eq :allow (car result))))
    ;; Write should be denied by catch-all
    (let ((result (claude-agent-permission-handle-request "Write" nil)))
      (should (eq :deny (car result)))
      (should (equal "Everything else denied" (plist-get (cdr result) :reason))))))

(ert-deftest claude-permission-catch-all-prompt ()
  "Test t matcher with :prompt action."
  (let ((claude-agent-permission-policy :rules)
        (claude-agent-permission-rules
         '((:match (:tool "Read") :action :allow :scope :session)
           (:match t :action :prompt))))
    ;; Read should be allowed
    (let ((result (claude-agent-permission-handle-request "Read" nil)))
      (should (eq :allow (car result))))
    ;; Write should fall through to prompt
    (let ((result (claude-agent-permission-handle-request "Write" nil)))
      (should (eq :prompt (car result))))))

;;;; Unified permission check function tests

(ert-deftest claude-permission-check-allow ()
  "Test claude-agent-permission-check returning allow."
  (let ((claude-agent-permission-policy :rules)
        (claude-agent-permission-rules
         '((:match (:tool "Read") :action :allow :scope :session))))
    (let ((result (claude-agent-permission-check "Read" nil)))
      (should (eq :allow (plist-get result :decision)))
      (should (eq :session (plist-get result :scope))))))

(ert-deftest claude-permission-check-deny ()
  "Test claude-agent-permission-check returning deny."
  (let ((claude-agent-permission-policy :rules)
        (claude-agent-permission-rules
         '((:match (:tool "Write") :action :deny :reason "No writes"))))
    (let ((result (claude-agent-permission-check "Write" nil)))
      (should (eq :deny (plist-get result :decision)))
      (should (equal "No writes" (plist-get result :reason))))))

(ert-deftest claude-permission-check-prompt ()
  "Test claude-agent-permission-check returning prompt."
  (let ((claude-agent-permission-policy :rules)
        (claude-agent-permission-rules
         '((:match t :action :prompt))))
    (let ((result (claude-agent-permission-check "AnyTool" nil)))
      (should (eq :prompt (plist-get result :decision))))))

(ert-deftest claude-permission-check-rule-precedence ()
  "Test rule evaluation order in claude-agent-permission-check."
  (let ((claude-agent-permission-policy :rules)
        (claude-agent-permission-rules-local
         '((:match (:tool "Read") :action :deny :reason "Local override")))
        (claude-agent-project-permission-rules
         '((:match (:tool "Read") :action :allow :scope :always)))
        (claude-agent-permission-rules
         '((:match (:tool "Read") :action :allow :scope :session))))
    ;; Local rules should take precedence
    (let ((result (claude-agent-permission-check "Read" nil)))
      (should (eq :deny (plist-get result :decision)))
      (should (equal "Local override" (plist-get result :reason))))))

(ert-deftest claude-permission-check-project-rules ()
  "Test project rules in claude-agent-permission-check."
  (let ((claude-agent-permission-policy :rules)
        (claude-agent-permission-rules-local nil)
        (claude-agent-project-permission-rules
         '((:match (:tool "Edit") :action :allow :scope :always)))
        (claude-agent-permission-rules
         '((:match t :action :prompt))))
    ;; Project rule should match before global
    (let ((result (claude-agent-permission-check "Edit" nil)))
      (should (eq :allow (plist-get result :decision)))
      (should (eq :always (plist-get result :scope))))))

;;;; Path extraction tests

(ert-deftest claude-permission-path-extraction-sdk-tools ()
  "Test path extraction for SDK tools."
  (should (equal '("/tmp/file.txt")
                 (claude-agent-permission--extract-paths
                  "Read" '((file_path . "/tmp/file.txt")))))
  (should (equal '("/home/user/file.el")
                 (claude-agent-permission--extract-paths
                  "Edit" '((file_path . "/home/user/file.el")
                           (old_string . "foo")
                           (new_string . "bar")))))
  (should (equal '("/project/")
                 (claude-agent-permission--extract-paths
                  "Glob" '((path . "/project/")
                           (pattern . "*.el"))))))

(provide 'claude-agent-permissions-test)
;;; claude-agent-permissions-test.el ends here
