;;; claude-acp.el --- ACP backend for Claude Agent -*- lexical-binding: t; -*-

;; This file is part of Claude Agent.

;;; Commentary:

;; This module provides the ACP (Agent Client Protocol) backend for Claude Agent.
;; It replaces the Python wrapper (claude_agent/) with a direct Emacs ↔ ACP
;; connection via acp.el and @zed-industries/claude-agent-acp.
;;
;; Architecture:
;;   Emacs (acp.el) ↔ JSON-RPC over stdio ↔ claude-agent-acp ↔ Anthropic API
;;        │
;;        └── Emacs MCP Server (Python) ↔ emacsclient  [KEPT]
;;
;; The ACP client handles:
;; - Session lifecycle (init, authenticate, new/resume session, prompt)
;; - Incoming notifications (session/update → buffer rendering)
;; - Incoming requests (permission, fs/read, fs/write → elisp handlers)
;; - Outgoing responses (permission decisions, file contents)

;;; Code:

;; Ensure vendored acp.el is on load-path if not already available
(unless (locate-library "acp")
  (let ((vendor-acp-dir
         (expand-file-name "vendor/acp.el"
                           (or (bound-and-true-p claude-agent-root-directory)
                               (when load-file-name
                                 (file-name-directory load-file-name))
                               (locate-library "claude-acp")))))
    (when (and vendor-acp-dir (file-directory-p vendor-acp-dir))
      (add-to-list 'load-path vendor-acp-dir))))

(require 'acp)
(require 'map)
(require 'json)
(require 'cl-lib)

;; Forward declarations
(declare-function claude--package-root "claude-agent")
(declare-function claude-agent--dispatch-message "claude-agent-repl")
(declare-function claude-agent--set-thinking "claude-agent-repl")
(declare-function claude-agent--render-dynamic-section "claude-agent-repl")
(declare-function claude-agent-permission-check "claude-agent-permissions")
(declare-function claude-agent-permission-handle-request "claude-agent-permissions")
(declare-function claude-agent-permission-scope-to-action "claude-agent-permissions")
(declare-function claude--generate-mcp-config "claude-mcp")

(defvar claude-agent--process)
(defvar claude-agent--session-info)
(defvar claude-agent--pending-output)

;;;; Customization

(defgroup claude-acp nil
  "ACP backend for Claude Agent."
  :group 'claude-agent)

(defcustom claude-acp-command nil
  "Command to run the ACP agent binary.
When nil (default), auto-detects from:
  1. `claude-agent-acp' on PATH
  2. Vendored wrapper script at <package-root>/scripts/claude-agent-acp-wrapper.sh
  3. Vendored binary at <package-root>/vendor/claude-agent-acp/node_modules/.bin/claude-agent-acp

Can be set to an absolute path to override auto-detection."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Command path"))
  :group 'claude-acp)

(defcustom claude-acp-command-args nil
  "Additional arguments to pass to the ACP agent command."
  :type '(repeat string)
  :group 'claude-acp)

(defcustom claude-acp-protocol-version "2025-01-01"
  "ACP protocol version to use during initialization."
  :type 'string
  :group 'claude-acp)

(defcustom claude-acp-authentication '(:login t)
  "Authentication method for ACP sessions.
Supported formats:
  (:login t)           - Use Claude login-based auth (default)
  (:api-key KEY)       - Use an API key directly
  (:api-key-fn FN)     - Call FN to get the API key"
  :type '(plist)
  :group 'claude-acp)

(defcustom claude-acp-enable-logging nil
  "Whether to enable ACP debug logging.
When non-nil, ACP traffic is logged to *acp-* buffers."
  :type 'boolean
  :group 'claude-acp)

;;;; Buffer-local state

(defvar-local claude-acp--client nil
  "The ACP client for this session.")

(defvar-local claude-acp--session-id nil
  "The ACP session ID for this buffer.")

(defvar-local claude-acp--initialized nil
  "Non-nil after ACP initialization handshake completes.")

(defvar-local claude-acp--authenticated nil
  "Non-nil after ACP authentication completes.")

(defvar-local claude-acp--agent-capabilities nil
  "Agent capabilities reported during initialization.")

(defvar-local claude-acp--pending-permission-request-id nil
  "The JSON-RPC request ID for the current permission request.
Used to send the response back via `acp-send-response'.")

(defvar-local claude-acp--current-assistant-text ""
  "Accumulated assistant message text for the current turn.")

(defvar-local claude-acp--turn-active nil
  "Non-nil when a prompt turn is in progress.")

;;;; Command resolution

(defun claude-acp--resolve-command ()
  "Resolve the ACP command to use.
Returns the command string, checking in order:
1. `claude-acp-command' if set
2. `claude-agent-acp' on PATH
3. Vendored wrapper script
4. Vendored binary directly"
  (or claude-acp-command
      (executable-find "claude-agent-acp")
      (let ((root (claude--package-root)))
        (when root
          (let ((wrapper (expand-file-name "scripts/claude-agent-acp-wrapper.sh" root))
                (vendored (expand-file-name
                           "vendor/claude-agent-acp/node_modules/.bin/claude-agent-acp"
                           root)))
            (cond
             ((and (file-exists-p wrapper) (file-executable-p wrapper))
              wrapper)
             ((file-exists-p vendored)
              vendored)))))
      (error "claude-agent-acp not found. Install with: npm install -g @zed-industries/claude-agent-acp")))

;;;; Environment setup

(defun claude-acp--environment-variables ()
  "Return environment variables for the ACP process."
  (let ((vars nil))
    ;; Authentication
    (cond
     ((plist-get claude-acp-authentication :api-key)
      (let ((key (plist-get claude-acp-authentication :api-key)))
        (push (format "ANTHROPIC_API_KEY=%s" key) vars)))
     ((plist-get claude-acp-authentication :api-key-fn)
      (let ((key (funcall (plist-get claude-acp-authentication :api-key-fn))))
        (push (format "ANTHROPIC_API_KEY=%s" key) vars)))
     ((plist-get claude-acp-authentication :login)
      ;; Login-based auth - clear API key so CLI uses login
      (push "ANTHROPIC_API_KEY=" vars)))
    ;; Load env vars from ~/.claude/settings.json
    (when-let ((cli-vars (claude-acp--load-cli-env-vars)))
      (setq vars (append cli-vars vars)))
    vars))

(defun claude-acp--load-cli-env-vars ()
  "Load environment variables from ~/.claude/settings.json.
Returns a list of \"VAR=VALUE\" strings."
  (let ((settings-file (expand-file-name "~/.claude/settings.json")))
    (when (file-exists-p settings-file)
      (condition-case err
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'string)
                 (settings (json-read-file settings-file))
                 (env-vars (cdr (assoc "env" settings))))
            (when env-vars
              (mapcar (lambda (pair)
                        (format "%s=%s" (car pair) (cdr pair)))
                      env-vars)))
        (error
         (message "Claude ACP: Failed to parse ~/.claude/settings.json: %s"
                  (error-message-string err))
         nil)))))

;;;; MCP server configuration for ACP

(defun claude-acp--mcp-server-config (work-dir buffer-name)
  "Build MCP server configuration alist for ACP session.
WORK-DIR is the session working directory.
BUFFER-NAME is the Claude buffer name."
  (let* ((this-dir (claude--package-root))
         (mcp-dir (when this-dir
                    (expand-file-name "emacs_mcp" this-dir)))
         (expanded-work-dir (expand-file-name work-dir)))
    (when mcp-dir
      `(("emacs" . ((command . "uv")
                    (args . ,(vector "run" "--python-preference" "managed"
                                     "--directory" mcp-dir
                                     "-m" "emacs_mcp.server"))
                    (env . ((CLAUDE_AGENT_CWD . ,expanded-work-dir)
                            (CLAUDE_AGENT_BUFFER_NAME . ,buffer-name)))))))))

;;;; Client creation and lifecycle

(defun claude-acp--create-client (buffer)
  "Create an ACP client for BUFFER."
  (let ((acp-logging-enabled claude-acp-enable-logging)
        (command (claude-acp--resolve-command)))
    (acp-make-client
     :context-buffer buffer
     :command command
     :command-params claude-acp-command-args
     :environment-variables (claude-acp--environment-variables))))

(defun claude-acp--initialize (client buffer callback)
  "Initialize ACP CLIENT in BUFFER, then call CALLBACK on success."
  (acp-send-request
   :client client
   :buffer buffer
   :request (acp-make-initialize-request
             :protocol-version claude-acp-protocol-version
             :client-info `((name . "claude-agent")
                            (title . "Claude Agent for Emacs")
                            (version . "0.1.0"))
             :read-text-file-capability t
             :write-text-file-capability t)
   :on-success (lambda (result)
                 (setq claude-acp--initialized t)
                 (setq claude-acp--agent-capabilities
                       (map-elt result 'agentCapabilities))
                 ;; Extract agent info for display
                 (when-let ((agent-info (map-elt result 'agentInfo)))
                   (message "Claude ACP: Connected to %s %s"
                            (or (map-elt agent-info 'title)
                                (map-elt agent-info 'name)
                                "agent")
                            (or (map-elt agent-info 'version) "")))
                 (funcall callback))
   :on-failure (lambda (error)
                 (message "Claude ACP: Initialization failed: %S" error))))

(defun claude-acp--authenticate (client buffer callback)
  "Authenticate ACP CLIENT in BUFFER, then call CALLBACK."
  (let ((method-id (cond
                    ((plist-get claude-acp-authentication :login) "login")
                    ((or (plist-get claude-acp-authentication :api-key)
                         (plist-get claude-acp-authentication :api-key-fn))
                     "api_key")
                    (t "login"))))
    (acp-send-request
     :client client
     :buffer buffer
     :request (acp-make-authenticate-request :method-id method-id)
     :on-success (lambda (_result)
                   (setq claude-acp--authenticated t)
                   (funcall callback))
     :on-failure (lambda (error)
                   (message "Claude ACP: Authentication failed: %S" error)))))

(defun claude-acp--create-session (client buffer work-dir buffer-name
                                          &optional callback system-prompt model)
  "Create a new ACP session via CLIENT in BUFFER.
WORK-DIR is the project directory.
BUFFER-NAME is the Claude buffer name.
CALLBACK is called with session-id on success.
SYSTEM-PROMPT is optional additional prompt text.
MODEL is optional model ID."
  (let* ((mcp-servers (claude-acp--mcp-server-config work-dir buffer-name))
         (meta (when system-prompt
                 `((systemPrompt . ((append . ,system-prompt)))))))
    (acp-send-request
     :client client
     :buffer buffer
     :request (acp-make-session-new-request
               :cwd work-dir
               :mcp-servers (or mcp-servers (vector))
               :meta meta)
     :on-success (lambda (result)
                   (let ((session-id (map-elt result 'sessionId)))
                     (setq claude-acp--session-id session-id)
                     ;; Set model if requested
                     (when model
                       (claude-acp--set-model client buffer session-id model))
                     ;; Update session info for display
                     (setq claude-agent--session-info
                           (plist-put claude-agent--session-info :session-id session-id))
                     (when callback
                       (funcall callback session-id))))
     :on-failure (lambda (error)
                   (message "Claude ACP: Session creation failed: %S" error)))))

(defun claude-acp--resume-session (client buffer session-id work-dir buffer-name
                                          &optional callback)
  "Resume an existing ACP session via CLIENT in BUFFER.
SESSION-ID is the session to resume.
WORK-DIR is the project directory.
BUFFER-NAME is the Claude buffer name.
CALLBACK is called on success."
  (let ((mcp-servers (claude-acp--mcp-server-config work-dir buffer-name)))
    (acp-send-request
     :client client
     :buffer buffer
     :request (acp-make-session-resume-request
               :session-id session-id
               :cwd work-dir
               :mcp-servers (or mcp-servers (vector)))
     :on-success (lambda (result)
                   (setq claude-acp--session-id
                         (or (map-elt result 'sessionId) session-id))
                   (setq claude-agent--session-info
                         (plist-put claude-agent--session-info :session-id
                                    claude-acp--session-id))
                   (when callback
                     (funcall callback claude-acp--session-id)))
     :on-failure (lambda (error)
                   (message "Claude ACP: Session resume failed: %S" error)
                   ;; Fallback: create new session
                   (claude-acp--create-session client buffer work-dir buffer-name
                                              callback)))))

(defun claude-acp--set-model (client buffer session-id model-id)
  "Set the model for SESSION-ID via CLIENT in BUFFER to MODEL-ID."
  (acp-send-request
   :client client
   :buffer buffer
   :request (acp-make-session-set-model-request
             :session-id session-id
             :model-id model-id)
   :on-success (lambda (_result)
                 (setq claude-agent--session-info
                       (plist-put claude-agent--session-info :model model-id)))
   :on-failure (lambda (error)
                 (message "Claude ACP: Set model failed: %S" error))))

;;;; Sending prompts

(defun claude-acp-send-prompt (text)
  "Send prompt TEXT to the ACP session in the current buffer."
  (unless claude-acp--client
    (error "No ACP client in this buffer"))
  (unless claude-acp--session-id
    (error "No ACP session active"))
  (setq claude-acp--turn-active t)
  (setq claude-acp--current-assistant-text "")
  (acp-send-request
   :client claude-acp--client
   :buffer (current-buffer)
   :request (acp-make-session-prompt-request
             :session-id claude-acp--session-id
             :prompt text)
   :on-success (lambda (result)
                 (setq claude-acp--turn-active nil)
                 ;; Process stop reason
                 (let ((stop-reason (map-elt result 'stopReason)))
                   (claude-acp--handle-turn-complete stop-reason result)))
   :on-failure (lambda (error)
                 (setq claude-acp--turn-active nil)
                 (claude-agent--dispatch-message "error"
                   `((type . "error")
                     (message . ,(format "Prompt failed: %S" error)))))))

(defun claude-acp-cancel ()
  "Cancel the current ACP session turn."
  (when (and claude-acp--client claude-acp--session-id)
    (acp-send-notification
     :client claude-acp--client
     :notification (acp-make-session-cancel-notification
                    :session-id claude-acp--session-id
                    :reason "User cancelled"))))

(defun claude-acp-shutdown ()
  "Shutdown the ACP client and clean up."
  (when claude-acp--client
    (condition-case nil
        (acp-shutdown :client claude-acp--client)
      (error nil))
    (setq claude-acp--client nil)
    (setq claude-acp--session-id nil)
    (setq claude-acp--initialized nil)
    (setq claude-acp--authenticated nil)))

;;;; Notification handling (session/update → dispatch to repl)

(defun claude-acp--on-notification (notification)
  "Handle incoming ACP NOTIFICATION, dispatching to the REPL renderer."
  (let ((method (map-elt notification 'method))
        (params (map-elt notification 'params)))
    (pcase method
      ("session/update"
       (claude-acp--handle-session-update params))
      (_
       (message "Claude ACP: Unknown notification: %s" method)))))

(defun claude-acp--handle-session-update (params)
  "Handle a session/update notification with PARAMS.
Maps ACP update types to existing claude-agent-repl dispatch messages."
  (let* ((session-update (map-elt params 'sessionUpdate))
         (update-type (map-elt session-update 'type)))
    (pcase update-type
      ;; Agent message chunk - assistant text streaming
      ("agent_message_chunk"
       (let ((text (map-elt session-update 'text)))
         (when text
           ;; Start assistant block if first chunk
           (when (string-empty-p claude-acp--current-assistant-text)
             (claude-agent--dispatch-message "assistant_start"
               '((type . "assistant_start"))))
           (setq claude-acp--current-assistant-text
                 (concat claude-acp--current-assistant-text text))
           (claude-agent--dispatch-message "assistant_text"
             `((type . "assistant_text") (text . ,text))))))

      ;; Agent thought chunk - thinking/reasoning
      ("agent_thought_chunk"
       (let ((text (map-elt session-update 'text)))
         (when text
           (claude-agent--dispatch-message "thinking"
             `((type . "thinking") (status . "Thinking..."))))))

      ;; Tool call announcement
      ("tool_call"
       ;; End any in-progress assistant message
       (claude-acp--maybe-end-assistant-message)
       (let* ((tool-call (map-elt session-update 'toolCall))
              (name (map-elt tool-call 'name))
              (input (map-elt tool-call 'input))
              (tool-use-id (map-elt tool-call 'id)))
         (claude-agent--dispatch-message "tool_call"
           `((type . "tool_call")
             (name . ,name)
             (input . ,input)
             (tool_use_id . ,tool-use-id)))))

      ;; Tool call status update
      ("tool_call_update"
       (let* ((tool-call (map-elt session-update 'toolCall))
              (tool-use-id (map-elt tool-call 'id))
              (status (map-elt tool-call 'status))
              (content (map-elt tool-call 'content)))
         (pcase status
           ("completed"
            (claude-agent--dispatch-message "tool_result"
              `((type . "tool_result")
                (tool_use_id . ,tool-use-id)
                (content . ,(or content ""))))
            (claude-agent--dispatch-message "tool_end"
              `((type . "tool_end")
                (tool_use_id . ,tool-use-id))))
           ("cancelled"
            (claude-agent--dispatch-message "tool_result"
              `((type . "tool_result")
                (tool_use_id . ,tool-use-id)
                (content . "Cancelled")))
            (claude-agent--dispatch-message "tool_end"
              `((type . "tool_end")
                (tool_use_id . ,tool-use-id))))
           ;; "pending" and "in_progress" - just update thinking status
           (_
            (claude-agent--dispatch-message "thinking"
              `((type . "thinking")
                (status . ,(format "Running tool: %s"
                                   (or (map-elt tool-call 'name) "...")))))))))

      ;; Plan update
      ("plan"
       (let ((steps (map-elt session-update 'steps)))
         (when steps
           (let ((todos (mapcar (lambda (step)
                                  `((content . ,(map-elt step 'description))
                                    (status . ,(map-elt step 'status))))
                                (append steps nil))))
             (claude-agent--dispatch-message "todo_update"
               `((type . "todo_update") (todos . ,todos)))))))

      ;; Cost/usage update
      ("usage"
       (let ((cost (map-elt session-update 'costUsd))
             (input-tokens (map-elt session-update 'inputTokens))
             (output-tokens (map-elt session-update 'outputTokens)))
         (when cost
           (claude-agent--dispatch-message "result"
             `((type . "result") (cost_usd . ,cost))))
         (when (or input-tokens output-tokens)
           (claude-agent--dispatch-message "progress"
             `((type . "progress")
               ,@(when input-tokens `((input_tokens . ,input-tokens)))
               ,@(when output-tokens `((output_tokens . ,output-tokens))))))))

      ;; Default - log unhandled update types
      (_
       (when update-type
         (message "Claude ACP: Unhandled session/update type: %s" update-type))))))

(defun claude-acp--maybe-end-assistant-message ()
  "End the current assistant message block if one is in progress."
  (unless (string-empty-p claude-acp--current-assistant-text)
    (claude-agent--dispatch-message "assistant_end"
      '((type . "assistant_end")))
    (setq claude-acp--current-assistant-text "")))

(defun claude-acp--handle-turn-complete (stop-reason result)
  "Handle completion of a prompt turn with STOP-REASON and RESULT."
  ;; End any in-progress assistant message
  (claude-acp--maybe-end-assistant-message)
  ;; Update cost if present in result
  (when-let ((cost (map-elt result 'costUsd)))
    (setq claude-agent--session-info
          (plist-put claude-agent--session-info :cost cost)))
  ;; Dispatch appropriate messages
  (claude-agent--dispatch-message "result"
    `((type . "result")
      ,@(when (map-elt result 'costUsd)
          `((cost_usd . ,(map-elt result 'costUsd))))))
  (claude-agent--dispatch-message "ready"
    '((type . "ready")))
  ;; Handle special stop reasons
  (pcase stop-reason
    ("max_tokens"
     (claude-agent--dispatch-message "session_message_start"
       '((type . "session_message_start")))
     (claude-agent--dispatch-message "session_message_text"
       '((type . "session_message_text")
         (text . "⚠ Response truncated: max tokens reached")))
     (claude-agent--dispatch-message "session_message_end"
       '((type . "session_message_end"))))
    ("refusal"
     (claude-agent--dispatch-message "error"
       '((type . "error")
         (message . "Agent refused to continue"))))))

;;;; Request handling (permission, fs operations)

(defun claude-acp--on-request (request)
  "Handle incoming ACP REQUEST."
  (let ((method (map-elt request 'method))
        (request-id (map-elt request 'id))
        (params (map-elt request 'params)))
    (pcase method
      ("session/request_permission"
       (claude-acp--handle-permission-request request-id params))
      ("fs/read_text_file"
       (claude-acp--handle-read-file request-id params))
      ("fs/write_text_file"
       (claude-acp--handle-write-file request-id params))
      (_
       (message "Claude ACP: Unhandled request method: %s" method)
       ;; Send error response for unhandled methods
       (acp-send-response
        :client claude-acp--client
        :response `((:request-id . ,request-id)
                    (:error . ((code . -32601)
                               (message . ,(format "Method not supported: %s" method))))))))))

;;; Permission handling

(defun claude-acp--handle-permission-request (request-id params)
  "Handle a session/request_permission request.
REQUEST-ID is the JSON-RPC request ID.
PARAMS contains the permission request details."
  (let* ((tool-name (or (map-elt params 'toolName)
                        (map-elt params 'tool_name)
                        "unknown"))
         (tool-input (or (map-elt params 'toolInput)
                         (map-elt params 'input)
                         nil))
         (options (map-elt params 'options))
         ;; Build data in the format expected by the existing permission system
         (permission-data `((tool_name . ,tool-name)
                            (tool_input . ,tool-input)
                            (tool_use_id . ,(format "acp-%s" request-id)))))
    ;; Store request-id for when permission response comes back
    (setq claude-acp--pending-permission-request-id request-id)
    ;; Dispatch through existing permission system
    ;; First check policy rules
    (let ((decision (claude-agent-permission-handle-request tool-name tool-input)))
      (pcase decision
        ;; Auto-allow
        (`(:allow . ,props)
         (let* ((scope (plist-get props :scope))
                (option-id (claude-acp--scope-to-option-id scope options)))
           (claude-acp--send-permission-response request-id option-id nil)))

        ;; Auto-deny
        (`(:deny . ,_props)
         (claude-acp--send-permission-response request-id nil t))

        ;; No policy match - show interactive prompt
        (_
         ;; Dispatch to the existing REPL permission UI
         (claude-agent--dispatch-message "permission_request" permission-data))))))

(defun claude-acp--scope-to-option-id (scope options)
  "Map permission SCOPE to an ACP option ID from OPTIONS.
OPTIONS is a vector of option objects with `id' fields."
  (let ((target-id (pcase scope
                     (:once "allow_once")
                     (:session "allow_session")
                     (:always "allow_always")
                     (_ "allow_once"))))
    ;; Try to find matching option, fall back to first option
    (if (and options (> (length options) 0))
        (or (cl-loop for opt across options
                     when (string= (map-elt opt 'id) target-id)
                     return (map-elt opt 'id))
            ;; Fall back to first available option
            (map-elt (aref options 0) 'id))
      target-id)))

(defun claude-acp--send-permission-response (request-id option-id cancelled)
  "Send a permission response for REQUEST-ID.
OPTION-ID is the selected option, or nil if CANCELLED."
  (when claude-acp--client
    (acp-send-response
     :client claude-acp--client
     :response (acp-make-session-request-permission-response
                :request-id request-id
                :option-id option-id
                :cancelled cancelled))
    (setq claude-acp--pending-permission-request-id nil)))

(defun claude-acp-respond-to-permission (action)
  "Respond to the pending ACP permission request with ACTION.
ACTION is one of: \"allow_once\", \"allow_session\", \"allow_always\", \"deny\".
This is called from the REPL permission UI."
  (when claude-acp--pending-permission-request-id
    (if (string= action "deny")
        (claude-acp--send-permission-response
         claude-acp--pending-permission-request-id nil t)
      (claude-acp--send-permission-response
       claude-acp--pending-permission-request-id action nil))))

;;; File system request handling

(defun claude-acp--handle-read-file (request-id params)
  "Handle fs/read_text_file request.
REQUEST-ID is the JSON-RPC request ID.
PARAMS contains path and optional line/limit."
  (condition-case err
      (let* ((path (map-elt params 'path))
             (line (or (map-elt params 'line) 1))
             (limit (map-elt params 'limit))
             (existing-buffer (find-buffer-visiting path))
             (content (if existing-buffer
                         ;; Read from open buffer (includes unsaved changes)
                         (with-current-buffer existing-buffer
                           (claude-acp--extract-buffer-text line limit))
                       ;; Read from file
                       (if (file-exists-p path)
                           (with-temp-buffer
                             (insert-file-contents path)
                             (claude-acp--extract-buffer-text line limit))
                         (signal 'file-missing (list path))))))
        (acp-send-response
         :client claude-acp--client
         :response (acp-make-fs-read-text-file-response
                    :request-id request-id
                    :content content)))
    (file-missing
     (acp-send-response
      :client claude-acp--client
      :response (acp-make-fs-read-text-file-response
                 :request-id request-id
                 :error (acp-make-error
                         :code -32602
                         :message (format "File not found: %s" (cadr err))))))
    (error
     (acp-send-response
      :client claude-acp--client
      :response (acp-make-fs-read-text-file-response
                 :request-id request-id
                 :error (acp-make-error
                         :code -32603
                         :message (format "Read error: %s"
                                          (error-message-string err))))))))

(defun claude-acp--extract-buffer-text (line limit)
  "Extract text from current buffer starting at LINE with optional LIMIT."
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- (max 1 line)))
    (let ((start (point))
          (end (if limit
                   (progn (forward-line limit) (point))
                 (point-max))))
      (buffer-substring-no-properties start end))))

(defun claude-acp--handle-write-file (request-id params)
  "Handle fs/write_text_file request.
REQUEST-ID is the JSON-RPC request ID.
PARAMS contains path and content."
  (condition-case err
      (let* ((path (map-elt params 'path))
             (content (map-elt params 'content))
             (dir (file-name-directory path))
             (buffer (find-buffer-visiting path)))
        ;; Ensure directory exists
        (when (and dir (not (file-exists-p dir)))
          (make-directory dir t))
        (if buffer
            ;; Write to existing buffer
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (save-restriction
                  (widen)
                  (erase-buffer)
                  (insert content)
                  (basic-save-buffer))))
          ;; Write directly to file
          (with-temp-file path
            (insert content)))
        (acp-send-response
         :client claude-acp--client
         :response (acp-make-fs-write-text-file-response
                    :request-id request-id)))
    (error
     (acp-send-response
      :client claude-acp--client
      :response (acp-make-fs-write-text-file-response
                 :request-id request-id
                 :error (acp-make-error
                         :code -32603
                         :message (format "Write error: %s"
                                          (error-message-string err))))))))

;;;; Error handling

(defun claude-acp--on-error (error)
  "Handle ACP agent ERROR."
  (let ((code (map-elt error 'code))
        (message-text (map-elt error 'message)))
    (claude-agent--dispatch-message "error"
      `((type . "error")
        (error_type . "api_error")
        (message . ,(or message-text (format "ACP error (code %s)" code)))))))

;;;; High-level session startup

(defun claude-acp-start-session (buffer work-dir buffer-name
                                        &optional resume-session-id system-prompt model)
  "Start a full ACP session in BUFFER for WORK-DIR.
BUFFER-NAME is the Claude buffer display name.
RESUME-SESSION-ID if non-nil, resume that session.
SYSTEM-PROMPT is optional additional system prompt text.
MODEL is optional model identifier."
  (with-current-buffer buffer
    ;; Create client
    (setq claude-acp--client (claude-acp--create-client buffer))
    ;; Subscribe to events
    (acp-subscribe-to-notifications
     :client claude-acp--client
     :buffer buffer
     :on-notification #'claude-acp--on-notification)
    (acp-subscribe-to-requests
     :client claude-acp--client
     :buffer buffer
     :on-request #'claude-acp--on-request)
    (acp-subscribe-to-errors
     :client claude-acp--client
     :buffer buffer
     :on-error #'claude-acp--on-error)
    ;; Initialize → Authenticate → Create/Resume session
    (claude-acp--initialize
     claude-acp--client buffer
     (lambda ()
       (claude-acp--authenticate
        claude-acp--client buffer
        (lambda ()
          (if resume-session-id
              (claude-acp--resume-session
               claude-acp--client buffer
               resume-session-id work-dir buffer-name
               (lambda (_session-id)
                 (claude-agent--dispatch-message "ready"
                   '((type . "ready")))))
            (claude-acp--create-session
             claude-acp--client buffer work-dir buffer-name
             (lambda (_session-id)
               (claude-agent--dispatch-message "ready"
                 '((type . "ready"))))
             system-prompt model))))))))

;;;; Integration bridge - compatibility layer for claude-mcp-process.el

(defun claude-acp--process-live-p ()
  "Return non-nil if the ACP client process is alive.
Drop-in replacement for checking `claude-agent--process'."
  (and claude-acp--client
       (acp--client-started-p claude-acp--client)))

(provide 'claude-acp)
;;; claude-acp.el ends here
