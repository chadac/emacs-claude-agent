;;; claude-mcp-process.el --- Process & buffer management for Claude MCP -*- lexical-binding: t; -*-

;; This file is part of Claudemacs.

;;; Commentary:

;; Process and buffer management functions for Claude MCP sessions.
;; Handles spawning agent processes, buffer lifecycle, and message sending.

;;; Code:

(require 'claude-agent-repl)
(require 'claude-mcp)  ; Will be renamed from claude-ai.el
(require 'claude-sessions)

;; Declare functions from claude-mcp (MCP config generation)
(declare-function claude--generate-mcp-config "claude-mcp")
(declare-function claude-mcp-setup-claude-environment "claude-mcp")

;; Declare variable from Claude.el
(defvar claude--package-dir)

;;;; Customization

(defcustom claude-mcp-switch-to-buffer-on-create t
  "Whether to switch to the Claude buffer when creating a new session.
If non-nil, automatically switch to the Claude buffer after starting.
If nil, create the session but don't switch focus to it."
  :type 'boolean
  :group 'claude-agent)

(defcustom claude-mcp-switch-to-buffer-on-toggle t
  "Whether to switch to the Claude buffer when toggling to show it.
If non-nil, switch to the Claude buffer when toggling from hidden to visible.
If nil, show the buffer but don't switch focus to it."
  :type 'boolean
  :group 'claude-agent)

(defcustom claude-mcp-notify-on-await t
  "Whether to show a system notification when Claude is awaiting the user.
When non-nil, display an OS notification popup when Claude completes a task.
When nil, no notification is shown (silent operation)."
  :type 'boolean
  :group 'claude-agent)

(defcustom claude-mcp-notification-sound-mac "Submarine"
  "The sound to use when displaying system notifications on macOS.

System sounds include: `Basso', `Blow', `Bottle', `Frog', `Funk',
`Glass', `Hero', `Morse', `Ping', `Pop', `Purr', `Sosumi', `Submarine',
`Tink'. Or put more sounds in the `/Library/Sound' folder and use those."
  :type 'string
  :group 'claude-agent)

(defcustom claude-mcp-notification-auto-dismiss-linux t
  "Whether to auto-dismiss notifications on Linux (don't persist to system tray).
When non-nil, notifications will automatically disappear and not stay in the tray.
When nil, notifications will persist in the system tray according to system defaults.
This setting only affects Linux systems using notify-send."
  :type 'boolean
  :group 'claude-agent)

(defcustom claude-mcp-notification-sound-linux "bell"
  "The sound to use when displaying system notifications on Linux.
Uses canberra-gtk-play if available.  Common sound IDs include:
`message-new-instant', `bell', `dialog-error', `dialog-warning'.
When empty string, no sound is played."
  :type 'string
  :group 'claude-agent)

(defcustom claude-mcp-startup-hook nil
  "Hook run after a claude-mcp session has finished starting up.
This hook is called after the agent buffer is initialized and the
process is spawned. The hook functions are executed with the
claude buffer as the current buffer."
  :type 'hook
  :group 'claude-agent)

(defcustom claude-mcp-system-prompt-file "claude-agent-prompt.md"
  "Path to a file containing additional system prompt instructions for Claude.
If set, the contents of this file are appended to Claude's system prompt.
This is useful for project-specific instructions or pair programming guidance.
The file path can be absolute or relative to the package directory.
Default is \"claude-agent-prompt.md\" which includes Emacs integration guidance."
  :type '(choice (const :tag "None" nil)
                 (file :tag "File path"))
  :group 'claude-agent)

;;;; Buffer-local Variables

(defvar-local claude-mcp--cwd nil
  "Buffer-local variable storing the current working directory for this Claude session.")

(defvar-local claude-mcp--session-id nil
  "Buffer-local variable storing the session ID (UUID) for this Claude session.
Used to resume the correct session when restarting agents with custom names.")

;;;; Utility Functions

(defun claude-mcp--session-id ()
  "Return an identifier for the current Claude MCP session.
If a workspace is active (checking various workspace packages),
use its name, otherwise fall back to the project root."
  (cond
   ;; Doom Emacs workspace
   ((and (fboundp '+workspace-current-name)
         (let ((ws (+workspace-current-name)))
           (and ws (stringp ws) (not (string-empty-p ws)))))
    (+workspace-current-name))
   ;; Perspective mode
   ((and (and (fboundp 'safe-persp-name) (fboundp 'get-current-persp))
         (let ((ws (safe-persp-name (get-current-persp))))
           (and ws (stringp ws) (not (string-empty-p ws)))))
    (safe-persp-name (get-current-persp)))
   ;; Fall back to project root
   (t (file-truename (or (vc-git-root default-directory) default-directory)))))

(defun claude-mcp--get-buffer-name ()
  "Generate the claude buffer name based on workspace session ID."
  (let ((session-id (claude-mcp--session-id)))
    (if (file-directory-p session-id)
        (format "*claude:%s*"
                (file-name-nondirectory (directory-file-name session-id)))
      (format "*claude:%s*" session-id))))

(defun claude-mcp--get-buffer ()
  "Return existing claude buffer for current session."
  (get-buffer (claude-mcp--get-buffer-name)))

(defun claude-mcp--is-mcp-buffer-p (&optional buffer)
  "Return t if BUFFER (or current buffer) is a claude-mcp buffer."
  (let ((buf (or buffer (current-buffer))))
    (and (buffer-live-p buf)
         (string-match-p "^\\*claude:" (buffer-name buf)))))

(defun claude-mcp--parse-buffer-name (buffer-name)
  "Parse claude buffer name into components.
Returns cons cell (name . agent-name) or (name . nil)."
  (when (string-match "^\\*claude:\\([^:*]+\\)\\(?::\\([^*]+\\)\\)?\\*$" buffer-name)
    (let ((name (match-string 1 buffer-name))
          (agent (match-string 2 buffer-name)))
      (cons name agent))))

(defun claude-mcp--switch-to-buffer ()
  "Switch to the claude buffer for current session.
Returns t if switched successfully, nil if no buffer exists."
  (if-let* ((buffer (claude-mcp--get-buffer)))
      (progn
        (with-current-buffer buffer
          (unless (and claude-agent--process (process-live-p claude-agent--process))
            (error "Claude session exists but process is not running. Please kill *claude:...* buffer and re-start")))
        ;; we have a running agent process
        (display-buffer buffer)
        (select-window (get-buffer-window buffer))
        t)
    nil))

(defun claude-mcp--validate-process ()
  "Validate that the Claude MCP process is alive and running."
  (let ((buffer (claude-mcp--get-buffer)))
    (unless buffer
      (error "No Claude MCP session is active"))
    (with-current-buffer buffer
      (unless (and claude-agent--process
                   (process-live-p claude-agent--process))
        (error "Claude agent process is not running"))))
  t)

(defun claude-mcp--system-notification (message &optional title)
  "Show a system notification with MESSAGE and optional TITLE.
This works across macOS, Linux, and Windows platforms."
  (let ((title (or title "Claude"))
        (message (or message "Claude is finished and awaiting your input")))
    (cond
     ;; macOS
     ((eq system-type 'darwin)
      (call-process "osascript" nil nil nil
                    "-e" (format "display notification \"%s\" with title \"%s\" sound name \"%s\""
                                message title claude-mcp-notification-sound-mac)))
     ;; Linux with notify-send and canberra-gtk-play
     ((and (eq system-type 'gnu/linux)
           (executable-find "notify-send"))
      (let ((args (if claude-mcp-notification-auto-dismiss-linux
                      (list "--hint=int:transient:1" title message)
                    (list title message))))
        (apply #'call-process "notify-send" nil nil nil args))
      (when (and (not (string-empty-p claude-mcp-notification-sound-linux))
                 (executable-find "canberra-gtk-play"))
        (call-process "canberra-gtk-play" nil nil nil
                      "--id" claude-mcp-notification-sound-linux)))
     ;; Linux with kdialog (KDE)
     ((and (eq system-type 'gnu/linux)
           (executable-find "kdialog"))
      (call-process "kdialog" nil nil nil "--passivepopup"
                    (format "%s: %s" title message) "3"))
     ;; Windows with PowerShell
     ((eq system-type 'windows-nt)
      (call-process "powershell" nil nil nil
                    "-Command"
                    (format "[System.Reflection.Assembly]::LoadWithPartialName('System.Windows.Forms'); [System.Windows.Forms.MessageBox]::Show('%s', '%s')"
                            message title)))
     ;; Fallback: show in Emacs message area
     (t (message "%s: %s" title message)))))

(defun claude-mcp--ready-handler (buffer)
  "Called when [READY] marker received, triggers notification if enabled."
  (when (and claude-mcp-notify-on-await (buffer-live-p buffer))
    (claude-mcp--system-notification
     "Claude finished and is awaiting your input")))

(defun claude-mcp--scroll-to-bottom ()
  "Scroll the claude buffer to bottom without switching to it."
  (interactive)
  (when-let* ((claude-buffer (claude-mcp--get-buffer))
              (claude-window (get-buffer-window claude-buffer)))
    (with-current-buffer claude-buffer
      (goto-char (point-max))
      (set-window-point claude-window (point-max)))))

(defun claude-mcp--scroll-to-top ()
  "Scroll the claude buffer to top without switching to it."
  (interactive)
  (when-let* ((claude-buffer (claude-mcp--get-buffer))
              (claude-window (get-buffer-window claude-buffer)))
    (with-current-buffer claude-buffer
      (goto-char (point-min))
      (set-window-point claude-window (point-min)))))

;;;; Process Management

(defun claude-agent--start-process-with-args (work-dir buffer args)
  "Start agent process with custom ARGS for BUFFER in WORK-DIR.
This is a wrapper that allows passing custom arguments to the Python wrapper
(for MCP config, resume, etc.)."
  (let* ((this-dir (or claude--package-dir
                       (when-let ((f (or load-file-name buffer-file-name)))
                         (file-name-directory f))
                       (when-let ((f (locate-library "Claude")))
                         (file-name-directory f))))
         (agent-dir (when this-dir
                      (expand-file-name "claude_agent" this-dir)))
         (process-connection-type t)
         (process-environment (cons "PYTHONUNBUFFERED=1" process-environment))
         (proc (apply #'start-process
                      "claude-agent"
                      buffer
                      "uv"
                      "run" "--directory" agent-dir
                      "python" "-m" "claude_agent"
                      args)))
    (set-process-coding-system proc 'utf-8 'utf-8)
    (set-process-filter proc #'claude-agent--process-filter)
    (set-process-sentinel proc #'claude-agent--process-sentinel)
    proc))

(defun claude-mcp--start (work-dir &rest args)
  "Start Claude agent in WORK-DIR with ARGS.
WORK-DIR can be either:
  - A string: \"/path\" creates buffer *claude:name*
  - A list: '(\"/path\" \"agent-name\") creates *claude:name:agent-name*"
  ;; Set up environment variables BEFORE spawning the Claude process
  (claude-mcp-setup-claude-environment)

  ;; Parse work-dir - it can be a string or (dir agent-name) list
  (let* ((dir-string (if (listp work-dir) (car work-dir) work-dir))
         (agent-name (when (listp work-dir) (cadr work-dir)))
         (expanded-dir (expand-file-name dir-string))
         ;; NEW: Use *claude:name* format
         (buffer-name (if agent-name
                         (format "*claude:%s:%s*"
                                 (file-name-nondirectory (directory-file-name expanded-dir))
                                 agent-name)
                       (format "*claude:%s*"
                               (file-name-nondirectory (directory-file-name expanded-dir)))))
         (short-name (file-name-nondirectory (directory-file-name expanded-dir)))
         (buf (get-buffer-create buffer-name))
         (mcp-config (claude--generate-mcp-config expanded-dir buffer-name))
         (agent-args (list "--work-dir" expanded-dir
                          "--mcp-config" mcp-config
                          "--log-file" (expand-file-name "claude-agent.log" expanded-dir)))
         ;; Resolve system prompt file path
         (prompt-file (when claude-mcp-system-prompt-file
                        (if (file-name-absolute-p claude-mcp-system-prompt-file)
                            claude-mcp-system-prompt-file
                          (expand-file-name claude-mcp-system-prompt-file
                                           (or claude--package-dir default-directory))))))
    ;; Add --system-prompt-file if configured
    (when (and prompt-file (file-exists-p prompt-file))
      (setq agent-args (append agent-args
                               (list "--system-prompt-file" prompt-file))))
    ;; Add --resume or --continue based on args
    (when (member "--resume" args)
      (let ((session-id (cadr (member "--resume" args))))
        (setq agent-args (append agent-args (list "--resume" session-id)))))
    (when (member "--continue" args)
      (setq agent-args (append agent-args (list "--continue"))))

    ;; Initialize buffer with claude-agent-mode
    (with-current-buffer buf
      (claude-agent-mode)
      (claude-agent--init-buffer short-name)
      (setq-local claude-mcp--cwd expanded-dir)
      ;; Start the agent process
      (let ((proc (claude-agent--start-process-with-args expanded-dir buf agent-args)))
        (setq claude-agent--process proc))
      ;; Run startup hook
      (run-hooks 'claude-mcp-startup-hook))

    ;; Display and return buffer
    (when claude-mcp-switch-to-buffer-on-create
      (pop-to-buffer buf))
    buf))

(defun claude-mcp--send-message (message &optional no-return no-switch clear-first)
  "Send MESSAGE to the active Claude MCP session.
If NO-RETURN is non-nil, don't send the message (just insert into input area).
If NO-SWITCH is non-nil, don't switch to the Claude buffer.
If CLEAR-FIRST is non-nil, clear the input area before inserting."
  (claude-mcp--validate-process)
  (let ((buffer (claude-mcp--get-buffer)))
    (with-current-buffer buffer
      (when clear-first
        ;; Clear input area (agent buffer specific)
        (let ((inhibit-read-only t))
          (delete-region claude-agent--input-start-marker (point-max))))

      (if no-return
          ;; Insert without sending (add to input area)
          (progn
            (goto-char (point-max))
            (let ((inhibit-read-only t))
              (insert message)))
        ;; Send message with [INPUT] framing
        (process-send-string claude-agent--process
                           (format "[INPUT]\n%s\n[/INPUT]\n" message))))

    (unless no-switch
      (claude-mcp--switch-to-buffer))))

(defun claude-mcp--get-most-recent-session-id (work-dir)
  "Get the most recent session ID for WORK-DIR.
Returns the UUID of the most recently modified session file, or nil if none found."
  (let* ((expanded-dir (expand-file-name work-dir))
         ;; Convert /home/user/.path/to/dir to -home-user--path-to-dir
         ;; Claude's format: replace / with -, replace . with -
         (slug-with-slashes (replace-regexp-in-string "/" "-" expanded-dir))
         (project-slug (replace-regexp-in-string "\\." "-" slug-with-slashes))
         (sessions-dir (expand-file-name project-slug "~/.claude/projects/")))
    (when (file-directory-p sessions-dir)
      (let* ((files (directory-files sessions-dir t "\\.jsonl$"))
             (sorted-files (sort files
                                 (lambda (a b)
                                   (time-less-p (nth 5 (file-attributes b))
                                               (nth 5 (file-attributes a)))))))
        (when sorted-files
          ;; Extract UUID from filename (remove path and .jsonl extension)
          (file-name-sans-extension (file-name-nondirectory (car sorted-files))))))))

(defun claude-mcp--send-message-when-ready (work-dir message delay &optional attempt target-buffer-name)
  "Send MESSAGE to Claude after DELAY seconds.
WORK-DIR identifies the session (can be nil if TARGET-BUFFER-NAME is provided).
DELAY is the number of seconds to wait before sending.
TARGET-BUFFER-NAME is the exact buffer name to use (optional)."
  (let ((buffer-name (or target-buffer-name
                         (when work-dir (format "*claude:%s*"
                                                (file-name-nondirectory
                                                 (directory-file-name work-dir)))))))
    (unless buffer-name
      (error "claude-mcp--send-message-when-ready: Cannot determine buffer name. work-dir=%S target-buffer-name=%S"
             work-dir target-buffer-name))
    (run-with-timer
     delay nil
     (lambda (buf-name msg)
       (let ((buffer (get-buffer buf-name)))
         (if (and buffer
                  (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (and claude-agent--process (process-live-p claude-agent--process))))
             ;; Send the message
             (with-current-buffer buffer
               (process-send-string claude-agent--process
                                  (format "[INPUT]\n%s\n[/INPUT]\n" msg))
               (message "Continuation message sent to Claude"))
           (message "Warning: Buffer %s not ready to receive message" buf-name))))
     buffer-name message)))

;;;; Interactive Commands

(defun claude-mcp--run-with-args (&optional arg &rest args)
  "Start Claude with ARGS or switch to existing session.
With prefix ARG, prompt for the project directory."
  (let* ((explicit-dir (when arg (read-directory-name "Project directory: ")))
         (work-dir (or explicit-dir (or (vc-git-root default-directory) default-directory))))
    (unless (claude-mcp--switch-to-buffer)
      (apply #'claude-mcp--start work-dir args))))

;;;###autoload
(defun claude-mcp-run (&optional arg)
  "Start Claude or switch to existing session.
With prefix ARG, prompt for the project directory."
  (interactive "P")
  (claude-mcp--run-with-args arg))

;;;###autoload
(defun claude-mcp-resume (&optional arg)
  "Start Claude with resume or switch to existing session.
With prefix ARG, prompt for the project directory."
  (interactive "P")
  (let ((claude-mcp-switch-to-buffer-on-create t))
    (claude-mcp--run-with-args arg "--resume")))

;;;###autoload
(defun claude-mcp-kill ()
  "Kill Claude MCP process and close its window."
  (interactive)
  (if-let* ((claude-buffer (claude-mcp--get-buffer)))
      (progn
        (with-current-buffer claude-buffer
          (when (and claude-agent--process (process-live-p claude-agent--process))
            (delete-process claude-agent--process))
          (kill-buffer claude-buffer))
        (message "Claude MCP session killed"))
    (error "There is no Claude MCP session in this workspace or project")))

;;;###autoload
(defun claude-mcp-spawn-agent (directory &optional agent-name &rest extra-args)
  "Spawn a new claude agent in DIRECTORY with optional AGENT-NAME.
When called interactively, uses current directory and prompts for agent identifier.
If AGENT-NAME is nil or empty, buffer will be named *claude:name*.
If provided, buffer will be named *claude:name:agent-name*.
EXTRA-ARGS are additional command-line arguments to pass to Claude.
Returns the buffer name."
  (interactive
   (list (if (claude-mcp--is-mcp-buffer-p)
            (or claude-mcp--cwd default-directory)
          (or (vc-git-root default-directory) default-directory))
         (let ((input (read-string "Agent identifier (leave empty for primary): " nil nil "")))
           (if (string-empty-p input) nil input))))
  (let* ((expanded-dir (expand-file-name directory))
         (buffer-name (if agent-name
                         (format "*claude:%s:%s*"
                                 (file-name-nondirectory (directory-file-name expanded-dir))
                                 agent-name)
                       (format "*claude:%s*"
                               (file-name-nondirectory (directory-file-name expanded-dir)))))
         (work-dir-arg (if agent-name
                          (list directory agent-name)
                        directory)))
    ;; Check if buffer already exists
    (when (get-buffer buffer-name)
      (error "Agent already exists with buffer name: %s" buffer-name))

    ;; Check directory exists
    (unless (file-directory-p expanded-dir)
      (error "Directory does not exist: %s" expanded-dir))

    ;; Spawn the agent with any extra args
    (apply #'claude-mcp--start work-dir-arg extra-args)

    (when (called-interactively-p 'interactive)
      (message "Spawned agent: %s" buffer-name))

    buffer-name))

;;;###autoload
(defun claude-mcp-toggle-buffer ()
  "Toggle Claude buffer visibility.
Hide if current, focus if visible elsewhere, show if hidden."
  (interactive)
  (let ((claude-buffer (claude-mcp--get-buffer)))
    (cond
     ;; Case 1: No Claude session exists
     ((not (ignore-errors (claude-mcp--validate-process)))
      (error "No Claude MCP session is active"))

     ;; Case 2: Current buffer IS the Claude buffer
     ((eq (current-buffer) claude-buffer)
      ;; Hide using quit-window (automatically handles window vs buffer logic)
      (quit-window))

     ;; Case 3: Claude buffer visible in another window
     ((get-buffer-window claude-buffer)
      ;; Quit that window (automatically handles created vs reused)
      (with-selected-window (get-buffer-window claude-buffer)
        (quit-window)))

     ;; Case 4: Claude buffer exists but not visible
     (t
      ;; Show Claude buffer
      (if claude-mcp-switch-to-buffer-on-toggle
          (claude-mcp--switch-to-buffer)
        (progn
          (display-buffer claude-buffer)
          ;; Move to bottom without switching focus
          (with-current-buffer claude-buffer
            (set-window-point (get-buffer-window claude-buffer) (point-max)))))))))

;; Hook up ready handler
(add-hook 'claude-agent-ready-hook #'claude-mcp--ready-handler)

(provide 'claude-mcp-process)
;;; claude-mcp-process.el ends here
