;;; inf-powershell.el --- Interact with inferior powershell process  -*- lexical-binding: t; -*-

;;; Commentary:

;; Interact with inferior powershell process.  See powershell.el.

;;; Code:

(eval-when-compile (require 'thingatpt))
(require 'shell)

(defgroup inf-powershell nil
  "Run inferior powershell process."
  :group 'powershell
  :prefix "inf-powershell-")

(defcustom inf-powershell-log-level 3
  "The current log level for powershell internal operations.
0 = NONE, 1 = Info, 2 = VERBOSE, 3 = DEBUG."
  :group 'inf-powershell
  :type 'integer)

;; Note: There are no explicit references to the variable
;; `explicit-powershell.exe-args'.  It is used implicitly by M-x shell
;; when the shell is `powershell.exe'.  See
;; http://blogs.msdn.com/b/dotnetinterop/archive/2008/04/10/run-powershell-as-a-shell-within-emacs.aspx
;; for details.
(defcustom explicit-powershell.exe-args '("-Command" "-" )
  "Args passed to inferior shell by \\[shell], if the shell is powershell.exe.
Value is a list of strings, which may be nil."
  :type '(repeat (string :tag "Argument"))
  :group 'inf-powershell)

(defcustom inf-powershell-exe
  (or (executable-find "powershell")
      (expand-file-name "system32/windowspowershell/v1.0/powershell.exe"
                        (getenv "windir")))
  "A string, providing the location of the Powershell.exe."
  :group 'inf-powershell
  :type 'string)

;; ------------------------------------------------------------
;;* Buffer local variables

(defvar-local inf-powershell-shell-process nil
  "Inferior shell process for interaction.")

(defvar inf-powershell-squish-results-of-silent-commands t
  "The function `inf-powershell-invoke-command-silently' returns the results
of a command in a string.  PowerShell by default, inserts newlines when
the output exceeds the configured width of the powershell virtual
window. In some cases callers might want to get the results with the
newlines and formatting removed. Set this to true, to do that.
Becomes buffer-local.")

(defvar inf-powershell-prompt-regex  "PS [^#$%>]+> "
  "Regexp to match the powershell prompt. Becomes buffer local when shell starts.
Powershell uses this regex to determine when a command has
completed.  Therefore, you need to set this appropriately if you
explicitly change the prompt function in powershell.  Any value
should include a trailing space, if the powershell prompt uses a
trailing space, but should not include a trailing newline.

The default value will match the default PowerShell prompt.")

(defvar inf-powershell-command-timeout-seconds 12
  "The timeout for a powershell command. Powershell will wait this long
 before giving up. Becomes buffer local when shell starts.")

(defvar inf-powershell--max-window-width  0
  "The maximum width of a powershell window.
You shouldn't need to ever set this.  It gets set automatically,
once, when the powershell starts up. Becomes buffer local when shell starts.")

(defvar inf-powershell--need-rawui-resize nil
  "No need to fuss with this.  It's intended for internal use
only.  It gets set when powershell needs to be informed that
emacs has resized its window. Becomes buffer local when shell starts.")

(defvar inf-powershell-command-reply nil
  "The reply of powershell commands. This is retained for housekeeping
 purposes. Becomes buffer local when shell starts.")

;; ------------------------------------------------------------
;;* Resizing Buffer

(defconst inf-powershell--find-max-window-width-command
  (concat
   "function _Emacs_GetMaxPhsWindowSize {\n"
   "  $rawui = (Get-Host).UI.RawUI\n"
   "  $mpws_exists = ($rawui | Get-Member | ? "
   "{$_.Name -eq 'MaxPhysicalWindowSize'})\n"
   "  if ($mpws_exists -eq $null) {\n"
   "    '210' | Out-Host\n"
   "  } else {\n"
   "    $rawui.MaxPhysicalWindowSize.Width | Out-Host\n"
   "  }\n"
   "}\n"
   "_Emacs_GetMaxPhsWindowSize\n")
  "The powershell logic to determine the max physical window width.")

(eval-and-compile

  (defconst inf-powershell--set-window-width-fn-name  "_Emacs_SetWindowWidth"
   "The name of the function this mode defines in PowerShell to
set the window width. Intended for internal use only."))

(defconst inf-powershell--text-of-set-window-width-ps-function
  ;; see
  ;; http://blogs.msdn.com/lior/archive/2009/05/27/ResizePowerShellConsoleWindow.aspx
  ;;
  ;; When making the console window narrower, you mus set the window
  ;; size first. When making the console window wider, you must set the
  ;; buffer size first.
  (eval-when-compile
    (concat  "function " inf-powershell--set-window-width-fn-name
             ;; "([string] $pswidth)\n"
             "{\n"
             " Param ($pswidth)\n"
             ;;"  \"resetting window width to $pswidth\n\" | Out-Host\n"
             "  $rawui = (Get-Host).UI.RawUI\n"
             "  # retrieve the values\n"
             "  $bufsize = $rawui.BufferSize\n"
             "  $winsize = $rawui.WindowSize\n"
             "  $cwidth = $winsize.Width\n"
             "  $winsize.Width = $pswidth \n"
             "  $bufsize.Width = $pswidth\n"
             "  if ($cwidth -lt $pswidth) {\n"
             "    # increase the width\n"
             "    $rawui.BufferSize = $bufsize\n"
             "    $rawui.WindowSize = $winsize\n"
             "  }\n"
             "  elseif ($cwidth -gt $pswidth) {\n"
             "    # decrease the width\n"
             "    $rawui.WindowSize = $winsize\n"
             "    $rawui.BufferSize = $bufsize\n"
             "  }\n"
             "  # destroy variables\n"
             "  Set-Variable -name rawui -value $null\n"
             "  Set-Variable -name winsize -value $null\n"
             "  Set-Variable -name bufsize -value $null\n"
             "  Set-Variable -name cwidth -value $null\n"
             "}\n\n"))

  "The text of the powershell function that will be used at runtime to
set the width of the virtual Window in PowerShell, as the Emacs window
gets resized.")

(defun inf-powershell--define-set-window-width-function (proc)
  "Sends a function definition to the PowerShell instance
identified by PROC.  The function sets the window width of the
PowerShell virtual window.  Later, the function will be called
when the width of the emacs window changes."
  (if proc
      (progn
        ;;process-send-string
        (comint-simple-send
         proc
         inf-powershell--text-of-set-window-width-ps-function))))

(defun inf-powershell--get-max-window-width  (buffer-name)
  "Gets the maximum width of the virtual window for PowerShell running
in the buffer with name BUFFER-NAME.

In PowerShell 1.0, the maximum WindowSize.Width for
PowerShell is 210, hardcoded, I believe. In PowerShell 2.0, the max
windowsize.Width is provided in the RawUI.MaxPhysicalWindowSize
property."
  (let ((proc (get-buffer-process buffer-name)))

    (if proc
        (with-current-buffer buffer-name
          (inf-powershell-invoke-command-silently
           proc
           inf-powershell--find-max-window-width-command
           0.90)

          (if (and (not (null inf-powershell-command-reply))
                   (string-match
                    "\\([1-9][0-9]*\\)[ \t\f\v\n]*"
                    inf-powershell-command-reply))
              (string-to-number (match-string 1 inf-powershell-command-reply))
            200))))) ;; could go to 210, but let's use 200 to be safe

(defun inf-powershell--set-window-width (proc)
  "Run the PowerShell function that sets the RawUI width
appropriately for a PowerShell shell.

This is necessary to get powershell to do the right thing, as far
as text formatting, when the emacs window gets resized.

The function gets defined in powershell upon powershell startup."
  (let ((ps-width
         (number-to-string (min inf-powershell--max-window-width (window-width)))))
    (progn
      ;;(process-send-string
      (comint-simple-send
       proc
       (format "%s %s" inf-powershell--set-window-width-fn-name ps-width)
       ;; (concat inf-powershell--set-window-width-fn-name
       ;;         "('" ps-width "')")
       ))))


;; ------------------------------------------------------------
;;* Process interaction

(defun inf-powershell-log (level text &rest args)
  "Log a message at level LEVEL.
If LEVEL is higher than `inf-powershell-log-level', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format')."
  (if (<= level inf-powershell-log-level)
      (let* ((msg (apply 'format text args)))
        (message "%s" msg))))

(defun inf-powershell--silent-cmd-filter (process result)
  "A process filter that captures output from a shell and stores it
to `inf-powershell-command-reply', rather than allowing the output to
be displayed in the shell buffer.

This function is intended for internal use only."
  (let ((end-of-result
         (string-match (concat ".*\n\\(" inf-powershell-prompt-regex "\\)[ \n]*\\'")
                       result)))
    (if (and end-of-result (numberp end-of-result))

        (progn
          ;; Store everything except the follow-on prompt.
          ;; The result probably includes a final newline!
          (setq result (substring result 0 (match-beginning 1)))

          (if inf-powershell-squish-results-of-silent-commands
              (setq result
                    (replace-regexp-in-string "\n" "" result)))

          (setq inf-powershell-command-reply
                (concat inf-powershell-command-reply result)))

      (progn
        (if inf-powershell-squish-results-of-silent-commands
            (setq result
                  (replace-regexp-in-string "\n" "" result)))

        (setq inf-powershell-command-reply
              (concat inf-powershell-command-reply result))

        ;; recurse.  For very very long output, the recursion can
        ;; cause stack overflow. Careful!
        (accept-process-output process inf-powershell-command-timeout-seconds)))))

(defun inf-powershell-invoke-command-silently (proc command
                                              &optional timeout-seconds)
  "In the PowerShell instance PROC, invoke COMMAND silently.
Neither the COMMAND is echoed nor the results to the associated
buffer.  Use TIMEOUT-SECONDS as the timeout, waiting for a
response.  The COMMAND should be a string, and need not be
terminated with a newline.

This is helpful when, for example, doing setup work. Or other sneaky
stuff, such as resetting the size of the PowerShell virtual window.

Returns the result of the command, a string, without the follow-on
command prompt.  The result will probably end in a newline. This result
is also stored in the buffer-local variable `inf-powershell-command-reply'.

In some cases the result can be prepended with the command prompt, as
when, for example, several commands have been send in succession and the
results of the prior command were not fully processed by the application.

If a PowerShell buffer is not the current buffer, this function
should be invoked within a call to `with-current-buffer' or
similar in order to insure that the buffer-local values of
`inf-powershell-command-reply', `inf-powershell-prompt-regex', and
`inf-powershell-command-timeout-seconds' are used.

Example:

    (with-current-buffer powershell-buffer-name
      (inf-powershell-invoke-command-silently
       proc
       command-string
       1.90))"

  (let ((old-timeout inf-powershell-command-timeout-seconds)
        (original-filter (process-filter proc)))

    (setq inf-powershell-command-reply nil)

    (if timeout-seconds
        (setq inf-powershell-command-timeout-seconds timeout-seconds))

    (set-process-filter proc 'inf-powershell--silent-cmd-filter)

    ;; Send the command plus the "prompt" command.  The filter
    ;; will know the command is finished when it sees the command
    ;; prompt.
    ;;
    (process-send-string proc (concat command "\nprompt\n"))

    (accept-process-output proc inf-powershell-command-timeout-seconds)

    ;; output of the command is now available in inf-powershell-command-reply

    ;; Trim prompt from the beginning of the output.
    ;; this can happen for the first command through
    ;; the shell.  I think there's a race condition.
    (if (and inf-powershell-command-reply
             (string-match (concat "^" inf-powershell-prompt-regex "\\(.*\\)\\'")
                           inf-powershell-command-reply))
        (setq inf-powershell-command-reply
              (substring inf-powershell-command-reply
                         (match-beginning 1)
                         (match-end 1))))

    ;; restore the original filter
    (set-process-filter proc original-filter)

    ;; restore the original timeout
    (if timeout-seconds
        (setq inf-powershell-command-timeout-seconds old-timeout))

    ;; the result:
    inf-powershell-command-reply))

(defun inf-powershell-delete-process (&optional proc)
  "Delete the current buffer process or PROC."
  (or proc
      (setq proc (get-buffer-process (current-buffer))))
  (and (processp proc)
       (delete-process proc)))

(defun inf-powershell-preoutput-filter-for-prompt (string)
  "Trim the newline from STRING, the prompt that we get back from
powershell.  This fn is set into the preoutput filters, so the
newline is trimmed before being put into the output buffer."
  (if (string-match (concat inf-powershell-prompt-regex "\n\\'") string)
      (substring string 0 -1) ;; remove newline
    string))

(defun inf-powershell-simple-send (proc string)
  "Override of the comint-simple-send function, with logic
specifically designed for powershell.  This just sends STRING,
plus the prompt command.

When running as an inferior shell with stdin/stdout redirected,
powershell is in noninteractive mode. This means no prompts get
emitted when a PS command completes. This makes it difficult for
a comint mode to determine when the command has completed.
Therefore, we send an explicit request for the prompt, after
sending the actual (primary) command. When the primary command
completes, Powershell then responds to the \"prompt\" command,
and emits the prompt.

This insures we get and display the prompt."
  ;; Tell PowerShell to resize its virtual window, if necessary. We do
  ;; this by calling a resize function in the PowerShell, before sending
  ;; the user-entered command to the shell.
  ;;
  ;; Powershell keeps track of its \"console\", and formats its output
  ;; according to the width it thinks it is using.  This is true even when
  ;; powershell is invoked with the - argument, which tells it to use
  ;; stdin as input.

  ;; Therefore, if the user has resized the emacs window since the last
  ;; PowerShell command, we need to tell PowerShell to change the size
  ;; of its virtual window. Calling that function does not change the
  ;; size of a window that is visible on screen - it only changes the
  ;; size of the virtual window that PowerShell thinks it is using.  We
  ;; do that by invoking the PowerShell function that this module
  ;; defined for that purpose.
  ;;
  (if inf-powershell--need-rawui-resize
      (progn
        (inf-powershell--set-window-width proc)
        (setq inf-powershell--need-rawui-resize nil)))
  (comint-simple-send proc (concat string "\n"))
  (comint-simple-send proc "prompt\n"))

(defun inf-powershell-shell-process (force)
  "Return inferior powershell shell process for interaction.  If FORCE is 
non-nil and no process is found, then create one."
  (if (process-live-p inf-powershell-shell-process)
      inf-powershell-shell-process
    (setq inf-powershell-shell-process
          (let ((found nil) proc
                (procs (process-list)))
            (while (and (not found) procs
                        (process-live-p (setq proc (pop procs)))
                        (process-command proc))
              (when (string= "powershell.exe"
                             (file-name-nondirectory (car (process-command proc))))
                (setq found proc)))
            (or found
                (and force
                     (get-buffer-process (inf-powershell))))))))

(defun inf-powershell-send-text (text)
  "Send TEXT to `inf-powershell-shell-process'."
  (let ((proc (inf-powershell-shell-process t)))
    (inf-powershell-simple-send
     proc
     (replace-regexp-in-string "[-({|`,+*/]\\(\\s-*\n\\)" " " text nil nil 1))))


;; ------------------------------------------------------------
;;* User Functions

(defun inf-powershell-send-region (start end)
  (interactive "r")
  (inf-powershell-send-text (buffer-substring start end)))

(defun inf-powershell-send-buffer ()
  "Send buffer contents to inferior powershell process.  Ignore 
starting <# ... #> blocks."
  (interactive)
  (save-excursion
    (let* ((pm (goto-char (point-min)))
           (start (or (condition-case nil (search-forward "#>")
                        (error nil))
                      pm)))
      (inf-powershell-send-text
       (buffer-substring-no-properties start (point-max))))))

(defun inf-powershell-cd-here ()
  "Change directory of inferior powershell to `default-directory'."
  (interactive)
  (inf-powershell-send-text (concat "cd " default-directory "\n")))

(defun inf-powershell-set-prompt (prompt)
  (interactive "sNew Prompt: ")
  (let ((proc (inf-powershell-shell-process nil)))
    (when proc
      (comint-simple-send proc (format "function prompt { '%s' }" prompt))
      (with-current-buffer (process-buffer proc)
        (setq-local inf-powershell-prompt-regex prompt)))))

;;;###autoload
(defun inf-powershell (&optional buffer prompt-string)
  "Run an inferior PowerShell.
If BUFFER is non-nil, use it to hold the powershell
process.  Defaults to *PowerShell*.

Interactively, a prefix arg means to prompt for BUFFER.

If BUFFER exists but the shell process is not running, it makes a
new shell.

If BUFFER exists and the shell process is running, just switch to
BUFFER.

If PROMPT-STRING is non-nil, sets the prompt to the given value.

See the help for `shell' for more details.  \(Type
\\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
         (read-buffer "Shell buffer: "
                      (generate-new-buffer-name "*PowerShell*")))))

  (setq buffer (get-buffer-create (or buffer "*PowerShell*")))
  (inf-powershell-log 1 "powershell starting up...in buffer %s" (buffer-name buffer))

  (let ((explicit-shell-file-name
         (if (and (eq system-type 'cygwin)
                  (fboundp 'cygwin-convert-file-name-from-windows))
             (cygwin-convert-file-name-from-windows inf-powershell-exe)
           inf-powershell-exe)))
    (shell buffer))
  
  (let ((proc (get-buffer-process (shell buffer))))

    (make-local-variable 'inf-powershell-command-reply)
    (make-local-variable 'inf-powershell-command-timeout-seconds)
    (make-local-variable 'inf-powershell-squish-results-of-silent-commands)

    ;; We need to tell powershell how wide the emacs window is, because
    ;; powershell pads its output to the width it thinks its window is.
    ;;
    ;; The way it's done: every time the width of the emacs window changes, we
    ;; set a flag. Then, before sending a powershell command that is
    ;; typed into the buffer, to the actual powershell process, we check
    ;; that flag.  If it is set, we  resize the powershell window appropriately,
    ;; before sending the command.

    ;; If we didn't do this, powershell output would get wrapped at a
    ;; column width that would be different than the emacs buffer width,
    ;; and everything would look ugly.

    ;; set flag to resize window
    (set (make-local-variable 'inf-powershell--need-rawui-resize) t)

    ;; set max window size
    (set (make-local-variable 'inf-powershell--max-window-width)
         (inf-powershell--get-max-window-width buffer))
    
    ;; define the function for use within powershell to resize the window
    (inf-powershell--define-set-window-width-function proc)

    ;; add the hook that sets the flag
    (add-hook 'window-size-change-functions
              '(lambda (&optional x)
                 (setq inf-powershell--need-rawui-resize t))
              nil t)

    ;; disallow backspace over prompt
    (set (make-local-variable 'comint-prompt-read-only) t)

    ;; wrap the comint-input-sender with a PS version
    ;; must do this after launching the shell!
    (set (make-local-variable 'comint-input-sender) 'inf-powershell-simple-send)
    
    ;; This sets up a prompt for the PowerShell.  The prompt is
    ;; important because later, after sending a command to the
    ;; shell, the scanning logic that grabs the output looks for
    ;; the prompt string to determine that the output is complete.
    (when prompt-string
      (progn
        (comint-simple-send
         proc
         (concat "function prompt { '" prompt-string "' }"))
        (set (make-local-variable 'inf-powershell-prompt-regex) prompt-string)))

    ;; hook the kill-buffer action so we can kill the inferior process?
    (add-hook 'kill-buffer-hook 'inf-powershell-delete-process nil t)
    
    ;; set a preoutput filter for powershell.  This will trim newlines
    ;; after the prompt.
    (add-hook 'comint-preoutput-filter-functions
              'inf-powershell-preoutput-filter-for-prompt nil t)

    ;; send a carriage-return  (get the prompt)
    (comint-send-input)
    (accept-process-output proc 1))
  
  ;; The launch hooks for powershell has not (yet?) been implemented
  ;;(run-hooks 'powershell-launch-hook)

  ;; return the buffer created
  buffer)

;;;###autoload
(defalias 'powershell 'inf-powershell)

;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;; Using powershell on emacs23, I get an error:
;;
;;    ansi-color-process-output: Marker does not point anywhere
;;
;; Here's what's happening.
;;
;; In order to be able to read the output from powershell, this shell
;; starts powershell.exe in "interactive mode", using the -i
;; option. This which has the curious side-effect of turning off the
;; prompt in powershell. Normally powershell will return its results,
;; then emit a prompt to indicate that it is ready for more input.  In
;; interactive mode it doesn't emit the prompt.  To work around this,
;; this code (powershell.el) sends an explicit `prompt` command after
;; sending any user-entered command to powershell. This tells powershell
;; to explicitly return the prompt, after the results of the prior
;; command. The prompt then shows up in the powershell buffer.  Lovely.
;;
;; But, `ansi-color-apply-on-region` gets called after every command
;; gets sent to powershell. It gets called with args `(begin end)`,
;; which are both markers. Turns out the very first time this fn is
;; called, the position for the begin marker is nil.
;;
;; `ansi-color-apply-on-region` calls `(goto-char begin)` (effectively),
;; and when the position on the marker is nil, the call errors with
;; "Marker does not point anywhere."
;;
;; The following advice suppresses the call to
;; `ansi-color-apply-on-region` when the begin marker points
;; nowhere.
(defadvice ansi-color-apply-on-region (around
                                       powershell-throttle-ansi-colorizing
                                       (begin end)
                                       compile)
  (progn
    (let ((start-pos (marker-position begin)))
      (cond
       (start-pos
        (progn
          ad-do-it))))))

(provide 'inf-powershell)
