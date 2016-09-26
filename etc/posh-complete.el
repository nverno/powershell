;;; posh-complete.el --- Nothing at the moment  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unused completion functions removed from powershell-mode.el

;;; Code:

;; Notes on TAB for completion.
;; -------------------------------------------------------
;; Emacs calls comint-dynamic-complete when the TAB key is pressed in a shell.
;; This is set up in shell-mode-map.
;;
;; comint-dynamic-complete calls the functions in
;; comint-dynamic-complete-functions, until one of them returns
;; non-nil.
;;
;; comint-dynamic-complete-functions is a good thing to set in the mode hook.
;;
;; The default value for that var in a powershell shell is:
;; (comint-replace-by-expanded-history
;;    shell-dynamic-complete-environment-variable
;;    shell-dynamic-complete-command
;;    shell-replace-by-expanded-directory
;;    comint-dynamic-complete-filename)

;; (defun powershell-dynamic-complete-command ()
;;   "Dynamically complete the command at point.  This function is
;; similar to `comint-dynamic-complete-filename', except that it
;; searches the commands from powershell and then the `exec-path'
;; (minus the trailing Emacs library path) for completion candidates.

;; Completion is dependent on the value of
;; `shell-completion-execonly', plus those that effect file
;; completion.  See `powershell-dynamic-complete-as-command'.

;; Returns t if successful."
;;   (interactive)
;;   (let ((filename (comint-match-partial-filename)))
;;     (if (and filename
;;              (save-match-data (not (string-match "[~/]" filename)))
;;              (eq (match-beginning 0)
;;                  (save-excursion (shell-backward-command 1) (point))))
;;         (prog2 (message "Completing command name...")
;;             (powershell-dynamic-complete-as-command)))))

;; (defun powershell-dynamic-complete-as-command ()
;;   "Dynamically complete at point as a command.
;; See `shell-dynamic-complete-filename'.  Returns t if successful."
;;   (let* ((filename (or (comint-match-partial-filename) ""))
;;          (filenondir (file-name-nondirectory filename))
;;          (path-dirs (cdr (reverse exec-path)))
;;          (cwd (file-name-as-directory (expand-file-name default-directory)))
;;          (ignored-extensions
;;           (and comint-completion-fignore
;;                (mapconcat (function (lambda (x) (concat (regexp-quote x) "$")))
;;                           comint-completion-fignore "\\|")))
;;          (dir "") (comps-in-dir ())
;;          (file "") (abs-file-name "") (completions ()))

;;     ;; Go thru each cmd in powershell's lexicon, finding completions.

;;     ;; Go thru each dir in the search path, finding completions.
;;     (while path-dirs
;;       (setq dir (file-name-as-directory (comint-directory (or (car path-dirs) ".")))
;;             comps-in-dir (and (file-accessible-directory-p dir)
;;                               (file-name-all-completions filenondir dir)))
;;       ;; Go thru each completion found, to see whether it should be used.
;;       (while comps-in-dir
;;         (setq file (car comps-in-dir)
;;               abs-file-name (concat dir file))
;;         (if (and (not (member file completions))
;;                  (not (and ignored-extensions
;;                            (string-match ignored-extensions file)))
;;                  (or (string-equal dir cwd)
;;                      (not (file-directory-p abs-file-name)))
;;                  (or (null shell-completion-execonly)
;;                      (file-executable-p abs-file-name)))
;;             (setq completions (cons file completions)))
;;         (setq comps-in-dir (cdr comps-in-dir)))
;;       (setq path-dirs (cdr path-dirs)))
;;     ;; OK, we've got a list of completions.
;;     (let ((success (let ((comint-completion-addsuffix nil))
;;                      (comint-dynamic-simple-complete filenondir completions))))
;;       (if (and (memq success '(sole shortest)) comint-completion-addsuffix
;;                (not (file-directory-p (comint-match-partial-filename))))
;;           (insert " "))
;;       success)))


;; ------------------------------------------------------------
;; ------------------------------------------------------------

;; (defun dino-powershell-complete (arg)
;; "do powershell completion on the given STRING. Pop up a buffer
;; with the completion list."
;;   (interactive
;;    (list (read-no-blanks-input "\
;; Stub to complete: ")))

;;   (let ((proc
;;          (get-buffer-process (current-buffer))))
;;    (comint-proc-query proc (concat "Get-Command " arg "*\n"))
;;    )
;; )

;; (defun dino-powershell-cmd-complete ()
;;   "try to get powershell completion to work."
;;   (interactive)
;;   (let ((proc
;;          (get-buffer-process (current-buffer))))
;; ;;   (comint-proc-query proc "Get-a\t")
;; ;;   (comint-simple-send proc "Get-a\t")
;;        (comint-send-string proc "Get-a\t\n")
;; ;;   (process-send-eof)
;;    )
;; )
