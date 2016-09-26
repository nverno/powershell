;;; powershell-eldoc.el --- Eldoc support for powershell-mode  -*- lexical-binding: t; -*-

;;; Commentary:

;; Separated from powershell.el.  Doesn't work very well in its current state.

;;; Code:

(eval-when-compile
  (require 'thingatpt))

(defgroup powershell-eldoc nil
  "Eldoc support for powershell-mode."
  :group 'powershell
  :prefix "powershell-eldoc-")

(defcustom powershell-eldoc-data-files nil
  "List of files containing function help strings used by function `eldoc-mode'.
These are the strings function `eldoc-mode' displays as help for
functions near point.  The format of the file must be exactly as
follows or who knows what happens.

   (set (intern \"<fcn-name1>\" powershell-eldoc-obarray) \"<helper string1>\")
   (set (intern \"<fcn-name2>\" powershell-eldoc-obarray) \"<helper string2>\")
...

Where <fcn-name> is the name of the function to which <helper string> applies.
      <helper-string> is the string to display when point is near <fcn-name>."
  :type '(repeat string)
  :group 'powershell-eldoc)

(defvar powershell-eldoc-default-data "eldoc-data.el"
  "Where to build default obarray.")

(defvar powershell-eldoc-script "build/eldoc.ps1"
  "Location of script to create obarray for eldoc.")

;; ------------------------------------------------------------

(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (setq powershell-eldoc-script (expand-file-name powershell-eldoc-script dir))
    (setq powershell-eldoc-default-data
          (expand-file-name powershell-eldoc-default-data dir))))

(defvar powershell-eldoc-obarray ()
  "Array for file entries by the function `eldoc'.
`powershell-eldoc-data-files' entries are added into this array.")

(defun powershell-eldoc-default-enable ()
  "Build default eldoc obarray using `powershell-eldoc-script'."
  (if (not (file-exists-p powershell-eldoc-default-data))
      (set-process-sentinel
       (start-process "powershell" "*powershell-eldoc build*" "powershell"
                      "-NoProfile" "-ExecutionPolicy" "ByPass"
                      "-f" powershell-eldoc-script powershell-eldoc-default-data)
       #'powershell-eldoc-build-sentinel)
    (powershell-eldoc-enable (cons powershell-eldoc-default-data nil))))

(defun powershell-eldoc-build-sentinel (proc msg)
  "Load and enable eldoc when build finishes."
  (message "%s: %s" (process-name proc) (replace-regexp-in-string "\n" "" msg))
  (when (eq 0 (process-exit-status proc))
    (powershell-eldoc-enable (cons powershell-eldoc-default-data nil))))

(defun powershell-eldoc-enable (files)
  "Load and enable eldoc."
  (condition-case file
      (mapc 'load files)
    (error (message "Failed to load %s" file)))
  (eldoc-mode))
 
;;;###autoload
(defun powershell-eldoc-setup ()
  "Load the function documentation for use with eldoc."
  (set (make-local-variable 'eldoc-documentation-function) #'powershell-eldoc-function)
  (unless (vectorp powershell-eldoc-obarray)
    (setq powershell-eldoc-obarray (make-vector 400 0))
    (if powershell-eldoc-data-files
        (powershell-eldoc-enable powershell-eldoc-data-files)
      (powershell-eldoc-default-enable))))

(defun powershell-eldoc-function ()
  "Return a documentation string appropriate for the current context or nil."
  (let ((word (thing-at-point 'symbol)))
    (if word
        (eval (intern-soft word powershell-eldoc-obarray)))))

(provide 'powershell-eldoc)
