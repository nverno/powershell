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

(defcustom powershell-eldoc-def-files '()
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

;; @@FIXME: not actually customizable, ps1 script needs to be mod
(defcustom powershell-eldoc-default-data
  (eval-when-compile (expand-file-name "powershell-eldoc-data" "."))
  "Build default obarray if no `powershell-eldoc-def-files' are nil."
  :group 'powershell-eldoc
  :type 'string)

(defcustom powershell-eldoc-exe
  (eval-when-compile (expand-file-name "build/eldoc.ps1" "."))
  "Location of script to create obarray for eldoc."
  :group 'powershell-eldoc
  :type 'string)

;; ------------------------------------------------------------
;;* Internal

(defvar powershell-eldoc-obarray ()
  "Array for file entries by the function `eldoc'.
`powershell-eldoc-def-files' entries are added into this array.")

(defun powershell-eldoc-default-setup ()
  "Build default eldoc obarray. Return default data location."
  (when (not (file-exists-p (concat powershell-eldoc-default-data ".el")))
    (call-process "powershell" nil nil nil "-NoProfile" "-ExecutionPolicy"
                  "ByPass" "-f" powershell-eldoc-exe)))

(defun powershell-eldoc-function ()
  "Return a documentation string appropriate for the current context or nil."
  (let ((word (thing-at-point 'symbol)))
    (if word
        (eval (intern-soft word powershell-eldoc-obarray)))))

;;;###autoload
(defun powershell-eldoc-setup ()
  "Load the function documentation for use with eldoc."
  (set (make-local-variable 'eldoc-documentation-function)
       'powershell-eldoc-function)
  (if (not (null powershell-eldoc-def-files))
      (unless (vectorp powershell-eldoc-obarray)
        (setq powershell-eldoc-obarray (make-vector 41 0))
        (condition-case var (mapc 'load powershell-eldoc-def-files)
          (error (message "*** powershell-setup-eldoc ERROR *** %s" var))))
    (powershell-eldoc-default-setup)
    (load powershell-eldoc-default-data))
  (eldoc-mode))

(provide 'powershell-eldoc)
