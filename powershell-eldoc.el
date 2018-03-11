;;; powershell-eldoc --- Eldoc support for powershell mode  -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/powershell
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created: 30 September 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Eldoc support for powershell-mode

;;; Code:

(eval-when-compile
  (require 'thingatpt)
  (require 'subr-x))
(require 'powershell-completion)

(defgroup powershell-eldoc nil
  "Eldoc support for powershell-mode."
  :group 'powershell
  :prefix "powershell-eldoc-")

(defcustom powershell-eldoc-data-files nil
  "List of files containing metadata for `eldoc-mode'.
They have the following format.

   (set (intern \"<fcn-name1>\" powershell-eldoc-obarray) '(<list of parameters1>))
   (set (intern \"<fcn-name2>\" powershell-eldoc-obarray) (<list of parameters2>))
...

Where <fcn-name> is the name of the function to which has <list of parameters1>."
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
      (progn
        (message "Creating powershell-eldoc data in %s"
                 powershell-eldoc-default-data)
        (set-process-sentinel
         (start-process "powershell" "*powershell-eldoc build*" "powershell"
                        "-NoProfile" "-ExecutionPolicy" "ByPass"
                        "-f" powershell-eldoc-script powershell-eldoc-default-data)
         #'powershell-eldoc-build-sentinel))
    (powershell-eldoc-enable (cons powershell-eldoc-default-data nil))))

(defun powershell-eldoc-build-sentinel (proc msg)
  "Load and enable eldoc when build finishes."
  (message "%s: %s" (process-name proc) (replace-regexp-in-string "\n" "" msg))
  (when (eq 0 (process-exit-status proc))
    (powershell-eldoc-enable (cons powershell-eldoc-default-data nil))))

(defun powershell-eldoc-enable (files)
  "Load and enable eldoc."
  (condition-case file
      (progn
        (mapc 'load files)
        (eldoc-mode))
    (error (message "Failed to load %s" file))))

;;;###autoload
(defun powershell-eldoc-setup ()
  "Load the function documentation for use with eldoc."
  (set (make-local-variable 'eldoc-documentation-function) #'powershell-eldoc-function)
  (if (not (vectorp powershell-eldoc-obarray))
      (progn
        (setq powershell-eldoc-obarray (make-vector 1400 0))
        (if powershell-eldoc-data-files
            (powershell-eldoc-enable powershell-eldoc-data-files)
          (powershell-eldoc-default-enable)))
    (eldoc-mode)))

(defun powershell-eldoc-function ()
  "Return parameters for current function."
  (when-let* ((func (or (powershell-function-name))))
    (and (not (car func))
         (let* ((name (cdr func))
                (pars (eval (intern-soft (downcase name) powershell-eldoc-obarray))))
           (when pars
             (format "%s: %s" (propertize name 'face 'font-lock-function-name-face)
                     (concat "-" (mapconcat 'identity pars " -"))))))))

(provide 'powershell-eldoc)

;;; powershell-eldoc.el ends here
