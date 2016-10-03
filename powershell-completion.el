;;; powershell-completion --- completion for powershell mode -*- lexical-binding: t -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/powershell
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created: 2 October 2016

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

;; Completion functions for powershell-mode.
;;
;; Uses posh- tables/obarray generated by build scripts to provide completions.
;;
;; Todo:
;;
;; - Allow adding to add to existing hashes
;; - Switch eldoc to use hashes instead of obarray

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'company)

(defvar powershell-data-files nil
  "Data files with data for `posh-functions', `posh-variables', `posh-env'.")

(defvar powershell-default-data-file "build/posh-data.el"
  "Default data file location.")

(defvar powershell-data-script "build/main.ps1"
  "Script to create data files.")

(defvar powershell-log-buffer nil)

;; ------------------------------------------------------------

(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (setq powershell-data-script (expand-file-name powershell-data-script dir))
    (setq powershell-default-data-file
          (expand-file-name powershell-default-data-file dir))))

;; log text in *POSH* buffer
(defun powershell-log (text)
  (unless (buffer-live-p powershell-log-buffer)
    (setq powershell-log-buffer (get-buffer-create "*POSH*")))
  (let (deactivate-mark)
    (with-current-buffer powershell-log-buffer
      (goto-char (point-max))
      (insert-before-markers text)
      (insert "\n"))))

;; hashes

(defun powershell-hash-test (s1 s2)
  (eq t (compare-strings s1 nil nil s2 nil nil t)))

(defun powershell-case-fold-hash (s)
  (sxhash (upcase s)))

(define-hash-table-test 'powershell-hash 'powershell-hash-test
  'powershell-case-fold-hash)

(defvar posh-variables (make-hash-table :test 'powershell-hash))
(defvar posh-functions (make-hash-table :test 'powershell-hash))
(defvar posh-env (make-hash-table :test 'powershell-hash))

;; ------------------------------------------------------------

;; Find the function name in the current context.
;; 
(defun powershell-function-name ()
  (let ((ppss (syntax-ppss))
        (start (or (cdr (bounds-of-thing-at-point 'symbol)) (point))))
    (when (not (nth 4 ppss))
      (save-excursion 
        (skip-chars-backward "^.=><|)}%\[\]" (line-beginning-position))
        (cond
         ((eq (char-before) ?%)
          `(nil . "Foreach-Object"))
         ((eq (char-before) ?\])
          (skip-chars-backward "^\[" (line-beginning-position))
          (when (re-search-forward "\\([-.A-Za-z]+\\)]::\\([-A-Za-z]+\\)" start t)
            (cons (match-string-no-properties 1) (match-string-no-properties 2))))
         (t
          (when (re-search-forward "\\s-*\\([-A-Za-z]+\\)" start t)
            (cons nil (match-string-no-properties 1)))))))))

;; lookup function/cmdlet: if alias lookup up its definition
(defun powershell-lookup-function (func)
  (when-let ((val (gethash func posh-functions)))
    (if (assoc 'alias val)
        (gethash (cdr (assoc 'alias val)) posh-functions)
      val)))

;; ------------------------------------------------------------
;;* Company

(defun powershell-capf--var-docsig (obj)
  (cdr (assoc 'value (gethash obj posh-variables))))

(defun powershell-capf--env-docsig (obj)
  (cdr (assoc 'value (gethash obj posh-env))))

(defun powershell-capf--fn-docsig (obj)
  (cdr (assoc 'synopsis (powershell-lookup-function obj))))

(defun powershell-capf--doc-buffer (obj)
  (company-doc-buffer
   (replace-regexp-in-string
    "\n\n+" "\n\n"
    (shell-command-to-string
     (format "powershell -c \"Get-Help %s | %%{$_.Description}\"" obj)))))

(defun powershell-capf--var-annotation (obj)
  (or (cdr (assoc 'annot (gethash obj posh-variables)))))

(defun powershell-capf--env-annotation (obj)
  (or (cdr (assoc 'annot (gethash obj posh-env)))))

(defun powershell-capf--fn-annotation (obj)
  (or (cdr (assoc 'type (gethash obj posh-functions)))))

(defun powershell-capf ()
  (let ((bnds (bounds-of-thing-at-point 'symbol)))
    (cond
     ;; let comint complete files
     ((or (eq (char-after (car bnds)) ?/)
          (eq (char-after (car bnds)) ?\\))
      nil)
     ;; function / cmdlet parameter
     ((eq (char-after (car bnds)) ?-)
      (when-let ((func (powershell-function-name)))
        (and (not (car func))
             (let ((pars (cdr (assoc 'params (powershell-lookup-function (cdr func))))))
               (and pars
                    (list (1+ (car bnds)) (cdr bnds) pars
                          :company-require-match nil))))))
     ;; $ variable
     ((eq (char-after (car bnds)) ?$)
      (goto-char (car bnds))
      (prog1
          (cond
           ((looking-at-p "$env:")
            (list (+ 5 (car bnds)) (cdr bnds) posh-env
                  :company-docsig #'powershell-capf--env-docsig
                  :annotation-function #'powershell-capf--env-annotation))
           (t
            (list (1+ (car bnds)) (cdr bnds) posh-variables
                  :company-docsig #'powershell-capf--var-docsig
                  :annotation-function #'powershell-capf--var-annotation
                  :company-require-match nil)))
        (goto-char (cdr bnds))))
     ;; either $drive:| or possible function
     ((when-let ((func (powershell-function-name)))
        (and (not (car func))
             ;; env:
             (or (and (compare-strings "env" nil nil (cdr func) nil nil t)
                      (list (car bnds) (cdr bnds) posh-env
                            :annotation-function #'powershell-capf--var-annotation
                            :company-docsig #'powershell-capf--var-docsig
                            :company-require-match nil))
                 ;; try function/cmdlet
                 (list (car bnds) (cdr bnds) posh-functions
                       :company-docsig #'powershell-capf--fn-docsig
                       :company-doc-buffer #'powershell-capf--doc-buffer
                       :annotation-function #'powershell-capf--fn-annotation
                       :company-require-match nil))))))))

;; ------------------------------------------------------------
;;* Build default data file

;; Create posh-functions, posh-variables, posh-env hashes
;; for globally available functions/cmdlets/variables/environment variables.
(defun powershell-build-default-data ()
  (if (not (file-exists-p powershell-default-data-file))
      (progn
        (powershell-log (format "Creating %s" powershell-default-data-file))
        (set-process-sentinel
         (start-process "powershell" "*POSH*" "powershell"
                        "-NoProfile" "-ExecutionPolicy" "ByPass"
                        "-f" powershell-data-script powershell-default-data-file)
         #'powershell-build-sentinel))
    (powershell-load-data (cons powershell-default-data-file nil))))

;; load powershell data when bulid completes
(defun powershell-build-sentinel (proc msg)
  (powershell-log
   (format "%s: %s with exit status %s"
           (process-name proc) (replace-regexp-in-string "\n" "" msg)
           (process-exit-status proc)))
  (when (eq 0 (process-exit-status proc))
    (powershell-load-data (cons powershell-default-data-file nil))))

;; load data files
(defun powershell-load-data (files)
  (powershell-log
   (format "Loading %s" (mapconcat 'identity files ", ")))
  (condition-case file
      (progn
        (mapc 'load files)
        (when powershell-use-eldoc
          (eldoc-mode)))
    (error (powershell-log (format "Failed to load %s" file)))))

;;;###autoload
(defun powershell-completion-setup ()
  "Setup completion data for powershell.  This runs the default program
to generate `powershell-default-data-file' if it doesn't exist."
  (setq-local eldoc-documentation-function #'powershell-eldoc-function)
  (if (not powershell-data-files)
      (powershell-build-default-data)
    (powershell-load-data powershell-data-files)))

;; ------------------------------------------------------------
;;* Eldoc

(defun powershell-eldoc-function ()
  "Format powershell cmdlet/function parameters for display in the minibuffer
with `eldoc-mode'."
  (when-let ((func (or (powershell-function-name))))
    (and (not (car func))
         (let* ((name (cdr func))
                (pars (cdr (assoc 'params (powershell-lookup-function name)))))
           (when pars
             (format "%s: %s" (propertize name 'face 'font-lock-function-name-face)
                     (concat "-" (mapconcat 'identity pars " -"))))))))

;; ------------------------------------------------------------
;;* obarray for completion at point

;; Using `powershell-eldoc-obarray'
(defun powershell-capf-obarray ()
  "Powershell completion at point function.  Uses `powershell-eldoc-obarray' to complete 
function arguments."
  (when (bound-and-true-p powershell-eldoc-obarray)
    (when-let ((bnds (bounds-of-thing-at-point 'symbol)))
      (cond
       ((eq (char-after (car bnds)) ?-)
        (when-let ((func (powershell-function-name)))
          (and (not (car func))
               (let ((pars (eval (intern-soft (downcase (cdr func))
                                              powershell-eldoc-obarray))))
                 (and pars
                      (list (1+ (car bnds)) (cdr bnds) pars))))))
       ;; $vars
       ))))

(provide 'powershell-completion)

;;; powershell-completion.el ends here
