;;; powershell-capf --- completion at point for function/cmdlet parameters -*- lexical-binding: t -*-

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

;; Completion at point for powershell-mode.
;;
;; Current completes function/cmdlet parameters by using `powershell-eldoc-obarray'.
;; So, if that doesn't exist it won't do anything.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(defun powershell-function-name ()
  "Roughly try to get current function name."
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

(defun powershell-capf ()
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


(provide 'powershell-capf)

;;; powershell-capf.el ends here
