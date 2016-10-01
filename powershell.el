;;; powershell.el --- Mode for editing Powershell scripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010 Frédéric Perrin
;; Copyright (C) 2012 Richard Bielawski rbielaws-at-i1-dot-net
;;               http://www.emacswiki.org/emacs/Rick_Bielawski

;; Author: Frédéric Perrin <frederic (dot) perrin (arobas) resel (dot) fr>

;; URL: http://github.com/nverno/powershell
;; Version: 0.3
;; Package-Requires: ((emacs "24"))
;; Keywords: powershell, languages

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'thingatpt))
(require 'inf-powershell)
(require 'powershell-capf)
(autoload 'powershell-eldoc-setup "powershell-eldoc")

(defgroup powershell nil
  "PowerShell scripting and programming utilities."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages
  :prefix "powershell-")

(defcustom powershell-indent 4
  "Amount of horizontal space to indent.
After, for instance, an opening brace"
  :type 'integer
  :group 'powershell)

(defcustom powershell-continuation-indent 2
  "Amount of horizontal space to indent a continuation line."
  :type 'integer
  :group 'powershell)

(defcustom powershell-continued-regexp  ".*\\(|[\\t ]*\\|`\\)$"
  "Regexp matching a continued line.
Ending either with an explicit backtick, or with a pipe."
  :type 'regexp
  :group 'powershell)

(defcustom powershell-ise-exe
  (or (executable-find "powershell_ise")
      (expand-file-name "system32/windowsPowershell/v1.0/powershell_ise.exe"
                        (getenv "windir")))
  "Location of powershell ISE."
  :group 'powershell
  :type 'string)

(defcustom powershell-exe
  (or (executable-find "powershell")
      (expand-file-name "system32/windowspowershell/v1.0/powershell.exe"
                        (getenv "windir")))
  "A string, providing the location of the Powershell.exe."
  :group 'powershell
  :type 'string)

(defcustom powershell-compile-command
  '(concat powershell-exe " -f " (buffer-file-name))
  "Default command used to invoke a powershell script (using compile)."
  :group 'powershell
  :type 'sexp)

(defcustom powershell-use-eldoc t
  "Setup eldoc if non-nil."
  :group 'powershell
  :type 'boolean)

;; ------------------------------------------------------------
;;* User functions

(defun powershell-ise ()
  "Launch external powershell ISE."
  (interactive)
  (call-process-shell-command powershell-ise-exe nil 0))

(defun powershell-describe-command (command)
  "Lookup help for command (default attempts to find symbol at point)
using 'Get-Help', with output to help buffer. With optional prefix, 
passes '-online' argument."
  (interactive
   (let* ((sym (symbol-at-point))
          (sym (and sym (symbol-name sym)))
          (enable-recursive-minibuffers t)
          val)
     (setq val (read-string (if sym
                                (format "Describe command (default %s): " sym)
                              "Describe command: ")))
     (list (if (string= val "") sym val))))
  (if current-prefix-arg
      (call-process powershell-exe nil 0 nil "Get-Help"
                    command "-online")
    (let ((buff "*powershell help*"))
      (with-output-to-temp-buffer buff
        (call-process powershell-exe nil buff t "Get-Help" command)))))

(defun powershell-describe-command-ss64 (command)
  "Lookup help for COMMAND online at 'http://ss64.com/ps'
 (default to symbol at point)."
  (interactive
   (let* ((sym (symbol-at-point))
          (sym (and sym (symbol-name sym)))
          (enable-recursive-minibuffers t)
          val)
     (setq val (read-string (if sym
                                (format "Lookup command (default %s): " sym)
                              "Lookup command: ")))
     (list (if (string= val "") sym val))))

  (let ((uri (format "http://ss64.com/ps/%s.html" (downcase command))))
    (browse-url uri)))

;; hack to autoindent

(defun powershell-auto-indent ()
  "Indent after retrun, move bracket a line forward in function."
  (interactive)
  (let ((b (or
            (and (looking-back "{" (line-beginning-position)) (looking-at "}"))
            (and (looking-back "(" (line-beginning-position)) (looking-at ")")))))
    (newline)
    (when b
      (save-excursion
        (newline)
        (powershell-indent-line))))
  (insert "a")
  (powershell-indent-line)
  (delete-char -1))

;; Compilation

(defvar compilation-read-command)
(defvar compilation-error-regexp-alist)
(defvar compilation-error-regexp-alist-alist)
(defvar powershell-error-regexp-alist
  '(posh-1 "At \\(.+\\):\\([0-9]+\\) char:\\([0-9]+\\)" 1 2))

(defun powershell-add-compile-regexp ()
  (when (not (assoc 'posh-1 compilation-error-regexp-alist))
    (push 'posh-1 compilation-error-regexp-alist)
    (push powershell-error-regexp-alist compilation-error-regexp-alist-alist)))

(eval-after-load 'compile
  '(powershell-add-compile-regexp))

(defun powershell-compile ()
  "Run script with output in compilation buffer."
  (interactive)
  (save-buffer)
  (let (compilation-read-command)
    (call-interactively 'compile)))

;; @@FIXME:
(defun powershell-compile-admin ()
  "Run script with administrator priviledge and output in compilation buffer."
  (interactive))

;; Code helpers

(defun powershell-quote-selection (beg end)
  "Quotes the selection between BEG and END.
Quotes with single quotes and doubles embedded single quotes."
  (interactive `(,(region-beginning) ,(region-end)))
  (if (not mark-active)
      (error "Command requires a marked region"))
  (goto-char beg)
  (while (re-search-forward "'" end t)
    (replace-match "''")(setq end (1+ end)))
  (goto-char beg)
  (insert "'")
  (setq end (1+ end))
  (goto-char end)
  (insert "'"))

(defun powershell-unquote-selection (beg end)
  "Unquotes the selected text between BEG and END.
Remove doubled single quotes as we go."
  (interactive `(,(region-beginning) ,(region-end)))
  (if (not mark-active)
      (error "Command requires a marked region"))
  (goto-char beg)
  (cond ((looking-at "'")
         (goto-char end)
         (when (looking-back "'" nil)
           (delete-char -1)
           (setq end (1- end))
           (goto-char beg)
           (delete-char 1)
           (setq end (1- end))
           (while (search-forward "'" end t)
             (delete-char -1)
             (forward-char)
             (setq end (1- end)))))
        ((looking-at "\"")
         (goto-char end)
         (when (looking-back "\"" nil)
           (delete-char -1)
           (setq end (1- end))
           (goto-char beg)
           (delete-char 1)
           (setq end (1- end))
           (while (search-forward "\"" end t)
             (delete-char -1)
             (forward-char)
             (setq end (1- end)))
           (while (search-forward "`" end t)
             (delete-char -1)
             (forward-char)
             (setq end (1- end)))))
        (t (error "Must select quoted text exactly"))))

(defun powershell-escape-selection (beg end)
  "Escape variables between BEG and END.
Also extend existing escapes."
  (interactive `(,(region-beginning) ,(region-end)))
  (if (not mark-active)
      (error "Command requires a marked region"))
  (goto-char beg)
  (while (re-search-forward "`" end t)
    (replace-match "```")(setq end (+ end 2)))
  (goto-char beg)
  (while (re-search-forward "\\(?:\\=\\|[^`]\\)[$]" end t)
    (goto-char (car (cdr (match-data))))
    (backward-char)
    (insert "`")
    (forward-char)
    (setq end (1+ end))))

(defun powershell-doublequote-selection (beg end)
  "Quotes the text between BEG and END with double quotes.
Embedded quotes are doubled."
  (interactive `(,(region-beginning) ,(region-end)))
  (if (not mark-active)
      (error "Command requires a marked region"))
  (goto-char beg)
  (while (re-search-forward "\"" end t)
    (replace-match "\"\"")(setq end (1+ end)))
  (goto-char beg)
  (while (re-search-forward "`'" end t)
    (replace-match "```")(setq end (+ 2 end)))
  (goto-char beg)
  (insert "\"")
  (setq end (1+ end))
  (goto-char end)
  (insert "\""))

(defun powershell-dollarparen-selection (beg end)
  "Wraps the text between BEG and END with $().
The point is moved to the closing paren."
  (interactive `(,(region-beginning) ,(region-end)))
  (if (not mark-active)
      (error "Command requires a marked region"))
  (save-excursion
    (goto-char end)
    (insert ")")
    (goto-char beg)
    (insert "$("))
  (forward-char))

(defun powershell-regexp-to-regex (beg end)
  "Turn the text between BEG and END into a regex.
The text is assumed to be `regexp-opt' output."
  (interactive `(,(region-beginning) ,(region-end)))
  (if (not mark-active)
      (error "Command requires a marked region"))
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (re-search-forward "\\\\(" nil t)
      (replace-match "("))
    (goto-char (point-min))
    (while (re-search-forward "\\\\)" nil t)
      (replace-match ")"))
    (goto-char (point-min))
    (while (re-search-forward "\\\\|" nil t)
      (replace-match "|"))))


;; ------------------------------------------------------------
;;* Indentation

(defun powershell-continuation-line-p ()
  "Return t is the current line is a continuation line.
The current line is a continued line when the previous line ends
with a backtick or a pipe"
  (save-excursion
    (forward-line -1)
    (looking-at powershell-continued-regexp)))

;; Rick added significant complexity to Frédéric's original version
(defun powershell-indent-line-amount ()
  "Return the column to which the current line ought to be indented."
  (save-excursion
    (beginning-of-line)
    (if (powershell-continuation-line-p)
        ;; on a continuation line (i.e. prior line ends with backtick
        ;; or pipe), indent relative to the continued line.
        (progn
          (while (and (not (bobp))(powershell-continuation-line-p))
            (forward-line -1))
          (+ (current-indentation) powershell-continuation-indent))
      ;; otherwise, indent relative to the block's opening char ([{
      (let ((closing-paren (looking-at "\\s-*\\s)"))
            new-indent
            block-open-line)
        (condition-case nil
            (progn
              (backward-up-list)   ;when at top level, throw to no-indent
              (setq block-open-line (line-number-at-pos))
              ;; We're in a block, calculate/return indent amount.
              (if (not (looking-at "\\s(\\s-*\\(#.*\\)?$"))
                  ;; code (not comments) follow the block open so
                  ;; vertically align the block with the code.
                  (if closing-paren
                      ;; closing indent = open
                      (setq new-indent (current-column))
                    ;; block indent = first line of code
                    (forward-char)
                    (skip-syntax-forward " ")
                    (setq new-indent (current-column)))
                ;; otherwise block open is at eol so indent is relative to
                ;; bol or another block open on the same line.
                (if closing-paren       ; this sets the default indent
                    (setq new-indent (current-indentation))
                  (setq new-indent (+ powershell-indent (current-indentation))))
                ;; now see if the block is nested on the same line
                (when (condition-case nil
                          (progn
                            (backward-up-list)
                            (= block-open-line (line-number-at-pos)))
                        (scan-error nil))
                  (forward-char)
                  (skip-syntax-forward " ")
                  (if closing-paren
                      (setq new-indent (current-column))
                    (setq new-indent (+ powershell-indent (current-column))))))
              new-indent)
          (scan-error ;; most likely, we are at the top-level
           0))))))

(defun powershell-indent-line ()
  "Indent the current line of powershell mode.
Leave the point in place if it is inside the meat of the line"
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (amount (powershell-indent-line-amount)))
    (if savep
        (save-excursion (indent-line-to amount))
      (indent-line-to amount))))

;; ------------------------------------------------------------
;;* Keywords

;; Taken from About_Keywords
(defvar powershell-keywords
  (concat "\\_<"
          (regexp-opt
           '("begin" "break" "catch" "class" "continue" "data" "do" "default"
             "dynamicparam" "else" "elseif" "end" "enum" "exit" "filter" "finally"
             "for" "foreach" "from" "function" "if" "in" "param" "process"
             "return" "switch" "throw" "trap" "try" "until" "where" "while")
           t)
          "\\_>")
  "Powershell keywords.")

;; Taken from About_Comparison_Operators and some questionable sources :-)
(defvar powershell-operators
  (concat "\\_<"
          (regexp-opt
           '("-eq" "-ne" "-gt" "-ge" "-lt" "-le"
             ;; case sensitive versions
             "-ceq" "-cne" "-cgt" "-cge" "-clt" "-cle"
             ;; explicitly case insensitive
             "-ieq" "-ine" "-igt" "-ige" "-ilt" "-ile"
             "-band" "-bor" "-bxor"
             "-and" "-or" "-xor"
             "-like" "-notlike" "-clike" "-cnotlike" "-ilike" "-inotlike"
             "-match" "-notmatch" "-cmatch" "-cnotmatch" "-imatch" "-inotmatch"
             "-contains" "-notcontains" "-ccontains" "-cnotcontains"
             "-icontains" "-inotcontains"
             "-replace" "-creplace" "-ireplace"
             "-is" "-as" "-f"
             ;; Questionable --> specific to certain contexts
             "-casesensitive" "-wildcard" "-regex" "-exact" ;specific to case
             "-begin" "-process" "-end" ;specific to scriptblock
             )
           t)
          "\\_>")
  "Powershell operators.")

(eval-when-compile
  (defvar powershell-scope-names
   '("global"   "local"    "private"  "script")
   "Names of scopes in Powershell mode.")

 (defvar powershell-variable-drive-names
   `("env" "function" "variable" "alias" ,@powershell-scope-names)
   "Names of scopes in Powershell mode."))

(defconst powershell-variables-regexp
  ;; There are 2 syntaxes detected: ${[scope:]name} and $[scope:]name
  ;; Match 0 is the entire variable name.
  ;; Match 1 is scope when the former syntax is found.
  ;; Match 2 is scope when the latter syntax is found.
  (eval-when-compile
    (concat
    "\\_<$\\(?:{\\(?:" (regexp-opt powershell-variable-drive-names t)
    ":\\)?[^}]+}\\|"
    "\\(?:" (regexp-opt powershell-variable-drive-names t)
    ":\\)?[a-zA-Z0-9_]+\\_>\\)"))
  "Identifies legal powershell variable names.")

(defconst powershell-function-names-regex
  ;; Syntax detected is [scope:]verb-noun
  ;; Match 0 is the entire name.
  ;; Match 1 is the scope if any.
  ;; Match 2 is the function name (which must exist)
  (eval-when-compile
    (concat
    "\\_<\\(?:" (regexp-opt powershell-scope-names t) ":\\)?"
    "\\([A-Z][a-zA-Z0-9]*-[A-Z0-9][a-zA-Z0-9]*\\)\\_>"))
  "Identifies legal function & filter names.")

(defconst powershell-object-types-regexp
  ;; Syntax is \[name[.name]\] (where the escaped []s are literal)
  ;; Only Match 0 is returned.
  "\\[\\(?:[a-zA-Z_][a-zA-Z0-9]*\\)\\(?:\\.[a-zA-Z_][a-zA-Z0-9]*\\)*\\]"
  "Identifies object type references.  I.E. [object.data.type] syntax.")

(defconst powershell-function-switch-names-regexp
  ;; Only Match 0 is returned.
  "\\_<-[a-zA-Z][a-zA-Z0-9]*\\_>"
  "Identifies function parameter names of the form -xxxx.")

;; Taken from Get-Variable on a fresh shell, merged with man
;; about_automatic_variables
(defvar powershell-builtin-variables-regexp
  (regexp-opt
   '("$"                              "?"
     "^"                              "_"
     "args"                           "ConsoleFileName"
     "Error"                          "Event"
     "EventSubscriber"                "ExecutionContext"
     "false"                          "Foreach"
     "HOME"                           "Host"
     "input"                          "LASTEXITCODE"
     "Matches"                        "MyInvocation"
     "NestedPromptLevel"              "null"
     "PID"                            "PROFILE"
     "PSBoundParameters"              "PSCmdlet"
     "PSCulture"                      "PSDebugContext"
     "PSHOME"                         "PSScriptRoot"
     "PSUICulture"                    "PSVersionTable"
     "PWD"                            "ReportErrorShowExceptionClass"
     "ReportErrorShowInnerException"  "ReportErrorShowSource"
     "ReportErrorShowStackTrace"      "Sender"
     "ShellId"                        "SourceArgs"
     "SourceEventArgs"                "StackTrace"
     "this"                           "true")
   t)
  "The names of the built-in Powershell variables.
They are highlighted differently from the other variables.")

(defvar powershell-config-variables-regexp
  (regexp-opt
   '("ConfirmPreference"           "DebugPreference"
     "ErrorActionPreference"       "ErrorView"
     "FormatEnumerationLimit"      "LogCommandHealthEvent"
     "LogCommandLifecycleEvent"    "LogEngineHealthEvent"
     "LogEngineLifecycleEvent"     "LogProviderHealthEvent"
     "LogProviderLifecycleEvent"   "MaximumAliasCount"
     "MaximumDriveCount"           "MaximumErrorCount"
     "MaximumFunctionCount"        "MaximumHistoryCount"
     "MaximumVariableCount"        "OFS"
     "OutputEncoding"              "ProgressPreference"
     "PSEmailServer"               "PSSessionApplicationName"
     "PSSessionConfigurationName"  "PSSessionOption"
     "VerbosePreference"           "WarningPreference"
     "WhatIfPreference"            ) t)
  "Names of variables that configure powershell features.")

;; ------------------------------------------------------------
;;* Font-Locking

(defun powershell-find-syntactic-comments (limit)
  "Find PowerShell comment begin and comment end characters.
Returns match 1 and match 2 for <# #> comment sequences respectively.
Returns match 3 and optionally match 4 for #/eol comments.
Match 4 is returned only if eol is found before LIMIT"
  (when (search-forward "#" limit t)
    (cond
     ((looking-back "<#" nil)
      (set-match-data (list (match-beginning 0) (1+ (match-beginning 0))
                            (match-beginning 0) (1+ (match-beginning 0)))))
     ((looking-at ">")
      (set-match-data (list (match-beginning 0) (match-end 0)
                            nil nil
                            (match-beginning 0) (match-end 0)))
      (forward-char))
     (t
      (let ((start (point)))
        (if (search-forward "\n" limit t)
            (set-match-data (list (1- start) (match-end 0)
                                  nil nil nil nil
                                  (1- start) start
                                  (match-beginning 0) (match-end 0)))
          (set-match-data (list start (match-end 0)
                                nil nil nil nil
                                (1- start) start))))))
    t))

(defun powershell-find-syntactic-quotes (limit)
  "Find PowerShell hear string begin and end sequences upto LIMIT.
Returns match 1 and match 2 for @' '@ sequences respectively.
Returns match 3 and match 4 for @\" \"@ sequences respectively."
  (when (search-forward "@" limit t)
    (cond
     ((looking-at "'$")
      (set-match-data (list (match-beginning 0) (1+ (match-beginning 0))
                            (match-beginning 0) (1+ (match-beginning 0))))
      (forward-char))
     ((looking-back "^'@" nil)
      (set-match-data (list (1- (match-end 0)) (match-end 0)
                            nil nil
                            (1- (match-end 0)) (match-end 0))))
     ((looking-at "\"$")
      (set-match-data (list (match-beginning 0) (1+ (match-beginning 0))
                            nil nil
                            nil nil
                            (match-beginning 0) (1+ (match-beginning 0))))
      (forward-char))
     ((looking-back "^\"@" nil)
      (set-match-data (list (1- (match-end 0)) (match-end 0)
                            nil nil
                            nil nil
                            nil nil
                            (1- (match-end 0)) (match-end 0)))))
    t))

(defvar powershell-font-lock-syntactic-keywords
  `((powershell-find-syntactic-comments (1 "!" t t) (2 "!" t t)
                                        (3 "<" t t) (4 ">" t t))
    (powershell-find-syntactic-quotes (1 "|" t t) (2 "|" t t)
                                      (3 "|" t t) (4 "|" t t)))
  "A list of regexp's or functions.
Used to add `syntax-table' properties to
characters that can't be set by the `syntax-table' alone.")


(defvar powershell-font-lock-keywords-1
  `( ;; Type annotations
    (,powershell-object-types-regexp . font-lock-type-face)
    ;; syntaxic keywords
    (,powershell-keywords . font-lock-keyword-face)
    ;; operators
    (,powershell-operators . font-lock-builtin-face)
    ;; the REQUIRES mark
    ("^#\\(REQUIRES\\)" 1 font-lock-warning-face t))
  "Keywords for the first level of font-locking in Powershell mode.")

(defvar powershell-font-lock-keywords-2
  (append
   powershell-font-lock-keywords-1
   `( ;; Built-in variables
     (,(concat "\\$\\(" powershell-builtin-variables-regexp "\\)\\>")
      0 font-lock-builtin-face t)
     (,(concat "\\$\\(" powershell-config-variables-regexp "\\)\\>")
      0 font-lock-builtin-face t)))
  "Keywords for the second level of font-locking in Powershell mode.")

(defvar powershell-font-lock-keywords-3
  (append
   powershell-font-lock-keywords-2
   `( ;; user variables
     (,powershell-variables-regexp
      (0 font-lock-variable-name-face)
      (1 (cons font-lock-type-face '(underline)) t t)
      (2 (cons font-lock-type-face '(underline)) t t))
     ;; function argument names
     (,powershell-function-switch-names-regexp
      (0 font-lock-reference-face)
      (1 (cons font-lock-type-face '(underline)) t t)
      (2 (cons font-lock-type-face '(underline)) t t))
     ;; function names
     (,powershell-function-names-regex
      (0 font-lock-function-name-face)
      (1 (cons font-lock-type-face '(underline)) t t))))
  "Keywords for the maximum level of font-locking in Powershell mode.")


(defun powershell-setup-font-lock ()
  "Set up the buffer local value for `font-lock-defaults'."
  ;; I use font-lock-syntactic-keywords to set some properties and I
  ;; don't want them ignored.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  ;; This is where all the font-lock stuff actually gets set up.  Once
  ;; font-lock-defaults has its value, setting font-lock-mode true should
  ;; cause all your syntax highlighting dreams to come true.
  (setq font-lock-defaults
        ;; The first value is all the keyword expressions.
        '((powershell-font-lock-keywords-1
           powershell-font-lock-keywords-2
           powershell-font-lock-keywords-3)
          ;; keywords-only means no strings or comments get fontified
          nil
          ;; case-fold (t ignores case)
          t
          ;; syntax-alist nothing special here
          nil
          ;; syntax-begin - no function defined to move outside syntactic block
          nil
          ;; font-lock-syntactic-keywords
          ;; takes (matcher (match syntax override lexmatch) ...)...
          (font-lock-syntactic-keywords
           . powershell-font-lock-syntactic-keywords))))

;; ------------------------------------------------------------
;;* Code helpers

;; imenu

(defvar powershell-imenu-expression
  `(("Functions" ,(concat "function " powershell-function-names-regex) 2)
    ("Filters" ,(concat "filter " powershell-function-names-regex) 2)
    ("Top variables"
     , (concat "^\\(" powershell-object-types-regexp "\\)?\\("
               powershell-variables-regexp "\\)\\s-*=")
     2))
  "List of regexps matching important expressions, for speebar & imenu.")

;; Speedbar

(when (require 'speedbar nil t)
  (declare-function speedbar-add-supported-extension "speedbar")
  (speedbar-add-supported-extension ".ps1?"))

;; Abbrevs

(define-abbrev-table 'powershell-mode-abbrev-table ())

(defun powershell-in-code-context-p ()
  "Don't expand in strings or comments."
  (let ((ppss (syntax-ppss)))
    (and (null (elt ppss 3))    ; inside string
         (null (elt ppss 4))))) ; inside comment

(defun powershell-pre-abbrev-expand-hook ()
  "Only expand abbrevs in code context."
  (setq local-abbrev-table
        (if (powershell-in-code-context-p)
            powershell-mode-abbrev-table)))


;; ------------------------------------------------------------
;;* Major Mode

;; Syntax

(defvar powershell-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?$  "_" table)
    (modify-syntax-entry ?:  "_" table)
    (modify-syntax-entry ?-  "_" table)
    (modify-syntax-entry ?^  "_" table)
    (modify-syntax-entry ?\\ "_" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?` "\\" table)
    (modify-syntax-entry ?_  "w" table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?*  "." table)
    (modify-syntax-entry ?/  "." table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?#  "<" table)
    table)
  "Syntax for PowerShell major mode.")

;; Menu

(defvar powershell-menu
  '("PowerShell"
    ["Start powershell" powershell t]
    ["Send Region" inf-powershell-send-region t]
    ["Send Buffer" inf-powershell-send-buffer t]
    ["Set Prompt" inf-powershell-set-prompt t]
    ["Change Directory" inf-powershell-cd-here
     :help "Change directory in inferior process to current"]
    ["Open ISE" powershell-ise t]
    "--"
    ["Compile" powershell-compile
     :help "Run script with output to compilation buffer."]
    ["Compile as admin" powershell-compile-admin
     :help "Run script as administrator, output to compilation buffer."]
    "--"
    ["Lookup Command" powershell-describe-command
     :help "Lookup help for command using Get-Help"]
    ["Lookup Command Online" powershell-descibe-command-ss64
     :help "Lookup help for command online at http://ss64.com/ps"]
    "--"
    ["DoubleQuote Selection" powershell-doublequote-selection
     :help "DoubleQuotes the selection escaping embedded double quotes"]
    ["SingleQuote Selection" powershell-quote-selection
     :help "SingleQuotes the selection escaping embedded single quotes"]
    ["UnQuote selection" powershell-unquote-selection
     :help "Un-Quotes the selection un-escaping any escaped quotes"]
    ["Escape Selection" powershell-escape-selection
     :help "Escapes variables in the selection and extends existing escapes."]
    ["DollarParen Selection" powershell-dollarparen-selection
     :help "Wraps the selection in $()"]))

;; Map

(defvar powershell-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil map nil powershell-menu)
    (define-key map (kbd "C-c C-b") #'inf-powershell-send-buffer)
    (define-key map (kbd "C-c C-d") #'inf-powershell-cd-here)
    (define-key map (kbd "C-c C-r") #'inf-powershell-send-region)
    (define-key map (kbd "C-c C-p") #'inf-powershell-set-prompt)
    (define-key map (kbd "RET")     #'powershell-auto-indent)
    (define-key map (kbd "M-\"")    #'powershell-doublequote-selection)
    (define-key map (kbd "M-'")     #'powershell-quote-selection)
    (define-key map (kbd "C-'")     #'powershell-unquote-selection)
    (define-key map (kbd "C-\"")    #'powershell-unquote-selection)
    (define-key map (kbd "M-`")     #'powershell-escape-selection)
    (define-key map (kbd "C-$")     #'powershell-dollarparen-selection)
    (define-key map (kbd "C-c C-z") #'powershell)
    (define-key map (kbd "C-c C-e") #'powershell-ise)
    (define-key map (kbd "C-c ?")   #'powershell-describe-command)
    (define-key map (kbd "C-c C-?") #'powershell-describe-command-ss64)
    (define-key map (kbd "<f5>")    #'powershell-compile)
    (define-key map (kbd "C-<f5>")  #'powershell-compile-admin)
    map)
  "Keymap for PS major mode.")

;;;###autoload
(define-derived-mode powershell-mode prog-mode "PS"
  "Major mode for editing PowerShell scripts.\n
Commands:
\\{powershell-mode-map}"
  (powershell-setup-font-lock)
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+\\s*")
  (setq-local parse-sexp-ignore-comments t)
  (setq-local compile-command powershell-compile-command)
  (setq-local indent-line-function 'powershell-indent-line)

  (setq-local imenu-case-fold-search nil)
  (setq-local imenu-generic-expression powershell-imenu-expression)
  (imenu-add-menubar-index)
  
  (setq local-abbrev-table powershell-mode-abbrev-table)
  (add-hook 'pre-abbrev-expand-hook
            #'powershell-pre-abbrev-expand-hook nil t)

  (setq-local completion-ignore-case t)
  (add-hook 'completion-at-point-functions #'powershell-capf nil t)
  
  (when powershell-use-eldoc (powershell-eldoc-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ps[dm]?1\\'" . powershell-mode))

(provide 'powershell)

;;; powershell.el ends here
