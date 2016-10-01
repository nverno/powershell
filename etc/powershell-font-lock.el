;; inline docs

(defface font-lock-powershell-docstring-face
  '((t :inherit font-lock-string-face))
  "Face to highlight <# 'words' #>."
  :group 'powershell)

(defface font-lock-powershell-doctitle-face
  '((t :inherit font-lock-type-face))
  "Face to highlight <# '.TITLE' #>"
  :group 'powershell)

(font-lock-add-keywords
 'powershell-mode
 '(("<#\\([^\\(?:#>\\)]*\\)#>"
    (0 (prog1 ()
         (let* ((expr (match-string-no-properties 1))
                (len (length expr)))
           ;; (compose-region (match-beginning 1)
           ;;                 ((match-end 1))
           ;;                 (aref expr ))
           )))
    (1 'font-lock-powershell-docstring-face t))))
