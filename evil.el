;;; Code:


(evil-define-operator fcd/substitute-inside (beg end type register yank-handler)
  "Substitute text inside delimiter with text in 0 register."
  :type inclusive
  (evil-delete beg end type)
  (goto-char beg)
  (let ((text (if register
                  (evil-get-register register)
                (evil-get-register 48))))
    (insert text))
  (goto-char beg))
