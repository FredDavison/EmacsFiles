;;; Code:

(require 'evil)

(evil-define-operator fcd/substitute-inside (beg end type register yank-handler)
  "Substitute text inside delimiter with text in 0 register or prefix-provided register."
  :type inclusive
  (evil-delete beg end)
  (goto-char beg)
  (let ((text (if register
                  (evil-get-register register)
                (evil-get-register (or evil-this-register 48)))))
    (insert text))
  (goto-char beg))

(provide 'evil)
;;; evil.el ends here
