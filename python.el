;;; package -- Summary

;;; Commentary:

;;; Code:


(evil-define-operator fcd/evil-try-except-wrap (beg end)
  "Wrap code in a Python try-except block."
  :type line
  (progn
    (let ((indent-line (line-number-at-pos beg))
          (end-indent-line (1+ (line-number-at-pos end)))
          (wrap-text (buffer-substring-no-properties beg end)))
      (delete-region beg end)
      (insert (concat "try:\n" wrap-text "except:\n"))
      (while (<= indent-line end-indent-line)
        (goto-line indent-line)
        (python-indent-line)
        (setq indent-line (1+ indent-line))
        ))))

(provide 'python)
;;; python.el ends here
