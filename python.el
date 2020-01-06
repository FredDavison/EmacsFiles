;;; package -- Summary

;;; Commentary:

;;; Code:


(evil-define-operator fcd/evil-try-except-wrap (beg end)
  "Wrap code in a Python try-except block."
  :type line
    (let ((indent-line (line-number-at-pos beg))
          (end-indent-line (1+ (line-number-at-pos end)))
          (wrap-text (buffer-substring-no-properties beg end)))
      (delete-region beg end)
      (insert (concat "try:\n" wrap-text "except:\n"))
      (while (<= indent-line end-indent-line)
        (goto-line indent-line)
        (python-indent-line)
        (setq indent-line (1+ indent-line))
        )))


(defun fcd/insert-ipdb-break ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert "\nimport ipdb; ipdb.set_trace()")
    (python-indent-line)
    ))


(defun fcd/insert-ipdb-break-with-traceback ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert "\nimport traceback; traceback.print_exc();")
    (python-indent-line)
    (fcd/insert-ipdb-break)
    ))


(provide 'python)
;;; python.el ends here
