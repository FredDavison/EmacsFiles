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
    (goto-line (1- (line-number-at-pos)))
    (end-of-line)
    (insert "\nimport ipdb; ipdb.set_trace()")
    (python-indent-line)
    ))


; BELOW FUNCTIONS NEED FIXING SO THEY DON'T USE INTERACTIVE COMMANDS

(defun fcd/insert-ipdb-break-with-traceback ()
  (interactive)
  (save-excursion
    (goto-line (1- (line-number-at-pos)))
    (end-of-line)
    (insert "\nimport traceback; traceback.print_exc();")
    (python-indent-line)
    (fcd/insert-ipdb-break)
    ))


(defun fcd/insert-ipdb-try-clause ()
  (interactive)
  (progn
    (evil-insert-line 0)
    (insert (kbd "TAB"))
    (evil-open-above 0)
    (insert "try:")
    (evil-next-line)
    (evil-open-below 0)
    (python-indent-dedent-line-backspace 4)
    (insert "except:")
    (fcd/insert-ipdb-break-with-traceback)))

(provide 'python)
;;; python.el ends here
