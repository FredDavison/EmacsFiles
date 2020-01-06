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

; BELOW FUNCTIONS NEED FIXING SO THEY DON'T USE INTERACTIVE COMMANDS

(defun fcd/insert-ipdb-break ()
  (interactive)
  (progn
    (evil-open-below 1)
    (insert "import ipdb; ipdb.set_trace()")
    (evil-normal-state)))


(defun fcd/insert-ipdb-break-with-traceback ()
  (interactive)
  (progn
    (evil-open-below 1)
    (insert "import traceback; traceback.print_exc();")
    (fcd/insert-ipdb-break)
    (evil-normal-state)))


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
