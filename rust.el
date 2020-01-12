;;; package -- Summary

;;; Commentary:

;;; Code:

(define-key rust-mode-map (kbd "C-c C-c") 'rust-compile)
(define-key rust-mode-map (kbd "C-c C-t") 'fcd-rust-save-test)


(defun fcd-rust-save-test ()
  (interactive)
  (progn
    (basic-save-buffer)
    (rust-test))
  )

(provide 'rust)
;;; rust.el ends here
