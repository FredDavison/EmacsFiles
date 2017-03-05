;;; Packages
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")

(blink-cursor-mode 0)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(setq my-package-list '(flatui-theme auto-complete evil flycheck jedi undo-tree nlinum-relative))
(if (eq system-type 'darwin)
    (add-to-list 'my-package-list 'exec-path-from-shell)
)
(mapc #'package-install my-package-list)

(defun set-windows-variables ()
    "setting the face when on Windows machines"
    (set-face-font (quote default) "-outline-Consolas-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1")
    (setq display-dont-pause 0) ; speeds up scrolling
  )

(if (eq system-type 'windows-nt)
    (set-windows-variables)
)

(load-theme 'flatui t)

; Initialize environment from the user's shell.
(if (eq system-type 'darwin)
    (exec-path-from-shell-initialize)
)


(evil-mode t)

; auto-complete settings
(global-auto-complete-mode t)
(setq ac-auto-start nil)
(define-key ac-mode-map (kbd "M-C-i") 'auto-complete)

;(require 'helm-config)
;(global-set-key (kbd "M-x") 'helm-M-x)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))

(global-nlinum-relative-mode)
;(linum-relative-global-mode) this mode slow
;(setq linum-relative-current-symbol "") this mode slow

(setq python-shell-interpreter "ipython"
       python-shell-interpreter-args "-i")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" default)))
 '(package-selected-packages
   (quote
    (flatui-theme nlinum-relative linum-relative undo-tree auto-complete jedi flycheck exec-path-from-shell evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
