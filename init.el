;;; Packages

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(setq my-package-list '(auto-complete evil exec-path-from-shell flycheck jedi undo-tree))
(mapc #'package-install my-package-list)

; Initialize environment from the user's shell.
(exec-path-from-shell-initialize)

(evil-mode t)
(global-auto-complete-mode t)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(add-hook 'after-init-hook #'global-flycheck-mode)

(global-linum-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq python-shell-interpreter "ipython"
       python-shell-interpreter-args "-i")
