;;; init.el ---  FCD config settings for Emacs.

;;; Commentary:
; Should port successfully between both OSX and Windows 7 machines

;;; Packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(package-install 'use-package)

(use-package undo-tree)

(use-package leuven-theme
  :init
  (load-theme 'leuven t)
  )

; Initialize environment from the user's shell.
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :init
  (exec-path-from-shell-initialize)
  )


(use-package evil
  :init
  (evil-mode t)
  (setq evil-symbol-word-search (quote symbol))
  )

(use-package evil-surround
  :init
  ()
  )

(use-package auto-complete
  :init
  (global-auto-complete-mode t)
  (setq ac-auto-start nil)
  (define-key ac-mode-map (kbd "M-C-i") 'auto-complete)
  )

(use-package jedi)

(use-package flycheck
  :init
  ;(add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  )

(use-package nlinum-relative
  :init
  (global-nlinum-relative-mode)
  (set-face-attribute 'nlinum-relative-current-face nil :inherit
		      'linum :background "#EDEDED" :foreground
		      "#9A9A9A" :weight 'normal
		      )
  )


;;; Packages that for whatever reason don't work with use-package
(package-install 'helm)
(helm-mode t)
(require 'helm-config)
(setq helm-mode-fuzzy-match nil)
(global-set-key (kbd "M-x") 'helm-M-x)

;(package-install evil-tabs)
(global-evil-tabs-mode)

(setq python-shell-interpreter "ipython"
       python-shell-interpreter-args "-i")

;;; Code

; Make underscore and dash not delimit words for Evil mode
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?- "w" (standard-syntax-table))

(setq inhibit-splash-screen t)
(setq initial-scratch-message "")

(blink-cursor-mode 0)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(set-default 'truncate-lines t)

(defun set-windows-variables ()
    "WINDOWS SPECIFIC OPTIONS"
    (set-face-font (quote default) "-outline-Consolas-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1")
  )

(if (eq system-type 'windows-nt)
    (set-windows-variables)
)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-surround yasnippet use-package tabbar s pyvenv nlinum-relative linum-relative leuven-theme jedi highlight-indentation helm flycheck flatui-theme find-file-in-project exec-path-from-shell evil-tabs company color-theme-sanityinc-tomorrow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
