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
  )

(use-package auto-complete
  :init
  (global-auto-complete-mode t)
  (setq ac-auto-start nil)
  (define-key ac-mode-map (kbd "M-C-i") 'auto-complete)
  )

(use-package jedi)

(use-package helm
  :init
  (require 'helm-config)
  (setq helm-mode-fuzzy-match nil)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (setq helm-mode nil)
  (setq helm-mode t)
  )

(use-package flycheck
  :init
  ;(add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  )

(use-package nlinum-relative
  :init
  (global-nlinum-relative-mode)
  )

(use-package evil-tabs
  :init
  (global-evil-tabs-mode)
  )

(setq python-shell-interpreter "ipython"
       python-shell-interpreter-args "-i")

;;; Code
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")

(blink-cursor-mode 0)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(defun set-windows-variables ()
    "SET THE FACE WHEN ON WINDOWS MACHINES."
    (set-face-font (quote default) "-outline-Consolas-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1")
  )

(if (eq system-type 'windows-nt)
    (set-windows-variables)
)
