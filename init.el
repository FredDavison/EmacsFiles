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

;; (use-package elscreen
;;   :init
;;   (elscreen-start)
;;   (setq elscreen-display-tab nil)
;;   (setq elscreen-prefix-key "C-.")
;;   (global-unset-key "C-.")
;;   (elscreen-set-prefix-key "^.")
;;   )

; Initialize environment from the user's shell.
(use-package fuzzy)

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
  (global-evil-surround-mode t)
  )

(use-package auto-complete
  :init
  (global-auto-complete-mode t)
  (setq ac-auto-start nil)
  (setq ac-max-width 0.3)
  (setq ac-quick-help-delay 0.25)
  (define-key ac-mode-map (kbd "M-C-i") 'auto-complete)
  )

(use-package jedi
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'set-python-ac-sources)
  (setq jedi:complete-on-dot t)
  (setq jedi:get-in-function-call-delay 500)
)

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

(use-package fuzzy)

(use-package evil-tabs
  :init 
  (global-evil-tabs-mode)
  )

(use-package helm
  :init
  (require 'helm-config)
  (helm-mode t)
  (setq helm-mode-fuzzy-match nil)
  (global-set-key (kbd "M-x") 'helm-M-x)
  )
  

;;; Remaps

; Configure # key to work as intended in evil-mode on Mac
(when (eq system-type 'darwin)
      (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
      (define-key evil-normal-state-map (kbd "M-3" ) 'evil-search-word-backward)
      (define-key isearch-mode-map (kbd "M-3") '(lambda () (interactive) (isearch-process-search-char ?\#)))
  )


;;; Code

(setq python-shell-interpreter "ipython"
       python-shell-interpreter-args "-i")

(defun set-python-ac-sources ()
  "Remmove the same buffers ac source"
  ;(setq ac-sources (remove 'ac-sources-worders-in-same-mode-buffers ac-sources))
    (setq ac-sources '(ac-source-jedi-direct))
  )

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key "\C-x\ \C-r" 'helm-recentf)

; Make underscore and dash not delimit words for Evil mode
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?- "w" (standard-syntax-table))

(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq echo-keystrokes 0.1)

(blink-cursor-mode 0)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-hl-line-mode t)

(set-default 'truncate-lines t)

; Add Gnu versions of find and grep to path. Don't do this in
; Windows settings because it will overwrite Windows system command
; find
(when (eq system-type 'windows-nt)
  (set-face-font (quote default) "-outline-Consolas-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1")
  (setenv "PATH"
	  (concat "C:/Users/fda/bin/GnuWin32/bin" ";" (getenv "PATH"))
	  )
  )
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
    (use-package nlinum-relative jedi helm fuzzy flycheck flatui-theme exec-path-from-shell evil-tabs evil-surround))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
