;;; init.el ---  FCD config settings for Emacs.

;;; Commentary:
; Should work on both OSX and Windows 7 machines

; ----------------------------------------------------------------------------- ;
; Packages                                                                      ;
; ----------------------------------------------------------------------------- ;
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(package-install 'use-package)
(setq use-package-always-ensure t)


(use-package undo-tree)


(use-package leuven-theme
  :config
  (load-theme 'leuven t)
  )


(use-package fuzzy)


; Initialize environment from the user's shell.
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :config
  (exec-path-from-shell-initialize)
  )


(use-package evil-leader
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-key "o" 'fcd/evil-open-below-return-normal)
  (evil-leader/set-key "O" 'fcd/evil-open-above-return-normal)
  (evil-leader/set-key "d=" 'fcd/substitute-before-equal-sign)
  (evil-leader/set-key "D=" 'fcd/substitute-after-equal-sign)
  (evil-leader/set-key-for-mode 'python-mode "i" 'fcd/insert-ipdb-break)
  (evil-leader/set-key-for-mode 'python-mode "t" 'fcd/insert-ipdb-break-with-traceback)
  )


(use-package evil
  :config
  (evil-mode t)
  (setq evil-symbol-word-search (quote symbol))
  (setq evil-normal-state-modes (append evil-motion-state-modes evil-normal-state-modes))
  (setq evil-motion-state-modes nil)
  )


(use-package evil-surround
  :config
  (global-evil-surround-mode t)
  )

(use-package evil-commentary
  :config
  (evil-commentary-mode))


(use-package auto-complete
  :config
  (global-auto-complete-mode t)
  (setq ac-auto-start nil)
  (setq ac-max-width 0.3)
  (setq ac-quick-help-delay 0.25)
  (define-key ac-mode-map (kbd "M-C-i") 'auto-complete)
  )


(use-package jedi
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'set-python-ac-sources)
  (setq jedi:complete-on-dot t)
  (setq jedi:get-in-function-call-delay 500)
)


(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-checker-error-threshold 1000)
  (setq flycheck-indication-mode nil)
  )


(use-package nlinum-relative
  :config
  (global-nlinum-relative-mode)
  (set-face-attribute 'nlinum-relative-current-face nil :inherit
		      'linum :background "#EDEDED" :foreground
		      "#9A9A9A" :weight 'normal
		      )
  )


(use-package fuzzy)


(use-package evil-tabs
  :config
  (global-evil-tabs-mode)
  )


(use-package helm
  :config
  (require 'helm-config)
  (helm-mode t)
  (setq helm-mode-fuzzy-match nil)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  )

(use-package smartparens
  :config
  (smartparens-global-mode t)
  (require 'smartparens-config)
  (global-set-key (kbd "C-c ]") 'sp-forward-slurp-sexp)
  (global-set-key (kbd "C-c [") 'sp-forward-barf-sexp)
  (global-set-key (kbd "C-c }") 'sp-backward-slurp-sexp)
  (global-set-key (kbd "C-c {") 'sp-backward-barf-sexp)
  )

(use-package helm-projectile
  :config
  (projectile-mode t)
  (helm-projectile-on)
  (setq projectile-globally-ignored-files (append '("*.exe" "*.sdf") projectile-globally-ignored-files))
  (setq projectile-indexing-method 'alien)
  )

(use-package evil-ediff)


; ----------------------------------------------------------------------------- ;
; Remaps                                                                        ;
; ----------------------------------------------------------------------------- ;

; Configure # key to work as intended in evil-mode on Mac
(when (eq system-type 'darwin)
      (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
      (define-key evil-normal-state-map (kbd "M-3" ) 'evil-search-word-backward)
      (define-key isearch-mode-map (kbd "M-3") '(lambda () (interactive) (isearch-process-search-char ?\#))))

(global-set-key (kbd "C-c f") 'fcd/show-buffer-file-name)


; ----------------------------------------------------------------------------- ;
;;; Code                                                                          ;
; ----------------------------------------------------------------------------- ;


(global-auto-revert-mode)
(setq auto-revert-interval 0.1)

(defun fcd/show-buffer-file-name ()
    (interactive)
  (message (buffer-file-name)))

(defun fcd/evil-open-below-return-normal (count)
  (interactive "p")
  (progn
    (evil-open-below count)
    (evil-normal-state)))


(defun fcd/evil-open-above-return-normal (count)
  (interactive "p")
  (progn
    (evil-open-above count)
    (evil-normal-state)))


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


(defun fcd/substitute-before-equal-sign ()
  "Evil change from start of line to equals sign."
  ;;; at new beginning of line
  ;TODO messes up alignment
  (interactive)
  (progn
    (evil-delete (line-beginning-position) (fcd/character-position-search-from-line-start "=") )
    (insert " ")
    (evil-beginning-of-line)
    (evil-insert-state)))


(defun fcd/substitute-after-equal-sign ()
  "Evil change from equals sign to end of line."
  (interactive)
  (progn
    (evil-delete (1+ (fcd/character-position-search-from-line-start "=")) (line-end-position))
    (evil-append-line 1)
    (insert " ")))


(defun fcd/character-position-search-from-line-start (pattern)
  (save-excursion
    (evil-move-beginning-of-line)
    (re-search-forward pattern))
   (if (> (match-beginning 0) (line-end-position))
       (line-beginning-position)
     (match-beginning 0)))


(setq python-shell-interpreter "ipython"
    python-shell-interpreter-args "-i")


(defun set-python-ac-sources ()
  "Only use jedi as auto-complete source."
  (setq ac-sources '(ac-source-jedi-direct)))


(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 200)
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
	  (concat "C:/Users/fda/bin/GnuWin32/bin" ";" (getenv "PATH"))))


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
    (evil-ediff evil-commentary helm-projectile smartparens evil-leader leuven-theme use-package nlinum-relative jedi helm fuzzy flycheck flatui-theme exec-path-from-shell evil-tabs evil-surround))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
