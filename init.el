;;; init.el ---  FCD config settings for Emacs.

					; TODO:
					; make dashes not separate words in elisp mode
					; Modify mode line

					; DONE:
					; load appearance functions from other file
					; horizontal divider for OSX (due to bugs in window-divider mode)
					; Separate functions for show linums and mode line
					; hide line nums by default - add to clear ui function

;;; Commentary:
; Should work on both OSX and Windows 7 machines

; TODO get pylint working with venvs
; TODO get autovenv working nicely with projects
; TODO get server working on Windows
; TODO get jedi auto complete working properly


; ----------------------------------------------------------------------------- ;
;;; Code:
; ----------------------------------------------------------------------------- ;
; Do appearance stuff right away
(blink-cursor-mode 0)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-hl-line-mode t)
(set-default 'truncate-lines t)
(setq-default indent-tabs-mode nil)

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
     
(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

; ----------------------------------------------------------------------------- ;
; Packages:
; ----------------------------------------------------------------------------- ;

(require 'server)
(unless (server-running-p) (server-start))


(require 'package)


(package-initialize)


(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))


(when (eq system-type 'darwin)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(setq use-package-always-ensure t)


(use-package f)


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))


(use-package yasnippet
  :config
  (yas-global-mode t))

(use-package yasnippet-snippets)

(use-package undo-tree)

(use-package csv-mode)

(use-package leuven-theme
  :config
  (load-theme 'leuven t)
  )

(when (eq system-type 'darwin)
  (use-package magit))

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
  (evil-leader/set-key-for-mode 'python-mode "T" 'fcd/insert-ipdb-try-clause)
  (evil-leader/set-key-for-mode 'python-mode "t" 'fcd/insert-ipdb-break-with-traceback)
  (evil-leader/set-key "n" 'fcd/toggle-global-nlinum-relative)
  (evil-leader/set-key "b" 'switch-to-buffer)
  (evil-leader/set-key "C" 'flycheck-clear)
  (evil-leader/set-key "3" 'fcd/duplicate-window-vertically)
  )


(use-package evil
  :config
  (evil-mode t)
  (setq evil-symbol-word-search (quote symbol))
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
  (setq ac-use-menu-map t)
  (setq ac-auto-start 4)
  (setq ac-max-width 0.3)
  (add-hook 'emacs-lisp-mode-hook 'set-elisp-ac-sources))


(use-package auto-virtualenvwrapper
  :config
  (when (not (eq system-type 'windows-nt))
    (add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
    (setq venv-location (expand-file-name "~/.virtualenvs"))))


(use-package jedi
  :config
  ;; Use C-c <tab> to activate completion menu.
  ;; Use C-s to activate fuzzy search in completion menu
  (add-hook 'python-mode-hook 'jedi:setup)
  (global-set-key (kbd  "C-c <tab>") 'jedi:complete)
  (setq python-environment-directory venv-location))


(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-checker-error-threshold 1000)
  (setq flycheck-indication-mode nil))


(use-package nlinum-relative)


(use-package fuzzy)



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
  (setq projectile-globally-ignored-file-suffixes '("pyc" "~" "#" "exe" "sdf"))
  (setq projectile-indexing-method 'alien)
  )


(when (eq system-type 'darwin)
  (use-package web-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    )
  )

(use-package helm-swoop
  :config
  (global-set-key (kbd "C-s") 'helm-swoop))

(global-set-key (kbd "C-c n") 'fcd/toggle-ui)
(define-key evil-normal-state-map " " 'fcd/toggle-ui)


; ----------------------------------------------------------------------------- ;
;;; Code                                                                          ;
; ----------------------------------------------------------------------------- ;

(defvar init-location (f-join (getenv "HOME") ".emacs.d"))
(load (f-join init-location "appearance.el"))

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


(defun fcd/open-init-file ()
  "Open the user's init.el file."
  (interactive)
  (find-file user-init-file)
  )


(defun fcd/venv-workon ()
  "Give user a choice of venv containing directories before selecting venv."
  (interactive)
  (when (eq system-type 'windows-nt)
    (let ((venv-locations `(,(expand-file-name "~/.virtualenvs") "c:/anaconda3/envs"))
          (venv-choice (read-char-choice
                        (message "Which venv root location?\na) %s\nb) %s"
                                 (car venv-locations) (car (cdr venv-locations)))
                        '(?a ?b))))
      (cond
       ((eq venv-choice 97)
        (progn
          (message "a selected")
          (setq venv-location (nth 0 venv-locations))))
       ((eq venv-choice 98)
        (progn
          (message "a selected")
          (setq venv-location (nth 1 venv-locations))))))
    )
  (progn
    (message "venv location: %s" venv-location)
    (venv-workon)))


(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--colors=Linux --simple-prompt"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
 )


(defun set-python-ac-sources ()
  "Only use jedi as auto-complete source."
  (setq ac-sources '(ac-source-jedi-direct)))


(defun set-elisp-ac-sources ()
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers)))


(when (eq system-type 'windows-nt)
  (setenv "PATH"
          (concat
           "C:/Users/fda/repositories/TECC/main/Start;"
           "C:/Users/fda/repositories/TECC/main/External/Python/Python27;"
           "C:/Users/fda/repositories/TECC/main/External/Python/Python27/Scripts;"
           "C:/Users/fda/bin/GnuWin32/bin;"
           (getenv "PATH")))
  (add-to-list
   'exec-path "C:/Users/fda/repositories/TECC/main/External/Python/Python27/Scripts;")
  (setq exec-path
        (append '("C:/Users/fda/bin/GnuWin32/bin") exec-path)))


(setq scroll-margin 0
      scroll-conservatively 1)
(setq-default scroll-up-aggressively 0.0
              scroll-down-aggressively 0.0)


(defun fcd/tfs-checkout-and-make-writeable ()
  (interactive)
  (message "%s" "TFS: checking out file...")
  (shell-command
   (replace-regexp-in-string "/" "\\\\" (concat "TF VC checkout " (buffer-file-name))))
  (read-only-mode -1))


(defun fcd/tfs-status ()
  (interactive)
  (message "%s" "TFS: checking status...")
  (shell-command
   (replace-regexp-in-string "/" "\\\\" (concat "TF VC status " (buffer-file-name)))))


(defun fcd/tfs-undo ()
  (interactive)
  (if (yes-or-no-p (message "Really revert changes to %s and undo check-out?" (buffer-file-name)))
      (progn
        (shell-command
         (replace-regexp-in-string "/" "\\\\" (concat "TF VC undo " (buffer-file-name))))
        (message "%s" "TFS: undoing changes to file...")
        (read-only-mode 1))
    (message "%s" "Undo aborted.")))


(setq backup-directory-alist '(("." . "~/.emacsbackups")))
(setq backup-by-copying t)

(global-auto-revert-mode)
(setq auto-revert-interval 0.1)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 200)
(setq recentf-max-saved-items 200)
(global-set-key "\C-x\ \C-r" 'helm-recentf)


                                        ; Make underscore and dash not delimit words for Evil mode
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?- "w" (standard-syntax-table))


(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq echo-keystrokes 0.1)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq startup-mode-line-format mode-line-format)
(setq ring-bell-function 'ignore)

                                        ; Hooks
;; (add-hook 'buffer-list-update-hook 'fcd/highlight-selected-window)
;; (remove-hook 'buffer-list-update-hook 'fcd/highlight-selected-window)
(add-hook 'after-change-major-mode-hook 'fcd/set-ui-to-current-ui-state)

(setq js-indent-level 4)


(defun fcd/set-ui-after-make-frame ()
  (progn
    (blink-cursor-mode 0)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (global-hl-line-mode t)
    (set-default 'truncate-lines t)
    (fcd/init-ui)
    (fcd/set-ui-to-current-ui-state)
    (fcd/set-face-font)))

(defun fcd/duplicate-window-vertically ()
  (delete-other-windows)
  (split-window-right)
  )


                                        ; Only do stuff with the UI after frames exist. Frames don't exist when daemon starts
                                        ; Run it as well for the case when the frame is already created before hook is added
(fcd/set-ui-after-make-frame)
(add-hook 'after-make-frame-functions 'fcd/set-ui-after-make-frame)


                                        ; ----------------------------------------------------------------------------- ;
                                        ; Remaps                                                                        ;
                                        ; ----------------------------------------------------------------------------- ;

                                        ; Remove space and ret keybindings from evil normal mode
(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "TAB"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

                                        ; Configure # key to work as intended in evil-mode on Mac
(when (eq system-type 'darwin)
  (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
  (define-key evil-normal-state-map (kbd "M-3" ) 'evil-search-word-backward)
  (define-key isearch-mode-map (kbd "M-3") '(lambda () (interactive) (isearch-process-search-char ?\#))))

(global-set-key (kbd "C-c c o") 'fcd/tfs-checkout-and-make-writeable)
(global-set-key (kbd "C-c c s") 'fcd/tfs-status)
(global-set-key (kbd "C-c f") 'fcd/show-buffer-file-name)
(global-set-key (kbd "C-c i") 'fcd/open-init-file)
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c b") 'helm-projectile-find-file)

(require 'python)
(define-key python-mode-map (kbd "C-c C-c")
  (progn
    (lambda () (interactive) (python-shell-send-buffer t))))

(global-set-key (kbd "C-c n") 'fcd/toggle-ui)
(global-set-key (kbd "C-<tab>") 'other-window)

(define-key evil-normal-state-map (kbd "M-3" ) 'evil-search-word-backward)
(define-key evil-normal-state-map " " 'fcd/toggle-ui)

(global-set-key (kbd "C-c SPC") 'redraw-display)

(define-key global-map (kbd "C-x C-j" )'dired-jump)
                                        ; ----------------------------------------------------------------------------- ;
                                        ; Auto
                                        ; ----------------------------------------------------------------------------- ;

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
    (markdown-mode command-log-mode esup avy yasnippet-snippets yasnippet-bundle ac-helm yasnippet auto-dim-other-buffers jedi csv-mode helm-swoop magit web-mode auto-virtualenvwrapper evil-commentary helm-projectile smartparens evil-leader leuven-theme use-package nlinum-relative helm fuzzy flycheck flatui-theme exec-path-from-shell evil-tabs evil-surround))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
