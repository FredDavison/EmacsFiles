;;; init.el ---  FCD config settings for Emacs.

					; TODO:
					; make dashes not separate words in elisp mode
					; Modify mode line
                                        ; check gls program exists before setting it as ls alternative

					; DONE:
					; load appearance functions from other file
					; horizontal divider for OSX (due to bugs in window-divider mode)
					; Separate functions for show linums and mode line
					; hide line nums by default - add to clear ui function

;;; Commentary:
; Should work on both OSX and Windows 7 machines

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


(setq-default tab-width 4)
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
(when (not (eq system-type 'windows-nt))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (setq use-package-always-ensure t))


(use-package f)


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))


;; (use-package yasnippet
;;   :config
;;   (yas-global-mode t))

;; (use-package yasnippet-snippets)

(use-package undo-tree)

(use-package csv-mode)

(use-package leuven-theme
  :config
  (load-theme 'leuven t)
  )

(when (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
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
  (evil-leader/set-key-for-mode 'python-mode "E" 'fcd/evil-try-except-wrap)
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
  ;; (add-hook 'python-mode-hook '(setq ac-sources '(ac-source-jedi-direct)))
  (add-hook 'python-mode-hook 'set-python-ac-sources)
  (global-set-key (kbd  "C-c <tab>") 'jedi:complete)
  (setq jedi:complete-on-dot t)
  (setq python-environment-directory venv-location))


(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-checker-error-threshold 1000)
  (setq flycheck-indication-mode nil))

(use-package flycheck-rust
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


(use-package nlinum-relative)


(use-package fuzzy)


(use-package xenops
  :load-path "~/Development/xenops"
  :config
  (define-key LaTeX-mode-map (kbd "C-c x") 'xenops-mode)
  (defconst xenops-dependencies
    ;; auctex is installed separately
    '(aio avy dash dash-functional f org s use-package))
  (add-hook 'LaTeX-mode-hook 'xenops-mode))


(use-package helm
  :config
  (require 'helm-config)
  (helm-mode t)
  (setq helm-mode-fuzzy-match nil)
  (setq helm-window-prefer-horizontal-split t)
  (setq  helm-buffer-max-length nil)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-mini)
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
  (when (not (eq system-type 'windows-nt))
    (projectile-mode t)
    (helm-projectile-on)
    (setq projectile-globally-ignored-file-suffixes
          '("pyc" "~" "#" "exe" "sdf" "xcf" "xlsm" "xlsx" "png" "bmp" "jpg" "zip" "whl"
            "docx" "doc"))
    (setq projectile-indexing-method 'alien)))

(use-package rjsx-mode)

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

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))

(use-package helm-swoop
  :config
  (global-set-key (kbd "C-s") 'helm-swoop))

(global-set-key (kbd "C-c n") 'fcd/toggle-ui)
(define-key evil-normal-state-map " " 'fcd/toggle-ui)


; ----------------------------------------------------------------------------- ;
;;; Code                                                                          ;
; ----------------------------------------------------------------------------- ;

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(defvar init-location (f-join (getenv "HOME") ".emacs.d"))
(load (f-join init-location "appearance.el"))
(load (f-join init-location "python.el"))

(when (require 'rust-mode nil 'no-error)
  (load (f-join init-location "rust.el")))

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
    (let* ((venv-locations `(,(expand-file-name "~/.virtualenvs") "c:/anaconda3/envs"))
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


(defun fcd/shell-python-version ()
  "Return Python version as a list e.g. (major minor micro)."
  (let ((fcd/python-version (shell-command-to-string "python -V")))
    (string-match "\\([0-9][0-9]*\\.[0-9][0-9]*\\.[0-9][0-9]*\\)" fcd/python-version)
    (split-string (match-string 0 fcd/python-version) "\\.")
    ))


(defun fcd/set-pylint-executable ()
  "Set the pylint executable to match current env Python version."
  (interactive)
  (when (eq system-type 'windows-nt)
    (let ((python-version (string-join (butlast (fcd/shell-python-version)) ".")))
      (progn
        (unless
            (cond ((string= python-version "2.7")
                   (message "pylint set for Python 2.7")
                   ;; (setq flycheck-python-pylint-executable "c:/anaconda3/envs/pylint27/scripts/pylint.exe"))
                   (setq flycheck-python-pylint-executable "c:/users/fda/repositories/tecc/main/external/python/python27/scripts/pylint.exe"))
                  ((string= python-version "3.5")
                   (message "pylint set for Python 3.5")
                   (setq flycheck-python-pylint-executable "c:/anaconda3/envs/pylint35/scripts/pylint.exe"))
                  ((string= python-version "3.6")
                   (message "pylint set for Python 3.6")
                   (setq flycheck-python-pylint-executable "c:/anaconda3/envs/pylint36/scripts/pylint.exe"))
                  ((string= python-version "3.7")
                   (message "pylint set for Python 3.7")
                   (setq flycheck-python-pylint-executable "c:/anaconda3/envs/pylint37/scripts/pylint.exe")))
          (message "pylint not available for Python %s" python-version))))))


(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--colors=Linux --simple-prompt"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
 )


(defun set-elisp-ac-sources ()
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers)))


(defun set-python-ac-sources ()
  "Only use jedi as auto-complete source."
  (setq ac-sources '(ac-source-jedi-direct)))


(when (eq system-type 'windows-nt)
  (setenv "PATH"
          (concat
           "C:/Users/fda/repositories/TECC/main/Start;"
           "C:/Users/fda/repositories/TECC/main/External/Python/Python27;"
           "C:/Users/fda/repositories/TECC/main/External/Python/Python27/Scripts;"
           "C:/Users/fda/bin/GnuWin32/bin;"
           "C:/Program Files/Git/usr/bin;"
           (getenv "PATH")))
  (setq exec-path
        (append '("C:/Users/fda/bin/GnuWin32/bin"
                  "C:/Program Files/Git/usr/bin"
                  "C:/Users/fda/repositories/TECC/main/External/Python/Python27/Scripts")
                exec-path)))


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
(add-hook 'find-file-hook 'recentf-save-list)
(setq recentf-max-menu-items 500)
(setq recentf-max-saved-items nil)
(global-set-key "\C-x\ \C-r" 'helm-recentf)



(setq bookmark-save-flag 1)


; dired settings
(setq dired-listing-switches "-aBhl  --group-directories-first")

; Make underscore delimit words
(modify-syntax-entry ?_ "w" (standard-syntax-table))
; Make hyphen delimit words in elisp
(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)


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

(setq js-indent-level 2)


(defun fcd/set-ui-appearancce ()
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

(defun fcd/set-ui-after-make-frame (frame)
  (fcd/set-ui-appearancce))


(defun fcd/duplicate-window-vertically ()
  (interactive)
  "Make two windows vertically split focussed on current buffer."
  (delete-other-windows)
  (split-window-right)
  (other-window 1))


                                        ; Only do stuff with the UI after frames exist. Frames don't exist when daemon starts
                                        ; Run it as well for the case when the frame is already created before hook is added
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
  (setq insert-directory-program "/usr/local/bin/gls")
  (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
  (define-key evil-normal-state-map (kbd "M-3" ) 'evil-search-word-backward)
  (define-key isearch-mode-map (kbd "M-3") '(lambda () (interactive) (isearch-process-search-char ?\#))))

(global-set-key (kbd "C-c c o") 'fcd/tfs-checkout-and-make-writeable)
(global-set-key (kbd "C-c c s") 'fcd/tfs-status)
(global-set-key (kbd "C-c f") 'fcd/show-buffer-file-name)
(global-set-key (kbd "C-c i") 'fcd/open-init-file)
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c b") 'helm-projectile-find-file)

(global-set-key (kbd "C-c .") 'xref-find-definitions)
(global-set-key (kbd "C-c ,") 'xref-pop-marker-stack)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

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
(define-key global-map (kbd "C-h F") 'find-function)
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
    (avy-flycheck flycheck-rust flycheck auctex dash-functional swiper aio flycheck-demjsonlint rjsx-mode edit-indirect markdown-mode command-log-mode esup avy yasnippet-snippets yasnippet-bundle ac-helm yasnippet auto-dim-other-buffers jedi csv-mode helm-swoop magit web-mode auto-virtualenvwrapper evil-commentary helm-projectile smartparens evil-leader leuven-theme use-package nlinum-relative helm fuzzy flatui-theme exec-path-from-shell evil-tabs evil-surround))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "firebrick" :slant italic))))
 '(hl-line ((t (:background "#F6FECD")))))
