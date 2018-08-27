;;; appearance.el --- Summary
; Collection of functions relating to editor appearance

;;; Commentary:
;-

;;; Code:


(defvar current-ui-state nil
  "Variable for communicating current ui state to new buffers.")


(defun fcd/set-ui (change-to-state)
  (cond
   ((equalp change-to-state "no-mode-line")
    (mapcar 'fcd/hide-mode-line (buffer-list))
    (setq current-ui-state "no-mode-line")
    )
   ((equalp change-to-state "mode-line-showing")
    (mapcar 'fcd/show-mode-line (buffer-list))
    (setq current-ui-state "mode-line-showing"))))

(defun fcd/set-face-font ()
  (when (eq system-type 'windows-nt)
    (set-face-font (quote default) "-outline-Consolas-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1")))


(defun fcd/toggle-ui ()
  (interactive)
  (cond ((equalp current-ui-state "mode-line-showing") (fcd/set-ui "no-mode-line"))
	((equalp current-ui-state "no-mode-line") (fcd/set-ui  "mode-line-showing"))))


(defun fcd/toggle-global-nlinum-relative ()
  (interactive)
  (if nlinum-mode
      (global-nlinum-mode 0)
    (progn
      (global-nlinum-mode t)
      (global-nlinum-relative-mode t))))


(defun fcd/set-ui-to-current-ui-state ()
  (fcd/set-ui current-ui-state)
  )


(defun fcd/hide-mode-line (buffer)
  (with-current-buffer buffer
    (when (not (minibufferp buffer))
      (when (eq system-type 'windows-nt)
	(setq mode-line-format nil)
	(window-divider-mode +1)
	(setq window-divider-default-places t
	      window-divider-default-bottom-width 1
	      window-divider-default-right-width 1))
      (when (eq system-type 'darwin)
	(setq mode-line-format "")
	(set-face-attribute 'mode-line nil :height 0.1)
	(set-face-attribute 'mode-line-inactive nil :height 0.1)))))

(defun fcd/show-mode-line (buffer)
  (with-current-buffer buffer
    (when (not (minibufferp buffer))
      (progn
	(setq mode-line-format startup-mode-line-format)
	(set-face-attribute 'mode-line nil :height 1.0)
	(set-face-attribute 'mode-line-inactive nil :height 1.0)
	(force-mode-line-update)))))


(defun fcd/init-ui ()
  (set-face-attribute 'mode-line nil
		      :background "dark blue"
		      :height 0.9)
  (set-face-attribute 'window-divider nil
		      :foreground "#000000")
  (fcd/set-ui "no-mode-line")
  (global-nlinum-mode t)
  (global-nlinum-relative-mode t))


;;; Faces:
(setq unsaved-buffer-with-focus-background "Light blue")
(setq unsaved-buffer-without-focus-background "Dark grey")
(setq saved-buffer-with-focus-background "White")
(setq saved-buffer-without-focus-background "Light grey")

(defun fcd/highlight-selected-window ()
  "Highlight selected window with a different background color."
  (walk-windows (lambda (w)
		  (unless (eq w (selected-window))
		    (with-current-buffer (window-buffer w)
		      (buffer-face-set `(:background   ,saved-buffer-with-focus-background))))
		  )
		(buffer-face-set 'default)))

(provide 'appearance)
