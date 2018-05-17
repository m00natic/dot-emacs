;;; my-ergo.el --- ErgoEmacs settings

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(require 'my-utils)

;;; ErgoEmacs minor mode
(custom-set-variables
 '(ergoemacs-keyboard-layout "colemak")
 '(ergoemacs-mode-line 'no-layout))

(when (require 'ergoemacs-mode nil t)
  (defun ergoemacs-change-keyboard (layout)
    "Change ErgoEmacs keyboard bindings according to LAYOUT."
    (interactive (list (completing-read "Enter layout (default us): "
					(ergoemacs-layouts--list)
					nil t nil nil "us")))

    (unless (string-equal layout ergoemacs-keyboard-layout)
      (setq ergoemacs-keyboard-layout layout)
      (ergoemacs-mode-reset)))

  (or +old-emacs+
      (when-library nil helm
		    (global-set-key "\M-V" 'helm-show-kill-ring)))

  (setq ergoemacs-handle-ctl-c-or-ctl-x 'only-C-c-and-C-x
	ergoemacs-use-ergoemacs-metaleft nil
	ergoemacs-use-ergoemacs-metaright nil)

  (ergoemacs-theme-option-off '(apps apps-apps apps-punctuation
				     apps-toggle apps-swap
				     save-options-on-exit ido-remaps
				     menu-bar-view))

  (or +old-emacs+ (require 'helm-config nil t))
  (defvar tabbar-mode nil)

  ;; activate
  (ergoemacs-mode 1))

(provide 'my-ergo)

;;; my-ergo.el ends here
