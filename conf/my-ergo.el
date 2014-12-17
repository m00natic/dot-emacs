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
					(ergoemacs-get-layouts)
					nil t nil nil "us")))

    (unless (string-equal layout ergoemacs-keyboard-layout)
      (setq ergoemacs-keyboard-layout layout)
      (ergoemacs-setup-keys)))

  (or +old-emacs+
      (when-library nil helm
		    (global-set-key "\M-V" 'helm-show-kill-ring)))

  (when-library
   nil slime
   (add-hook 'slime-connected-hook
	     (lambda () (ergoemacs-define-overrides
		    (define-keys slime-mode-map
		      (kbd "<f11>") 'slime-next-note
		      (kbd "<f12>") 'slime-previous-note)
		    (define-keys slime-repl-mode-map
		      "\M-r" nil "\M-s" nil
		      (kbd "<f11>") 'slime-repl-previous-input
		      (kbd "<f12>") 'slime-repl-next-input
		      (kbd "S-<f11>")
		      'slime-repl-previous-matching-input
		      (kbd "S-<f12>")
		      'slime-repl-next-matching-input)))))

  (setq ergoemacs-handle-ctl-c-or-ctl-x 'only-C-c-and-C-x
	ergoemacs-use-ergoemacs-metaleft nil
	ergoemacs-use-ergoemacs-metaright nil)

  (ergoemacs-theme-option-off '(apps apps-apps apps-punctuation
				     apps-swap save-options-on-exit))

  (or +old-emacs+ (require 'helm-config nil t))

  ;; activate
  (ergoemacs-mode 1))

(provide 'my-ergo)

;;; my-ergo.el ends here
