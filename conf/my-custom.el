;;; my-custom.el --- My customizations

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(custom-set-variables
 '(cua-enable-cua-keys nil)
 '(cua-mode t)
 '(default-input-method "bulgarian-phonetic")
 '(delete-old-versions t)
 '(global-hl-line-mode t)
 '(global-prettify-symbols-mode t)
 '(global-subword-mode t)
 '(global-visual-line-mode t)
 '(ido-enable-flex-matching t)
 '(ido-mode 'both)
 `(ido-save-directory-list-file ,(concat user-emacs-directory
					 ".ido.last"))
 '(ido-use-virtual-buffers t)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'org-mode)
 '(initial-scratch-message nil)
 '(ispell-program-name "aspell")
 '(ispell-dictionary "en")
 '(jit-lock-defer-time 0.2)
 '(nxml-sexp-element-flag t)
 '(org-src-fontify-natively t)
 '(org-use-speed-commands t)
 '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
		      ("elpa" . "http://tromey.com/elpa/")
		      ("melpa" . "http://melpa.org/packages/")
		      ("melpa-stable" .
		       "http://stable.melpa.org/packages/")
		      ("marmalade" .
		       "https://marmalade-repo.org/packages/")))
 '(proced-format 'medium)
 '(read-file-name-completion-ignore-case t)
 '(recentf-max-saved-items 100)
 '(recentf-mode t)
 `(recentf-save-file ,(concat user-emacs-directory "recentf"))
 '(save-place t nil (saveplace))
 `(save-place-file ,(concat user-emacs-directory ".emacs-places"))
 '(show-paren-mode t)
 '(uniquify-buffer-name-style 'post-forward-angle-brackets
			      nil (uniquify))
 '(version-control t)
 '(view-read-only t)
 '(visible-bell t)
 '(winner-mode t))

;;; open files in view-mode
;; (add-hook 'find-file-hook 'view-mode)

;;; text mode
(add-hook 'text-mode-hook
	  (lambda () "Set proportional font, auto-fill and spell-checking."
	    (variable-pitch-mode t)
	    (turn-on-auto-fill)
	    (turn-on-flyspell)))

;;; miscellaneous
(fset 'yes-or-no-p 'y-or-n-p)
(put 'dired-find-alternate-file 'disabled nil)

;;; backup
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

;;; ido and subword
(add-hook 'ido-minibuffer-setup-hook (lambda () (subword-mode -1)))

;;; add custom bin to path
(let ((bin-path (concat +conf-path+ "bin")))
  (if (file-exists-p bin-path) (add-to-list 'exec-path bin-path)))

;;; eshell
(if +old-emacs+
    (custom-set-variables
     '(eshell-directory-name (concat user-emacs-directory
				     "eshell/"))))

;;; windmove
(windmove-default-keybindings)
;; Make windmove work in org-mode
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;;; spell-check
(defadvice toggle-input-method (after switch-dictionary
				      activate compile)
  "Change dictionary on input method switch."
  (ignore-errors
    (ispell-change-dictionary (if current-input-method "bg"
				ispell-dictionary))))

(provide 'my-custom)

;;; my-custom.el ends here
