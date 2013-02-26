;;; my-custom.el --- My customizations  -*- lexical-binding: t -*-

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(require 'my-utils)

(custom-set-variables
 '(cua-enable-cua-keys nil)
 '(cua-mode t)
 '(default-input-method "bulgarian-phonetic")
 '(delete-old-versions t)
 '(electric-pair-mode t)
 '(global-subword-mode t)
 '(icomplete-mode t)
 '(ido-enable-flex-matching t)
 '(ido-mode 'both)
 `(ido-save-directory-list-file ,(concat user-emacs-directory
					 ".ido.last"))
 '(ido-use-virtual-buffers t)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'org-mode)
 '(initial-scratch-message nil)
 '(ispell-dictionary "en")
 '(jit-lock-defer-time 0.2)
 '(nxml-sexp-element-flag t)
 '(org-src-fontify-natively t)
 '(package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
		      ("elpa" . "http://tromey.com/elpa/")
		      ("melpa" . "http://melpa.milkbox.net/packages/")
		      ("marmalade" .
		       "http://marmalade-repo.org/packages/")))
 '(read-file-name-completion-ignore-case t)
 '(recentf-max-saved-items 100)
 '(recentf-mode t)
 `(recentf-save-file ,(concat user-emacs-directory "recentf"))
 '(save-place t nil (saveplace))
 `(save-place-file ,(concat user-emacs-directory ".emacs-places"))
 '(version-control t)
 '(view-read-only t))

(global-set-key (kbd "M-RET") 'newline-and-indent)

;;; goto line
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting \
for the line number input."
  (interactive)
  (unwind-protect
      (progn (linum-mode 1)
	     (let ((line (read-number "Goto line: ")))
	       (goto-char (point-min))
	       (forward-line (1- line))))
    (linum-mode -1)))

(global-set-key "\C-cl" 'goto-line-with-feedback)

;;; Use y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;;; backup
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

;;; ido and subword
(when-library t ido
	      (when-library nil subword
			    (add-hook 'ido-minibuffer-setup-hook
				      (lambda () (subword-mode -1)))))

;;; add custom bin to path
(let ((bin-path (concat +conf-path+ "bin")))
  (if (file-exists-p bin-path) (add-to-list 'exec-path bin-path)))

;;; eshell
(if +old-emacs+
    (custom-set-variables
     '(eshell-directory-name (concat user-emacs-directory
				     "eshell/"))))

;;; windmove
(when-library
 t windmove
 (windmove-default-keybindings)

 ;; Make windmove work in org-mode
 (when-library
  t org-mode
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)))

(provide 'my-custom)

;;; my-custom.el ends here
