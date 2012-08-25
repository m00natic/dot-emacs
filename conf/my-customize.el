;;; my-customize.el --- My customizations

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
 `(ido-save-directory-list-file
   ,(win-or-nix #1=(concat user-emacs-directory ".ido.last")
		(eval-when-compile #1#)))
 '(ido-use-virtual-buffers t)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'org-mode)
 '(initial-scratch-message nil)
 '(ispell-dictionary "en")
 '(org-src-fontify-natively t)
 '(package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
		      ("elpa" . "http://tromey.com/elpa/")
		      ("melpa" . "http://melpa.milkbox.net/packages/")
		      ("marmalade" .
		       "http://marmalade-repo.org/packages/")))
 '(read-file-name-completion-ignore-case t)
 '(recentf-max-saved-items 100)
 '(recentf-mode t)
 `(recentf-save-file
   ,(win-or-nix #2=(concat user-emacs-directory "recentf")
		(eval-when-compile #2#)))
 '(require-final-newline t)
 '(save-place t nil (saveplace))
 `(save-place-file
   ,(win-or-nix #3=(concat user-emacs-directory ".emacs-places")
		(eval-when-compile #3#)))
 '(version-control t)
 '(view-read-only t))

;;; some keybindings
(global-set-key "\C-cl" 'goto-line)
(global-set-key (kbd "M-RET") 'newline-and-indent)

;;; Use y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;;; backup
(setq backup-directory-alist
      `(("." . ,(win-or-nix #4=(concat user-emacs-directory "backup/")
			    (eval-when-compile #4#))))
      tramp-backup-directory-alist backup-directory-alist)

;;; ido and subword
(when-library t (ido subword)
	      (add-hook 'ido-minibuffer-setup-hook
			(lambda () (subword-mode -1))))

;;; add custom bin to path
(let ((bin-path (win-or-nix #5=(concat +conf-path+ "bin")
			    (eval-when-compile #5#))))
  (if (file-exists-p bin-path) (add-to-list 'exec-path bin-path)))

(provide 'my-customize)

;;; my-customize.el ends here
