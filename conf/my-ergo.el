;;; my-ergo.el --- ErgoEmacs settings
;;; -*- lexical-bind: t -*-

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(require 'my-utils)

;;; ErgoEmacs minor mode
(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "colemak")

(when (require 'ergoemacs-mode nil t)
  (if (fboundp 'recenter-top-bottom)
      (define-key isearch-mode-map ergoemacs-recenter-key
	'recenter-top-bottom))
  (define-keys ergoemacs-keymap
    "\M-2" 'move-cursor-previous-pane
    "\M-@" 'move-cursor-next-pane
    "\C-f" 'search-forward-regexp
    "\C-F" 'search-backward-regexp
    "\M-<" 'beginning-of-buffer
    "\M->" 'end-of-buffer)

  (when-library
   t doc-view
   (add-hook 'doc-view-mode-hook
	     (lambda () "Set some Ergo keys."
	       (ergoemacs-local-set-key
		ergoemacs-isearch-forward-key 'doc-view-search)
	       (define-keys ergoemacs-local-keymap
		 ergoemacs-isearch-backward-key
		 'doc-view-search-backward
		 ergoemacs-next-line-key
		 'doc-view-next-line-or-next-page
		 ergoemacs-previous-line-key
		 'doc-view-previous-line-or-previous-page))))

  (defmacro ergoemacs-fix (layout)
    "Fix some keybindings when using ErgoEmacs."
    `(progn
       ,(if (fboundp 'recenter-top-bottom)
	    '(define-key ergoemacs-keymap ergoemacs-recenter-key
	       'recenter-top-bottom))
       (let ((ergo-layout ,layout))
	 ,(when-library
	   nil paredit
	   `(eval-after-load 'paredit
	      '(progn
		 (define-keys paredit-mode-map
		   ergoemacs-comment-dwim-key 'paredit-comment-dwim
		   ergoemacs-isearch-forward-key nil
		   ergoemacs-backward-kill-word-key
		   'paredit-backward-kill-word
		   ergoemacs-kill-word-key 'paredit-forward-kill-word
		   ergoemacs-delete-backward-char-key
		   'paredit-backward-delete
		   ergoemacs-delete-char-key 'paredit-forward-delete
		   ergoemacs-kill-line-key 'paredit-kill
		   ergoemacs-recenter-key nil
		   "\M-R" 'paredit-raise-sexp)
		 (if (string-equal ,layout "colemak")
		     (define-keys paredit-mode-map
		       "\M-r" 'paredit-splice-sexp
		       ergoemacs-next-line-key nil
		       "\M-g" nil)))))
	 ,(when-library
	   nil slime
	   '(cond ((string-equal ergo-layout "colemak")
		   (eval-after-load "slime"
		     '(define-keys slime-mode-map
			"\M-k" 'slime-next-note
			"\M-K" 'slime-previous-note
			"\M-n" nil
			"\M-p" nil))
		   (eval-after-load "slime-repl"
		     '(define-key slime-repl-mode-map "\M-n" nil)))
		  ((string-equal ergo-layout "en")
		   (eval-after-load "slime"
		     '(define-keys slime-mode-map
			"\M-N" 'slime-previous-note
			"\M-p" nil)))))
	 ,(when-library
	   nil helm-config
	   '(define-key ergoemacs-keymap ergoemacs-yank-pop-key
	      'helm-show-kill-ring)))))

  (defun ergoemacs-change-keyboard (layout)
    "Change ErgoEmacs keyboard bindings according to LAYOUT."
    (interactive (list (completing-read "Enter layout (default us): "
					'("us" "dv" "colemak")
					nil t nil nil "us")))
    (unless (string-equal layout ergoemacs-keyboard-layout)
      (ergoemacs-mode 0)
      (setenv "ERGOEMACS_KEYBOARD_LAYOUT" layout)
      (setq ergoemacs-keyboard-layout layout)
      (load "ergoemacs-mode")
      (ergoemacs-fix (getenv "ERGOEMACS_KEYBOARD_LAYOUT"))
      (ergoemacs-mode 1)))

  (when-library
   nil helm-config
   (defvar ergoemacs-minibuffer-keymap
     (copy-keymap ergoemacs-keymap))

   (defadvice ergoemacs-minibuffer-setup-hook
     (after ergoemacs-minibuffer-yank-pop activate compile)
     (define-keys ergoemacs-minibuffer-keymap
       ergoemacs-yank-pop-key 'yank-pop
       "\M-SPC" 'helm-mark-candidate)))

  (ergoemacs-fix (getenv "ERGOEMACS_KEYBOARD_LAYOUT"))
  (ergoemacs-mode 1)
  (global-set-key "\C-@" 'cua-set-mark))

(provide 'my-ergo)

;;; my-ergo.el ends here
