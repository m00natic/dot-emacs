;;; my-ergo.el --- ErgoEmacs settings
;;; -*- lexical-bind: t -*-

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(require 'my-utils)

;;; ErgoEmacs minor mode
(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "colemak")

(when (require 'ergoemacs-mode nil t)
  (global-set-key "\C-@" 'cua-set-mark)
  (ergoemacs-key "M-2" 'move-cursor-previous-pane)
  (ergoemacs-key "M-@" 'move-cursor-next-pane)
  (ergoemacs-key "M-<" 'beginning-of-buffer)
  (ergoemacs-key "M->" 'end-of-buffer)

  (defun ergoemacs-change-keyboard (layout)
    "Change ErgoEmacs keyboard bindings according to LAYOUT."
    (interactive (list (completing-read "Enter layout (default us): "
					(ergoemacs-get-layouts)
					nil t nil nil "us")))

    (unless (string-equal layout ergoemacs-keyboard-layout)
      (setq ergoemacs-keyboard-layout layout)
      (ergoemacs-setup-keys)))

;;; fixes for some modes
  (when-library
   t doc-view
   (add-to-list
    'ergoemacs-minor-mode-layout
    '(doc-view-mode-hook
      ((isearch-forward doc-view-search doc-view-mode-map)
       (isearch-backward doc-view-search-backward doc-view-mode-map)
       (next-line doc-view-next-line-or-next-page doc-view-mode-map)
       (previous-line doc-view-previous-line-or-previous-page
		      doc-view-mode-map)))))

  ;; occur fixes
  (eval-after-load "replace"
    '(define-keys occur-mode-map
       "n" 'occur-next "p" 'occur-prev
       "o" 'occur-mode-display-occurrence
       "\C-c\C-c" 'occur-mode-goto-occurrence-other-window))

  (when-library
   nil browse-kill-ring
   (ergoemacs-key "M-V" 'browse-kill-ring)
   (ergoemacs-minor-key 'minibuffer-setup-hook
			'("M-V" yank-pop
			  minor-mode-overriding-map-alist)))

  (when-library
   nil paredit
   (add-to-list
    'ergoemacs-minor-mode-layout
    '(activate-lisp-minor-modes-hook
      (("M-r" nil paredit-mode-map)
       ("M-R" nil paredit-mode-map)
       ("M-S" nil paredit-mode-map)
       ("M-J" nil paredit-mode-map)
       ("M-;" nil paredit-mode-map)
       ("<f9>" paredit-split-sexp paredit-mode-map)
       ("<f10>" paredit-raise-sexp paredit-mode-map)
       ("S-<f9>" paredit-join-sexps paredit-mode-map)
       ("S-<f10>" paredit-splice-sexp paredit-mode-map)
       (comment-dwim paredit-comment-dwim paredit-mode-map)
       (backward-kill-word paredit-backward-kill-word
			   paredit-mode-map)
       (forward-kill-word paredit-forward-kill-word paredit-mode-map)
       (delete-backward-char paredit-backward-delete paredit-mode-map)
       (delete-cha paredit-forward-delete paredit-mode-map)
       (kill-line paredit-kill paredit-mode-map)))))

  (when-library
   nil slime
   (add-to-list
    'ergoemacs-minor-mode-layout
    '(slime-connected-hook
      (("M-n" nil slime-mode-map)
       ("M-p" nil slime-mode-map)
       ("<f11>" slime-next-note slime-mode-map)
       ("<f12>" slime-previous-note slime-mode-map))))

   (add-to-list
    'ergoemacs-minor-mode-layout
    '(slime-repl-mode-hook
      (("M-n" nil slime-repl-mode-map)
       ("M-p" nil slime-repl-mode-map)
       ("M-r" nil slime-repl-mode-map)
       ("M-s" nil slime-repl-mode-map)
       ("<f11>" slime-repl-previous-input slime-repl-mode-map)
       ("<f12>" slime-repl-next-input slime-repl-mode-map)
       ("S-<f11>" slime-repl-previous-matching-input
	slime-repl-mode-map)
       ("S-<f12>" slime-repl-next-matching-input
	slime-repl-mode-map)))))

  ;; activate
  (if +old-emacs+
      (ignore-errors (ergoemacs-mode 1))
    (ergoemacs-mode 1))

  (ergoemacs-global-set-key "\C-f" 'search-forward-regexp)
  (ergoemacs-global-set-key "\C-F" 'search-backward-regexp))

(provide 'my-ergo)

;;; my-ergo.el ends here
