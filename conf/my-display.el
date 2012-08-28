;;; my-display.el --- Display settings
;;; -*- lexical-bind: t -*-

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(require 'my-utils)

(custom-set-variables
 '(column-number-mode t)
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(frame-title-format "emacs %@ %b (%f)")
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-visual-line-mode t)
 '(line-number-mode nil)
 '(proced-format 'medium)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(uniquify-buffer-name-style 'post-forward-angle-brackets
			      nil (uniquify))
 '(winner-mode t))

(add-hook 'text-mode-hook (lambda () "Set proportional font."
			    (variable-pitch-mode t)))

;;; sml-modeline
(if (require 'sml-modeline nil t)
    (if +old-emacs+
	(ignore-errors (sml-modeline-mode 1))
      (sml-modeline-mode 1)))

;;; fullscreen stuff
(defvar *fullscreen-p* nil "Check if fullscreen is on or off.")

(defun fullscreen-toggle ()
  "Toggle fullscreen view on and off."
  (interactive)
  (if (setq *fullscreen-p* (not *fullscreen-p*))
      (win-or-nix
       (w32-send-sys-command 61488)    ; WM_SYSCOMMAND maximize #xf030
       (set-frame-parameter nil 'fullscreen 'fullboth))
    (win-or-nix
     (w32-send-sys-command 61728)	; WM_SYSCOMMAND restore #xf120
     (set-frame-parameter nil 'width 100)
     (set-frame-parameter nil 'fullscreen 'fullheight))))

;;; TabBar
(when (require 'tabbar nil t)
  (defadvice tabbar-buffer-help-on-tab (after tabbar-add-file-path
					      activate compile)
    "Attach full file path to help message.
If not a file, attach current directory."
    (let* ((tab-buffer (tabbar-tab-value tab))
	   (full-path (buffer-file-name tab-buffer)))
      (if full-path
	  (setq ad-return-value (concat full-path "\n"
					ad-return-value))
	(with-current-buffer tab-buffer
	  (setq ad-return-value (concat default-directory "\n"
					ad-return-value))))))

  (defadvice tabbar-buffer-groups (around tabbar-groups-extension
					  activate compile)
    "Add some rules for grouping tabs to run before original."
    (cond
     ((memq major-mode '(woman-mode completion-list-mode
				    slime-fuzzy-completions-mode))
      (setq ad-return-value (list "Help")))
     ((eq major-mode 'asdf-mode)
      (setq ad-return-value (list "Lisp")))
     ((string-match-p "^*tramp" (buffer-name))
      (setq ad-return-value (list "Tramp")))
     ((string-match-p "^*helm" (buffer-name))
      (setq ad-return-value (list "Helm")))
     ((string-match-p "^emms" (symbol-name major-mode))
      (setq ad-return-value (list "EMMS")))
     ((string-match-p "^*inferior" (buffer-name))
      (setq ad-return-value (list "Process")))
     ((string-match-p "^*slime" (buffer-name))
      (setq ad-return-value (list "Slime")))
     ((memq major-mode '(fundamental-mode org-mode))
      (setq ad-return-value (list "Common")))
     (t ad-do-it)))	      ; if none of above applies, run original

  (if +old-emacs+
      (ignore-errors (tabbar-mode 1))
    (tabbar-mode 1))

  (setq-default mode-line-buffer-identification "")

  (defun next-tab (arg)
    "Go to next tab. With prefix, next group."
    (interactive "P")
    (if arg (tabbar-forward-group)
      (tabbar-forward-tab)))

  (defun prev-tab (arg)
    "Go to previous tab. With prefix, previous group."
    (interactive "P")
    (if arg (tabbar-backward-group)
      (tabbar-backward-tab)))

  (global-set-key (kbd "C-<tab>") 'next-tab)
  (global-set-key (win-or-nix (kbd "C-S-<tab>")
			      (kbd "<C-S-iso-lefttab>"))
		  'prev-tab)

  (when-library
   t org
   (add-hook 'org-load-hook
	     (lambda () "Allow tabbar keys in Org."
	       (define-key org-mode-map (kbd "C-<tab>") nil)))))

(provide 'my-display)

;;; my-display.el ends here
