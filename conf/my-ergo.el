;;; my-ergo.el --- ErgoEmacs settings

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(require 'my-utils)

;;; ErgoEmacs minor mode
(custom-set-variables
 '(ergoemacs-keyboard-layout "colemak")
 '(ergoemacs-theme "lvl3"))

(when (require 'ergoemacs-mode nil t)
  (defun ergoemacs-change-keyboard (layout)
    "Change ErgoEmacs keyboard bindings according to LAYOUT."
    (interactive (list (completing-read "Enter layout (default us): "
					(ergoemacs-get-layouts)
					nil t nil nil "us")))

    (unless (string-equal layout ergoemacs-keyboard-layout)
      (setq ergoemacs-keyboard-layout layout)
      (ergoemacs-setup-keys)))

  ;; occur fixes
  (eval-after-load "replace"
    '(define-keys occur-mode-map
       "n" 'occur-next "p" 'occur-prev
       "o" 'occur-mode-display-occurrence
       "\C-c\C-c" 'occur-mode-goto-occurrence-other-window))

  (or +old-emacs+
      (when-library
       nil helm
       (ergoemacs-key "M-V" 'helm-show-kill-ring)
       (ergoemacs-fixed-key "C-o" 'find-file)))

  (when-library
   nil paredit
   (add-to-list
    'ergoemacs-minor-mode-layout-lvl3
    '(activate-lisp-minor-modes-hook
      (("M-r")
       ("M-s")
       ("M-S")
       ("M-J")
       ("M-;")
       ("<f9>" paredit-split-sexp)
       ("<f10>" paredit-raise-sexp)
       ("S-<f9>" paredit-join-sexps)
       ("S-<f10>" paredit-splice-sexp)
       (kill-visual-line paredit-kill)))))

  (when-library
   nil slime
   (add-to-list
    'ergoemacs-minor-mode-layout-lvl3
    '(slime-connected-hook
      (("<f11>" slime-next-note)
       ("<f12>" slime-previous-note))))

   (add-to-list
    'ergoemacs-minor-mode-layout-lvl3
    '(slime-repl-mode-hook
      (("M-r")
       ("M-s")
       ("<f11>" slime-repl-previous-input)
       ("<f12>" slime-repl-next-input)
       ("S-<f11>" slime-repl-previous-matching-input)
       ("S-<f12>" slime-repl-next-matching-input)))))

  (delete "C-d" ergoemacs-redundant-keys)
  (delete "C-j" ergoemacs-redundant-keys)
  (delete "C-t" ergoemacs-redundant-keys)

  (ergoemacs-fixed-key "C-f" 'search-forward-regexp)
  (ergoemacs-fixed-key "C-S-f" 'search-backward-regexp)
  (ergoemacs-replace-key 'backward-paragraph "C-M-u")

  (ergoemacs-fixed-key "<M-up>" nil)
  (ergoemacs-fixed-key "<M-down>" nil)

  ;; activate
  (ergoemacs-mode 1))

(provide 'my-ergo)

;;; my-ergo.el ends here
