;;; my-themes.el --- My appearance settings

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(custom-set-variables
 '(calendar-latitude 42.7)
 '(calendar-longitude 23.3)
 `(custom-theme-directory ,(concat +conf-path+ "themes"))
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

;;; font alternatives
(add-to-list 'face-font-family-alternatives
	     '("Inconsolata" "Anonymous Pro" "terminus"))

(when (and (if +old-emacs+
	       (and (load-theme 'andr)
		    (load-theme 'andr-dark))
	     (and (load-theme 'andr t t)
		  (load-theme 'andr-dark t t)))
	   (require 'rase nil t))

  (or +old-emacs+ (ignore-errors (load-theme 'zenburn t t)
				 (load-theme 'anti-zenburn t t)))

  (defun my-switch-colours (&optional light)
    "Switch themes.  If LIGHT is not given, let it be dark."
    (if light
	(progn (enable-theme 'andr)
		(ignore-errors (enable-theme 'anti-zenburn)))
      (enable-theme 'andr-dark)
      (ignore-errors (enable-theme 'zenburn)))
    (if (ignore-errors (require 'powerline nil t))
	(powerline-reset)))

  (defun my-switch-themes (sun-event &optional first-run)
    "Switch themes on SUN-EVENT sunrise and sunset even on FIRST-RUN."
    (cond ((eq sun-event 'sunrise)
	   (my-switch-colours t))
	  ((eq sun-event 'sunset)
	   (my-switch-colours))
	  ((consp first-run)
	   (if (< (length (memq 'sunset first-run))
		  (length (memq 'sunrise first-run)))
	       (my-switch-colours t)
	     (my-switch-colours)))
	  (first-run
	   (if (eq sun-event 'midday)
	       (my-switch-colours t)
	     (my-switch-colours)))))

  (add-to-list 'rase-functions 'my-switch-themes)
  (rase-start t))

(provide 'my-themes)

;;; my-themes.el ends here
