;;; my-themes.el --- My appearance settings
;;; -*- lexical-bind: t -*-

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

(when (and (if +old-emacs+
	       (and (load-theme 'andr)
		    (load-theme 'andr-dark))
	     (and (load-theme 'andr t t)
		  (load-theme 'andr-dark t t)))
	   (require 'rase nil t))

  (if (and (not +old-emacs+)
	   (ignore-errors (load-theme 'zenburn t)))
      (enable-theme 'zenburn))

  (defmacro my-switch-colours (&optional light)
    "Switch themes.  If LIGHT is not given, let it be dark."
    (if light
	'(progn (ignore-errors (disable-theme 'zenburn))
		(enable-theme 'andr))
      '(progn (enable-theme 'andr-dark)
	      (ignore-errors (enable-theme 'zenburn)))))

  (defun my-switch-themes (sun-event first-run)
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

  (add-to-list 'rase-hook 'my-switch-themes)
  (rase-start t))

(provide 'my-themes)

;;; my-themes.el ends here
