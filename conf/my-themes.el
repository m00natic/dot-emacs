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

  (defun my-switch-themes (sun-event first-run)
    "Switch themes on SUN-EVENT sunrise and sunset even on FIRST-RUN."
    (cond ((eq sun-event 'sunrise)
	   (enable-theme 'andr))
	  ((eq sun-event 'sunset)
	   (enable-theme 'andr-dark))
	  ((consp first-run)
	   (if (< (length (memq 'sunset first-run))
		  (length (memq 'sunrise first-run)))
	       (enable-theme 'andr)
	     (enable-theme 'andr-dark)))
	  (first-run
	   (if (eq sun-event 'midday)
	       (enable-theme 'andr)
	     (enable-theme 'andr-dark)))))

  (add-to-list 'rase-hook 'my-switch-themes)
  (rase-start t))

(provide 'my-themes)

;;; my-themes.el ends here
