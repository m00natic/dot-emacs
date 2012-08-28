;;; my-themes.el --- My appearance settings
;;; -*- lexical-bind: t -*-

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(custom-set-variables
 '(calendar-latitude 42.68)
 '(calendar-longitude 23.31)
 `(custom-theme-directory ,(concat +conf-path+ "themes"))
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

(when (if +old-emacs+
	  (and (load-theme 'andr)
	       (load-theme 'andr-dark))
	(and (load-theme 'andr t t)
	     (load-theme 'andr-dark t t)))

  (when +old-emacs+
    (disable-theme 'andr)
    (disable-theme 'andr-dark))

  (defmacro switch-faces (&optional light)
    "Set dark faces.  With prefix, LIGHT."
    (if light '(progn (disable-theme 'andr-dark)
		      (enable-theme 'andr))
      '(progn (disable-theme 'andr)
	      (enable-theme 'andr-dark))))

  ;; automatically switch faces on sunrise and sunset
  (when (require 'solar nil t)
    (defun solar-time-to-24 (time-str)
      "Convert solar type string TIME-STR to 24 hour format."
      (if (string-match "\\(.*\\)[/:-]\\(..\\)\\(.\\)" time-str)
	  (format "%02d:%s"
		  (if (string-equal (match-string 3 time-str) "p")
		      (+ 12 (string-to-number
			     (match-string 1 time-str)))
		    (string-to-number (match-string 1 time-str)))
		  (match-string 2 time-str))
	time-str))

    (defun my-colours-set ()
      "Set colours of new FRAME according to time of day.
Set timer that runs on next sunset or sunrise, whichever sooner."
      (if (and calendar-latitude calendar-longitude
	       calendar-time-zone)
	  (let ((solar-info (solar-sunrise-sunset
			     (calendar-current-date))))
	    (let ((sunrise-string (solar-time-string
				   (caar solar-info)
				   (car (cdar solar-info))))
		  (sunset-string (solar-time-string
				  (car (cadr solar-info))
				  (cadr (cadr solar-info))))
		  (current-time-string (format-time-string "%H:%M")))
	      (cond
	       ((string-lessp current-time-string ; before dawn
			      (solar-time-to-24 sunrise-string))
		(switch-faces)
		(run-at-time sunrise-string nil 'my-colours-set))
	       ((not (string-lessp current-time-string ; evening
				   (solar-time-to-24 sunset-string)))
		(switch-faces)
		(run-at-time
		 (let ((tomorrow (calendar-current-date 1)))
		   (let* ((next-solar-rise (car (solar-sunrise-sunset
						 tomorrow)))
			  (next-rise (solar-time-to-24
				      (solar-time-string
				       (car next-solar-rise)
				       (cadr next-solar-rise)))))
		     (encode-time 0 (string-to-number
				     (substring next-rise 3 5))
				  (string-to-number
				   (substring next-rise 0 2))
				  (cadr tomorrow) (car tomorrow)
				  (car (cddr tomorrow)))))
		 nil 'my-colours-set))
	       (t (switch-faces t)	; daytime
		  (run-at-time sunset-string nil 'my-colours-set)))))
	(switch-faces t)))

    (my-colours-set)))

(provide 'my-themes)

;;; my-themes.el ends here
