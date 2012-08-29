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

  (defmacro switch-faces (&optional light)
    "Set dark faces.  With prefix, LIGHT."
    (if light '(enable-theme 'andr)
      '(enable-theme 'andr-dark)))

  ;; automatically switch faces on sunrise and sunset
  (when (and (require 'solar nil t)
	     calendar-latitude calendar-longitude calendar-time-zone)

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

    (defun my-colours-switch (event)
      "Switch colours according to sun EVENT.
Set timer to do another switch at the next sun event."
      (if (eq event 'sunrise)
	  (progn (switch-faces t)
		 (let* ((solar-info (solar-sunrise-sunset
				     (calendar-current-date)))
			(sunset-string (solar-time-string
					(car (cadr solar-info))
					(cadr (cadr solar-info)))))
		   (run-at-time sunset-string nil
				'my-colours-switch 'sunset)))
	(switch-faces)
	(let* ((tomorrow (calendar-current-date 1))
	       (solar-rise (car (solar-sunrise-sunset tomorrow)))
	       (sunrise (solar-time-to-24 (solar-time-string
					   (car solar-rise)
					   (cadr solar-rise)))))
	  (run-at-time (encode-time 0 (string-to-number
				       (substring sunrise 3 5))
				    (string-to-number
				     (substring sunrise 0 2))
				    (cadr tomorrow) (car tomorrow)
				    (car (cddr tomorrow)))
		       nil 'my-colours-switch 'sunrise))))

    ;; set colours according to time of day
    ;; and timer that runs on next sunset or sunrise
    (let ((solar-info (solar-sunrise-sunset
		       (calendar-current-date))))
      (let ((sunrise-string (solar-time-string
			     (caar solar-info)
			     (car (cdar solar-info))))
	    (sunset-string (solar-time-string
			    (car (cadr solar-info))
			    (cadr (cadr solar-info))))
	    (current-time-string (format-time-string "%H:%M")))
	(cond ((string-lessp current-time-string ; before dawn
			     (solar-time-to-24 sunrise-string))
	       (switch-faces)
	       (run-at-time sunset-string nil
			    'my-colours-switch 'sunrise))
	      ((string-lessp current-time-string ; daytime
			     (solar-time-to-24 sunset-string))
	       (switch-faces t)
	       (run-at-time sunset-string nil
			    'my-colours-switch 'sunset))
	      (t (my-colours-switch 'sunset))))))) ; evening

(provide 'my-themes)

;;; my-themes.el ends here
