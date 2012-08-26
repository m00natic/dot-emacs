;;; my-fun.el --- Media and games settings
;;; -*- lexical-bind: t -*-

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(require 'my-utils)

;;; EMMS
(when-library
 nil emms
 (autoload 'emms-browser "emms-browser"
   "Launch or switch to the EMMS Browser." t)
 (autoload 'emms "emms-playlist-mode"
   "Switch to the current emms-playlist buffer." t)

 (eval-after-load "emms"
   `(progn
      (emms-devel)
      (emms-default-players)
      (if (require 'emms-info-libtag nil t)
	  (add-to-list 'emms-info-functions 'emms-info-libtag
		       nil 'eq))
      (require 'emms-mark nil t)
      ;; swap time and other track info
      (let ((new-global-mode-string nil))
	(while (and (not (memq (car global-mode-string)
			       '(emms-mode-line-string
				 emms-playing-time-string)))
		    global-mode-string)
	  (push (car global-mode-string) new-global-mode-string)
	  (setq global-mode-string (cdr global-mode-string)))
	(setq global-mode-string
	      (nconc (nreverse new-global-mode-string)
		     '(emms-playing-time-string
		       emms-mode-line-string))))
      (add-hook 'emms-player-started-hook 'emms-show)

      (defun string-shift-left (str &optional offset)
	"Shift STR content to the left OFFSET characters."
	(or offset (setq offset 1))
	(let ((str-len (length str)))
	  (if (< offset str-len)
	      (concat (substring-no-properties str offset)
		      (substring-no-properties str 0 offset))
	    str)))

      (defun emms-tick-mode-line-description (offset)
	"Tick emms track description OFFSET characters."
	(setq emms-mode-line-string
	      (string-shift-left emms-mode-line-string offset)))

      (defun my-emms-default-info (track)
	"Get some generic meta data."
	(concat
	 "(" (number-to-string
	      (or (emms-track-get track 'play-count) 0))
	 ", " (emms-last-played-format-date
	       (or (emms-track-get track 'last-played) '(0 0 0)))
	 ")" (let ((time (emms-track-get track 'info-playing-time)))
	       (if time (format " %d:%02d" (/ time 60) (mod time 60))
		 ""))))

      (defun my-emms-track-description-function (track)
	"Return a description of the current TRACK."
	(let ((type (emms-track-type track)))
	  (cond
	   ((eq 'file type)
	    (let ((artist (emms-track-get track 'info-artist)))
	      (if artist
		  (concat
		   artist " - "
		   (let ((num (emms-track-get track
					      'info-tracknumber)))
		     (if num
			 (format "%02d. " (string-to-number num))
		       ""))
		   (or (emms-track-get track 'info-title)
		       (file-name-sans-extension
			(file-name-nondirectory
			 (emms-track-name track))))
		   " [" (let ((year (emms-track-get track
						    'info-year)))
			  (if year (concat year " - ")))
		   (or (emms-track-get track 'info-album) "unknown")
		   "]" (my-emms-default-info track))
		(concat (emms-track-name track)
			(my-emms-default-info track)))))
	   ((eq 'url type)
	    (emms-format-url-track-name (emms-track-name track)))
	   (t (concat (symbol-name type) ": " (emms-track-name track)
		      (my-emms-default-info track))))))

      (defun my-emms-covers (dir type)
	"Choose album cover in DIR deppending on TYPE.
Small cover should be less than 80000 bytes.
Medium - less than 120000 bytes."
	(let* ((pics (directory-files-and-attributes
		      dir t "\\.\\(jpe?g\\|png\\|gif\\|bmp\\)$" t))
	       (pic (car pics))
	       (pic-size (car (cddr (cddr (cddr (cddr pic)))))))
	  (let (temp)
	    (cond
	     ((eq type 'small)
	      (while (setq temp (cadr pics))
		(let ((temp-size (car (cddr (cddr (cddr
						   (cddr temp)))))))
		  (if (< temp-size pic-size)
		      (setq pic temp
			    pic-size temp-size)))
		(setq pics (cdr pics)))
	      (if (<= (or pic-size 80001) 80000)
		  (car pic)))
	     ((eq type 'medium)
	      (if (and pic (setq temp (cadr pics)))
		  (progn
		    (setq pics (cdr pics))
		    (let ((temp-size (car (cddr
					   (cddr
					    (cddr (cddr temp)))))))
		      (let ((small temp)
			    (small-size temp-size))
			(if (< pic-size small-size)
			    (setq small pic
				  small-size pic-size
				  pic temp
				  pic-size temp-size))
			(while (setq temp (cadr pics))
			  (setq temp-size
				(car (cddr (cddr (cddr
						  (cddr temp))))))
			  (cond
			   ((< temp-size small-size)
			    (setq pic small
				  pic-size small-size
				  small temp
				  small-size temp-size))
			   ((< temp-size pic-size)
			    (setq pic temp
				  pic-size temp-size)))
			  (setq pics (cdr pics)))
			(car (if (<= pic-size 120000) pic
			       small)))))
		(car pic)))
	     ((eq type 'large)
	      (while (setq temp (cadr pics))
		(let ((temp-size (car (cddr (cddr (cddr
						   (cddr temp)))))))
		  (if (> temp-size pic-size)
		      (setq pic temp
			    pic-size temp-size)))
		(setq pics (cdr pics)))
	      (car pic))))))

      (byte-compile 'string-shift-left)
      (byte-compile 'emms-tick-mode-line-description)
      (byte-compile 'my-emms-default-info)
      (byte-compile 'my-emms-track-description-function)
      (byte-compile 'my-emms-covers)

      (setq emms-show-format "EMMS: %s"
	    emms-mode-line-format "%s"
	    emms-source-file-default-directory
	    ,(win-or-nix #1=(concat +home-path+ "Music/")
			 (eval-when-compile #1#))
	    emms-track-description-function
	    'my-emms-track-description-function
	    emms-browser-covers 'my-emms-covers)

      (run-at-time t 2 'emms-tick-mode-line-description 5)

      (when (and (require 'my-secret "my-secret.el.gpg" t)
		 (require 'emms-lastfm-client nil t)
		 (let ((url-request-method "GET"))
		   (ignore-errors      ; check for internet connection
		     (url-retrieve-synchronously
		      "http://post.audioscrobbler.com"))))
	(setq emms-lastfm-client-username "m00natic"
	      emms-lastfm-client-api-key
	      my-emms-lastfm-client-api-key
	      emms-lastfm-client-api-secret-key
	      my-emms-lastfm-client-api-secret-key)
	(condition-case err
	    (emms-lastfm-scrobbler-enable)
	  (error (message "No scrobbling: %s" err))))

      (when (and (executable-find "mpd")
		 (require 'emms-player-mpd nil t))
	(add-to-list 'emms-info-functions 'emms-info-mpd nil 'eq)
	(add-to-list 'emms-player-list 'emms-player-mpd nil 'eq)
	(setq emms-player-mpd-music-directory
	      emms-source-file-default-directory)
	,(win-or-nix
	  nil
	  (when-library
	   nil notify
	   '(defadvice emms-player-started
	      (after emms-player-mpd-notify activate compile)
	      "Notify new track for MPD."
	      (if (eq emms-player-playing-p 'emms-player-mpd)
		  (notify
		   "EMMS"
		   (emms-track-description
		    (emms-playlist-current-selected-track))))))))

      (global-set-key [XF86AudioPlay] 'emms-pause)
      (global-set-key "\C-cp" 'emms-pause)
      ,(win-or-nix
	nil
	(when-library
	 nil notify
	 '(setq emms-player-next-function
		(byte-compile
		 (lambda () "Notify on new track."
		   (emms-next-noerror)
		   (if emms-player-playing-p
		       (notify
			"EMMS"
			(emms-track-description
			 (emms-playlist-current-selected-track))))))))))))

;;; chess
(when-library nil chess
	      (setq chess-sound-play-function nil))

;;; sudoku
(when-library
 nil sudoku
 (autoload 'sudoku "sudoku" "Start a sudoku game." t)
 (eval-after-load "sudoku"
   `(progn
      (setq sudoku-level "evil")
      ,(win-or-nix
	'(let ((wget-path (executable-find "wget")))
	   (if wget-path (setq sudoku-wget-process wget-path)))))))

(provide 'my-fun)

;;; my-fun.el ends here
