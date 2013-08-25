;;; my-mail.el --- Mail and news settings

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(require 'my-utils)

(custom-set-variables
 '(gnus-buttonized-mime-types '("multipart/.*"))
 '(gnus-posting-styles '((".*" (address "m00naticus@gmail.com")
			  ("X-SMTP-Server" "smtp.gmail.com"))
			 ("vayant" (address "akotlarski@vayant.com")
			  ("X-SMTP-Server" "mail.vayant.com"))))
 '(gnus-select-method '(nntp "news.gmane.org"))
 '(gnus-secondary-select-methods
   '((nnimap "gmail" (nnimap-address "imap.gmail.com"))
     (nnimap "vayant" (nnimap-address "mail.vayant.com"))))
 '(mail-envelope-from 'header)
 '(mail-specify-envelope-from t)
 '(message-citation-line-format "[ %e %B %Y, %R %z, %A ] %N:\n")
 '(message-citation-line-function
   'message-insert-formatted-citation-line)
 '(message-send-mail-function 'smtpmail-send-it)
 '(mm-inline-large-images 'resize)
 '(smtpmail-smtp-service 587))

;;; Gnus
(when-library
 t gnus
 (defvar *gnus-new-mail-count* "" "Unread messages count.")
 (put '*gnus-new-mail-count* 'risky-local-variable t)
 (add-to-list 'global-mode-string '*gnus-new-mail-count* t 'eq)

 (eval-after-load "gnus"
   `(progn
      (defun gnus-demon-notify (&optional notify)
	"When NOTIFY check for more unread mails.
Otherwise check for less."
	(and (or notify (not (string-equal *gnus-new-mail-count* "")))
	     (gnus-alive-p)
	     (let ((unread-count 0)
		   unread-groups)
	       (dolist (group '("nnimap+gmail:INBOX"
				"nnimap+vayant:INBOX"))
		 (let ((unread (gnus-group-unread group)))
		   (and (numberp unread) (> unread 0)
			(setq unread-count (+ unread-count unread)
			      unread-groups (concat unread-groups
						    ", " group)))))
	       (setq *gnus-new-mail-count*
		     (if (null unread-groups) ""
		       (win-or-nix
			nil
			(when-library
			 nil notify
			 (if (> unread-count (string-to-number
					      *gnus-new-mail-count*))
			     (notify
			      "Gnus"
			      (format
			       "%d new mail%s in %s"
			       unread-count
			       (if (= unread-count 1) "" "s")
			       (substring unread-groups 2))))))
		       (propertize (format "%d" unread-count)
				   'face 'font-lock-warning-face))))))

      (defun gnus-demon-scan-important ()
	"Check for new messages in level 1 and notify in modeline."
	(when (gnus-alive-p)
	  (let ((method (gnus-server-to-method "nnimap:vayant")))
	    (gnus-close-server method)	; reopen vayant
	    (gnus-open-server method))
	  (let ((win (current-window-configuration)))
	    (unwind-protect
		(save-window-excursion
		  (with-current-buffer gnus-group-buffer
		    (gnus-group-get-new-news 1))
		  (gnus-demon-notify t))
	      (set-window-configuration win)))))

      (gnus-demon-add-handler 'gnus-demon-scan-important 10 nil)
      (gnus-demon-add-handler 'gnus-demon-notify 1 nil)
      ;; run (gnus-demon-init) to track emails
      (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
      (add-hook 'kill-emacs-hook (lambda () "Quit Gnus."
				   (setq gnus-interactive-exit nil)
				   (gnus-group-exit))))))

(when-library
 t message
 (add-hook 'message-mode-hook 'flyspell-mode)

 (defun set-smtp-server-from-header ()
   "Set smtp server according to the `X-SMTP-Server' header.
If missing, try to deduce it from the `From' header."
   (let ((from ""))
     (save-restriction
       (message-narrow-to-headers)
       (setq smtpmail-smtp-server
	     (message-fetch-field "X-SMTP-Server")
	     from (message-fetch-field "from")))
     (cond (smtpmail-smtp-server
	    (message-remove-header "X-SMTP-Server"))
	   ((string-match "@\\([^ >]*\\)" from)
	    (let ((domain (match-string-no-properties 1 from)))
	      (setq smtpmail-smtp-server
		    (if (string-equal domain "vayant.com")
			"mail.vayant.com"
		      (concat "smtp." domain))))))))

 (add-hook 'message-send-mail-hook 'set-smtp-server-from-header)

 (when-library
  t fortune
  (when (executable-find "fortune")
    (eval-after-load "fortune"
      (let ((fortune-d (concat user-emacs-directory "fortune/")))
	`(if (file-exists-p ,fortune-d)
	     (setq fortune-dir ,fortune-d
		   fortune-file ,(concat fortune-d "sigs")))))
    (add-hook 'message-signature-setup-hook 'fortune-to-signature))))

;;; The insidious Big Brother Database
(when-library
 nil bbdb
 (eval-after-load "gnus"
   '(progn (setq bbdb-file (concat user-emacs-directory ".bbdb"))
 	   (bbdb-initialize 'gnus 'message)
 	   (define-key message-mode-map "\C-b" 'bbdb-complete-mail))))

(provide 'my-mail)

;;; my-mail.el ends here
