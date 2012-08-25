;;; init.el --- Andrey Kotlarski's .emacs
;;; -*- lexical-bind: t -*-

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

;;; do some OS recognition
(defconst +win-p+ (eval-when-compile
		    (memq system-type '(windows-nt ms-dos)))
  "Windows detection.")

(defmacro win-or-nix (win &rest nix)
  "OS conditional.  WIN may be a list and is executed on windows systems.
NIX forms are executed on all other platforms."
  (if +win-p+
      (if (consp win)
	  (let ((form (car win)))
	    (cond ((not (consp form)) win)
		  ((cadr win) `(progn ,@win))
		  (t form)))
	win)
    (if (cadr nix) `(progn ,@nix)
      (car nix))))

;;; set some path constants.
(defconst +home-path+
  (win-or-nix
   (cond ((string-match "\\(.*[/\\]home[/\\]\\)" exec-directory)
	  (match-string 0 exec-directory)) ; usb
	 ((file-exists-p (concat exec-directory "../../home"))
	  (file-truename (concat exec-directory "../../home/")))
	 (t #1=(eval-when-compile (concat (getenv "HOME") "/"))))
   #1#)
  "Home path.")

(defconst +conf-path+ (win-or-nix
		       #2=(concat user-emacs-directory "conf/")
		       (eval-when-compile #2#))
  "Elisp configuration path.")

;;; add `+conf-path+' and subdirs to `load-path'
(add-to-list 'load-path +conf-path+)
(let ((default-directory +conf-path+))
    (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; load configurations

(require 'my-themes)
(require 'my-utils)
(require 'my-customize)

(if (require 'package) (package-initialize))

(require 'my-display)

(win-or-nix (require 'my-windows))
(when-library nil ergoemacs-mode (require 'my-ergo))

(require 'my-mail)
(require 'my-network)
(require 'my-lisp)
(require 'my-prog)
(require 'my-fun)
(require 'my-misc)

;;; init.el ends here
