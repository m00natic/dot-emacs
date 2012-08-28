;;; init.el --- Andrey Kotlarski's .emacs
;;; -*- lexical-bind: t -*-

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

;;; do some OS recognition
(defconst +win-p+ (eval-when-compile
		    (memq system-type '(windows-nt ms-dos)))
  "Windows detection.")

(defconst +old-emacs+ (< (string-to-number emacs-version) 24)
  "Not using latest Emacs?")

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
	 (t #1=(concat (getenv "HOME") "/")))
   #1#)
  "Home path.")

(defconst +conf-path+ (concat user-emacs-directory "conf"
			      (if +old-emacs+ "-srv") "/")
  "Elisp configuration path.")

;;; add `+conf-path+' and subdirs to `load-path'
(add-to-list 'load-path +conf-path+)
(let ((default-directory +conf-path+))
  (normal-top-level-add-subdirs-to-load-path))

(if +old-emacs+
;;; add elpa and subdirs to `load-path'
    (let ((elpa-path (concat user-emacs-directory "elpa/")))
      (add-to-list 'load-path elpa-path)
      (let ((default-directory elpa-path))
	(normal-top-level-add-subdirs-to-load-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; load configurations

(require 'my-themes)
(require 'my-utils)
(require 'my-customize)

(if (require 'package nil t)
    (package-initialize)

  (defmacro require-multi (&rest packages)
    "Require PACKAGES."
    `(progn
       ,@(mapcar (lambda (package)
		   `(require ',package nil t))
		 packages)))

  (require-multi
   ace-jump-mode-autoloads auctex-autoloads auto-complete-autoloads
   clips-mode-autoloads clojure-mode-autoloads csharp-mode-autoloads
   cygwin-mount-autoloads dictionary-autoloads ecb_snap-autoloads
   emms-autoloads ergoemacs-keybindings-autoloads ess-autoloads
   ghc-autoloads gnugo-autoloads graphviz-dot-mode-autoloads
   haskell-mode-autoloads helm-autoloads
   highlight-parentheses-autoloads hl-sexp-autoloads
   mldonkey-autoloads notify-autoloads oz-autoloads paredit-autoloads
   popup-autoloads quack-autoloads redshank-autoloads sauron-autoloads
   shen-mode-autoloads slime-autoloads sml-modeline-autoloads
   sudoku-autoloads tabbar-autoloads w3-autoloads w3m-autoloads
   wgrep-autoloads wget-autoloads))

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
