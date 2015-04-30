;;; my-lisp.el --- Lisp languages settings

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(require 'my-utils)
(win-or-nix (require 'my-windows))

;;; set inferior lisp and scheme
(custom-set-variables
 '(scheme-program-name "gsi")
 '(sp-base-key-bindings 'sp))

(setq inferior-lisp-program
      (win-or-nix
       (cond ((file-exists-p (concat +home-path+ "clisp"))
	      (concat +home-path+ "clisp/clisp.exe -K full"))
	     ((file-exists-p (concat +win-path+
				     "Program Files/clisp"))
	      (concat +win-path+
		      "Program Files/clisp/clisp.exe -K full"))
	     (t "clisp"))
       "sbcl"))

;;; Smartparens
(when (require 'smartparens-config nil t)
  (define-keys smartparens-mode-map
    (kbd "C-)") 'sp-forward-slurp-sexp
    (kbd "C-}") 'sp-forward-barf-sexp
    (kbd "C-(") 'sp-backward-slurp-sexp
    (kbd "C-{") 'sp-backward-barf-sexp
    (kbd "C-M-s") 'sp-split-sexp (kbd "C-M-r") 'sp-splice-sexp
    (kbd "C-M-a") nil (kbd "C-M-e") nil)
  (smartparens-global-mode 1))

;;; Redshank
(when-library
 nil redshank
 (redshank-setup '(lisp-mode-hook slime-repl-mode-hook
				  inferior-lisp-mode-hook)
		 t))

;;; elisp stuff
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(hook-modes turn-on-eldoc-mode
  emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook)

(or (featurep 'ergoemacs-mode)
    (define-key emacs-lisp-mode-map "\M-g" 'lisp-complete-symbol))

;;; Common Lisp hyperspec info look-up
(if (require 'info-look nil t)
    (info-lookup-add-help :mode 'lisp-mode :regexp "[^][()'\" \t\n]+"
			  :ignore-case t
			  :doc-spec '(("(ansicl)Symbol Index"
				       nil nil nil))))

;;; SLIME
(when-library
 nil slime
 (eval-after-load "slime"
   `(progn
      (slime-setup ,(win-or-nix
		     ''(slime-fancy slime-banner slime-indentation)
		     ''(slime-fancy slime-banner slime-indentation
				    slime-asdf)))
      (add-to-list 'slime-lisp-implementations
		   (list ',(win-or-nix 'clisp 'sbcl)
			 ',(split-string inferior-lisp-program " +")))

      (win-or-nix
       nil (add-to-list 'slime-lisp-implementations '(clozure ("ccl"))))

      (if (file-exists-p ,(concat +home-path+
				  "Documents/HyperSpec/"))
	  (setq common-lisp-hyperspec-root
		,(concat "file://" +home-path+
			 "Documents/HyperSpec/")))
      (setq slime-default-lisp ',(win-or-nix 'clisp 'sbcl)
	    slime-complete-symbol*-fancy t
	    slime-complete-symbol-function
	    'slime-fuzzy-complete-symbol
	    slime-net-coding-system
	    (find-if 'slime-find-coding-system
		     '(utf-8-unix iso-latin-1-unix iso-8859-1-unix
				  binary)))

      (or (featurep 'ergoemacs-mode)
	  (define-key slime-mode-map "\M-g"
	    'slime-complete-symbol)))))

;;; Clojure
(when-library
 nil (clojure-mode slime)
 (eval-after-load "slime"
   `(progn
      (when (file-exists-p
	     ,(win-or-nix #1=(concat +home-path+ "Documents/javadoc")
			  #2="/usr/share/doc/java-sdk-docs-1.6.0.23"))
	(defun slime-browse-local-javadoc (ci-name)
	  "Browse local JavaDoc documentation on class/interface CI-NAME."
	  (interactive
	   (list (slime-read-symbol-name "Class/Interface name: ")))
	  (or ci-name (error "No name given"))
	  (let ((name (replace-regexp-in-string "\\$" "." ci-name))
		(path (concat
		       ,(win-or-nix #1# #2#)
		       "/api/")))
	    (with-temp-buffer
	      (insert-file-contents
	       (concat path "allclasses-noframe.html"))
	      (let ((l (delq
			nil
			(mapcar (lambda (rgx)
				  (let* ((r (concat
					     "\\.?\\(" rgx
					     "[^./]+\\)[^.]*\\.?$"))
					 (n (if (string-match r name)
						(match-string 1 name)
					      name)))
				    (if (re-search-forward
					 (concat
					  "<A HREF=\"\\(.+\\)\" +.*>"
					  n "<.*/A>")
					 nil t)
					(match-string 1)
				      nil)))
				'("[^.]+\\." "")))))
		(if l (browse-url (concat "file://" path (car l)))
		  (error (concat "Not found: " ci-name)))))))

	(define-key slime-mode-map "\C-cb" 'slime-browse-local-javadoc)
	(define-key slime-repl-mode-map
	  "\C-cb" 'slime-browse-local-javadoc))

      (add-hook 'slime-repl-mode-hook
		(lambda () "Clojure REPL hook."
		  (if (and (string-equal "clojure"
					 (slime-connection-name))
			   (require 'clojure-mode nil t))
		      (progn
			(if (slime-inferior-process)
			    (slime-redirect-inferior-output))
			(custom-set-variables
			 '(slime-use-autodoc-mode nil))
			(set-syntax-table clojure-mode-syntax-table)
			(clojure-mode-font-lock-setup)
			(setq lisp-indent-function
			      'clojure-indent-function))
		    (custom-set-variables
		     '(slime-use-autodoc-mode t))))))))

;;; Quack
(when-library
 nil quack
 (autoload 'quack-scheme-mode-hookfunc "quack")
 (autoload 'quack-inferior-scheme-mode-hookfunc "quack")
 (add-hook 'scheme-mode-hook 'quack-scheme-mode-hookfunc)
 (add-hook 'inferior-scheme-mode-hook
	   'quack-inferior-scheme-mode-hookfunc)
 (setq quack-global-menu-p nil)
 (eval-after-load "quack"
   `(setq quack-default-program "gsi"
	  quack-pltcollect-dirs (list ,(win-or-nix
					(concat +home-path+
						"Documents/plt")
					"/usr/share/plt/doc")))))

;;; CLIPS
(when-library
 nil inf-clips
 (eval-after-load "clips"
   '(add-hook 'clips-mode-hook
	      (lambda () (setq indent-region-function nil))))
 (eval-after-load "inf-clips"
   `(progn
      (setq inferior-clips-program
	    ,(win-or-nix
	      (concat +win-path+
		      "Program Files/CLIPS/Bin/CLIPSDOS.exe")
	      "clips"))
      (add-hook 'inferior-clips-mode-hook
		(lambda () (setq indent-region-function nil))))))

(provide 'my-lisp)

;;; my-lisp.el ends here
