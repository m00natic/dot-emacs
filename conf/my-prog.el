;;; my-prog.el --- Additional programming modes settings

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(require 'my-utils)

(custom-set-variables
 '(c-default-style '((java-mode . "java")
		     (awk-mode . "awk")
		     (other . "stroustrup")))
 '(ecb-options-version "2.40")
 '(gdb-many-windows t)
 '(indent-tabs-mode nil)
 '(prolog-system 'swi)
 '(which-function-mode t))

;;; highlight parens
(when-library
 nil rainbow-delimiters
 (add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable))

;;; Semantic
(when-library
 t semantic
 (eval-after-load "semantic"
   '(progn
      (add-to-list 'semantic-default-submodes
		   'global-semantic-decoration-mode)
      (add-to-list 'semantic-default-submodes
		   'global-semantic-idle-summary-mode)
      (add-to-list 'semantic-default-submodes
		   'global-semantic-idle-local-symbol-highlight-mode)
      (add-to-list 'semantic-default-submodes
		   'global-semantic-mru-bookmark-mode)))

 (add-hook 'prog-mode-hook (lambda () (or semantic-mode (semantic-mode))))

;;; Emacs Code Browser
 (when-library
  nil ecb
  (eval-after-load "ecb"
    `(progn (defvar stack-trace-on-error nil)
	    (defconst ecb-cedet-required-version-max '(2 1 4 9))
	    (let ((prog-path ,(concat +home-path+ "Programs")))
	      (ecb-add-source-path prog-path prog-path t))))))

;;; AutoComplete
(when (and (not +old-emacs+)
	   (require 'auto-complete-config nil t))
  (ac-config-default)
  (ac-flyspell-workaround)
  (add-to-list 'ac-modes 'org-mode)

  (when-library
   t semantic
   (eval-after-load "semantic"
     '(add-to-list 'ac-sources 'ac-source-semantic))))

;;; find file in project
(when-library
 nil find-file-in-project
 (define-key global-map "\C-cf" 'find-file-in-project)

 (eval-after-load "find-file-in-project"
   '(progn
      (defun my-ffip-project-root-function ()
	"Check for `ffip-project-file' and if no such, \
return current directory."
	(let ((project-directory
	       (if (listp ffip-project-file)
		   (some (apply-partially 'locate-dominating-file
					  default-directory)
			 ffip-project-file)
		 (locate-dominating-file default-directory
					 ffip-project-file))))
	  (or project-directory default-directory)))

      (setq-default
       ffip-project-file ".emacs-project"
       ffip-patterns (nconc '("*.cpp" "*.h" "*.hpp" "*.c" "*.sql")
			    ffip-patterns)
       ffip-find-options
       "-not -regex \".*\\(debug\\|release\\|svn\\|git\\).*\""
       ffip-limit 4096
       ffip-project-root-function 'my-ffip-project-root-function))))

;;; Oz
(when-library
 nil oz
 (autoload 'oz-mode "oz" "Oz editing mode." t)
 (autoload 'ozm-mode "mozart"
   "Major mode for displaying Oz machine code." t)
 (autoload 'oz-gump-mode "oz"
   "Major mode for editing Oz code with embedded Gump specifications."
   t)
 (setq auto-mode-alist (nconc '(("\\.oz$" . oz-mode)
				("\\.ozm$" . ozm-mode)
				("\\.ozg$" . oz-gump-mode))
			      auto-mode-alist)))

;;; Haskell
(when-library
 nil haskell-mode
 (hook-modes ((haskell-indentation-mode t)
	      (haskell-doc-mode t))
	     haskell-mode-hook)

 (when-library nil ghc
	       (when (executable-find "ghc-mod")
		 (autoload 'ghc-init "ghc" nil t)
		 (add-hook 'haskell-mode-hook (lambda () (ghc-init)
						(flymake-mode))))))

;;; cc-mode settings
(add-hook 'c-mode-common-hook
	  (lambda () (hs-minor-mode 1)
	    (when-library nil hl-sexp (hl-sexp-mode 1))
	    (local-set-key [backtab] 'hs-toggle-hiding)))

;;; Emacs Speaks Statistics
(when-library
 nil ess-site
 (autoload 'Rd-mode "ess-site"
   "Major mode for editing R documentation source files." t)
 (setq auto-mode-alist (nconc '(("\\.[rR]$" . R-mode)
				("\\.Rd$" . Rd-mode))
			      auto-mode-alist)))

;;; AUCTeX
(when-library
 nil auctex-autoloads
 (add-to-list 'auto-mode-alist '("\\.[tT]e[xX]\\'" . TeX-latex-mode))

 (eval-after-load "latex"
   '(progn (load "preview" t)
	   (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)))

 (eval-after-load "tex"
   '(progn
      (and
       (executable-find "ps2pdf") (executable-find "dvips")
       (executable-find "latex")
       (add-to-list
	'TeX-command-list
	'("Optimized PDF"
	  "latex %s && dvips %s.dvi && ps2pdf -dEmbedAllFonts=true \
-dOptimize=true -dUseFlateCompression=true %s.ps"
	  TeX-run-command nil (latex-mode)
	  :help "Produce optimized pdf")))

      (or (featurep 'ergoemacs-mode)
	  (define-key TeX-mode-map "\M-g" 'TeX-complete-symbol)))))

;;; commit logs
(hook-modes (auto-fill-mode flyspell-mode)
	    log-edit-mode-hook magit-log-edit-mode-hook)

;;; ebrowse
(when (executable-find "ebrowse-c++")
  (defun my-ebrowse-refresh (arg)
    "Refresh existing ebrowse db or if *Tree* buffer is missing \
or ARG is non nil - locate project file for current directory."
    (interactive "P")
    (save-excursion
      (if (and (not arg) (ignore-errors (set-buffer "*Tree*")))
	  (let ((ebrowse-file buffer-file-name))
	    (call-process "ebrowse-c++")
	    (kill-buffer)
	    (find-file-noselect ebrowse-file))
	(let ((project-dir (locate-dominating-file default-directory
						   ".emacs-project")))
	  (if project-dir
	      (save-excursion
		(call-process "ebrowse-c++" nil nil nil project-dir)
		(find-file-noselect (concat project-dir
					    "BROWSE")))))))))

(provide 'my-prog)

;;; my-prog.el ends here
