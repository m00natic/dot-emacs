;;; init.el --- Andrey Kotlarski's .emacs

;;; Commentary:
;; Utilized extensions:
;;  Anything related:
;;   Anything (http://www.emacswiki.org/emacs/Anything)
;;   anything-etags (http://www.emacswiki.org/emacs/anything-etags.el)
;;   anything-match (http://www.emacswiki.org/emacs/AnythingPlugins)
;;   fuzzy-match (http://www.emacswiki.org/emacs/Icicles_-_Fuzzy_Completion)
;;   Proel (http://www.stifflog.com/2008/12/14/proel-project-support-for-emacs)
;;  AutoInstall related: (http://www.emacswiki.org/emacs/AutoInstall)
;;   auto-install
;;   anything-auto-install
;;  Programming languages related:
;;   SLIME (http://common-lisp.net/project/slime)
;;   Quack (http://www.neilvandyke.org/quack/)
;;   clojure-mode (http://github.com/jochu/clojure-mode/tree)
;;   swank-clojure (http://github.com/jochu/swank-clojure/tree)
;;   clips-mode (http://www.cs.us.es/software/clips)
;;   Prolog (http://bruda.ca/emacs-prolog)
;;   haskell-mode (http://www.haskell.org/haskell-mode)
;;   tuareg-mode (http://www-rocq.inria.fr/~acohen/tuareg)
;;   CSharpMode (http://www.emacswiki.org/emacs/CSharpMode)
;;   VisualBasicMode (http://www.emacswiki.org/emacs/VisualBasicMode)
;;  Lisp goodies:
;;   highlight-parentheses (http://nschum.de/src/emacs/highlight-parentheses)
;;   hl-sexp (http://edward.oconnor.cx/elisp/hl-sexp.el)
;;   ParEdit (http://www.emacswiki.org/emacs/ParEdit)
;;   Redshank (http://www.foldr.org/~michaelw/emacs/redshank)
;;  misc:
;;   AUCTeX (http://www.gnu.org/software/auctex)
;;   CompletionUI (http://www.emacswiki.org/emacs/CompletionUI)
;;   ColorTheme (http://www.emacswiki.org/emacs/ColorTheme)
;;   cygwin-mount (http://www.emacswiki.org/emacs/cygwin-mount.el)
;;   gtags (http://www.gnu.org/software/global)
;;   traverselisp (http://www.emacswiki.org/emacs/traverselisp.el)
;;   TabBar (http://www.emacswiki.org/emacs/TabBarMode)
;;   w3m (http://emacs-w3m.namazu.org)
;;   Wanderlust (http://www.gohome.org/wl)

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; extension independent macros

;; do some OS recognition
(defconst *winp* (or (eq system-type 'windows-nt)
		     (eq system-type 'ms-dos)) "Windows detection.")

(defmacro win-or-nix (win &rest nix)
  "OS conditional.  WIN may be a list and is executed on windows systems.
NIX forms are executed on all other platforms."
  (if *winp*
      (if (consp win)
	  (let ((form (car win)))
	    (cond ((not (consp form)) win)
		  ((cadr win) `(progn ,@win))
		  (t form)))
	win)
    (if (cadr nix)
	`(progn ,@nix)
      (car nix))))

;;; `require-maybe' (http://www.emacswiki.org/cgi-bin/wiki/LocateLibrary)
;; this is useful when this .emacs is used in an env where not all of
;; the other stuff is available
(defmacro require-maybe (feature &optional file)
  "Try to require FEATURE in FILE but don't signal an error on fail."
  `(require ,feature ,file 'noerror))

(defmacro hook-modes (functions &rest modes)
  "Hook a list of FUNCTIONS \(or atom\) to MODES.
Each function may be an atom or a list with parameters."
  (cons 'progn
	(if (consp functions)
	    (if (cdr functions)
		(let ((fns (mapcar (lambda (fn)
				     (if (consp fn)
					 fn
				       (list fn)))
				   functions)))
		  (mapcar (lambda (mode)
			    `(add-hook ',mode (lambda () ,@fns)))
			  modes))
	      (let ((fst (car functions)))
		(if (consp fst)
		    (mapcar (lambda (mode)
			      `(add-hook (lambda () ,fst)))
			    modes)
		  (mapcar (lambda (mode)
			    `(add-hook ',mode ',fst))
			  modes))))
	  (mapcar (lambda (mode)
		    `(add-hook ',mode ',functions))
		  modes))))

(defmacro delete-many (elts sequence)
  "Delete ELTS from SEQUENCE."
  (if (null elts)
      sequence
    `(delete ,(car elts) (delete-many ,(cdr elts) ,sequence))))

(defmacro active-lisp-modes ()
  "Build a function for activating existing convenient minor modes for s-exp."
  `(defun activate-lisp-minor-modes ()
     "Activate some convenient minor modes for editing s-exp"
     (pretty-lambdas)
;;; Highlight s-exps and parens
     ,(when (require-maybe 'hl-sexp) '(hl-sexp-mode +1))
     ,(when (require-maybe 'highlight-parentheses)
	'(highlight-parentheses-mode +1))
;;; Paredit
     ,(when (require-maybe 'paredit) '(paredit-mode +1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; extension independent functions

(defun nuke-buffers (reg-ex currentp)
  "Kill all buffers whose name is matched by REG-EX.
Leave current if CURRENTP."
  (interactive
   (let ((reg-str (read-regexp "Buffer names to kill?" ".*")))
     (list reg-str (when (string-equal reg-str ".*")
		     (y-or-n-p "Keep current buffer? ")))))
  (macrolet ((check-kill (reg-ex buf)
			 `(when (string-match-p ,reg-ex
						(or (buffer-name ,buf)
						    ""))
			    (kill-buffer buf))))
    (if currentp
	(let ((curr (current-buffer)))
	  (dolist (buf (buffer-list))
	    (or (eq buf curr)
		(check-kill reg-ex buf))))
      (dolist (buf (buffer-list))
	(check-kill reg-ex buf))))
  (when (string-equal reg-ex ".*") (delete-other-windows)))

(defun nuke-modes (reg-ex)
  "Kill all buffers whose major mode name is matched by REG-EX."
  (interactive (list (read-regexp "Major modes to kill?" ".*")))
  (dolist (buf (buffer-list))
    (set-buffer buf)
    (when (string-match-p reg-ex (symbol-name major-mode))
      (kill-buffer buf))))

;;; fullscreen stuff
(defvar *fullscreen-p* nil "Check if fullscreen is on or off.")
(defconst *width* 100 "My prefered non-fullscreen width.")

(defun my-non-fullscreen ()
  "Exit fullscreen."
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND restore #xf120
      (w32-send-sys-command 61728)
    (set-frame-parameter nil 'width *width*)
    (set-frame-parameter nil 'fullscreen 'fullheight)))

(defun my-fullscreen ()
  "Go fullscreen."
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND maximize #xf030
      (w32-send-sys-command 61488)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(defun my-toggle-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (if (setq *fullscreen-p* (not *fullscreen-p*))
      (my-fullscreen)
    (my-non-fullscreen)))

(defun opacity-modify (&optional dec)
  "Modify the transparency of the Emacs frame.
If DEC is t, decrease the transparency;
otherwise increase it in 5%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
	 (oldalpha (or alpha-or-nil 99))
	 (newalpha (cond (dec (- oldalpha 5))
			 ((>= oldalpha 95) 100)
			 (t (+ oldalpha 5)))))
    (and (>= newalpha frame-alpha-lower-limit)
	 (<= newalpha 100)
	 (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(defun faces-graphical ()
  "Set some faces which --daemon doesn't set correctly.
Execute only once."
  (custom-set-faces
   '(default ((default (:background "black" :height 80))
  	      (((class color) (min-colors 88)) (:foreground "wheat"))
   	      (t (:foreground "white")))))
  (ignore-errors
    (set-face-font 'default (win-or-nix "Consolas" "Inconsolata")))
  (let ((frame (selected-frame)))
    (modify-frame-parameters frame '((alpha . 99)))
    (set-frame-height frame 40)))

(win-or-nix
 (defun hide-emacs ()
   "Keep emacs running hidden on exit."
   (interactive)
   (server-edit)
   (make-frame-invisible nil t))

 (defun reset-frame-faces (frame)
   "Execute once on the first graphical new FRAME.
Reset some faces which --daemon doesn't quite set.
Remove hooh when done."
   (select-frame frame)
   (if (window-system frame)
       (progn (faces-graphical)
	      (remove-hook 'after-make-frame-functions
			   'reset-frame-faces))
     (progn (set-face-background 'default "black" frame)
	    (set-face-foreground 'default "white" frame)))))

(defun pretty-lambdas ()
  "Show an actual lambda instead of the string `lambda'."
  (font-lock-add-keywords nil
			  `(("(\\(lambda\\>\\)"
			     (0 (progn
				  (compose-region
				   (match-beginning 1) (match-end 1)
				   ,(make-char 'greek-iso8859-7 107))
				  nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set some path constants.
(win-or-nix (defconst *win-path* "C:/" "Windows root path."))
(defconst *home-path*
  (win-or-nix
   (if (string-match "\\(.*[/\\]home[/\\]\\)" exec-directory)
       (match-string-no-properties 0 exec-directory)
     (concat (getenv "HOME") "/"))
   (if (file-exists-p "/home/andrey/")
       "/home/andrey/"
     "~/"))
  "Home path.")
(defconst *extras-path* (concat *home-path* ".emacs.d/extras/")
  "Elisp extensions' path.")

;; add `*extras-path*' and subdirs to `load-path'
(and (file-exists-p *extras-path*)
     (fboundp 'normal-top-level-add-subdirs-to-load-path)
     (let ((default-directory *extras-path*))
       (setq load-path (cons *extras-path* load-path))
       (normal-top-level-add-subdirs-to-load-path)))

;; set default directory for `*scratch*'
(setq default-directory (concat (getenv "HOME") "/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; mode a bit emacs

(custom-set-variables
 '(display-time-day-and-date t)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(column-number-mode t)
 '(size-indication-mode t)
 '(display-battery-mode t)
 '(initial-major-mode 'org-mode)
 '(default-major-mode 'org-mode)
 '(word-wrap t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(show-paren-delay 0)
 '(show-paren-style 'parenthesis)
 '(transient-mark-mode t)
 '(delete-selection-mode t)
 '(global-font-lock-mode t)
 '(font-lock-maximum-decoration t)
 '(inhibit-startup-message t)
 '(frame-title-format "emacs - %b (%f)") ; Format the title-bar
 '(mouse-wheel-mode t)		   ; Make the mouse wheel scroll Emacs
 '(next-line-add-newlines nil)
 '(global-linum-mode 1)			; show line numbers
 ;;'(visible-bell t)		 ; Flash instead of that annoying bell
 '(icomplete-prospects-height 2)	; don't spam my minibuffer
 '(completion-ignore-case t)	      ; ignore case when completing...
 '(read-file-name-completion-ignore-case t) ; ...filenames too
 '(search-highlight t)			; highlight when searching...
 '(query-replace-highlight t)		; ...and replacing
 '(require-final-newline t)		; end files with a newline
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Theme styles

(set-face-background 'highlight "SeaGreen")
(set-face-background 'show-paren-match-face "DarkRed")

(custom-set-faces
 '(font-lock-comment-face
   ((((class grayscale) (background light))
     (:foreground "DimGray" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :weight bold :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "Firebrick" :slant italic))
    (((class color) (min-colors 88) (background dark))
     (:foreground "chocolate1" :slant italic))
    (((class color) (min-colors 16) (background light))
     (:foreground "red" :slant italic))
    (((class color) (min-colors 16) (background dark))
     (:foreground "red1" :slant italic))
    (((class color) (min-colors 8) (background light))
     (:foreground "red" :slant italic))
    (((class color) (min-colors 8) (background dark))
     (:foreground "red" :slant italic))
    (t (:weight bold :slant italic))))
 '(mode-line
   ((default (:foreground "black" :box (:line-width 1 :style "none")
			  :width condensed :height 90 :family "neep"))
    (((class color) (min-colors 88)) (:background "DarkSlateGray"))
    (t (:background "green"))))
 '(mode-line-inactive
   ((default (:inherit mode-line))
    (((class color) (min-colors 88))
     (:foreground "DarkSlateGray" :background "honeydew4"))
    (t (:foreground "white" :background "black"))))
 '(mode-line-buffer-id
   ((default (:inherit mode-line :foreground "black"))
    (((class color) (min-colors 88))
     (:background "CadetBlue" :weight extrabold))
    (t (:background "green" :weight normal))))
 '(tabbar-default ((t (:inherit variable-pitch :background "#111"
				:foreground "#0c0"))))
 '(tabbar-default-face ((t (:inherit variable-pitch :background "#111"
				     :foreground "#0c0"))))
 '(tabbar-selected
   ((default (:inherit tabbar-default :background "black"
		       :box (:line-width 2 :color "black")))
    (((class color) (min-colors 88)) (:foreground "DeepSkyBlue"))
    (t (:foreground "white"))))
 '(tabbar-unselected
   ((default (:inherit tabbar-default))
    (((class color) (min-colors 88))
     (:background "#222" :foreground "DarkCyan"
		  :box (:line-width 2 :color "#090909")))
    (t (:background "gray" :foreground "cyan"
		    :box (:line-width 2 :color "gray")))))
 '(tabbar-button
   ((t (:inherit tabbar-default :background "black"
		 :foreground "#0c0"
		 :box (:line-width 2 :color "black")))))
 '(tabbar-button-face
   ((t (:inherit tabbar-default-face :background "black"
		 :foreground "#0c0"
		 :box (:line-width 2 :color "black")))))
 '(tabbar-selected-face
   ((t (:inherit tabbar-default-face :background "black"
		 :foreground "SpringGreen"
		 :box (:line-width 2 :color "black")))))
 '(tabbar-separator ((t (:foreground "#0c0" :background "#111"))))
 '(tabbar-separator-face ((t (:foreground "#0c0" :background "#111"))))
 '(highlight-changes ((((class color) (min-colors 88))
		       (:background "#382f2f"))
		      (t (:background "orange"))))
 '(highlight-changes-delete ((((class color) (min-colors 88))
			      (:background "#916868"))
			     (default (:background "red"))))
 '(region ((((class color) (min-colors 88))
	    (:foreground nil :background "DarkSlateGray"))
	   (t (:foreground "white" :background "cyan"))))
 '(hl-line ((((class color) (min-colors 88)) (:background "#123"))
	    (t (:background "blue"))))
 '(cursor ((((class color) (min-colors 88))
	    (:background "DeepSkyBlue"))
	   (t (:background "white")))))

;;; set geometry
(add-to-list 'default-frame-alist (cons 'width *width*))
(win-or-nix (set-frame-width (selected-frame) *width*))

(win-or-nix
 ((faces-graphical)
  (global-set-key (kbd "C-x C-c") 'hide-emacs))

 (if (window-system)
     (faces-graphical)
   ;; hook, execute only first time in graphical frame
   ;;  (and indefinite times in terminal frames till then)
   (add-hook 'after-make-frame-functions 'reset-frame-faces)))

;;; Colour theme
(when (require-maybe 'color-theme)
  ;; (eval-after-load "color-theme"
  ;;   '(progn
  ;; 	(color-theme-initialize)
  ;; 	(color-theme-euphoria)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; windowing stuff

(global-set-key [f11] 'my-toggle-fullscreen)

;; C-9 will increase opacity (== decrease transparency)
;; C-8 will decrease opacity (== increase transparency)
;; C-0 will returns the state to normal
(global-set-key (kbd "C-9") (lambda () (interactive) (opacity-modify)))
(global-set-key (kbd "C-8") (lambda () (interactive) (opacity-modify t)))
(global-set-key (kbd "C-0") (lambda () (interactive)
			      (modify-frame-parameters
			       nil '((alpha . 99)))))

;;; TabBar
(when (require-maybe 'tabbar)
  (tabbar-mode)

  (defmacro defun-prefix-alt (name on-no-prefix on-prefix
				   &optional do-always)
    "NAME ON-NO-PREFIX ON-PREFIX DO-ALWAYS."
    `(defun ,name (arg)
       (interactive "P")
       ,do-always
       (if (equal nil arg)
	   ,on-no-prefix
	 ,on-prefix)))

  (defun-prefix-alt shk-tabbar-next
    (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
  (defun-prefix-alt shk-tabbar-prev
    (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))

  (global-set-key (kbd "C-<tab>") 'shk-tabbar-next)
  (global-set-key (win-or-nix (kbd "C-S-<tab>")
			      (kbd "<C-S-iso-lefttab>"))
		  'shk-tabbar-prev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; UTF and languages

;;; unicode + bulgarian phonetic
(add-hook 'set-language-environment-hook
	  (lambda ()
	    (setq default-input-method "bulgarian-phonetic")))
(win-or-nix nil (setq locale-coding-system 'utf-8))
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(global-set-key (kbd "C-s-c")
		(lambda ()
		  (interactive)
		  (revert-buffer-with-coding-system 'cp1251)))

;;; ispell
(setq-default ispell-program-name "aspell")
(setq ispell-dictionary "english")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; usefull stuff

;; highlight current line, turn it on for all modes by default
(when (fboundp 'global-hl-line-mode) (global-hl-line-mode t))

;;; highlight changes in documents
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil) ; hide initially

;; toggle changes visibility
(global-set-key (kbd "<f7>") 'highlight-changes-visible-mode)

;; remove the change-highlight in region
(global-set-key (kbd "S-<f7>") 'highlight-changes-remove-highlight)

;; alt-pgup/pgdown jump to the previous/next change
(global-set-key (kbd "<M-prior>") 'highlight-changes-previous-change)
(global-set-key (kbd "<M-next>") 'highlight-changes-next-change)

;;; jump through errors/results
(global-set-key (kbd "<C-M-prior>") 'previous-error)
(global-set-key (kbd "<C-M-next>") 'next-error)

;;; clipboard
(global-set-key (kbd "C-s-y") 'clipboard-yank)
(global-set-key (kbd "C-s-w") 'clipboard-kill-ring-save)

;;; enable some actions
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; Use y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;;; recentf
(when (require-maybe 'recentf)		; save recently used files
  (setq recentf-max-saved-items 100	; max save 100
	recentf-save-file (concat *home-path*
				  ".emacs.d/recentf") ; keep ~/ clean
	recentf-max-menu-items 15)	       ; max 15 in menu
  (recentf-mode t))			       ; turn it on

;;; backups
(setq make-backup-files t		; do make backups
      backup-by-copying t		; and copy them ...
      backup-directory-alist `(("." . ,(concat *home-path* ; ... here
					       ".emacs.d/backup/")))
      version-control t
      kept-new-versions 5
      kept-old-versions 2
      delete-old-versions t)

(mouse-avoidance-mode 'jump)	  ; mouse ptr when cursor is too close
(icomplete-mode t)		  ; completion in minibuffer
(partial-completion-mode t)	  ; be smart with completion

(when (fboundp 'set-fringe-mode)	; emacs22+
  (set-fringe-mode 1))			; space left of col1 in pixels

(when (fboundp file-name-shadow-mode)	; emacs22+
  (file-name-shadow-mode t))	    ; be smart about filenames in mbuf

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Lisp goodies

;; build appropriate `activate-lisp-minor-modes'
(eval (macroexpand '(active-lisp-modes)))

;; Hook convenient s-exp minor modes to some major modes.
(hook-modes activate-lisp-minor-modes
	    inferior-lisp-mode-hook lisp-mode-hook
	    lisp-interaction-mode-hook
	    emacs-lisp-mode-hook ielm-mode-hook
	    inferior-scheme-mode-hook scheme-mode-hook)

;;; Redshank
(when (require-maybe 'redshank-loader
		     (concat *extras-path*
			     "lisp-modes/redshank/redshank-loader"))
  (eval-after-load "redshank-loader"
    `(redshank-setup '(lisp-mode-hook slime-repl-mode-hook
				      inferior-lisp-mode-hook)
		     t)))

;;; ElDoc for Emacs Lisp
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(hook-modes turn-on-eldoc-mode
	    emacs-lisp-mode-hook lisp-interaction-mode-hook
	    ielm-mode-hook)

;;; elisp stuff
(push '("\\.emacs$" . emacs-lisp-mode) auto-mode-alist)

(define-key emacs-lisp-mode-map (win-or-nix (kbd "S-<tab>")
					    (kbd "<S-iso-lefttab>"))
  'lisp-complete-symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Programming extensions

;;; Clojure
(when (require-maybe 'clojure-mode)
  (defconst *clojure-dir* (concat *home-path* (win-or-nix "bin/" ".")
				  "clojure")
    "Path to the Clojure directory.")
  (and (file-exists-p *clojure-dir*)
       (require-maybe 'swank-clojure-autoload)
       (swank-clojure-config
	(setq
	 swank-clojure-jar-path (concat *clojure-dir* "/clojure.jar")
	 swank-clojure-extra-classpaths
	 (directory-files *clojure-dir* t ".\\(jar\\|clj\\)$")
	 swank-clojure-extra-vm-args
	 '("-server" "-Xdebug"
	   "-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8888"))))

  (add-hook 'clojure-mode-hook 'activate-lisp-minor-modes))

;;; Set up SLIME
(when (require-maybe 'slime)
  (eval-after-load "slime"
    '(progn
       (slime-setup (win-or-nix
		     '(slime-fancy slime-banner slime-indentation
				   slime-indentation-fu)
		     '(slime-fancy slime-banner slime-indentation
				   slime-indentation-fu slime-asdf)))
       (slime-autodoc-mode)
       (setq slime-complete-symbol*-fancy t
	     slime-complete-symbol-function 'slime-fuzzy-complete-symbol
	     ;;lisp-indent-function 'common-lisp-indent-function
	     common-lisp-hyperspec-root
	     (win-or-nix
	      (concat *home-path* "docs/HyperSpec/")
	      (concat "file://" *home-path* "Documents/HyperSpec/"))
	     slime-net-coding-system
	     (find-if 'slime-find-coding-system
		      '(utf-8-unix iso-latin-1-unix
				   iso-8859-1-unix binary)))))

  (add-hook 'slime-repl-mode-hook 'activate-lisp-minor-modes)
  (define-key slime-mode-map (win-or-nix (kbd "S-<tab>")
					 (kbd "<S-iso-lefttab>"))
    'slime-complete-symbol)

;;; Online JavaDoc to Slime
  (defun slime-java-describe (symbol-name)
    "Get details on Java class/instance at point SYMBOL-NAME."
    (interactive (list (slime-read-symbol-name
			"Java Class/instance: ")))
    (or symbol-name (error "No symbol given"))
    (save-excursion
      (set-buffer (slime-output-buffer))
      (or (eq (current-buffer) (window-buffer))
	  (pop-to-buffer (current-buffer) t))
      (goto-char (point-max))
      (insert (concat "(show " symbol-name ")"))
      (when symbol-name
	(slime-repl-return)
	(other-window 1))))

  (defun slime-javadoc (symbol-name)
    "Get JavaDoc documentation on Java class at point SYMBOL-NAME."
    (interactive (list (slime-read-symbol-name "JavaDoc info for: ")))
    (or symbol-name (error "No symbol given"))
    (set-buffer (slime-output-buffer))
    (or (eq (current-buffer) (window-buffer))
	(pop-to-buffer (current-buffer) t))
    (goto-char (point-max))
    (insert (concat "(javadoc " symbol-name ")"))
    (when symbol-name
      (slime-repl-return)
      (other-window 1)))

  (add-hook 'slime-connected-hook
	    (lambda ()
	      (interactive)
	      (slime-redirect-inferior-output)
	      (define-key slime-mode-map
		(kbd "C-c d") 'slime-java-describe)
	      (define-key slime-repl-mode-map
		(kbd "C-c d") 'slime-java-describe)
	      (define-key slime-mode-map (kbd "C-c D") 'slime-javadoc)
	      (define-key slime-repl-mode-map
		(kbd "C-c D") 'slime-javadoc)))

;;; Local JavaDoc to Slime
  (setq slime-browse-local-javadoc-root
	(concat *home-path* (win-or-nix "docs" "Documents")
		"/JDK-Doc"))

  (defun slime-browse-local-javadoc (ci-name)
    "Browse local JavaDoc documentation on Java class/Interface at point CI-NAME."
    (interactive
     (list (slime-read-symbol-name "Class/Interface name: ")))
    (or ci-name
	(error "No name given"))
    (let ((name (replace-regexp-in-string "\\$" "." ci-name))
	  (path (concat (expand-file-name
			 slime-browse-local-javadoc-root)
			"/docs/api/")))
      (with-temp-buffer
	(insert-file-contents (concat path "allclasses-noframe.html"))
	(let ((l (delq nil
		       (mapcar (lambda (rgx)
				 (let* ((r (concat
					    "\\.?\\("
					    rgx
					    "[^./]+\\)[^.]*\\.?$"))
					(n (if (string-match r name)
					       (match-string 1 name)
					     name)))
				   (if (re-search-forward
					(concat "<A HREF=\"\\(.+\\)\" +.*>"
						n "<.*/A>")
					nil t)
				       (match-string 1)
				     nil)))
			       '("[^.]+\\." "")))))
	  (if l
	      (browse-url (concat "file://" path (car l)))
	    (error (concat "Not found: " ci-name)))))))

  (add-hook 'slime-connected-hook (lambda ()
				    (define-key slime-mode-map
				      (kbd "C-c b")
				      'slime-browse-local-javadoc)
				    (define-key slime-repl-mode-map
				      (kbd "C-c b")
				      'slime-browse-local-javadoc)))

;;; (or CLisp SBCL)
  (add-to-list 'slime-lisp-implementations
	       (win-or-nix (list 'clisp (list
					 (concat *home-path*
						 "bin/clisp/clisp.exe")
					 "-K" "full"))
			   '(sbcl ("/usr/local/bin/sbcl")))))

(setq inferior-lisp-program
      (win-or-nix (concat *home-path* "bin/clisp/clisp.exe -K full")
		  "/usr/local/bin/sbcl"))

;;; Scheme
(when (file-exists-p (concat *extras-path* "lisp-modes/quack.el"))
  (setq quack-global-menu-p nil)
  (require-maybe 'quack))

;;; CLIPS
(when (require-maybe 'inf-clips)
  (push '("\.clp$" . clips-mode) auto-mode-alist)
  (setq inferior-clips-program
	(win-or-nix (concat *win-path*
			    "Program Files/CLIPS/Bin/CLIPSDOS.exe")
		    "clips"))

  (hook-modes activate-lisp-minor-modes
	      clips-mode-hook inferior-clips-mode-hook))

;;; Caml
(when (require-maybe 'tuareg)
  (autoload 'tuareg-mode "tuareg"
    "Major mode for editing Caml code" t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
  (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode)
			      auto-mode-alist)))

;;; Haskell
(when (file-exists-p (concat *extras-path* "haskell"))
  ;;(require-maybe 'haskell-site-file)
  (load "haskell-site-file")
  (hook-modes (turn-on-haskell-doc-mode turn-on-haskell-indent)
	      haskell-mode-hook)
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

  ;; fixes the repeating input and ^J issues that have been occurring
  (defun inferior-haskell-fix-repeated-input (output)
    (let ((start (string-match "\\^J" output)))
      (if start
	  (substring output (+ 2 start))
	output)))

  (defadvice inferior-haskell-mode (before
				    inferior-haskell-mode-input-fix)
    (add-hook 'comint-preoutput-filter-functions
	      'inferior-haskell-fix-repeated-input nil t)
    (set (make-local-variable 'comint-process-echoes) nil)))

;;; Prolog
(when (require-maybe 'prolog)
  (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
  (autoload 'prolog-mode
    "prolog" "Major mode for editing Prolog programs." t)
  (autoload 'mercury-mode "prolog"
    "Major mode for editing Mercury programs." t)
  (setq prolog-system 'swi	  ; optional, the system you are using
	auto-mode-alist (nconc '(("\\.pl$" . prolog-mode)
				 ("\\.m$" . mercury-mode))
			       auto-mode-alist)))

;;; Python
(when (require-maybe 'python-mode)
  (push '("\\.py$" . python-mode) auto-mode-alist)
  (push '("python" . python-mode) interpreter-mode-alist)
  (autoload 'python-mode "python-mode" "Python editing mode." t))

;;; VB
(when (require-maybe 'visual-basic-mode)
  (autoload 'visual-basic-mode
    "visual-basic-mode" "Visual Basic mode." t)
  (push '("\\.\\(frm\\|bas\\|cls\\)$" . visual-basic-mode)
	auto-mode-alist))

;;; C#
(when (require-maybe 'csharp-mode)
  (autoload 'csharp-mode "csharp-mode"
    "Major mode for editing C# code." t)
  (push '("\\.cs$" . csharp-mode) auto-mode-alist))

;;; cc-mode - hide functions
(add-hook 'c-mode-common-hook (lambda ()
				(hs-minor-mode 1)
				(local-set-key (kbd "C-c C-c")
					       'hs-toggle-hiding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Auxiliary extensions

;;; AUCTeX
(when (file-exists-p (concat *extras-path* "tex"))
  (load "auctex" nil t)
  (load "preview-latex" nil t)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (hook-modes flyspell-mode
	      latex-mode-hook tex-mode-hook bibtex-mode-hook))

;;; CompletionUI
(when (require-maybe 'completion-ui)
  (global-set-key (kbd "M-/") 'complete-dabbrev)
  (define-key emacs-lisp-mode-map (kbd "C-c TAB") 'complete-elisp))

;;; Anything
(when (and (require-maybe 'anything-config)
	   (require-maybe 'anything-match-plugin)
	   (require-maybe 'anything-etags))
  (global-set-key (kbd "<f5>") 'anything)
  (setq anything-sources
	'(anything-c-source-buffers+
	  anything-c-source-recentf
	  anything-c-source-locate
	  anything-c-source-ffap-guesser
	  anything-c-source-files-in-current-dir+
	  anything-c-source-etags-select
	  anything-c-source-emacs-commands
	  anything-c-source-emacs-functions-with-abbrevs
	  anything-c-source-info-elisp
	  anything-c-source-info-pages
	  anything-c-source-man-pages))

;;; Proel
  (setq proel-dirs-with-projects
	(list (expand-file-name
	       (let ((default-proel (win-or-nix
				     (concat *win-path*
					     "Program Files")
				     (concat *home-path* "Programs"))))
		 (if (file-exists-p default-proel)
		     default-proel
		   *home-path*)))))
  (when (require-maybe 'proel)
    (grep-compute-defaults)
    (global-set-key (kbd "<f6>") 'proel-grep-in-project)
    (setq anything-sources (nconc anything-sources
				  (list 'proel-anything-projects)))))

;;; Auto Install
(when (require-maybe 'auto-install)
  (setq auto-install-directory (concat *extras-path*
				       "auto-install-dir/"))
  (when (require-maybe 'anything-auto-install)
    (defun anything-toggle-auto-install ()
      "Toggle anything-c-source-auto-installs in anything sources."
      (interactive)
      (if (memq 'anything-c-source-auto-install-from-emacswiki
		anything-sources)
	  (progn
	    (delete-many
	     ('anything-c-source-auto-install-from-emacswiki
	      'anything-c-source-auto-install-from-library)
	     anything-sources)
	    (ignore-errors
	      (auto-install-update-emacswiki-package-name nil))
	    (message "Auto-install removed from anything sources."))
	(setq anything-sources
	      (nconc anything-sources
		     '(anything-c-source-auto-install-from-emacswiki
		       anything-c-source-auto-install-from-library)))
	(ignore-errors (auto-install-update-emacswiki-package-name t))
	(message "Auto-install added to anything sources.")))))

;;; Git
(require-maybe 'vc-git)

;;; Traverse
(require-maybe 'traverselisp)

;;; Global tags
(when (require-maybe 'gtags)
  (win-or-nix
   nil
   (defun gtags-create-or-update ()
     "Create or update the gnu global tag file."
     (interactive)
     (or (= 0 (call-process "global"
			    nil nil nil " -p")) ; tagfile doesn't exist?
	 (let ((olddir default-directory)
	       (topdir (read-directory-name "gtags: top of source tree:"
					    default-directory)))
	   (cd topdir)
	   (shell-command "gtags && echo 'created tagfile'")
	   (cd olddir))		 ; restore
	 ;;  tagfile already exists; update it
	 (shell-command "global -u && echo 'updated tagfile'"))))

  (add-hook 'gtags-mode-hook (lambda ()
			       (local-set-key (kbd "M-.") ; find a tag
					      'gtags-find-tag)
			       (local-set-key (kbd "M-,") ;reverse tag
					      'gtags-find-rtag)))
  (add-hook 'c-mode-common-hook (lambda ()
				  (gtags-mode t)
				  ;;(gtags-create-or-update)
				  )))

;;; Wanderlust
(when (and (require-maybe 'wl)
	   (require-maybe 'wl-draft))
  (autoload 'wl "wl" "Wanderlust" t)
  (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
  (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
  ;;(setq wl-init-file (concat *home-path* ".emacs.d/wl.el"))

  ;; IMAP
  (setq elmo-imap4-default-server "imap.gmail.com"
	elmo-imap4-default-user "m00naticus@gmail.com"
	elmo-imap4-default-authenticate-type 'clear
	elmo-imap4-default-port '993
	elmo-imap4-default-stream-type 'ssl
	elmo-imap4-use-modified-utf7 t
	;; SMTP
	wl-smtp-connection-type 'starttls
	wl-smtp-posting-port 587
	wl-smtp-authenticate-type "plain"
	wl-smtp-posting-user "m00natic"
	wl-smtp-posting-server "smtp.gmail.com"
	wl-local-domain "gmail.com"

	wl-default-folder "%Inbox"
	wl-default-spec "%"
	wl-draft-folder "%[Gmail]/Drafts" ; Gmail IMAP
	wl-trash-folder "%[Gmail]/Trash"

	wl-folder-check-async t
	elmo-imap4-use-modified-utf7 t)

  (autoload 'wl-user-agent-compose "wl-draft" nil t)
  (when (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
  (when (fboundp 'define-mail-user-agent)
    (define-mail-user-agent 'wl-user-agent 'wl-user-agent-compose
      'wl-draft-send 'wl-draft-kill 'mail-send-hook))

  (setq
   wl-forward-subject-prefix "Fwd: "	; use "Fwd: " not "Forward: "

   ;;; from a WL-mailinglist post by David Bremner
   ;; Invert behaviour of with and without argument replies.
   ;; just the author
   wl-draft-reply-without-argument-list
   '(("Reply-To" ("Reply-To") nil nil)
     ("Mail-Reply-To" ("Mail-Reply-To") nil nil)
     ("From" ("From") nil nil))

   ;; bombard the world
   wl-draft-reply-with-argument-list
   '(("Followup-To" nil nil ("Followup-To"))
     ("Mail-Followup-To" ("Mail-Followup-To") nil ("Newsgroups"))
     ("Reply-To" ("Reply-To") ("To" "Cc" "From") ("Newsgroups"))
     ("From" ("From") ("To" "Cc") ("Newsgroups"))))

  ;; (when (require-maybe 'wl-spam)
  ;;   (wl-spam-setup)
  ;;   (setq elmo-spam-scheme 'sa
  ;; 	   wl-spam-folder ".spam"))	; maildir to store spam

  (defun wl-summary-refile (&optional folder)
    "Refile the current message to FOLDER.
If FOLDER is nil, use the default."
    (interactive)
    (wl-summary-refile (wl-summary-message-number) folder)
    (wl-summary-next)
    (message (concat "refiled to " folder)))

  ;; (define-key wl-summary-mode-map (kbd "b x") ; => Project X
  ;;   (lambda ()
  ;;     (interactive)
  ;;     (wl-summary-refile ".project-x")))

  ;; suggested by Masaru Nomiya on the WL mailing list
  (defun wl-draft-subject-check ()
    "Check whether the message has a subject before sending."
    (and (< (length (std11-field-body "Subject")) 1)
	 (not (y-or-n-p "No subject! Send current draft? "))
	 (error "Abort")))

  ;; note, this check could cause some false positives; anyway, better
  ;; safe than sorry...
  (defun wl-draft-attachment-check ()
    "If attachment is mention but none included, warn the the user."
    (save-excursion
      (goto-char 0)
      (or ;; don't we have an attachment?
       (re-search-forward "^Content-Disposition: attachment" nil t)
       (when ;; no attachment; did we mention an attachment?
	   (re-search-forward "attach" nil t)
	 (or (y-or-n-p
	      "Possibly missing an attachment.  Send current draft? ")
	     (error "Abort"))))))

  (hook-modes (wl-draft-subject-check wl-draft-attachment-check)
	      wl-mail-send-pre-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Specific OS extensions

(win-or-nix
;;; Cygwin
 (let ((cygwin-dir (concat *win-path* "cygwin/bin")))
   (when (and (file-exists-p cygwin-dir)
	      (require-maybe 'cygwin-mount))
     (cygwin-mount-activate)
     (setq w32shell-cygwin-bin cygwin-dir)))

;;; w3m
 (when (require-maybe 'w3m-load)
   (defun w3m-browse-url-other-window (url &optional newwin)
     (interactive (browse-url-interactive-arg "w3m URL: "))
     (let ((pop-up-frames nil))
       (switch-to-buffer-other-window (w3m-get-buffer-create "*w3m*"))
       (w3m-browse-url url)))

   (setq browse-url-browser-function
	 '(("^ftp:/.*" . (lambda (url &optional nf)
			   (call-interactively
			    'find-file-at-point url)))
	   ("hyperspec" . w3m-browse-url)
	   ("weitz" . w3m-browse-url)
	   ("." . browse-url-firefox))
	 browse-url-new-window-flag t
	 browse-url-firefox-new-window-is-tab t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (server-start) ; using --daemon or emacsW32 instead

;;; init.el ends here
