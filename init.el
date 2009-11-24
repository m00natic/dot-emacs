;; init.el --- Andrey Kotlarski's .emacs

;;; Commentary:
;; Utilized extensions:
;;  Anything related:
;;   Anything http://www.emacswiki.org/emacs/Anything
;;   anything-etags http://www.emacswiki.org/emacs/anything-etags.el
;;   anything-match http://www.emacswiki.org/emacs/AnythingPlugins
;;   fuzzy-match http://www.emacswiki.org/emacs/Icicles_-_Fuzzy_Completion
;;  AutoInstall related: http://www.emacswiki.org/emacs/AutoInstall
;;   auto-install
;;   anything-auto-install
;;  Programming languages related:
;;   SLIME http://common-lisp.net/project/slime
;;   Quack http://www.neilvandyke.org/quack/
;;   clojure-mode http://github.com/jochu/clojure-mode
;;   swank-clojure http://github.com/jochu/swank-clojure
;;   clips-mode http://www.cs.us.es/software/clips
;;   Prolog http://bruda.ca/emacs-prolog
;;   haskell-mode http://www.haskell.org/haskell-mode
;;   tuareg-mode http://www-rocq.inria.fr/~acohen/tuareg
;;   CSharpMode http://www.emacswiki.org/emacs/CSharpMode
;;   VisualBasicMode http://www.emacswiki.org/emacs/VisualBasicMode
;;  Lisp goodies:
;;   highlight-parentheses http://nschum.de/src/emacs/highlight-parentheses
;;   hl-sexp http://edward.oconnor.cx/elisp/hl-sexp.el
;;   ParEdit http://www.emacswiki.org/emacs/ParEdit
;;   Redshank http://www.foldr.org/~michaelw/emacs/redshank
;;  misc:
;;   ErgoEmacs-mode http://xahlee.org/emacs/ergonomic_emacs_keybinding.html
;;   AUCTeX http://www.gnu.org/software/auctex
;;   LaTeX-beamer http://latex-beamer.sourceforge.net
;;   Ditaa http://ditaa.sourceforge.net
;;   CompletionUI http://www.emacswiki.org/emacs/CompletionUI
;;   cygwin-mount http://www.emacswiki.org/emacs/cygwin-mount.el
;;   gtags http://www.gnu.org/software/global
;;   traverselisp http://www.emacswiki.org/emacs/traverselisp.el
;;   TabBar http://www.emacswiki.org/emacs/TabBarMode
;;   w3m http://emacs-w3m.namazu.org
;;   Wanderlust http://www.gohome.org/wl

;;; Code:
;; do some OS recognition and set main parameters
(defconst +winp+ (memq system-type '(windows-nt ms-dos))
  "Windows detection.")

(defmacro win-or-nix (win &rest nix)
  "OS conditional.  WIN may be a list and is executed on windows systems.
NIX forms are executed on all other platforms."
  (if +winp+
      (if (consp win)
	  (let ((form (car win)))
	    (cond ((not (consp form)) win)
		  ((cadr win) `(progn ,@win))
		  (t form)))
	win)
    (if (cadr nix)
	`(progn ,@nix)
      (car nix))))

;; Set some path constants.
(win-or-nix (defconst +win-path+ "C:/" "Windows root path."))

(defconst +home-path+
  (win-or-nix
   (if (string-match "\\(.*[/\\]home[/\\]\\)" exec-directory)
       (match-string 0 exec-directory)
     (concat (getenv "HOME") "/"))
   (concat (getenv "HOME") "/"))
  "Home path.")

(defconst +extras-path+ (concat +home-path+ ".emacs.d/extras/")
  "Elisp extensions' path.")

;; add `+extras-path+' and subdirs to `load-path'
(and (fboundp 'normal-top-level-add-subdirs-to-load-path)
     (file-exists-p +extras-path+)
     (let ((default-directory +extras-path+))
       (push +extras-path+ load-path)
       (normal-top-level-add-subdirs-to-load-path)))

;; set default directory for `*scratch*' and some info
(setq default-directory +home-path+
      add-log-full-name "Andrey Kotlarski")

(setenv "EMAIL" "m00naticus@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; extension independent macros

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

(defmacro define-keys (mode &rest keys)
  "Define cascade of keys for a MODE.
KEYS is alternating list of key-value."
  (cons 'progn
	(let ((res nil))
	  (while keys
	    (push `(define-key ,mode ,(car keys) ,(cadr keys)) res)
	    (setq keys (cddr keys)))
	  (nreverse res))))

(defmacro def-interactive-arg (fun comment on-no-prefix on-prefix
				   &optional do-always)
  "Create a one-argument interactive function FUN with COMMENT.
 ON-NO-PREFIX is executed if no prefix is given, ON-PREFIX otherwise.
 DO-ALWAYS is always executed beforehand."
  `(defun ,fun (arg)
     ,comment
     (interactive "P")
     ,do-always
     (if (null arg)
	 ,on-no-prefix
       ,on-prefix)))

(defmacro do-buffers (buf &rest body)
  "Execute action over all buffers with BUF as iterator.
With prefix arg, skip executing BODY over current."
  `(if current-prefix-arg
       (let ((curr (current-buffer)))
	 (dolist (,buf (buffer-list))
	   (,(if (cdr body)
		 'unless
	       'or)
	    (eq ,buf curr)
	    ,@body)))
     (dolist (,buf (buffer-list))
       ,@body)))

(defmacro active-lisp-modes ()
  "Build a function for activating existing convenient minor modes for s-exp."
  `(defun activate-lisp-minor-modes ()
     "Activate some convenient minor modes for editing s-exp"
     (pretty-lambdas)
;;; Highlight s-expressions and parenthesis
     ,(when (require-maybe 'hl-sexp) '(hl-sexp-mode +1))
     ,(when (require-maybe 'highlight-parentheses)
	'(highlight-parentheses-mode +1))
;;; Paredit
     ,(when (require-maybe 'paredit)
	(when (boundp 'ergoemacs-mode)
	  (define-keys paredit-mode-map
	    ergoemacs-comment-dwim-key 'paredit-comment-dwim
	    ergoemacs-isearch-forward-key 'isearch-forward
	    ergoemacs-backward-kill-word-key
	    'paredit-backward-kill-word
	    ergoemacs-kill-word-key 'paredit-forward-kill-word
	    ergoemacs-delete-backward-char-key
	    'paredit-backward-delete
	    ergoemacs-delete-char-key 'paredit-forward-delete
	    ergoemacs-kill-line-key 'paredit-kill
	    ergoemacs-recenter-key 'recenter-top-bottom
	    "\M-R" 'paredit-raise-sexp)
	  (when (equal (getenv "ERGOEMACS_KEYBOARD_LAYOUT") "colemak")
	    (define-key paredit-mode-map "\M-r"
	      'paredit-splice-sexp)))
	'(paredit-mode +1))))

(defmacro opacity-modify (&optional dec)
  "Modify the transparency of the Emacs frame.
If DEC is t, decrease transparency;
otherwise increase it in 5%-steps"
  `(let* ((oldalpha (or (frame-parameter nil 'alpha) 99))
	  (newalpha ,(if dec
			 `(if (<= oldalpha 5)
			      0
			    (- oldalpha 5))
		       `(if (>= oldalpha 95)
			    100
			  (+ oldalpha 5)))))
     (and (>= newalpha frame-alpha-lower-limit)
	  (<= newalpha 100)
	  (modify-frame-parameters nil (list (cons 'alpha
						   newalpha))))))

;;; fullscreen stuff
(defvar *fullscreen-p* nil "Check if fullscreen is on or off.")
(defconst +width+ 100 "My prefered non-fullscreen width.")

(defmacro my-non-fullscreen ()
  "Exit fullscreen."
  `(if (fboundp 'w32-send-sys-command)
       ;; WM_SYSCOMMAND restore #xf120
       (w32-send-sys-command 61728)
     (set-frame-parameter nil 'width +width+)
     (set-frame-parameter nil 'fullscreen 'fullheight)))

(defmacro my-fullscreen ()
  "Go fullscreen."
  `(if (fboundp 'w32-send-sys-command)
       ;; WM_SYSCOMMAND maximize #xf030
       (w32-send-sys-command 61488)
     (set-frame-parameter nil 'fullscreen 'fullboth)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; extension independent functions

(defun my-toggle-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (if (setq *fullscreen-p* (not *fullscreen-p*))
      (my-fullscreen)
    (my-non-fullscreen)))

(defun nuke-buffers (reg-ex)
  "Kill buffers whose name is matched by REG-EX.
With prefix arg, leave current."
  (interactive (list (read-regexp "Buffer names to kill?" ".*")))
  (do-buffers buf
	      (when (string-match-p reg-ex (or (buffer-name buf) ""))
		(kill-buffer buf)))
  (when (string-equal reg-ex ".*") (delete-other-windows)))

(defun nuke-modes (reg-ex)
  "Kill buffers whose major mode name is matched by REG-EX.
With prefix arg, leave current."
  (interactive (list (read-regexp "Major modes to kill?"
				  (symbol-name major-mode))))
  (save-excursion
    (do-buffers buf
		(set-buffer buf)
		(when (string-match-p reg-ex (symbol-name major-mode))
		  (kill-buffer buf)))))

;;; themes
(defun faces-generic ()
  "My prefered faces which differ from default."
  (custom-set-faces
   '(font-lock-comment-face
     ((((class grayscale) (background light))
       :foreground "DimGray" :weight bold :slant italic)
      (((class grayscale) (background dark))
       :foreground "LightGray" :weight bold :slant italic)
      (((class color) (min-colors 88) (background light))
       :foreground "Firebrick")
      (((class color) (min-colors 88) (background dark))
       :foreground "chocolate1")
      (((class color) (min-colors 16) (background light))
       :foreground "red")
      (((class color) (min-colors 16) (background dark))
       :foreground "red1")
      (((class color) (min-colors 8))
       :foreground "red")
      (t :weight bold :slant italic)))
   '(mode-line
     ((default :foreground "black" :box (:line-width 1 :style "none")
	:width condensed :height 90 :family "neep")
      (((class color) (min-colors 88)) :background "DarkSlateGray")
      (t :background "green")))
   '(mode-line-inactive
     ((default :box (:line-width 1 :style "none")
	:width condensed :height 80 :family "neep")
      (((class color) (min-colors 88))
       :foreground "DarkSlateGray" :background "honeydew4")
      (t :foreground "white" :background "black")))
   '(mode-line-buffer-id
     ((default :inherit mode-line :foreground "black")
      (((class color) (min-colors 88))
       :background "CadetBlue" :weight extrabold)
      (t :background "green" :weight normal)))
   '(tabbar-default ((default :inherit variable-pitch)
		     (((background dark))
		      :background "#111" :foreground "#0c0")
		     (t :background "LightGray" :foreground "black")))
   '(tabbar-selected
     ((default :inherit tabbar-default)
      (((class color) (min-colors 88) (background dark))
       :background "black" :box (:line-width 2 :color "black")
       :foreground "DeepSkyBlue")
      (((background dark)) :background "black" :foreground "white"
       :box (:line-width 2 :color "black"))
      (t :background "white" :box (:line-width 2 :color "LightGray")
	 :foreground "DeepSkyBlue")))
   '(tabbar-unselected
     ((default :inherit tabbar-default)
      (((class color) (min-colors 88) (background dark))
       :background "#222" :foreground "DarkCyan"
       :box (:line-width 2 :color "#090909"))
      (((background dark))
       :background "gray" :foreground "cyan"
       :box (:line-width 2 :color "gray"))
      (((class color) (min-colors 88) (background light))
       :background "gray" :foreground "black"
       :box (:line-width 2 :color "white"))
      (t :background "black" :foreground "white"
	 :box (:line-width 2 :color "white"))))
   '(tabbar-button
     ((default (:inherit tabbar-default))
      (((background dark)) :background "black" :foreground "#0c0"
       :box (:line-width 2 :color "black"))
      (t :background "white" :foreground "black"
	 :box (:line-width 2 :color "LightGray"))))
   '(tabbar-button-face
     ((default (:inherit tabbar-default-face))
      (((background dark)) :background "black" :foreground "#0c0"
       :box (:line-width 2 :color "black"))
      (t :background "white" :foreground "black"
	 :box (:line-width 2 :color "LightGray"))))
   '(tabbar-selected-face
     ((t :inherit tabbar-default-face :background "black"
	 :foreground "SpringGreen"
	 :box (:line-width 2 :color "black"))))
   '(tabbar-separator ((t :foreground "#0c0" :background "#111")))
   '(tabbar-separator-face ((t :foreground "#0c0" :background "#111")))
   '(highlight-changes ((((class color) (min-colors 88))
			 :background "#382f2f")
			(t :background "orange")))
   '(highlight-changes-delete ((((class color) (min-colors 88))
				:background "#916868")
			       (t :background "red")))
   '(highlight ((((class color) (min-colors 88) (background light))
		 :background "darkseagreen2")
		(((class color) (min-colors 88) (background dark))
		 :background "SeaGreen")
		(((class color) (min-colors 16) (background light))
		 :background "darkseagreen2")
		(((class color) (min-colors 16) (background dark))
		 :background "darkolivegreen")
		(((class color) (min-colors 8))
		 :background "green" :foreground "black")
		(t :inverse-video t)))
   '(region ((((class color) (min-colors 88) (background dark))
	      :background "DarkSlateGray" :foreground nil)
	     (((class color) (min-colors 88) (background light))
	      :background "lightgoldenrod2")
	     (((class color) (min-colors 16) (background dark))
	      :background "blue3")
	     (((class color) (min-colors 16) (background light))
	      :background "lightgoldenrod2")
	     (((class color) (min-colors 8))
	      :background "cyan" :foreground "white")
	     (((type tty) (class mono)) :inverse-video t)
	     (t :background "gray")))
   '(hl-line ((((class color) (min-colors 88) (background light))
	       :background "darkseagreen2")
	      (((class color) (min-colors 88) (background dark))
	       :background "#123")
	      (((class color) (min-colors 16) (background light))
	       :background "darkseagreen2")
	      (((background dark)) :background "blue")
	      (t :inherit highlight)))
   '(cursor ((((class color)) :background "DeepSkyBlue")
	     (((background light)) :background "black")
	     (t :background "white")))
   '(show-paren-match-face
     ((((class color) (background dark)) :background "DarkRed")
      (((class color) (background light)) :background "red")
      (((background dark)) :background "grey50")
      (t :background "gray")))))

(defun faces-fix (&optional light)
  "Set some important faces.  If LIGHT is not given, look current.
If LIGHT is `:dark', let it be darkness, otherwise light."
  (if (or (eq light :dark)
	  (and (not light)
	       (equal (face-background 'default) "black")))
      (custom-set-faces
       '(default ((default (:background "black" :height 80))
		  (((class color) (min-colors 88))
		   (:foreground "wheat"))
		  (t (:foreground "white")))))
    (custom-set-faces
     '(default ((t (:foreground "black" :height 80
				:background "cornsilk"))))))
  (ignore-errors
    (set-face-font 'default (win-or-nix "Consolas" "Inconsolata")))
  (let ((frame (selected-frame)))
    (modify-frame-parameters frame '((alpha . 99)))
    (set-frame-height frame 40)))

(defun switch-faces (light)
  "Set dark faces.  With prefix, light."
  (interactive "P")
  (faces-fix (or light :dark))
  (faces-generic))

(defun reset-frame-faces (frame)
  "Execute once in the first graphical new FRAME.
Reset some faces which --daemon doesn't quite set.
Remove hook when done."
  (select-frame frame)
  (cond ((window-system frame)
	 (faces-fix :dark)		; start dark
	 (remove-hook 'after-make-frame-functions 'reset-frame-faces))
	((equal (face-background 'default) "black")
	 (set-face-background 'default "black" frame)
	 (set-face-foreground 'default "white" frame))))

(win-or-nix (defun hide-emacs ()
	      "Keep emacs running hidden on exit."
	      (interactive)
	      (server-edit)
	      (make-frame-invisible nil t)))

(defun pretty-lambdas ()
  "Show an actual lambda instead of the string `lambda'."
  (font-lock-add-keywords nil
			  `(("(\\(lambda\\>\\)"
			     (0 (progn
				  (compose-region
				   (match-beginning 1) (match-end 1)
				   ,(make-char 'greek-iso8859-7 107))
				  nil))))))

(defun su-mode-line-function ()
  "Change modeline when root."
  (when (string-match "^/su\\(do\\)?:" default-directory)
    (make-local-variable 'mode-line-buffer-identification)
    (setq mode-line-buffer-identification
	  (cons
	   (propertize
	    (concat "su" (match-string 1 default-directory) ": ")
	    'face 'font-lock-warning-face)
	   (default-value 'mode-line-buffer-identification)))))

(defconst +apropos-url-alist+
  '(("^gw?:? +\\(.*\\)" .		; Google Web
     "http://www.google.bg/search?q=\\1")
    ("^g!:? +\\(.*\\)" .		; Google Lucky
     "http://www.google.bg/search?btnI=I%27m+Feeling+Lucky&q=\\1")
    ("^gl:? +\\(.*\\)" .		; Google Linux
     "http://www.google.bg/linux?q=\\1")
    ("^gi:? +\\(.*\\)" .		; Google Images
     "http://images.google.bg/images?sa=N&tab=wi&q=\\1")
    ("^gv:? +\\(.*\\)" .		; Google Video
     "http://video.google.bg/videosearch?q=\\1")
    ("^gg:? +\\(.*\\)" .		; Google Groups
     "http://groups.google.bg/groups?q=\\1")
    ("^gdir:? +\\(.*\\)" .		; Google Directory
     "http://www.google.bg/search?&sa=N&cat=gwd/Top&tab=gd&q=\\1")
    ("^gn:? +\\(.*\\)" .		; Google News
     "http://news.google.bg/news?sa=N&tab=dn&q=\\1")
    ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(\\w+://.*\\)" . ; Google Translate URL
     "http://translate.google.bg/translate?langpair=\\1|\\2&u=\\3")
    ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(.*\\)" . ; Google Translate Text
     "http://translate.google.bg/translate_t?langpair=\\1|\\2&text=\\3")
    ("^gd:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(.*\\)" . ; Google Dictionary
     "http://www.google.bg/dictionary?aq=f&langpair=\\1|\\2&q=\\3&hl=\\1")
    ("^w:? +\\(.*\\)" .			; Wikipedia en
     "http://en.wikipedia.org/wiki/Special:Search?search=\\1")
    ("^yt:? +\\(.*\\)" .		; YouTube
     "http://www.youtube.com/results?search_query=\\1&search=Search")
    ("^/\\.$" .				; Slashdot
     "http://www.slashdot.org")
    ("^/\\.:? +\\(.*\\)" .		; Slashdot search
     "http://www.osdn.com/osdnsearch.pl?site=Slashdot&query=\\1")
    ("^fm$" .				; Freshmeat
     "http://www.freshmeat.net")
    ("^r:? +\\(.*\\)" .			; sub Reddits (mobile)
     "http://m.reddit.com/r/\\1")
    ("^hn$" .				; HackerNews
     "http://news.ycombinator.com")
    ("^ewiki$" .			; Emacs Wiki
     "http://www.emacswiki.org")
    ("^ewiki:? +\\(.*\\)" .		; Emacs Wiki Search
     "http://www.emacswiki.org/cgi-bin/wiki?search=\\1")
    ("^ewiki2:? +\\(.*\\)" .		; Google Emacs Wiki
     "http://www.google.bg/cse?cx=004774160799092323420%3A6-ff2s0o6yi&q=\\1&sa=Search")
    ("^arda$" .				; The Encyclopedia of Arda
     "http://www.glyphweb.com/arda/")
    ("^hayoo:? +\\(.*\\)" .		; Hayoo
     "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query=\\1")
    ("^ma:? +\\(.*\\)" .	       ; Encyclopaedia Metallum, bands
     ;;"http://www.metal-archives.com/search.php?type=band&string=\\1"
     "http://www.google.bg/search?q=\\1&as_sitesearch=metal-archives.com")
    ("^aur:? +\\(.*\\)" .	       ; Search in Arch Linux's Aur packages
     "http://aur.archlinux.org/packages.php?&K=\\1"))
  "Search engines and sites.")

(defun browse-apropos-url (text &optional new-window)
  "Search for TEXT by some search engine in NEW-WINDOW if needed."
  (interactive (browse-url-interactive-arg "Location: "))
  (let ((text (replace-regexp-in-string
	       "^ *\\| *$" ""
	       (replace-regexp-in-string "[ \t\n]+" " " text)))
	(apropo-reg "^$"))
    (let ((url (assoc-default text +apropos-url-alist+
			      (lambda (a b)
				(when (string-match a b)
				  (setq apropo-reg a)))
			      text)))
      (browse-url (replace-regexp-in-string apropo-reg url text)
      		  new-window))))

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
 '(completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(search-highlight t)
 '(query-replace-highlight t)
 '(require-final-newline t)
 '(tool-bar-mode nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; appearance

;;; geometry
(add-to-list 'default-frame-alist (cons 'width +width+))
(win-or-nix (set-frame-width (selected-frame) +width+))

(if (window-system)
    (faces-fix :dark)
  ;; hook, execute only first time in graphical frame
  ;;  (and indefinite times in terminal frames till then)
  (add-hook 'after-make-frame-functions 'reset-frame-faces))

(faces-generic)

(win-or-nix (global-set-key "\C-x\C-c" 'hide-emacs))

(global-set-key [f10] 'my-toggle-fullscreen)
(global-set-key (kbd "M-<f10>") 'menu-bar-mode)

;;; opacity
(global-set-key (kbd "C-=") (lambda () "Increase window opacity."
			      (interactive)
			      (opacity-modify)))
(global-set-key (kbd "C-+") (lambda () "Decrease window opacity."
			      (interactive)
			      (opacity-modify t)))
(global-set-key (kbd "C-M-=") (lambda () "Set window opacity to 99%."
				(interactive)
				(modify-frame-parameters
				 nil '((alpha . 99)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; UTF and languages

;;; unicode + bulgarian phonetic
(add-hook 'set-language-environment-hook
	  (lambda ()			; set alternating input language (C-\)
	    (setq default-input-method "bulgarian-phonetic")))
(win-or-nix nil (setq locale-coding-system 'utf-8))
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(global-set-key (kbd "C-s-k")	  ; revert buffer encoding to cyrillic
		(lambda () (interactive)
		  (revert-buffer-with-coding-system 'cp1251)))

;;; ispell
(setq-default ispell-program-name "aspell")
(setq ispell-dictionary "english")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; useful stuff

;;; browse web
(global-set-key [f6] 'browse-apropos-url)

(setq browse-url-new-window-flag t
      browse-url-firefox-new-window-is-tab t
      browse-url-mozilla-new-window-is-tab t
      browse-url-epiphany-new-window-is-tab t
      browse-url-galeon-new-window-is-tab t
      browse-url-netscape-new-window-is-tab t)

;; gdb
(setq gdb-many-windows t)

;; Imenu
(global-set-key (kbd "C-`") 'imenu)

;; highlight current line, turn it on for all modes by default
(when (fboundp 'global-hl-line-mode) (global-hl-line-mode t))

;;; highlight changes in documents
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil) ; hide initially

;; toggle changes visibility
(global-set-key [f7] 'highlight-changes-visible-mode)

;; remove the change-highlight in region
(global-set-key (kbd "S-<f7>") 'highlight-changes-remove-highlight)

;; alt-pgup/pgdown jump to the previous/next change
(global-set-key (kbd "<M-prior>") 'highlight-changes-previous-change)
(global-set-key (kbd "<M-next>") 'highlight-changes-next-change)

;;; jump through errors/results
(global-set-key (kbd "<C-M-prior>") 'previous-error)
(global-set-key (kbd "<C-M-next>") 'next-error)

;;; clipboard
(global-set-key (kbd "C-s-v") 'clipboard-yank)
(global-set-key (kbd "C-s-c") 'clipboard-kill-ring-save)

;;; enable some actions
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; Use y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

(defadvice kill-ring-save (before slick-copy
				  activate compile)
  "When called interactively with no active region, copy current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line.")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut
			       activate compile)
  "When called interactively with no active region, kill current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Killed line.")
     (list (line-beginning-position) (line-beginning-position 2)))))

;;; recentf
(when (require-maybe 'recentf)		; save recently used files
  (setq recentf-max-saved-items 100	; max save 100
	recentf-save-file (concat +home-path+ ".emacs.d/recentf")
	recentf-max-menu-items 15)	       ; max 15 in menu
  (recentf-mode t))			       ; turn it on

;;; backups
(setq make-backup-files t		; do make backups
      backup-by-copying t		; and copy them ...
      backup-directory-alist `(("." . ,(concat +home-path+ ; ... here
					       ".emacs.d/backup/")))
      tramp-backup-directory-alist backup-directory-alist
      version-control t
      kept-new-versions 5
      kept-old-versions 2
      delete-old-versions t)

;;; bookmarks
(setq
 bookmark-default-file "~/.emacs.d/bookmarks" ; keep my ~/ clean
 bookmark-save-flag 1)                        ; autosave each change

;;; tramp-ing as root
(hook-modes su-mode-line-function
	    find-file-hooks dired-mode-hook)

(mouse-avoidance-mode 'jump)	  ; mouse ptr when cursor is too close
(icomplete-mode t)		  ; completion in minibuffer
(partial-completion-mode t)	  ; be smart with completion

(when (fboundp 'set-fringe-mode)	; emacs22+
  (set-fringe-mode 1))			; space left of col1 in pixels

(when (fboundp 'file-name-shadow-mode)	; emacs22+
  (file-name-shadow-mode t))	    ; be smart about filenames in mbuf

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; extensions

;;; TabBar
(when (require-maybe 'tabbar)
  (tabbar-mode)

  (defadvice tabbar-buffer-help-on-tab (after tabbar-add-file-path
					      (tab) activate compile)
    "Attach full file path to help message.
If not a file, attach current directory."
    (let* ((tab-buffer (tabbar-tab-value tab))
	   (full-path (buffer-file-name tab-buffer)))
      (if full-path
	  (setq ad-return-value (concat full-path "\n"
					ad-return-value))
	(save-excursion
	  (set-buffer tab-buffer)
	  (setq ad-return-value (concat default-directory "\n"
					ad-return-value))))))

  (defadvice tabbar-buffer-groups (around tabbar-groups-extension
					  activate compile)
    "Add some rules for grouping tabs to run before original."
    (cond
     ((memq major-mode '(woman-mode completion-list-mode
				    slime-fuzzy-completions-mode))
      (setq ad-return-value (list "Help")))
     ((string-match "^\\(wl\\|mime\\)" (symbol-name major-mode))
      (setq ad-return-value (list "Mail")))
     ((string-match "^*inferior" (buffer-name))
      (setq ad-return-value (list "Process")))
     ((string-match "^*slime" (buffer-name))
      (setq ad-return-value (list "Slime")))
     ((memq major-mode '(fundamental-mode org-mode))
      (setq ad-return-value (list "Common")))
     (t ad-do-it)))	      ; if none of above applies, run original

  (def-interactive-arg tabbar-move-next
    "Go to next tab. With prefix, next group."
    (tabbar-forward-tab) (tabbar-forward-group))
  (def-interactive-arg tabbar-move-prev
    "Go to previous tab. With prefix, previous group."
    (tabbar-backward-tab) (tabbar-backward-group))

  (global-set-key (kbd "C-<tab>") 'tabbar-move-next)
  (global-set-key [backtab] 'tabbar-move-prev) ; for terminal
  (global-set-key (win-or-nix (kbd "C-S-<tab>")
  			      (kbd "<C-S-iso-lefttab>"))
  		  'tabbar-move-prev)

  (add-hook 'org-load-hook
	    (lambda ()
	      (define-keys org-mode-map
		(kbd "C-<tab>") nil
		[backtab] nil
		(win-or-nix (kbd "C-S-<tab>")
			    (kbd "<C-S-iso-lefttab>")) nil)))

  ;; remove buffer name from modeline as it now becomes redundant
  (setq-default mode-line-buffer-identification ""))

;;; Anything
(when (require-maybe 'anything-config)
  (global-set-key [f5] 'anything)
  (setq anything-sources
	'(anything-c-source-buffers+
	  anything-c-source-recentf
	  anything-c-source-files-in-current-dir+
	  anything-c-source-ffap-guesser
	  anything-c-source-locate
	  anything-c-source-emacs-functions-with-abbrevs
	  anything-c-source-emacs-variables
	  anything-c-source-info-elisp
	  anything-c-source-info-pages
	  anything-c-source-man-pages
	  anything-c-source-bookmarks
	  anything-c-source-semantic))

  (require-maybe 'anything-match-plugin)
  (when (require-maybe 'anything-etags)
    (setq anything-sources
	  (nconc anything-sources
		 '(anything-c-source-etags-select)))))

;;; ErgoEmacs minor mode
(when (file-exists-p (concat +extras-path+ "/ergo"))
  (setenv "ERGOEMACS_KEYBOARD_LAYOUT" "colemak")
  (load "ergoemacs-mode")

  (define-key isearch-mode-map ergoemacs-recenter-key
    'recenter-top-bottom)
  (when (fboundp 'recenter-top-bottom)
    (define-key ergoemacs-keymap ergoemacs-recenter-key
      'recenter-top-bottom))
  (define-keys ergoemacs-keymap
    "\M-3" 'move-cursor-previous-pane
    "\M-#" 'move-cursor-next-pane)

  ;; workaround arrows not active in terminal with ErgoEmacs active
  (when (require-maybe 'anything)
    (define-keys anything-map
      "\C-d" 'anything-next-line
      "\C-u" 'anything-previous-line
      (kbd "C-M-d") 'anything-next-source
      (kbd "C-M-u") 'anything-previous-source))

  (ergoemacs-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Lisp goodies

;; build appropriate `activate-lisp-minor-modes'
(eval (macroexpand '(active-lisp-modes))) ; ugly but works

;; Hook convenient s-exp minor modes to some major modes.
(hook-modes activate-lisp-minor-modes
	    inferior-lisp-mode-hook lisp-mode-hook
	    lisp-interaction-mode-hook
	    emacs-lisp-mode-hook ielm-mode-hook
	    inferior-scheme-mode-hook scheme-mode-hook)

;;; Redshank
(when (require-maybe 'redshank-loader
		     (concat +extras-path+
			     "lisp-modes/redshank/redshank-loader"))
  (eval-after-load "redshank-loader"
    `(redshank-setup '(lisp-mode-hook slime-repl-mode-hook
				      inferior-lisp-mode-hook)
		     t)))

;;; elisp stuff
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(hook-modes turn-on-eldoc-mode
	    emacs-lisp-mode-hook lisp-interaction-mode-hook
	    ielm-mode-hook)
(define-key emacs-lisp-mode-map "\M-g" 'lisp-complete-symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Programming extensions

;;; Clojure
(when (require-maybe 'clojure-mode)
  (defconst +clojure-dir+ (concat +home-path+ (win-or-nix "bin/" ".")
				  "clojure")
    "Path to the Clojure directory.")
  (and (file-exists-p +clojure-dir+)
       (require-maybe 'swank-clojure-autoload)
       (swank-clojure-config
	(setq
	 swank-clojure-jar-path (concat +clojure-dir+ "/clojure.jar")
	 swank-clojure-extra-classpaths
	 (cons (concat +extras-path+
		       "/clojure/swank-clojure/src/main/clojure/")
	       (directory-files +clojure-dir+ t ".\\(jar\\|clj\\)$"))
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
	     common-lisp-hyperspec-root
	     (concat "file://" +home-path+ (win-or-nix "docs"
						       "Documents")
		     "/HyperSpec/")
	     slime-net-coding-system
	     (find-if 'slime-find-coding-system
		      '(utf-8-unix iso-latin-1-unix
				   iso-8859-1-unix binary)))))

  (add-hook 'slime-repl-mode-hook 'activate-lisp-minor-modes)
  (define-key slime-mode-map "\M-g" 'slime-complete-symbol)

  (when (boundp 'ergoemacs-mode)	; fix some shortcuts
    (let ((ergo-layout (getenv "ERGOEMACS_KEYBOARD_LAYOUT")))
      (cond ((equal ergo-layout "colemak")
	     (define-keys slime-mode-map
	       "\M-k" 'slime-next-note
	       "\M-K" 'slime-previous-note
	       "\M-n" nil
	       "\M-p" nil))
	    ((equal ergo-layout "en")
	     (define-keys slime-mode-map
	       "\M-N" 'slime-previous-note
	       "\M-p" nil)))))

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
	    (lambda () (interactive)
	      (slime-redirect-inferior-output)
	      (define-keys slime-mode-map
		"\C-cd" 'slime-java-describe
		"\C-cD" 'slime-javadoc)
	      (define-keys slime-repl-mode-map
		"\C-cd"	'slime-java-describe
		"\C-cD" 'slime-javadoc)))

;;; Local JavaDoc to Slime
  (setq slime-browse-local-javadoc-root
	(concat (win-or-nix (concat +home-path+ "docs")
			    "/usr/share/doc")
		"/javadoc"))

  (defun slime-browse-local-javadoc (ci-name)
    "Browse local JavaDoc documentation on Java class/Interface at point CI-NAME."
    (interactive
     (list (slime-read-symbol-name "Class/Interface name: ")))
    (or ci-name	(error "No name given"))
    (let ((name (replace-regexp-in-string "\\$" "." ci-name))
	  (path (concat (expand-file-name
			 slime-browse-local-javadoc-root)
			"/docs/api/")))
      (with-temp-buffer
	(insert-file-contents (concat path "allclasses-noframe.html"))
	(let ((l (delq nil
		       (mapcar (lambda (rgx)
				 (let* ((r (concat
					    "\\.?\\(" rgx
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
				    (define-key slime-mode-map "\C-cb"
				      'slime-browse-local-javadoc)
				    (define-key slime-repl-mode-map
				      "\C-cb"
				      'slime-browse-local-javadoc)))

;;; (or CLisp SBCL)
  (add-to-list 'slime-lisp-implementations
	       (win-or-nix (list 'clisp (list
					 (concat +home-path+
						 "bin/clisp/clisp.exe")
					 "-K" "full"))
			   '(sbcl ("/usr/local/bin/sbcl")))))

(setq inferior-lisp-program
      (win-or-nix (concat +home-path+ "bin/clisp/clisp.exe -K full")
		  "/usr/local/bin/sbcl"))

;;; Scheme
(when (file-exists-p (concat +extras-path+ "lisp-modes/quack.el"))
  (setq quack-global-menu-p nil)
  (require-maybe 'quack))

;;; CLIPS
(when (require-maybe 'inf-clips)
  (push '("\.clp$" . clips-mode) auto-mode-alist)
  (setq inferior-clips-program
	(win-or-nix (concat +win-path+
			    "Program Files/CLIPS/Bin/CLIPSDOS.exe")
		    "clips"))

  (hook-modes (activate-lisp-minor-modes (setq indent-region-function
					       nil))
	      clips-mode-hook inferior-clips-mode-hook))

;;; Caml
(when (require-maybe 'tuareg)
  (autoload 'tuareg-mode "tuareg"
    "Major mode for editing Caml code" t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
  (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode)
			      auto-mode-alist)))

;;; Haskell
(when (file-exists-p (concat +extras-path+ "haskell"))
  (load "haskell-site-file")
  (hook-modes (turn-on-haskell-doc-mode turn-on-haskell-indentation)
	      haskell-mode-hook)

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
  (setq prolog-program-name "pl"
	prolog-system 'swi
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
  (autoload 'visual-basic-mode "visual-basic-mode"
    "Visual Basic mode." t)
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
(when (file-exists-p (concat +extras-path+ "tex"))
  (load "auctex" nil t)
  (load "preview-latex" nil t)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'TeX-mode-hook
	    (lambda ()
	      (define-key TeX-mode-map "\M-g" 'TeX-complete-symbol))))

;;; LaTeX beamer
;; allow for export=>beamer by placing
;; #+LaTeX_CLASS: beamer in org files
(or (boundp 'org-export-latex-classes)
    (setq org-export-latex-classes nil))

(add-to-list 'org-export-latex-classes
	     '("beamer"
	       "\\documentclass[11pt]{beamer}\n
      \\mode<presentation>\n
      \\usetheme{Antibes}\n
      \\usecolortheme{lily}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[bulgarian]{babel}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{Sofia University, FMI}\n
      \\subject{RMRF}\n"
	       ("\\section{%s}" . "\\section*{%s}")

	       ("\\begin{frame}[fragile]\\frametitle{%s}"
		"\\end{frame}"
		"\\begin{frame}[fragile]\\frametitle{%s}"
		"\\end{frame}")))

;;; Ditaa
(let ((ditaa-path (concat +extras-path+ "misc/ditaa.jar")))
  (when (file-exists-p ditaa-path)
    (setq org-ditaa-jar-path ditaa-path)

    (defun ditaa-generate ()
      "Invoke ditaa over current buffer."
      (interactive)
      (shell-command (concat "java -jar " org-ditaa-jar-path " "
			     buffer-file-name)))))

;;; CompletionUI
(when (require-maybe 'completion-ui)
  (global-set-key "\M-G" 'complete-dabbrev)
  (define-key emacs-lisp-mode-map (kbd "C-c TAB") 'complete-elisp))

;;; Auto Install
(when (require-maybe 'auto-install)
  (setq auto-install-directory (concat +extras-path+
				       "auto-install-dir/"))
  (and (require-maybe 'anything)
       (require-maybe 'anything-auto-install)

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
	       (message "Auto-install removed from Anything."))
	   (setq anything-sources
		 (nconc
		  anything-sources
		  '(anything-c-source-auto-install-from-emacswiki
		    anything-c-source-auto-install-from-library)))
	   (ignore-errors
	     (auto-install-update-emacswiki-package-name t))
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
     (or (= 0 (call-process "global" nil nil nil " -p"))
	 (let ((olddir default-directory)
	       (topdir (read-directory-name
			"gtags: top of source tree:"
			default-directory)))
	   (cd topdir)
	   (shell-command "gtags && echo 'created tagfile'")
	   (cd olddir))		 ; restore
	 ;;  tagfile already exists; update it
	 (shell-command "global -u && echo 'updated tagfile'"))))

  (add-hook 'gtags-mode-hook (lambda ()
			       (local-set-key "\M-." 'gtags-find-tag)
			       (local-set-key "\M-,"
					      'gtags-find-rtag)))
  (add-hook 'c-mode-common-hook (lambda ()
				  (gtags-mode t)
				  ;;(gtags-create-or-update)
				  )))

;;; Wanderlust
(when (and (require-maybe 'wl) (require-maybe 'wl-draft))
  (autoload 'wl "wl" "Wanderlust" t)
  (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
  (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
  ;;(setq wl-init-file (concat +home-path+ ".emacs.d/wl.el"))

  ;; IMAP
  (setq elmo-maildir-folder-path (concat +home-path+ "Mail")
	elmo-imap4-default-server "imap.gmail.com"
	elmo-imap4-default-user "m00naticus@gmail.com"
	elmo-imap4-default-authenticate-type 'clear
	elmo-imap4-default-port 993
	elmo-imap4-default-stream-type 'ssl
	elmo-imap4-use-modified-utf7 t
	;; SMTP
	wl-smtp-connection-type 'starttls
	wl-smtp-posting-port 587
	wl-smtp-authenticate-type "plain"
	wl-smtp-posting-user "m00naticus@gmail.com"
	wl-smtp-posting-server "smtp.gmail.com"
	wl-local-domain "gmail.com"

	wl-default-folder "%inbox"
	wl-draft-folder ".drafts"
	wl-trash-folder ".trash"
	wl-spam-folder ".trash"
	wl-queue-folder ".queue"

	wl-folder-check-async t
	wl-fcc ".sent"
	wl-fcc-force-as-read t
	wl-from "Andrey Kotlarski <m00naticus@gmail.com>"
	wl-message-id-use-wl-from t
	wl-insert-mail-followup-to t
	wl-draft-buffer-style 'keep
	wl-subscribed-mailing-list '("slime-devel@common-lisp.net"
				     "tramp-devel@gnu.org"))

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
  ;;   (setq elmo-spam-scheme 'sa))

  ;; (defun my-wl-summary-refile (&optional folder)
  ;;     "Refile the current message to FOLDER.
  ;; If FOLDER is nil, use the default."
  ;;     (interactive)
  ;;     (wl-summary-refile (wl-summary-message-number) folder)
  ;;     (wl-summary-next)
  ;;     (message (concat "refiled to " folder)))

  ;; (define-key wl-summary-mode-map (kbd "b x") ; => Project X
  ;;   (lambda () (interactive)
  ;;     (my-wl-summary-refile ".project-x")))

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
 (let ((cygwin-dir (concat +win-path+ "cygwin/bin")))
   (when (and (file-exists-p cygwin-dir)
	      (require-maybe 'cygwin-mount))
     (cygwin-mount-activate)
     (setq w32shell-cygwin-bin cygwin-dir)))

;;; w3m
 (when (require-maybe 'w3m-load)
   (require-maybe 'mime-w3m)	     ; for integration with Wanderlust

   (defun w3m-browse-url-other-window (url &optional newwin)
     (interactive (browse-url-interactive-arg "w3m URL: "))
     (let ((pop-up-frames nil))
       (switch-to-buffer-other-window (w3m-get-buffer-create "*w3m*"))
       (w3m-browse-url url)))

   (define-keys w3m-mode-map
     "i" 'w3m-save-image
     "l" 'w3m-horizontal-recenter)
   (setq w3m-use-toolbar t
	 ;; w3m-use-mule-ucs t
	 w3m-use-cookies t
	 ;; detect w3m command, if present,
	 ;; make it default for most URLs
	 browse-url-browser-function
	 (cons '("^ftp:/.*" . (lambda (url &optional nf)
				(call-interactively
				 'find-file-at-point url)))
	       (if (equal (substring (shell-command-to-string
				      "w3m -version")
				     0 11)
			  "w3m version")
		   `(("video" . ,browse-url-browser-function)
		     ("youtube" . ,browse-url-browser-function)
		     ("." . w3m-browse-url))
		 `(("." . ,browse-url-browser-function)))))))

;;; handle ftp with emacs, if not set above
(or (consp browse-url-browser-function)
    (setq browse-url-browser-function
	  `(("^ftp:/.*" . (lambda (url &optional nf)
			    (call-interactively
			     'find-file-at-point url)))
	    ("." . ,browse-url-browser-function))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (server-start) ; using --daemon or emacsW32 instead

;;; init.el ends here
