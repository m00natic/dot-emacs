;; init.el --- Andrey Kotlarski's .emacs -*- mode:emacs-lisp -*-

;;; Commentary:
;; Utilized extensions:
;;  Anything related:
;;   Anything http://www.emacswiki.org/emacs/Anything
;;   anything-etags http://www.emacswiki.org/emacs/anything-etags.el
;;   anything-match http://www.emacswiki.org/emacs/AnythingPlugins
;;  Packages related:
;;   ELPA http://tromey.com/elpa
;;   auto-install http://www.emacswiki.org/emacs/AutoInstall
;;   anything-auto-install
;;  Programming languages related:
;;   SLIME http://common-lisp.net/project/slime
;;   Quack http://www.neilvandyke.org/quack
;;   clojure-mode http://github.com/technomancy/clojure-mode
;;   swank-clojure http://github.com/technomancy/swank-clojure
;;   clips-mode http://www.cs.us.es/software/clips
;;   Prolog http://bruda.ca/emacs-prolog
;;   haskell-mode http://projects.haskell.org/haskellmode-emacs
;;   tuareg-mode http://www-rocq.inria.fr/~acohen/tuareg/index.html.en
;;   Oz-mode http://www.mozart-oz.org
;;   Qi-mode http://code.google.com/p/qilang
;;   python-mode https://launchpad.net/python-mode
;;   CSharpMode http://www.emacswiki.org/emacs/CSharpMode
;;   VisualBasicMode http://www.emacswiki.org/emacs/VisualBasicMode
;;  Lisp goodies:
;;   highlight-parentheses http://nschum.de/src/emacs/highlight-parentheses
;;   hl-sexp http://edward.oconnor.cx/elisp/hl-sexp.el
;;   ParEdit http://www.emacswiki.org/emacs/ParEdit
;;   Redshank http://www.foldr.org/~michaelw/emacs/redshank
;;  networking:
;;   emacs-w3m http://emacs-w3m.namazu.org
;;   emacs-wget http://pop-club.hp.infoseek.co.jp/emacs/emacs-wget
;;   MLDonkey-el http://www.emacswiki.org/emacs/MlDonkey
;;   Wanderlust http://www.gohome.org/wl
;;   Gnus http://www.gnus.org
;;  misc:
;;   ErgoEmacs-mode http://xahlee.org/emacs/ergonomic_emacs_keybinding.html
;;   AUCTeX http://www.gnu.org/software/auctex
;;   Ditaa http://ditaa.sourceforge.net
;;   Ido-mode http://www.emacswiki.org/emacs/InteractivelyDoThings
;;   TabBar http://www.emacswiki.org/emacs/TabBarMode
;;   sml-modeline http://bazaar.launchpad.net/~nxhtml/nxhtml/main/annotate/head%3A/util/sml-modeline.el
;;   notify http://www.emacswiki.org/emacs/notify.el
;;   CompletionUI http://www.emacswiki.org/emacs/CompletionUI
;;   cygwin-mount http://www.emacswiki.org/emacs/cygwin-mount.el
;;   gtags http://www.gnu.org/software/global
;;   traverselisp http://mercurial.intuxication.org/hg/traverselisp
;;   Dictionary http://www.myrkr.in-berlin.de/dictionary/index.html
;;   EMMS http://www.gnu.org/software/emms
;;   Emacs Chess http://github.com/jwiegley/emacs-chess
;;   sudoku http://www.columbia.edu/~jr2075/elisp/index.html

;;; Code:
;; do some OS recognition and set main parameters
(defconst +winp+ (eval-when-compile
		   (memq system-type '(windows-nt ms-dos)))
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
     (eval-when-compile (concat (getenv "HOME") "/")))
   (eval-when-compile (concat (getenv "HOME") "/")))
  "Home path.")

(defconst +extras-path+ (eval-when-compile
			  (concat +home-path+ ".emacs.d/extras/"))
  "Elisp extensions' path.")

;; add `+extras-path+' and subdirs to `load-path'
(and (fboundp 'normal-top-level-add-subdirs-to-load-path)
     (file-exists-p (eval-when-compile +extras-path+))
     (let ((default-directory (eval-when-compile +extras-path+)))
       (push (eval-when-compile +extras-path+) load-path)
       (normal-top-level-add-subdirs-to-load-path)))

(let ((bin-path (eval-when-compile (concat +extras-path+ "bin"))))
  (when (file-exists-p bin-path)
    (add-to-list 'exec-path bin-path)))

;; set default directory for `*scratch*' and some info
(setq default-directory (eval-when-compile +home-path+)
      add-log-full-name "Andrey Kotlarski")

(setenv "EMAIL" "m00naticus@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; extension independent macros

(defmacro when-library (library &rest body)
  "When LIBRARY is in `load-path' leave BODY.
If LIBRARY is a list, check whether every element is in `load-path'."
  (if (consp library)
      (let ((loadp t))
	(dolist (lib library)
	  (when loadp
	    (setq loadp (locate-library lib))))
	(when loadp
	  (if (consp body)
	      `(progn ,@body)
	    body)))
    (when (locate-library library)
      (if (consp body)
	  `(progn ,@body)
	body))))

(defmacro hook-modes (functions &rest modes)
  "Hook a list of FUNCTIONS (or atom) to MODES.
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

(defmacro active-lisp-modes ()
  "Activate convenient s-expressions which are present."
  `(progn (pretty-lambdas)
	  ,(when-library "hl-sexp" '(hl-sexp-mode +1))
	  ,(when-library "highlight-parentheses" ; from ELPA
			 '(highlight-parentheses-mode +1))
	  ,(when-library "paredit" '(paredit-mode +1))))

(defmacro faces-generic ()
  "My prefered faces which differ from default."
  `(progn
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
	 (((class color) (background light)) :foreground "red")
	 (((class color) (background dark)) :foreground "red1")
	 (t :weight bold :slant italic)))
      '(mode-line
	((default :box (:line-width 1 :style "none")
	   :width condensed :height 90 :family "neep")
	 (((class color) (min-colors 88) (background dark))
	  :foreground "black" :background "DarkSlateGray")
	 (((class color) (min-colors 88) (background light))
	  :foreground "white" :background "DarkSlateGray")
	 (t :background "green")))
      '(mode-line-inactive
	((default :box (:line-width 1 :style "none")
	   :width condensed :height 80 :family "neep")
	 (((class color) (min-colors 88))
	  :foreground "DarkSlateGray" :background "honeydew4")
	 (t :foreground "white" :background "black")))
      '(mode-line-buffer-id
	((default :inherit mode-line :foreground "black")
	 (((class color) (min-colors 88) (background light))
	  :background "CadetBlue" :weight extrabold)
	 (((class color) (min-colors 88) (background dark))
	  :background "honeydew4" :weight extrabold)
	 (t :background "green" :weight normal)))
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
		 :background "#333" :foreground nil)
		(((class color) (min-colors 88) (background light))
		 :background "lightgoldenrod2" :foreground nil)
		(((class color) (min-colors 16) (background dark))
		 :background "blue3" :foreground nil)
		(((class color) (min-colors 16) (background light))
		 :background "lightgoldenrod2" :foreground nil)
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
	 (t :background "gray"))))
     ,(when-library
       "tabbar"
       '(custom-set-faces
	 '(tabbar-default ((t :inherit variable-pitch)))
	 '(tabbar-selected
	   ((default :inherit tabbar-default)
	    (((class color) (min-colors 88) (background light))
	     :background "white" :foreground "DeepSkyblue"
	     :box (:line-width 1 :color "LightGray"))
	    (((class color) (min-colors 88) (background dark))
	     :background "black" :foreground "DeepSkyBlue"
	     :box (:line-width 1 :color "black"))
	    (((background dark)) :background "black"
	     :foreground "white")
	    (t :background "white" :foreground "cyan")))
	 '(tabbar-unselected
	   ((default :inherit tabbar-default)
	    (((class color) (min-colors 88) (background light))
	     :background "gray" :foreground "DarkSlateGray"
	     :box (:line-width 2 :color "white"))
	    (((class color) (min-colors 88) (background dark))
	     :background "#222" :foreground "DarkCyan"
	     :box (:line-width 2 :color "#090909"))
	    (((background dark)) :background "white"
	     :foreground "black")
	    (t :background "black" :foreground "cyan")))
	 '(tabbar-button
	   ((default (:inherit tabbar-default))
	    (((background dark)) :background "black"
	     :foreground "#0c0"
	     :box (:line-width 2 :color "black"))
	    (t :background "white" :foreground "black"
	       :box (:line-width 2 :color "LightGray"))))
	 '(tabbar-separator ((t :background "#111")))))
     ,(when-library
       "sml-modeline"
       '(custom-set-faces
	 '(sml-modeline-vis-face ((default (:inherit hl-line))
				  (((class color) (min-colors 88))
				   :foreground "green"
				   :box (:line-width 1))
				  (t :foreground "SeaGreen")))
	 '(sml-modeline-end-face ((t :inherit default
				     :box (:line-width 1))))))))

(defmacro opacity-modify (&optional dec)
  "Modify the transparency of the Emacs frame.
If DEC is t, decrease transparency;
otherwise increase it in 5%-steps"
  `(let* ((oldalpha (or (frame-parameter nil 'alpha) 99))
	  (newalpha ,(if dec
			 '(if (<= oldalpha 5)
			      0
			    (- oldalpha 5))
		       '(if (>= oldalpha 95)
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
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND restore #xf120
      '(w32-send-sys-command 61728)
    '(progn
       (set-frame-parameter nil 'width +width+)
       (set-frame-parameter nil 'fullscreen 'fullheight))))

(defmacro my-fullscreen ()
  "Go fullscreen."
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND maximize #xf030
      '(w32-send-sys-command 61488)
    '(set-frame-parameter nil 'fullscreen 'fullboth)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; extension independent functions

(defun my-toggle-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (if (setq *fullscreen-p* (not *fullscreen-p*))
      (my-fullscreen)
    (my-non-fullscreen)))

;;; themes

(defun switch-faces (light)
  "Set dark faces.  With prefix, LIGHT."
  (interactive "P")
  (if light
      (custom-set-faces
       '(default ((t (:foreground "black" :height 80
				  :background "cornsilk")))))
    (custom-set-faces
     '(default ((default (:background "black" :height 80))
		(((class color) (min-colors 88))
		 (:foreground "wheat"))
		(t (:foreground "white"))))))
  (ignore-errors
    (set-face-font 'default (win-or-nix "Consolas" "Inconsolata")))
  (let ((frame (selected-frame)))
    (modify-frame-parameters frame '((alpha . 99)))
    (set-frame-height frame 50))
  (faces-generic))

(defun solar-time-to-24 (time-str)
  (if (string-match "\\(.*\\)[/:-]\\(..\\)\\(.\\)"
		    time-str)
      (format "%02d:%s"
	      (if (equal (match-string 3 time-str) "p")
		  (+ (string-to-number
		      (match-string 1 time-str))
		     12)
		(string-to-number
		 (match-string 1 time-str)))
	      (match-string 2 time-str))
    time-str))

(defun my-colours-set ()
  "Set colors of new FRAME according to time of day.
Set timer that runs on next sunset or sunrise, whichever sooner."
  (if (and calendar-latitude calendar-longitude calendar-time-zone)
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
	    (switch-faces nil)
	    (run-at-time sunrise-string nil 'my-colours-set))
	   ((not (string-lessp current-time-string ; evening
			       (solar-time-to-24 sunset-string)))
	    (switch-faces nil)
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
	   (t (switch-faces t)		; daytime
	      (run-at-time sunset-string nil 'my-colours-set)))))
    (switch-faces t)))

(defun reset-frame-faces (frame)
  "Execute once in the first graphical new FRAME.
Reset some faces which --daemon doesn't quite set.
Remove hook when done and add `my-colours-set' instead."
  (select-frame frame)
  (cond ((window-system frame)
	 (my-colours-set)
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

(defconst +apropos-url-alist+
  '(("^gw?:? +\\(.*\\)" .		; Google Web
     "http://www.google.com/search?q=\\1")
    ("^gs:? +\\(.*\\)" .		; Google Scholar
     "http://scholar.google.com/scholar?q=\\1")
    ("^g!:? +\\(.*\\)" .		; Google Lucky
     "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=\\1")
    ("^gl:? +\\(.*\\)" .		; Google Linux
     "http://www.google.com/linux?q=\\1")
    ("^gi:? +\\(.*\\)" .		; Google Images
     "http://images.google.com/images?sa=N&tab=wi&q=\\1")
    ("^gv:? +\\(.*\\)" .		; Google Video
     "http://video.google.com/videosearch?q=\\1")
    ("^gg:? +\\(.*\\)" .		; Google Groups
     "http://groups.google.com/groups?q=\\1")
    ("^gdir:? +\\(.*\\)" .		; Google Directory
     "http://www.google.com/search?&sa=N&cat=gwd/Top&tab=gd&q=\\1")
    ("^gn:? +\\(.*\\)" .		; Google News
     "http://news.google.com/news?sa=N&tab=dn&q=\\1")
    ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(\\w+://.*\\)" . ; Google Translate URL
     "http://translate.google.com/translate?langpair=\\1|\\2&u=\\3")
    ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(.*\\)" . ; Google Translate Text
     "http://translate.google.com/translate_t?langpair=\\1|\\2&text=\\3")
    ("^gd:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(.*\\)" . ; Google Dictionary
     "http://www.google.com/dictionary?aq=f&langpair=\\1|\\2&q=\\3&hl=\\1")
    ("^w:? +\\(.*\\)" .			; Wikipedia en
     "http://en.wikipedia.org/wiki/Special:Search?search=\\1")
    ("^yt:? +\\(.*\\)" .		; YouTube
     "http://www.youtube.com/results?search_query=\\1")
    ("^/\\.:? +\\(.*\\)" .		; Slashdot search
     "http://www.osdn.com/osdnsearch.pl?site=Slashdot&query=\\1")
    ("^fm:? +\\(.*\\)" .		; Freshmeat
     "http://www.freshmeat.net/search?q=\\1")
    ("^rd:? +\\(.*\\)" .		; sub Reddits (mobile)
     "http://m.reddit.com/r/\\1")
    ("^ewiki:? +\\(.*\\)" .		; Emacs Wiki Search
     "http://www.emacswiki.org/cgi-bin/wiki?search=\\1")
    ("^ewiki2:? +\\(.*\\)" .		; Google Emacs Wiki
     "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&q=\\1&sa=Search")
    ("^hayoo:? +\\(.*\\)" .		; Hayoo
     "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query=\\1")
    ("^ma:? +\\(.*\\)" .	       ; Encyclopaedia Metallum, bands
     ;;"http://www.metal-archives.com/search.php?type=band&string=\\1"
     "http://www.google.com/search?q=\\1&as_sitesearch=metal-archives.com")
    ("^aur:? +\\(.*\\)" .	 ; Search in Arch Linux's Aur packages
     "http://aur.archlinux.org/packages.php?&K=\\1")
    ("^fp:? +\\(.*\\)" .	       ; FreeBSD's FreshPorts
     "http://www.FreshPorts.org/search.php?query=\\1&num=20"))
  "Search engines and sites.")

(defun browse-apropos-url (text &optional new-window)
  "Search for TEXT by some search engine in NEW-WINDOW if needed."
  (interactive (if (require 'browse-url nil t)
		   (browse-url-interactive-arg "Location: ")
		 (read-string "Location: ")))
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

(defconst +skeleton-pair-alist+
  '((?\( _ ?\)) (?[  _ ?]) (?{  _ ?}) (?\" _ ?\"))
  "List of pair symbols.")

(defun autopairs-ret (arg)
  "Eclectic newline inside closing brackets.
If ARG, stay on the original line."
  (interactive "P")
  (dolist (pair +skeleton-pair-alist+)
    (when (eq (char-after) (car (last pair)))
      (save-excursion (newline-and-indent))))
  (newline arg)
  (indent-according-to-mode))

(defun activate-lisp-minor-modes ()
  "Activate some convenient minor modes for editing s-exp."
  (active-lisp-modes))

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
 '(tool-bar-mode nil)
 '(menu-bar-mode nil)
 '(transient-mark-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; appearance

;;; geometry
(add-to-list 'default-frame-alist (cons 'width +width+))
(win-or-nix (set-frame-width (selected-frame) +width+))

;; Solar info
(when-library "solar"
	      (when (load "solar" t)	; Sofia coordinates
		(setq calendar-latitude +42.68
		      calendar-longitude +23.31)))

(if (window-system)
    (my-colours-set)
  ;; hook, execute only first time in graphical frame
  ;;  (and indefinite times in terminal frames till then)
  (add-hook 'after-make-frame-functions 'reset-frame-faces)
  (faces-generic))

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

;; handle window configuration
(when-library "winner" (winner-mode 1))

;; gdb
(when-library "gud" (setq gdb-many-windows t))

;; Imenu
(when-library "imenu" (global-set-key (kbd "C-`") 'imenu))

;; Proced
(when-library "proced" (global-set-key (kbd "C-M-`") 'proced))

;; Ido
(when-library
 "ido"
 (when (require 'ido nil t)
   (ido-mode t)
   (setq ido-enable-flex-matching t)

   (defvar *ido-enable-replace-completing-read* t
     "If t, use ido-completing-read instead of completing-read if possible.
Set it to nil using let in around-advice for functions where the
original completing-read is required.  For example, if a function
foo absolutely must use the original completing-read, define some
advice like this:
 (defadvice foo (around original-completing-read-only activate)
   (let (ido-enable-replace-completing-read) ad-do-it))")

   (defadvice completing-read (around use-ido-when-possible activate)
     "Replace completing-read wherever possible, unless directed otherwise."
     (if (or (not *ido-enable-replace-completing-read*)
	     (boundp 'ido-cur-list)) ; Avoid infinite loop
	 ad-do-it		     ; from ido calling this
       (let ((allcomp (all-completions "" collection predicate)))
	 (if allcomp
	     (setq ad-return-value
		   (ido-completing-read prompt allcomp nil
					require-match initial-input
					hist def))
	   ad-do-it))))))

;; don't count on same named files, rather show path difference
(when-library "uniquify"
	      (when (require 'uniquify nil t)
		(setq uniquify-buffer-name-style 'post-forward
		      uniquify-separator ":")))

;;; CUA rectangle action
(setq cua-enable-cua-keys nil)		; only for rectangles
(cua-mode t)

;; highlight current line, turn it on for all modes by default
(when-library "hl-line" (global-hl-line-mode t))

(when-library
 "hilit-chg"
;;; highlight changes in documents
 (global-highlight-changes-mode t)
 (setq highlight-changes-visibility-initial-state nil) ; hide initially

 ;; toggle changes visibility
 (global-set-key [f7] 'highlight-changes-visible-mode)

 ;; remove the change-highlight in region
 (global-set-key (kbd "S-<f7>") 'highlight-changes-remove-highlight)

 ;; alt-pgup/pgdown jump to the previous/next change
 (global-set-key (kbd "<M-prior>") 'highlight-changes-previous-change)
 (global-set-key (kbd "<M-next>") 'highlight-changes-next-change))

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

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region,copy current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line.")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region,kill current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Killed line.")
     (list (line-beginning-position) (line-beginning-position 2)))))

;;; recentf
(when-library "recentf"
	      (when (require 'recentf nil t) ; save recently used files
		(setq recentf-max-saved-items 100 ; max save 100
		      recentf-save-file (concat +home-path+
						".emacs.d/recentf")
		      recentf-max-menu-items 15) ; max 15 in menu
		(recentf-mode t)))		 ; turn it on

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
(setq					; keep my ~/ clean
 bookmark-default-file (concat +home-path+ ".emacs.d/bookmarks")
 bookmark-save-flag 1)			; autosave each change

(mouse-avoidance-mode 'jump)	  ; mouse ptr when cursor is too close
(when-library "icomplete" (icomplete-mode t)) ; completion in minibuffer
(partial-completion-mode t)	  ; be smart with completion

(when (fboundp 'file-name-shadow-mode)	; emacs22+
  (file-name-shadow-mode t))	    ; be smart about filenames in mbuf

;;; tramp-ing
(when-library
 "tramp"
 (defun tramping-mode-line ()
   "Change modeline when root or remote."
   (let ((host-name (when (file-remote-p default-directory)
		      (tramp-file-name-host
		       (tramp-dissect-file-name
			default-directory)))))
     (when host-name
       (setq host-name
	     (concat (if (string-match "^[^0-9][^.]*\\(\\..*\\)"
				       host-name)
			 (substring host-name 0 (match-beginning 1))
		       host-name)
		     ":")))
     (if (string-match "^/su\\(do\\)?:" default-directory)
	 (progn
	   (make-local-variable 'mode-line-buffer-identification)
	   (setq mode-line-buffer-identification
		 (cons
		  (propertize
		   (concat (or host-name "") "su"
			   (match-string 1 default-directory) ": ")
		   'face 'font-lock-warning-face)
		  (default-value 'mode-line-buffer-identification))))
       (when host-name
	 (make-local-variable 'mode-line-buffer-identification)
	 (setq mode-line-buffer-identification
	       (cons
		(propertize host-name 'face 'font-lock-warning-face)
		(default-value
		  'mode-line-buffer-identification)))))))

 (hook-modes tramping-mode-line
	     find-file-hooks dired-mode-hook))

(when-library "epg"
;;; bypass gpg graphical pop-up on passphrase
	      (defadvice epg--start (around advice-epg-disable-agent
					    activate compile)
		"Make epg--start not able to find a gpg-agent."
		(let ((agent (getenv "GPG_AGENT_INFO")))
		  (setenv "GPG_AGENT_INFO" nil)
		  ad-do-it
		  (setenv "GPG_AGENT_INFO" agent))))

;;; Gnus
(when-library
 "gnus"
 (defvar *gnus-new-mail-count* "" "Unread messages count.")
 (setq global-mode-string (append global-mode-string
				  (list '*gnus-new-mail-count*)))

 (eval-after-load "gnus"
   `(progn
      (setq gnus-select-method '(nntp "news.gmane.org")
	    gnus-secondary-select-methods
	    `((nnimap "gmail" (nnimap-address "imap.gmail.com")
		      (nnimap-server-port 993) (nnimap-stream ssl)
		      (nnimap-authinfo-file ,(concat +home-path+
						     ".authinfo.gpg")))
	      (nnml "yahoo"))
	    gnus-ignored-newsgroups
	    "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
	    message-send-mail-function 'smtpmail-send-it
	    smtpmail-starttls-credentials '(("smtp.gmail.com"
					     587 nil nil))
	    smtpmail-auth-credentials '(("smtp.gmail.com" 587
					 "m00naticus@gmail.com" nil))
	    smtpmail-default-smtp-server "smtp.gmail.com"
	    smtpmail-smtp-server "smtp.gmail.com"
	    smtpmail-smtp-service 587
	    mail-sources '((pop :server "pop.mail.yahoo.co.uk"
				:port 995 :stream ssl
				:user "m00natic@yahoo.co.uk"))
	    epa-file-cache-passphrase-for-symmetric-encryption t)

      (defun my-gnus-demon-scan-mail ()
	"Rescan just mail and notify on new messages."
	(save-excursion
	  ;; fetch new messages
	  (let ((nnmail-fetched-sources (list t)))
	    (dolist (server-status gnus-opened-servers)
	      (let* ((server (car server-status))
		     (backend (car server)))
		(and (not (eq backend 'nnshimbun))
		     (gnus-check-backend-function
		      'request-scan backend)
		     (or (gnus-server-opened server)
			 (gnus-open-server server))
		     (gnus-request-scan nil server)))))
	  ;; scan for new mail
	  (let ((unread-count 0)
		unread-groups)
	    (dolist (group '("nnml+yahoo:mail.misc"
			     "nnimap+gmail:INBOX"))
	      (gnus-group-remove-mark group)
	      (let ((method (gnus-find-method-for-group group)))
		;; Bypass any previous denials from the server.
		(gnus-remove-denial method)
		(when (gnus-activate-group group 'scan nil method)
		  (let ((info (gnus-get-info group))
			(active (gnus-active group)))
		    (when info (gnus-request-update-info info method))
		    (gnus-get-unread-articles-in-group info active)
		    (unless (gnus-virtual-group-p group)
		      (gnus-close-group group))
		    (when gnus-agent
		      (gnus-agent-save-group-info
		       method (gnus-group-real-name group) active))
		    (gnus-group-update-group group))
		  (let ((unread (gnus-group-unread group)))
		    (when (and (numberp unread) (> unread 0))
		      (setq unread-count (+ unread-count unread)
			    unread-groups (concat unread-groups
						  ", " group)))))))
	    ;; show popup on new mail and change mode line
	    (setq *gnus-new-mail-count*
		  (if (null unread-groups) ""
		    ,(win-or-nix
		      nil
		      (when-library
		       ("dbus" "notify")
		       '(notify "Gnus"
				(format
				 (concat "%d new mail%s in "
					 (substring unread-groups 2))
				 unread-count
				 (if (= unread-count 1) "" "s")))))
		    (put '*gnus-new-mail-count*
			 'risky-local-variable t)
		    (propertize (format "%d" unread-count)
				'face 'font-lock-warning-face))))))

      (byte-compile 'my-gnus-demon-scan-mail)

      ;; run (gnus-demon-init) to track emails
      (gnus-demon-add-handler 'my-gnus-demon-scan-mail 10 nil)))

 (when-library "shimbun"
	       (eval-after-load "gnus-group"
		 '(define-key gnus-group-mode-map "Gn"
		    'gnus-group-make-shimbun-group))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; external extensions

;;; ELPA
(when-library "package"
	      (when (load "package" t) (package-initialize)))

;;; sml-modeline
(when-library "sml-modeline"
	      (when (load "sml-modeline" t)
		(scroll-bar-mode -1)
		(sml-modeline-mode 1)))

;;; TabBar
(when-library
 "tabbar"
 (when (load "tabbar" t)
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
	 (with-current-buffer tab-buffer
	   (setq ad-return-value (concat default-directory "\n"
					 ad-return-value))))))

   (defadvice tabbar-buffer-groups (around tabbar-groups-extension
					   activate compile)
     "Add some rules for grouping tabs to run before original."
     (cond
      ((string-match "^*tramp" (buffer-name))
       (setq ad-return-value (list "Tramp")))
      ((memq major-mode '(woman-mode completion-list-mode
				     slime-fuzzy-completions-mode))
       (setq ad-return-value (list "Help")))
      ((eq major-mode 'asdf-mode)
       (setq ad-return-value (list "Lisp")))
      ((string-match "^emms" (symbol-name major-mode))
       (setq ad-return-value (list "EMMS")))
      ((string-match "^\\(wl\\|mime\\)" (symbol-name major-mode))
       (setq ad-return-value (list "Mail")))
      ((string-match "^*inferior" (buffer-name))
       (setq ad-return-value (list "Process")))
      ((string-match "^*slime" (buffer-name))
       (setq ad-return-value (list "Slime")))
      ((memq major-mode '(fundamental-mode org-mode))
       (setq ad-return-value (list "Common")))
      (t ad-do-it)))	      ; if none of above applies, run original

   (eval-when-compile
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
	    ,on-prefix))))

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

   (when-library
    "org"
    (add-hook 'org-load-hook
	      (lambda () (define-keys org-mode-map
		      (kbd "C-<tab>") nil
		      [backtab] nil
		      (win-or-nix (kbd "C-S-<tab>")
				  (kbd "<C-S-iso-lefttab>")) nil))))

   ;; remove buffer name from modeline as it now becomes redundant
   (setq-default mode-line-buffer-identification "")))

;; Wget
(when-library
 "wget"
 (when (executable-find "wget")
   (autoload 'wget "wget" "Wget interface for Emacs." t)
   (autoload 'wget-web-page "wget"
     "Wget interface to download whole web page." t)
   (autoload 'wget-cd-download-dir "wget"
     "Change directory to wget download dir.")
   (autoload 'wget-uri "wget" "Wget URI asynchronously.")

   (setq
    wget-download-directory-filter 'wget-download-dir-filter-regexp
    wget-download-directory
    (eval-when-compile
      `(("\\.\\(jpe?g\\|png\\)$" . ,(concat +home-path+ "Pictures"))
	("." . ,(concat +home-path+ "Downloads")))))

   (defun wget-site (uri)
     "Get a whole web-site pointed by URI through Wget.
Make links point to local files."
     (interactive (list (read-string "Web Site URI: "
				     (thing-at-point-url-at-point))))
     (let ((dir (wget-cd-download-dir t uri)))
       (when dir
	 (if (string= uri "") (error "There is no uri")
	   (wget-uri uri dir '("-krmnp" "-E" "-X/page,/message"
			       "--no-check-certificate"))))))))

;;; Anything
(when-library "anything-config"
	      (when (load "anything-config" t)
		(global-set-key [f5] 'anything)
		(setq anything-sources
		      '(anything-c-source-bookmarks
			anything-c-source-files-in-current-dir+
			anything-c-source-recentf
			anything-c-source-locate
			anything-c-source-emacs-functions-with-abbrevs
			anything-c-source-emacs-variables
			anything-c-source-info-elisp
			anything-c-source-info-pages
			anything-c-source-man-pages
			anything-c-source-semantic))

		(when-library "anything-match-plugin"
			      (load "anything-match-plugin" t))

		(when-library
		 "anything-etags"
		 (when (load "anything-etags" t)
		   (setq anything-sources
			 (nconc anything-sources
				'(anything-c-source-etags-select)))))))

;;; ErgoEmacs minor mode
(when-library
 "ergoemacs-mode"
 (setenv "ERGOEMACS_KEYBOARD_LAYOUT" "colemak")
 (when (load "ergoemacs-mode" t)
   (when (fboundp 'recenter-top-bottom)
     (define-key ergoemacs-keymap ergoemacs-recenter-key
       'recenter-top-bottom)
     (define-key isearch-mode-map ergoemacs-recenter-key
       'recenter-top-bottom))
   (define-keys ergoemacs-keymap
     "\M-3" 'move-cursor-previous-pane
     "\M-#" 'move-cursor-next-pane
     "\C-f" 'search-forward-regexp)

   ;; workaround arrows not active in terminal with ErgoEmacs active
   (when-library "anything"
		 (when (require 'anything nil t)
		   (define-keys anything-map
		     "\C-d" 'anything-next-line
		     "\C-u" 'anything-previous-line
		     (kbd "C-M-d") 'anything-next-source
		     (kbd "C-M-u") 'anything-previous-source)))

   (eval-when-compile
     (defmacro ergoemacs-fix (layout)
       "Fix some keybindings when using ErgoEmacs."
       `(progn
	  (let ((ergo-layout ,layout))
	    ,(when-library
	      "paredit"
	      `(eval-after-load 'paredit
		 '(progn
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
		    (when (equal ,layout "colemak")
		      (define-key paredit-mode-map "\M-r"
			'paredit-splice-sexp)))))

	    ,(when-library
	      "slime"
	      '(cond ((equal ergo-layout "colemak")
		      (eval-after-load "slime"
			'(define-keys slime-mode-map
			   "\M-k" 'slime-next-note
			   "\M-K" 'slime-previous-note
			   "\M-n" nil
			   "\M-p" nil)))
		     ((equal ergo-layout "en")
		      (eval-after-load "slime"
			'(define-keys slime-mode-map
			   "\M-N" 'slime-previous-note
			   "\M-p" nil))))

	      '(cond ((equal ergo-layout "colemak")
		      (eval-after-load "slime-repl"
			'(define-key slime-repl-mode-map "\M-n" nil)))))

	    ,(when-library
	      "emms"
	      '(cond ((equal ergo-layout "en")
		      (eval-after-load "emms"
			'(global-set-key (kbd "s-r") 'emms-pause)))
		     ((equal ergo-layout "colemak")
		      (eval-after-load "emms"
			'(global-set-key (kbd "s-p") 'emms-pause)))))))))

   (defun ergoemacs-change-keyboard (layout)
     "Change ErgoEmacs keyboard bindings according to LAYOUT."
     (interactive (list (read-string "Enter layout (default us): "
				     nil nil "us")))
     (unless (equal layout ergoemacs-keyboard-layout)
       (ergoemacs-mode 0)
       (setenv "ERGOEMACS_KEYBOARD_LAYOUT" layout)
       (setq ergoemacs-keyboard-layout layout)
       (load "ergoemacs-mode")
       (ergoemacs-fix (getenv "ERGOEMACS_KEYBOARD_LAYOUT"))
       (ergoemacs-mode 1)))

   (ergoemacs-fix "colemak")
   (ergoemacs-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Lisp goodies

(when-library "hl-sexp"
	      (autoload 'hl-sexp-mode "hl-sexp"
		"Highlight s-expressions minor mode." t))

;;; Paredit
(when-library
 "paredit"				; from ELPA
 (eval-after-load "eldoc"
   '(eldoc-add-command 'paredit-backward-delete
		       'paredit-close-round))
;;; Redshank
 (when-library
  "redshank-loader"
  (when (load "redshank-loader" t)
    (redshank-setup '(lisp-mode-hook slime-repl-mode-hook
				     inferior-lisp-mode-hook)
		    t))))

(setq inferior-lisp-program
      (win-or-nix (eval-when-compile
		    (concat +home-path+
			    "bin/clisp/clisp.exe -K full"))
		  "/usr/local/bin/sbcl"))

;;; elisp stuff
(eval-after-load "eldoc" '(eldoc-add-command 'autopairs-ret))
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(hook-modes turn-on-eldoc-mode
	    emacs-lisp-mode-hook lisp-interaction-mode-hook
	    ielm-mode-hook)

(define-key emacs-lisp-mode-map "\M-g" 'lisp-complete-symbol)

;; common lisp hyperspec info look-up
(when-library
 "info-look"
 (when (require 'info-look nil t)
   (info-lookup-add-help :mode 'lisp-mode :regexp "[^][()'\" \t\n]+"
			 :ignore-case t
			 :doc-spec '(("(ansicl)Symbol Index"
				      nil nil nil)))))

;; Hook convenient s-exp minor modes to some major modes.
(hook-modes activate-lisp-minor-modes
	    inferior-lisp-mode-hook lisp-mode-hook
	    lisp-interaction-mode-hook
	    emacs-lisp-mode-hook ielm-mode-hook
	    inferior-scheme-mode-hook scheme-mode-hook)

;; set eclectic return in lisp modes
(define-key lisp-mode-shared-map (kbd "RET") 'autopairs-ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; extensions

;;; Lisp

;;; Qi
(when-library
 "qi-mode"
 (autoload 'qi-mode "qi-mode" "Major mode for editing Qi progams" t)
 (autoload 'run-qi "qi-mode" "Run an inferior Qi process." t)

 (push '("\\.qi\\'" . qi-mode) auto-mode-alist)
 (eval-after-load "qi-mode"
   `(progn
      (setq inferior-qi-program
	    ,(eval-when-compile
	       (win-or-nix
		(concat inferior-lisp-program
			" -M " +home-path+
			"bin/Qi.mem -q")
		(concat inferior-lisp-program
			" --core " +home-path+
			"Programs/Qi/Qi.core"))))
      (hook-modes activate-lisp-minor-modes
		  qi-mode-hook inferior-qi-mode-hook)
      (define-key qi-mode-map (kbd "RET") 'autopairs-ret))))

;;; Set up SLIME
(when (load "slime-autoloads" t)
  (eval-after-load "slime"
    `(progn
       (slime-setup ,(win-or-nix
		      ''(slime-fancy slime-banner slime-indentation)
		      ''(slime-fancy slime-banner slime-indentation
				     slime-asdf)))

       (push (list ',(win-or-nix 'clisp 'sbcl)
		   ',(split-string inferior-lisp-program " +"))
	     slime-lisp-implementations)

       (setq slime-default-lisp ',(win-or-nix 'clisp 'sbcl)
	     slime-complete-symbol*-fancy t
	     slime-complete-symbol-function 'slime-fuzzy-complete-symbol
	     common-lisp-hyperspec-root
	     ,(eval-when-compile
		(concat "file://" +home-path+ "Documents/HyperSpec/"))
	     slime-net-coding-system
	     (find-if 'slime-find-coding-system
		      '(utf-8-unix iso-latin-1-unix iso-8859-1-unix
				   binary)))

       (add-hook 'slime-repl-mode-hook 'activate-lisp-minor-modes)
       (define-key slime-mode-map (kbd "RET") 'autopairs-ret)
       (define-key slime-mode-map "\M-g" 'slime-complete-symbol))))

;;; Clojure
(when-library
 "clojure-mode"				; from ELPA
 (eval-after-load "clojure-mode"
   '(progn
      (add-hook 'clojure-mode-hook 'activate-lisp-minor-modes)
      (define-key clojure-mode-map (kbd "RET") 'autopairs-ret)))

 (when-library		     ; at the moment (slime-autodoc-mode)
  ("slime" "swank-clojure")  ; has to be turned off with swank-clojure
  (autoload 'swank-clojure-init "swank-clojure"
    "Initialize clojure for swank")
  (autoload 'swank-clojure-cmd "swank-clojure"
    "Command to start clojure")
  (autoload 'swank-clojure-slime-mode-hook "swank-clojure"
    "Swank to Slime hook")
  (autoload 'slime-read-interactive-args "swank-clojure"
    "Add clojure to slime implementations")
  (autoload 'swank-clojure-project "swank-clojure"
    "Invoke clojure with a project path" t)

  (eval-after-load "slime"
    '(progn
       (setq
	swank-clojure-extra-vm-args
	'("-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8888"
	  "-server" "-Xdebug"))
       (aput 'slime-lisp-implementations 'clojure
	     (list (swank-clojure-cmd) :init 'swank-clojure-init))))

  (eval-after-load "swank-clojure"
    `(progn
;;; Online JavaDoc to Slime
       (defun slime-java-describe (symbol-name)
	 "Get details on Java class/instance at point SYMBOL-NAME."
	 (interactive
	  (list (slime-read-symbol-name "Java Class/instance: ")))
	 (or symbol-name (error "No symbol given"))
	 (with-current-buffer (slime-output-buffer)
	   (or (eq (current-buffer) (window-buffer))
	       (pop-to-buffer (current-buffer) t))
	   (goto-char (point-max))
	   (insert (concat "(show " symbol-name ")"))
	   (when symbol-name
	     (slime-repl-return)
	     (other-window 1))))

       (defun slime-javadoc (symbol-name)
	 "Get JavaDoc documentation on Java class at point SYMBOL-NAME."
	 (interactive
	  (list (slime-read-symbol-name "JavaDoc info for: ")))
	 (or symbol-name (error "No symbol given"))
	 (set-buffer (slime-output-buffer))
	 (or (eq (current-buffer) (window-buffer))
	     (pop-to-buffer (current-buffer) t))
	 (goto-char (point-max))
	 (insert (concat "(javadoc " symbol-name ")"))
	 (when symbol-name
	   (slime-repl-return)
	   (other-window 1)))

;;; Local JavaDoc to Slime
       (defconst +slime-browse-local-javadoc-root+
	 ,(eval-when-compile
	    (concat (win-or-nix (concat +home-path+ "Documents")
				"/usr/share")
		    "/javadoc/java-1.6.0-openjdk"))
	 "Path to javadoc.")

       (defun slime-browse-local-javadoc (ci-name)
	 "Browse local JavaDoc documentation on Java class/Interface at point CI-NAME."
	 (interactive
	  (list (slime-read-symbol-name "Class/Interface name: ")))
	 (or ci-name (error "No name given"))
	 (let ((name (replace-regexp-in-string "\\$" "." ci-name))
	       (path (concat (expand-file-name
			      +slime-browse-local-javadoc-root+)
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

       (byte-compile 'slime-java-describe)
       (byte-compile 'slime-javadoc)
       (byte-compile 'slime-browse-local-javadoc)

       (add-hook 'slime-connected-hook
		 (lambda () (slime-redirect-inferior-output)
		   (define-keys slime-mode-map
		     "\C-cd" 'slime-java-describe
		     "\C-cD" 'slime-javadoc)
		   (define-keys slime-repl-mode-map
		     "\C-cd" 'slime-java-describe
		     "\C-cD" 'slime-javadoc)
		   (define-key slime-mode-map "\C-cb"
		     'slime-browse-local-javadoc)
		   (define-key slime-repl-mode-map "\C-cb"
		     'slime-browse-local-javadoc)))))))

;;; Scheme
(when-library
 "quack"
 (autoload 'quack-scheme-mode-hookfunc "quack")
 (autoload 'quack-inferior-scheme-mode-hookfunc "quack")

 (add-hook 'scheme-mode-hook 'quack-scheme-mode-hookfunc)
 (add-hook 'inferior-scheme-mode-hook
	   'quack-inferior-scheme-mode-hookfunc)

 (setq quack-global-menu-p nil)
 (eval-after-load "quack"
   `(setq quack-default-program "gsi"
	  quack-pltcollect-dirs (list ,(win-or-nix
					(eval-when-compile
					  (concat +home-path+
						  "Documents/plt"))
					"/usr/share/plt/doc")))))

;;; CLIPS
(when-library
 "inf-clips"
 (autoload 'clips-mode "clips-mode"
   "Major mode for editing Clips code." t)
 (autoload 'run-clips "inf-clips" "Run an inferior Clips process." t)
 (push '("\\.clp$" . clips-mode) auto-mode-alist)

 (eval-after-load "clips-mode"
   '(progn
      (add-hook 'clips-mode-hook (lambda () (activate-lisp-minor-modes)
				   (setq indent-region-function nil)))
      (define-key clips-mode-map (kbd "RET") 'autopairs-ret)))

 (eval-after-load "inf-clips"
   `(progn
      (setq inferior-clips-program
	    ,(win-or-nix
	      (eval-when-compile
		(concat +win-path+
			"Program Files/CLIPS/Bin/CLIPSDOS.exe"))
	      "clips"))

      (add-hook 'inferior-clips-mode-hook
		(lambda () (activate-lisp-minor-modes)
		  (setq indent-region-function nil))))))

(when-library
 "prog/prolog"
 (fset 'run-prolog '(autoload "prog/prolog"
		      "Start a Prolog sub-process." t nil))
 (autoload 'prolog-mode "prog/prolog"
   "Major mode for editing Prolog programs." t)
 (autoload 'mercury-mode "prog/prolog"
   "Major mode for editing Mercury programs." t)
 (setq prolog-system 'swi auto-mode-alist
       (nconc '(("\\.pl$" . prolog-mode)
		("\\.m$" . mercury-mode))
	      auto-mode-alist)))

;;; Oz
(when-library
 "oz"
 (autoload 'oz-mode "oz" "Major mode for editing Oz code." t)
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
(when-library "haskell-site-file"
	      (when (load "haskell-site-file" t)
		(hook-modes ((haskell-indentation-mode t)
			     turn-on-haskell-doc-mode)
			    haskell-mode-hook)))

;;; Caml
(when-library
 "tuareg"
 (autoload 'tuareg-mode "tuareg"
   "Major mode for editing Caml code" t)
 (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
 (push '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))

;;; Python
(when-library
 "python-mode"
 (autoload 'python-mode "python-mode" "Python editing mode." t)
 (push '("\\.py$" . python-mode) auto-mode-alist)
 (push '("python" . python-mode) interpreter-mode-alist))

;;; VB
(when-library "visual-basic-mode"
	      (autoload 'visual-basic-mode "visual-basic-mode"
		"Visual Basic mode." t)
	      (push '("\\.\\(frm\\|bas\\|cls\\|rvb\\|vbs\\)$"
		      . visual-basic-mode)
		    auto-mode-alist))

;;; C#
(when-library "csharp-mode"
	      (autoload 'csharp-mode "csharp-mode"
		"Major mode for editing C# code." t)
	      (push '("\\.cs$" . csharp-mode) auto-mode-alist))

;;; cc-mode - hide functions
(add-hook 'c-mode-common-hook (lambda () (hs-minor-mode 1)
				(local-set-key [backtab]
					       'hs-toggle-hiding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Auxiliary extensions

;;; AUCTeX
(when-library
 "auctex"
 (when (load "auctex" t)
   (when-library "preview-latex" (load "preview-latex" t))
   (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
   (eval-after-load "tex"
     '(add-hook 'TeX-mode-hook (lambda () (define-key TeX-mode-map "\M-g"
				       'TeX-complete-symbol))))))

;;; LaTeX beamer
;; allow for export=>beamer by placing
;; #+LaTeX_CLASS: beamer in org files
(when-library
 "org"
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
		 "\\end{frame}"))))

;;; Ditaa
(let ((ditaa-path (eval-when-compile
		    (concat +extras-path+ "bin/ditaa.jar"))))
  (when (file-exists-p ditaa-path)
    (setq org-ditaa-jar-path ditaa-path)

    (defun ditaa-generate ()
      "Invoke ditaa over current buffer."
      (interactive)
      (shell-command (concat "java -jar " org-ditaa-jar-path " "
			     buffer-file-name)))))

;;; CompletionUI
(when-library "completion-ui"
	      (when (load "completion-ui" t)
		(global-set-key "\M-G" 'complete-dabbrev)
		(define-key emacs-lisp-mode-map (kbd "C-c TAB")
		  'complete-elisp)))

;;; Auto Install
(when-library
 "auto-install"
 (when (load "auto-install" t)
   (setq auto-install-directory (eval-when-compile
				  (concat +extras-path+
					  "auto-install-dir/")))
   (when-library
    ("anything" "anything-auto-install")
    (and (require 'anything nil t)
	 (load "anything-auto-install" t)

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
	     (message "Auto-install added to anything sources.")))))))

;;; w3m
(when-library
 "w3m-load"
 (when (and (executable-find "w3m") (load "w3m-load" t))
   (setq ;; make w3m default for most URLs
    browse-url-browser-function
    `(("^ftp:/.*" . (lambda (url &optional nf)
		      (call-interactively
		       'find-file-at-point url)))
      ("video" . ,browse-url-browser-function)
      ("\\.tv" . ,browse-url-browser-function)
      ("youtube" . ,browse-url-browser-function)
      ("." . w3m-browse-url)))

   ;; integration with other packages
   (when-library "wl" (autoload 'mime-w3m-preview-text/html "mime-w3m"
			"View messages as html."))
   (when-library "gnus" (autoload 'gnus-group-mode-map "nnshimbun"
			  "Add shimbun group to Gnus." t))

   (eval-after-load "w3m"
     `(progn
	(setq w3m-home-page (eval-when-compile
			      (concat "file://" +home-path+
				      ".w3m/bookmark.html"))
	      w3m-use-toolbar t
	      w3m-use-cookies t)

	,(when-library "wget" '(require 'w3m-wget nil t))

	(defun w3m-browse-url-other-window (url &optional newwin)
	  "Open URL in new window when NEWWIN."
	  (interactive (browse-url-interactive-arg "w3m URL: "))
	  (let ((pop-up-frames nil))
	    (switch-to-buffer-other-window
	     (w3m-get-buffer-create "*w3m*"))
	    (w3m-browse-url url)))

	(defun dired-w3m-find-file ()
	  "Open a file with w3m."
	  (interactive)
	  (let ((file (dired-get-filename)))
	    (if (y-or-n-p (format "Use emacs-w3m to browse %s? "
				  (file-name-nondirectory file)))
		(w3m-find-file file))))

	(byte-compile 'w3m-browse-url-other-window)
	(byte-compile 'dired-w3m-find-file)

	(define-keys w3m-mode-map
	  "i" 'w3m-save-image
	  "l" 'w3m-horizontal-recenter)))

   (eval-after-load "dired"
     '(define-key dired-mode-map "\C-xm" 'dired-w3m-find-file))))

;;; handle ftp with emacs, if not set above
(or (consp browse-url-browser-function)
    (setq browse-url-browser-function
	  `(("^ftp:/.*" . (lambda (url &optional nf)
			    (call-interactively
			     'find-file-at-point url)))
	    ("." . ,browse-url-browser-function))))


;;; Traverse
(when-library "traverselisp" (load "traverselisp" t))

;;; Wanderlust
(when-library
 "wl"
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
  wl-forward-subject-prefix "Fwd: " ; use "Fwd: " not "Forward: "

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

 ;; (when (require 'wl-spam nil t)
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
 ;; (byte-compile (lambda () (interactive)
 ;; 		 (my-wl-summary-refile ".project-x"))))

 ;; suggested by Masaru Nomiya on the WL mailing list
 (defun wl-draft-subject-check ()
   "Check whether the message has a subject before sending."
   (and (< (length (std11-field-body "Subject")) 1)
	(not (y-or-n-p "No subject! Send current draft? "))
	(error "Abort")))

 ;; note, this check could cause some false positives;
 ;; anyway, better safe than sorry...
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

;;; mldonkey
(when-library
 "mldonkey"
 (autoload 'mldonkey "mldonkey" "Run the MlDonkey interface." t)

 (eval-after-load "mldonkey"
   '(setq mldonkey-host "localhost"
	  mldonkey-port 4000)))

;;; EMMS
(when-library
 "emms"
 (when (load "emms-auto" t)
   (autoload 'emms-smart-browse "emms-browser"
     "Display browser and playlist." t)
   (autoload 'emms-browser "emms-browser"
     "Launch or switch to the EMMS Browser." t)
   (autoload 'emms "emms-playlist-mode"
     "Switch to the current emms-playlist buffer." t)

   (eval-after-load "emms"
     `(progn
	(emms-devel)
	(emms-default-players)

	,(when-library "emms-info-libtag"
		       '(when (require 'emms-info-libtag nil t)
			  (add-to-list 'emms-info-functions
				       'emms-info-libtag)))

	,(when-library "emms-mark" '(require 'emms-mark nil t))

	;; swap time and other track info
	(let ((new-global-mode-string nil))
	  (while (and (not (eq 'emms-mode-line-string
			       (car global-mode-string)))
		      global-mode-string)
	    (push (car global-mode-string) new-global-mode-string)
	    (setq global-mode-string (cdr global-mode-string)))
	  (push 'emms-playing-time-string new-global-mode-string)
	  (push 'emms-mode-line-string new-global-mode-string)
	  (setq global-mode-string (nreverse new-global-mode-string)))

	(add-hook 'emms-player-started-hook 'emms-show)

	(defun my-emms-default-info ()
	  (concat
	   " (" (number-to-string
		 (or (emms-track-get track 'play-count) 0))
	   ", " (emms-last-played-format-date
		 (or (emms-track-get track 'last-played) '(0 0 0)))
	   ")" (let ((time (emms-track-get track 'info-playing-time)))
		 (if time
		     (format " %d:%02d" (/ time 60) (mod time 60))
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
			    (if year (concat year " - ") ""))
		     (or (emms-track-get track 'info-album) "unknown")
		     "]" (my-emms-default-info))
		  (concat (emms-track-name track)
			  (my-emms-default-info)))))
	     ((eq 'url type)
	      (emms-format-url-track-name (emms-track-name track)))
	     (t (concat (symbol-name type) ": "
			(emms-track-name track) (my-emms-default-info))))))

	(defun file-size (file-descr)
	  (car (cddr (cddr (cddr (cddr file-descr))))))

	(defun my-emms-covers (dir type)
	  "Choose album cover in DIR deppending on TYPE.
Small cover should be less than 100000 bytes.
Medium - less than 120000 bytes."
	  (let* ((pics (sort (directory-files-and-attributes
			      dir t "\\.\\(jpg\\|jpeg\\|png\\)$" t)
			     (lambda (p1 p2)
			       (< (file-size p1) (file-size p2)))))
		 (small (car pics)))
	    (when (<= (or (file-size small) 100001) 100000)
	      (let ((medium (cadr pics)))
		(car (case type
		       ('small small)
		       ('medium (if (<= (or (file-size medium) 120001)
					120000)
				    medium
				  small))
		       ('large (or (car (cddr pics)) medium
				   small))))))))

	(byte-compile 'my-emms-track-description-function)
	(byte-compile 'my-emms-covers)

	(setq emms-show-format "EMMS: %s"
	      emms-mode-line-format "%s"
	      emms-info-asynchronously t
	      later-do-interval 0.0001
	      emms-source-file-default-directory
	      ,(eval-when-compile (concat +home-path+ "Music/"))
	      emms-last-played-format-alist
	      '(((emms-last-played-seconds-today) . "%a %H:%M")
		(604800 . "%a %H:%M")	; this week
		((emms-last-played-seconds-month) . "%d")
		((emms-last-played-seconds-year) . "%d/%m")
		(t . "%d/%m/%Y"))
	      emms-track-description-function
	      'my-emms-track-description-function
	      emms-browser-covers 'my-emms-covers)

	,(when-library "emms-lastfm"
		       '(when (require 'emms-lastfm nil t)
			  (setq emms-lastfm-username "m00natic"
				emms-lastfm-password "very-secret")
			  (emms-lastfm 1)))

	,(when (executable-find "mpd")
	   `(when (require 'emms-player-mpd nil t)
	      (add-to-list 'emms-info-functions 'emms-info-mpd)
	      (push 'emms-player-mpd emms-player-list)
	      (setq emms-player-mpd-music-directory
		    emms-source-file-default-directory)

	      ,(win-or-nix
		nil
		(when-library
		 ("dbus" "notify")
		 '(defadvice emms-player-started
		    (after emms-player-mpd-notify
			   (player) activate compile)
		    "Notify new track for MPD."
		    (when (eq emms-player-playing-p 'emms-player-mpd)
		      (notify "EMMS" (substring (emms-show) 6))))))))

	(global-set-key [XF86AudioStop] 'emms-pause)

	(unless (locate-library "ergoemacs")
	  (global-set-key (kbd "s-r") 'emms-pause))

	,(win-or-nix
	  nil
	  (when-library ("dbus" "notify")
			'(progn
			   (defun my-emms-notify ()
			     "Notify on new track."
			     (emms-next-noerror)
			     (when emms-player-playing-p
			       (notify "EMMS" (substring (emms-show)
							 6))))

			   (byte-compile 'my-emms-notify)
			   (setq emms-player-next-function
				 'my-emms-notify))))))))

;;; Dictionary
(when-library "dictionary"
	      (global-set-key (kbd "C-s-w") 'dictionary-search))

;;; chess
(when-library "chess"			; from ELPA
	      ;;(autoload 'chess "chess" "Play a game of chess" t)
	      (setq chess-sound-play-function nil))

;;; sudoku
(when-library
 "sudoku"
 (autoload 'sudoku "sudoku" "Start a sudoku game." t)
 (eval-after-load "sudoku" '(setq sudoku-level "evil")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Specific OS extensions

(win-or-nix
;;; Cygwin
 (when-library "cygwin-mount"
	       (let ((cygwin-dir (eval-when-compile
				   (concat +win-path+ "cygwin/bin"))))
		 (when (and (file-exists-p cygwin-dir)
			    (load "cygwin-mount" t))
		   (cygwin-mount-activate)
		   (setq w32shell-cygwin-bin cygwin-dir))))

 ;;; Notify
 (when-library
  "notify"
  (when (require 'dbus nil t)
    (autoload 'notify "notify" "Use pop-up notifications for events.")
    (eval-after-load "notify"
      '(setq notify-defaults
	     (list :app "Emacs"
		   :icon "/usr/local/share/icons/hicolor/48x48/apps/emacs.png"
		   :timeout 5000 :urgency "low"
		   :category "emacs.event")))))

;;; Global tags
 (when-library
  "gtags"
  (autoload 'gtags-mode "gtags"
    "Minor mode for utilizing global tags." t)

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
	  (cd olddir))	; restore
	;; tagfile already exists; update it
	(shell-command "global -u && echo 'updated tagfile'")))

  (add-hook 'gtags-mode-hook (lambda ()
			       (local-set-key "\M-." 'gtags-find-tag)
			       (local-set-key "\M-,"
					      'gtags-find-rtag)))
  (add-hook 'c-mode-common-hook (lambda () (gtags-mode t)
				  ;;(gtags-create-or-update)
				  ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (server-start) ; using --daemon or emacsW32 instead

;;; init.el ends here
