;; init.el --- Andrey Kotlarski's .emacs -*- mode:emacs-lisp -*-

;;; Commentary:
;; Utilized extensions:
;;  Anything related:
;;   Anything http://www.emacswiki.org/emacs/Anything
;;   anything-match http://www.emacswiki.org/emacs/AnythingPlugins
;;  Programming languages related:
;;   SLIME http://common-lisp.net/project/slime
;;   Quack http://www.neilvandyke.org/quack
;;   clojure-mode http://github.com/technomancy/clojure-mode
;;   clips-mode http://www.cs.us.es/software/clips
;;   haskell-mode http://projects.haskell.org/haskellmode-emacs
;;   ghc-mod http://www.mew.org/~kazu/proj/ghc-mod/en
;;   Oz-mode http://www.mozart-oz.org
;;   ESS http://ess.r-project.org
;;   ECB http://ecb.sourceforge.net
;;   AutoComplete http://cx4a.org/software/auto-complete
;;  Lisp goodies:
;;   highlight-parentheses http://nschum.de/src/emacs/highlight-parentheses
;;   hl-sexp http://edward.oconnor.cx/elisp/hl-sexp.el
;;   ParEdit http://www.emacswiki.org/emacs/ParEdit
;;   Redshank http://www.foldr.org/~michaelw/emacs/redshank
;;  networking:
;;   emacs-w3m http://emacs-w3m.namazu.org
;;   emacs-wget http://pop-club.hp.infoseek.co.jp/emacs/emacs-wget
;;  misc:
;;   ErgoEmacs-mode http://xahlee.org/emacs/ergonomic_emacs_keybinding.html
;;   AUCTeX http://www.gnu.org/software/auctex
;;   Ditaa http://ditaa.sourceforge.net
;;   TabBar http://www.emacswiki.org/emacs/TabBarMode
;;   sml-modeline http://bazaar.launchpad.net/~nxhtml/nxhtml/main/annotate/head:/util/sml-modeline.el
;;   Ace Jump http://www.emacswiki.org/emacs/AceJump
;;   notify http://www.emacswiki.org/emacs/notify.el
;;   cygwin-mount http://www.emacswiki.org/emacs/cygwin-mount.el
;;   Dictionary http://www.myrkr.in-berlin.de/dictionary
;;   EMMS http://www.gnu.org/software/emms
;;   Emacs Chess http://github.com/jwiegley/emacs-chess
;;   sudoku http://sourceforge.net/projects/sudoku-elisp
;;   Sauron https://github.com/djcb/sauron

;;; Code:
(if (boundp '+win-p+) (error "Trying to re-initialize"))

;; do some OS recognition and set main parameters
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

;; Set some path constants.
(defconst +home-path+
  (win-or-nix
   (cond ((string-match "\\(.*[/\\]home[/\\]\\)" exec-directory)
	  (match-string 0 exec-directory)) ; usb
	 ((file-exists-p (concat exec-directory "../../home"))
	  (file-truename (concat exec-directory "../../home/")))
	 (t (eval-when-compile (concat (getenv "HOME") "/"))))
   (eval-when-compile (concat (getenv "HOME") "/")))
  "Home path.")

(win-or-nix
 ((defconst +win-path+ "C:/" "Windows root path.")
  (setq user-emacs-directory (concat +home-path+ ".emacs.d/")
	default-directory +home-path+)))

(defconst +extras-path+ (win-or-nix
			 (concat user-emacs-directory "extras/")
			 (eval-when-compile
			   (concat user-emacs-directory "extras/")))
  "Elisp extensions' path.")

;; add `+extras-path+' and subdirs to `load-path'
(when (and (file-exists-p +extras-path+)
	   (not (member +extras-path+ load-path)))
  (setq load-path (cons +extras-path+ load-path))
  (let ((default-directory +extras-path+))
    (normal-top-level-add-subdirs-to-load-path)))
;; add custom bin to path
(let ((bin-path (win-or-nix
		 (concat +extras-path+ "bin")
		 (eval-when-compile (concat +extras-path+ "bin")))))
  (if (file-exists-p bin-path) (add-to-list 'exec-path bin-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; mode a bit emacs
(custom-set-variables
 '(browse-url-new-window-flag t)
 '(calendar-latitude 42.68)
 '(calendar-longitude 23.31)
 '(column-number-mode t)
 '(cua-enable-cua-keys nil)
 '(cua-mode t)
 '(default-input-method "bulgarian-phonetic")
 '(delete-old-versions t)
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(electric-pair-mode t)
 `(eshell-directory-name
   ,(win-or-nix (concat user-emacs-directory ".eshell/")
		(eval-when-compile
		  (concat user-emacs-directory ".eshell/"))))
 '(frame-title-format "emacs %@ %b (%f)")
 '(gdb-many-windows t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-subword-mode t)
 '(icomplete-mode t)
 '(ido-enable-flex-matching t)
 '(ido-mode 'both)
 `(ido-save-directory-list-file
   ,(win-or-nix (concat user-emacs-directory ".ido.last")
		(eval-when-compile
		  (concat user-emacs-directory ".ido.last"))))
 '(ido-use-virtual-buffers t)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'org-mode)
 '(initial-scratch-message nil)
 '(ispell-dictionary "en")
 '(line-number-mode nil)
 '(mail-envelope-from 'header)
 '(mail-specify-envelope-from t)
 '(message-send-mail-function 'smtpmail-send-it)
 '(menu-bar-mode nil)
 '(org-src-fontify-natively t)
 '(package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
		      ("elpa" . "http://tromey.com/elpa/")
		      ("melpa" . "http://melpa.milkbox.net/packages/")
		      ("marmalade" .
		       "http://marmalade-repo.org/packages/")))
 '(proced-format 'medium)
 '(prolog-system 'swi)
 '(read-file-name-completion-ignore-case t)
 '(recentf-max-saved-items 100)
 '(recentf-mode t)
 `(recentf-save-file
   ,(win-or-nix (concat user-emacs-directory "recentf")
		(eval-when-compile
		  (concat user-emacs-directory "recentf"))))
 '(require-final-newline t)
 '(save-place t nil (saveplace))
 `(save-place-file
   ,(win-or-nix (concat user-emacs-directory ".emacs-places")
		(eval-when-compile
		  (concat user-emacs-directory ".emacs-places"))))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(smtpmail-smtp-service 587)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'post-forward-angle-brackets
			      nil (uniquify))
 '(version-control t)
 '(view-read-only t)
 '(winner-mode t)
 '(word-wrap t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; extension independent macros

(defmacro when-library (check-on-complile library &rest body)
  "When CHECK-ON-COMPLILE and LIBRARY is in `load-path' leave BODY.
If LIBRARY is a list, check whether every element is in `load-path'.
If not CHECK-ON-COMPLILE, perform library search at run-time."
  (if (consp library)
      (if check-on-complile
	  (let ((loadp t))
	    (dolist (lib library)
	      (if loadp
		  (setq loadp (locate-library (symbol-name lib)))))
	    (if loadp `(progn ,@body)))
	`(when (and ,@(mapcar (lambda (lib)
				`(locate-library ,(symbol-name lib)))
			      library))
	   ,@body))
    (if check-on-complile
	(if (locate-library (symbol-name library)) `(progn ,@body))
      `(when (locate-library ,(symbol-name library)) ,@body))))

(defmacro hook-modes (functions &rest modes)
  "Hook a list of FUNCTIONS (or atom) to MODES.
Each function may be an atom or a list with parameters."
  (cons 'progn
	(if (consp functions)
	    (if (cdr functions)
		(let ((fns (mapcar (lambda (fn) (if (consp fn) fn
					     (list fn)))
				   functions)))
		  (mapcar (lambda (mode) `(add-hook ',mode (lambda () ,@fns)))
			  modes))
	      (let ((fst (car functions)))
		(if (consp fst)
		    (mapcar (lambda (mode) `(add-hook ',mode (lambda () ,fst)))
			    modes)
		  (mapcar (lambda (mode) `(add-hook ',mode ',fst))
			  modes))))
	  (mapcar (lambda (mode) `(add-hook ',mode ',functions))
		  modes))))

(defmacro define-keys (mode &rest keys)
  "Define cascade of keys for a MODE.
KEYS is alternating key-value list."
  `(progn ,@(let ((res nil))
	      (while keys
		(setq res (cons `(define-key ,mode ,(car keys)
				   ,(cadr keys))
				res))
		(setq keys (cddr keys)))
	      (nreverse res))))

(defmacro active-lisp-modes ()
  "Activate convenient s-expression modes which are present."
  `(progn
     (font-lock-add-keywords		; pretty lambdas
      nil `(("(\\(lambda\\>\\)"
	     (0 (progn
		  (compose-region (match-beginning 1) (match-end 1)
				  ,(make-char 'greek-iso8859-7 107))
		  nil)))))
     ,(when-library nil hl-sexp '(hl-sexp-mode 1))
     ,(when-library nil highlight-parentheses ; from ELPA
		    '(highlight-parentheses-mode 1))
     ,(when-library nil paredit '(paredit-mode 1))))

(defmacro switch-faces (&optional light)
  "Set dark faces.  With prefix, LIGHT."
  (if light '(progn (or (disable-theme 'wombat)
			(disable-theme 'andr-dark))
		    (enable-theme 'andr))
    '(progn (disable-theme 'andr)
	    (condition-case nil
		(enable-theme 'wombat)
	      (error (enable-theme 'andr-dark))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; extension independent functions

;;; themes
(if (ignore-errors (load-theme 'wombat))
    (disable-theme 'wombat))
(deftheme andr "My corrections over default theme.")
(deftheme andr-dark "My corrections over default dark theme.")

(defun andr-themes ()
  "Define my custom themes.  Should be done in graphical frame."
  (condition-case nil
      (set-face-font 'default (win-or-nix "Consolas" "Anonymous Pro"))
    (error (ignore-errors (set-face-font 'default "terminus"))))
  (let ((class '((class color) (min-colors 88))))
    (custom-theme-set-faces
     'andr
     `(default ((default :foreground "black")
		(,class :background "cornsilk")
		(t :background "white")))
     `(mode-line
       ((default :width condensed :family "neep")
	(,class :foreground "white" :background "DarkSlateGray")
	(t :background "cyan")))
     `(mode-line-inactive
       ((default :inherit mode-line :weight light)
	(,class :box (:line-width -1 :color "grey75")
		:foreground "grey20" :background "grey90")
	(t :inverse-video t)))
     '(tabbar-selected ((t :inherit default :weight bold)))
     `(tabbar-unselected ((default :inherit tabbar-default)
			  (,class
			   :background "gray75" :foreground "white"
			   :box (:line-width 2 :color "white"))
			  (t :inverse-video t)))
     '(tabbar-button ((t :background "white" :foreground "gray75")))
     '(sml-modeline-end-face ((t :family "neep" :inherit default
				 :width condensed))))
    (custom-theme-set-faces
     'andr-dark
     `(default ((default :background "#242424")
		(,class :foreground "#f6f3e8")
		(t :foreground "white")))
     `(mode-line
       ((default :width condensed :family "neep")
	(,class :background "#444444" :foreground "#f6f3e8")
	(t :background "cyan")))
     `(mode-line-inactive
       ((default :inherit mode-line :weight light)
	(,class :box (:line-width -1 :color "grey40")
		:background "#444444" :foreground "#857b6f")
	(t :inverse-video t)))
     '(tabbar-selected ((t :inherit default :weight bold)))
     `(tabbar-unselected ((default :inherit tabbar-default)
			  (,class
			   :background "gray50" :foreground "black"
			   :box (:line-width 2 :color "black"))
			  (t :inverse-video t)))
     '(tabbar-button ((t :background "black" :foreground "gray50")))
     '(sml-modeline-end-face ((t :family "neep" :inherit default
				 :width condensed))))))

(defun solar-time-to-24 (time-str)
  "Convert solar type string TIME-STR to 24 hour format."
  (if (string-match "\\(.*\\)[/:-]\\(..\\)\\(.\\)" time-str)
      (format "%02d:%s"
	      (if (string-equal (match-string 3 time-str) "p")
		  (+ 12 (string-to-number (match-string 1 time-str)))
		(string-to-number (match-string 1 time-str)))
	      (match-string 2 time-str))
    time-str))

(defun my-colours-set ()
  "Set colours of new FRAME according to time of day.
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
	    (switch-faces)
	    (run-at-time sunrise-string nil 'my-colours-set))
	   ((not (string-lessp current-time-string ; evening
			       (solar-time-to-24 sunset-string)))
	    (switch-faces)
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

(defun activate-lisp-minor-modes ()
  "Activate some convenient minor modes for editing s-exp."
  (active-lisp-modes))

;;; fullscreen stuff
(defvar *fullscreen-p* t "Check if fullscreen is on or off.")

(defun fullscreen-toggle ()
  "Toggle fullscreen view on and off."
  (interactive)
  (if (setq *fullscreen-p* (not *fullscreen-p*))
      (win-or-nix
       (w32-send-sys-command 61488)    ; WM_SYSCOMMAND maximize #xf030
       (set-frame-parameter nil 'fullscreen 'fullboth))
    (win-or-nix
     (w32-send-sys-command 61728)	; WM_SYSCOMMAND restore #xf120
     (set-frame-parameter nil 'width 100)
     (set-frame-parameter nil 'fullscreen 'fullheight))))

(defun set-frame-faces (&optional frame)
  "Load my custom themes.  Execute once in the first graphical FRAME."
  (if frame
      (progn
	(select-frame frame)
	(when (window-system)
	  (remove-hook 'after-make-frame-functions 'set-frame-faces)
	  (andr-themes)
	  (if (require 'solar nil t) (my-colours-set))
	  (if *fullscreen-p*
	      (set-frame-parameter nil 'fullscreen 'fullboth))))
    (andr-themes)
    (if (require 'solar nil t) (my-colours-set))
    (if *fullscreen-p*
	(set-frame-parameter nil 'fullscreen 'fullboth))))

(defun browse-ftp-tramp (url &optional new-window)
  "Open ftp URL in NEW-WINDOW with TRAMP."
  (if (string-match "\\(.*?\\)\\(\\/.*\\)" url 6)
      (find-file (concat "/ftp:anonymous@" (match-string 1 url)
			 ":" (match-string 2 url)))
    (find-file (concat "/ftp:anonymous@" (substring url 6) ":/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; appearance

(win-or-nix
 ((when (window-system)
    (defun hide-frame ()
      "Keep emacs running hidden.
Use emacsclient -e '(make-frame-visible)' to restore it."
      (interactive)
      (server-edit)
      (make-frame-invisible nil t))

    (global-set-key "\C-x\C-c" 'hide-frame))
  (set-frame-faces))

 (if (window-system)
     (set-frame-faces)
   (add-hook 'after-make-frame-functions 'set-frame-faces)))

(add-hook 'text-mode-hook (lambda () "Set proportional font."
			    (variable-pitch-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; useful stuff

;;; some keybindings
(global-set-key "\C-cl" 'goto-line)
(global-set-key (kbd "M-RET") 'newline-and-indent)

;;; Use y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;;; backup
(setq backup-directory-alist
      `(("." . ,(win-or-nix
		 (concat user-emacs-directory "backup/")
		 (eval-when-compile (concat user-emacs-directory
					    "backup/")))))
      tramp-backup-directory-alist backup-directory-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; built-in packages

;; Imenu
(when-library t imenu (global-set-key (kbd "C-`") 'imenu))

;; Proced
(when-library t proced (global-set-key (kbd "C-~") 'proced))

;;; ido and subword
(when-library t (ido subword)
	      (add-hook 'ido-minibuffer-setup-hook
			(lambda () (subword-mode -1))))

;;; tramp-ing
(when-library
 t tramp
 (defun tramping-mode-line ()
   "Change modeline when root or remote."
   (let ((host-name (if (file-remote-p default-directory)
			(tramp-file-name-host
			 (tramp-dissect-file-name
			  default-directory)))))
     (if host-name
	 (setq mode-line-buffer-identification
	       (cons
		(propertize
		 (if (string-match "^/su\\(do\\)?:" default-directory)
		     (concat "su" (match-string 1 default-directory)
			     "@" host-name)
		   host-name)
		 'face 'highlight)
		(default-value 'mode-line-buffer-identification))))))

 (hook-modes tramping-mode-line
	     find-file-hooks dired-mode-hook))

;;; Gnus
(when-library
 t gnus
 (defvar *gnus-new-mail-count* "" "Unread messages count.")
 (put '*gnus-new-mail-count* 'risky-local-variable t)
 (add-to-list 'global-mode-string '*gnus-new-mail-count* t 'eq)

 (eval-after-load "gnus"
   `(progn
      (setq gnus-select-method '(nntp "news.gmane.org")
	    gnus-secondary-select-methods
	    '((nnimap "gmail" (nnimap-address "imap.gmail.com"))
	      (nnimap "vayant" (nnimap-address "mail.vayant.com")
		      (nnimap-streaming nil)))
	    gnus-posting-styles
	    '((".*" (address "m00naticus@gmail.com")
	       ("X-SMTP-Server" "smtp.gmail.com"))
	      ("vayant" (address "akotlarski@vayant.com")
	       ("X-SMTP-Server" "mail.vayant.com"))))

      (defun gnus-demon-notify (&optional notify)
	"When NOTIFY check for more unread mails.
Otherwise check for less."
	(and (or notify (not (equal *gnus-new-mail-count* "")))
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

      (byte-compile 'gnus-demon-notify)
      (byte-compile 'gnus-demon-scan-important)
      (gnus-demon-add-handler 'gnus-demon-scan-important 10 nil)
      (gnus-demon-add-handler 'gnus-demon-notify 1 nil)
      ;; run (gnus-demon-init) to track emails
      (add-hook 'kill-emacs-hook (byte-compile
				  (lambda () "Quit Gnus."
				    (setq gnus-interactive-exit nil)
				    (gnus-group-exit)))))))

(when-library
 t smtpmail
 (eval-after-load "smtpmail"
   '(defadvice smtpmail-via-smtp (around set-smtp-server-from-header
					 activate compile)
      "Set smtp server according to the `X-SMTP-Server' header.
If missing, try to deduce it from the `From' header."
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
		       (concat "smtp." domain))))))
      ad-do-it)))

(when-library
 t fortune
 (when (executable-find "fortune")
   (let ((fortune-d (concat user-emacs-directory "fortune/")))
     (if (file-exists-p fortune-d)
	 (setq fortune-dir fortune-d
	       fortune-file (concat fortune-dir "sigs"))))
   (add-hook 'message-signature-setup-hook 'fortune-to-signature)))

(when-library
 t browse-url
 (defvar my-search
   '("startingpage" . "https://startingpage.com/do/search?query=")
   "My default search engine.")

 (defconst +apropos-url-alist+
   '(("^i +\\(.*\\)" . "https://startingpage.com/do/search?query=\\1")
     ("^g +\\(.*\\)" . "http://www.google.com/search?q=\\1")
     ("^gs +\\(.*\\)" . "http://scholar.google.com/scholar?q=\\1")
     ("^gt +\\(\\w+\\)|? *\\(\\w+\\) +\\(\\w+://.*\\)" . ; Translate URL
      "http://translate.google.com/translate?langpair=\\1|\\2&u=\\3")
     ("^gt +\\(\\w+\\)|? *\\(\\w+\\) +\\(.*\\)" . ; Translate Text
      "http://translate.google.com/translate_t?langpair=\\1|\\2&text=\\3")
     ("^w +\\(.*\\)" .			; Wikipedia en
      "http://en.wikipedia.org/wiki/Special:Search?search=\\1")
     ("^bgw +\\(.*\\)" .		; Wikipedia bg
      "http://bg.wikipedia.org/wiki/Special:Search?search=\\1")
     ("^rd +\\(.*\\)" . "http://m.reddit.com/r/\\1") ; sub Reddits
     ("^imdb +\\(.*\\)" . "http://imdb.com/find?q=\\1")
     ("^ma +\\(.*\\)" .			; Encyclopaedia Metallum
      "http://www.google.com/search?q=site:metal-archives.com+\\1")
     ("^ewiki +\\(.*\\)" .		; Google Emacs Wiki
      "http://www.google.com/search?q=site:emacswiki.org+\\1")
     ("^cliki +\\(.*\\)" .		; Common Lisp wiki
      "http://www.cliki.net/admin/search?words=\\1")
     ("^hoog +\\(.*\\)" .		; Hoogle
      "http://haskell.org/hoogle/?hoogle=\\1")
     ("^clj +\\(.*\\)" .		; ClojureDocs
      "http://clojuredocs.org/search?q=\\1")
     ("^fp +\\(.*\\)" .			; FreeBSD's FreshPorts
      "http://www.FreshPorts.org/search.php?query=\\1&num=20")
     ("^nnm +\\(.*\\)" . "http://nnm.ru/search?in=news&q=\\1"))
   "Search engines and sites.")

 (autoload 'browse-url-interactive-arg "browse-url")

 (defun browse-apropos-url (text &optional new-window)
   "Search for TEXT by some search engine in `+apropos-url-alist+'.
Open in new tab if NEW-WINDOW."
   (interactive (browse-url-interactive-arg
		 (concat "Location" (if current-prefix-arg
					" (new tab)")
			 ": ")))
   (let ((text (mapconcat (lambda (s) (encode-coding-string s 'utf-8))
			  (split-string text) " "))
	 (apropo-reg "^$"))
     (let ((url (assoc-default text +apropos-url-alist+
			       (lambda (a b) (if (string-match-p a b)
					    (setq apropo-reg a)))
			       text)))
       (browse-url
	(if (and (string-equal apropo-reg "^$")	; no match
		 (or (string-match-p ".+ .+" text) ; multiple words
		     (not (string-match-p ".+\\..+" text))))
	    (concat (cdr my-search)
		    (replace-regexp-in-string " " "+" text))
	  (replace-regexp-in-string
	   " " "+" (replace-regexp-in-string apropo-reg url text)))
	(not new-window)))))

 (global-set-key [f6] 'browse-apropos-url))

;;; ELPA
(if (require 'package nil t) (package-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; external extensions

;;; Ace Jump
(if (require 'ace-jump-mode nil t)
    (define-key global-map "\C-c " 'ace-jump-mode))

;;; sml-modeline
(when (require 'sml-modeline nil t)
  (scroll-bar-mode -1)
  (sml-modeline-mode 1))

;;; TabBar
(when (require 'tabbar nil t)
  (defadvice tabbar-buffer-help-on-tab (after tabbar-add-file-path
					      activate compile)
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
     ((memq major-mode '(woman-mode completion-list-mode
				    slime-fuzzy-completions-mode))
      (setq ad-return-value (list "Help")))
     ((eq major-mode 'asdf-mode)
      (setq ad-return-value (list "Lisp")))
     ((string-match-p "^*tramp" (buffer-name))
      (setq ad-return-value (list "Tramp")))
     ((string-match-p "^*anything" (buffer-name))
      (setq ad-return-value (list "Anything")))
     ((string-match-p "^emms" (symbol-name major-mode))
      (setq ad-return-value (list "EMMS")))
     ((string-match-p "^*inferior" (buffer-name))
      (setq ad-return-value (list "Process")))
     ((string-match-p "^*slime" (buffer-name))
      (setq ad-return-value (list "Slime")))
     ((memq major-mode '(fundamental-mode org-mode))
      (setq ad-return-value (list "Common")))
     (t ad-do-it)))	      ; if none of above applies, run original

  (tabbar-mode 1)
  (setq-default mode-line-buffer-identification "")

  (defun next-tab (arg)
    "Go to next tab. With prefix, next group."
    (interactive "P")
    (if arg (tabbar-forward-group)
      (tabbar-forward-tab)))

  (defun prev-tab (arg)
    "Go to previous tab. With prefix, previous group."
    (interactive "P")
    (if arg (tabbar-backward-group)
      (tabbar-backward-tab)))

  (global-set-key (kbd "C-<tab>") 'next-tab)
  (global-set-key (win-or-nix (kbd "C-S-<tab>")
			      (kbd "<C-S-iso-lefttab>"))
		  'prev-tab)

  (when-library
   t org
   (add-hook 'org-load-hook
	     (lambda () "Allow tabbar keys in Org."
	       (define-key org-mode-map (kbd "C-<tab>") nil)))))

;; Wget
(when-library
 nil wget
 (when (executable-find "wget")
   (autoload 'wget-cd-download-dir "wget"
     "Change directory to wget download dir.")
   (autoload 'wget-uri "wget" "Wget URI asynchronously.")
   (setq
    wget-download-directory-filter 'wget-download-dir-filter-regexp
    wget-download-directory
    (eval-when-compile
      `(("\\.\\(jpe?g\\|png\\|gif\\|bmp\\)$"
	 . ,(concat +home-path+ "Pictures"))
	("." . ,(concat +home-path+ "Downloads")))))

   (defun wget-site (uri)
     "Get a whole web-site pointed by URI through Wget.
Make links point to local files."
     (interactive (list (read-string "Web Site URI: "
				     (thing-at-point-url-at-point))))
     (let ((dir (wget-cd-download-dir t uri)))
       (if dir (if (string= uri "") (error "There is no uri")
		 (wget-uri uri dir '("-pkrmnp" "-E" "-X/page,/message"
				     "--no-check-certificate" "-w" "1"
				     "--random-wait"))))))))

;;; Anything
(when-library
 nil anything
 (defalias 'my-anything 'anything)
 (global-set-key (kbd "<f5> m") 'my-anything)
 (when-library
  nil anything-config
  (unless (featurep 'ergoemacs-keybindings)
    (global-set-key "\M-y" 'anything-show-kill-ring)
    (define-key minibuffer-local-map "\M-y" 'yank-pop))
  (global-set-key (kbd "<f5> f") 'anything-for-files)
  (global-set-key (kbd "<f5> a h i") 'anything-info-at-point)
  (win-or-nix
   nil (if (eval-when-compile
	     (string-match-p "\\(ge\\|fu\\)ntoo"
			     (shell-command-to-string "uname -r")))
	   (global-set-key (kbd "<f5> a g") 'anything-gentoo)))

  (eval-after-load "anything"
    '(when (require 'anything-config nil t)
       (defun my-anything ()
	 (interactive)
	 (anything-other-buffer
	  '(anything-c-source-bookmarks
	    anything-c-source-emacs-variables
	    anything-c-source-emacs-functions-with-abbrevs
	    anything-c-source-man-pages)
	  "*anything-custom*"))

       (byte-compile 'my-anything))))

 (when-library
  nil anything-match-plugin
  (eval-after-load "anything"
    `(if (require 'anything-match-plugin nil t)
	 ,(win-or-nix
	   nil
	   (if (file-exists-p "/dev/shm")
	       '(setq anything-grep-candidates-fast-directory-regexp
		      "^/dev/shm/")))))))

;;; ErgoEmacs minor mode
(when-library
 nil ergoemacs-keybindings
 (setenv "ERGOEMACS_KEYBOARD_LAYOUT" "colemak")
 (when (require 'ergoemacs-keybindings nil t)
   (if (fboundp 'recenter-top-bottom)
       (define-key isearch-mode-map ergoemacs-recenter-key
	 'recenter-top-bottom))
   (define-keys ergoemacs-keymap
     "\M-2" 'move-cursor-previous-pane
     "\M-@" 'move-cursor-next-pane
     "\C-f" 'search-forward-regexp)

   ;; workaround arrows not active in terminal with ErgoEmacs active
   (when-library nil anything
		 (eval-after-load "anything"
		   '(define-keys anything-map
		      "\C-d" 'anything-next-line
		      "\C-u" 'anything-previous-line
		      (kbd "C-M-d") 'anything-next-source
		      (kbd "C-M-u") 'anything-previous-source)))

   (when-library
    t doc-view
    (add-hook 'doc-view-mode-hook
	      (lambda () "Set some Ergo keys."
		(ergoemacs-local-set-key
		 ergoemacs-isearch-forward-key 'doc-view-search)
		(define-keys ergoemacs-local-keymap
		  ergoemacs-isearch-backward-key
		  'doc-view-search-backward
		  ergoemacs-next-line-key
		  'doc-view-next-line-or-next-page
		  ergoemacs-previous-line-key
		  'doc-view-previous-line-or-previous-page))))

   (defmacro ergoemacs-fix (layout)
     "Fix some keybindings when using ErgoEmacs."
     `(progn
	,(if (fboundp 'recenter-top-bottom)
	     '(define-key ergoemacs-keymap ergoemacs-recenter-key
		'recenter-top-bottom))
	(let ((ergo-layout ,layout))
	  ,(when-library
	    nil paredit
	    `(eval-after-load 'paredit
	       '(progn
		  (define-keys paredit-mode-map
		    ergoemacs-comment-dwim-key 'paredit-comment-dwim
		    ergoemacs-isearch-forward-key nil
		    ergoemacs-backward-kill-word-key
		    'paredit-backward-kill-word
		    ergoemacs-kill-word-key 'paredit-forward-kill-word
		    ergoemacs-delete-backward-char-key
		    'paredit-backward-delete
		    ergoemacs-delete-char-key 'paredit-forward-delete
		    ergoemacs-kill-line-key 'paredit-kill
		    ergoemacs-recenter-key nil
		    "\M-R" 'paredit-raise-sexp)
		  (if (string-equal ,layout "colemak")
		      (define-keys paredit-mode-map
			"\M-r" 'paredit-splice-sexp
			ergoemacs-next-line-key nil
			"\M-g" nil)))))
	  ,(when-library
	    nil slime
	    '(cond ((string-equal ergo-layout "colemak")
		    (eval-after-load "slime"
		      '(define-keys slime-mode-map
			 "\M-k" 'slime-next-note
			 "\M-K" 'slime-previous-note
			 "\M-n" nil
			 "\M-p" nil))
		    (eval-after-load "slime-repl"
		      '(define-key slime-repl-mode-map "\M-n" nil)))
		   ((string-equal ergo-layout "en")
		    (eval-after-load "slime"
		      '(define-keys slime-mode-map
			 "\M-N" 'slime-previous-note
			 "\M-p" nil)))))
	  ,(when-library
	    nil anything-config
	    '(define-key ergoemacs-keymap ergoemacs-yank-pop-key
	       'anything-show-kill-ring)))))

   (defun ergoemacs-change-keyboard (layout)
     "Change ErgoEmacs keyboard bindings according to LAYOUT."
     (interactive (list (completing-read "Enter layout (default us): "
					 '("us" "dv" "colemak")
					 nil t nil nil "us")))
     (unless (string-equal layout ergoemacs-keyboard-layout)
       (ergoemacs-mode 0)
       (setenv "ERGOEMACS_KEYBOARD_LAYOUT" layout)
       (setq ergoemacs-keyboard-layout layout)
       (load "ergoemacs-keybindings")
       (ergoemacs-fix (getenv "ERGOEMACS_KEYBOARD_LAYOUT"))
       (ergoemacs-mode 1)))

   (when-library
    nil anything-config
    (defvar ergoemacs-minibuffer-keymap
      (copy-keymap ergoemacs-keymap))

    (defadvice ergoemacs-minibuffer-setup-hook
      (after ergoemacs-minibuffer-yank-pop activate compile)
      (define-key ergoemacs-minibuffer-keymap
	ergoemacs-yank-pop-key 'yank-pop)))

   (ergoemacs-fix (getenv "ERGOEMACS_KEYBOARD_LAYOUT"))
   (ergoemacs-mode 1)
   (global-set-key "C-@" 'cua-set-mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Lisp goodies

;;; Paredit
(when-library
 nil paredit				; from ELPA
 (eval-after-load "eldoc"
   '(eldoc-add-command 'paredit-backward-delete 'paredit-close-round))
;;; Redshank
 (if (require 'redshank-loader nil t)
     (redshank-setup '(lisp-mode-hook slime-repl-mode-hook
				      inferior-lisp-mode-hook)
		     t)))

(setq inferior-lisp-program
      (win-or-nix
       (cond ((file-exists-p (concat +home-path+ "clisp"))
	      (concat +home-path+ "clisp/clisp.exe -K full"))
	     ((file-exists-p (eval-when-compile
			       (concat +win-path+
				       "Program Files/clisp")))
	      (eval-when-compile
		(concat +win-path+
			"Program Files/clisp/clisp.exe -K full")))
	     (t "clisp"))
       "sbcl")
      scheme-program-name "gsi")

;;; elisp stuff
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(hook-modes turn-on-eldoc-mode
	    emacs-lisp-mode-hook lisp-interaction-mode-hook
	    ielm-mode-hook)
(or (featurep 'ergoemacs-keybindings)
    (define-key emacs-lisp-mode-map "\M-g" 'lisp-complete-symbol))

;; common lisp hyperspec info look-up
(if (require 'info-look nil t)
    (info-lookup-add-help :mode 'lisp-mode :regexp "[^][()'\" \t\n]+"
			  :ignore-case t
			  :doc-spec '(("(ansicl)Symbol Index"
				       nil nil nil))))

;; Hook convenient s-exp minor modes to some major modes.
(hook-modes activate-lisp-minor-modes
	    inferior-lisp-mode-hook lisp-mode-hook
	    lisp-interaction-mode-hook
	    emacs-lisp-mode-hook ielm-mode-hook
	    inferior-scheme-mode-hook scheme-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; extensions

;;; Lisp

;;; Set up SLIME
(if (require 'slime-autoloads nil t)
    (eval-after-load "slime"
      `(progn
	 (slime-setup ,(win-or-nix
			''(slime-fancy slime-banner slime-indentation)
			''(slime-fancy slime-banner slime-indentation
				       slime-asdf)))
	 (add-to-list 'slime-lisp-implementations
		      (list ',(win-or-nix 'clisp 'sbcl)
			    ',(split-string inferior-lisp-program
					    " +")))
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

	 (add-hook 'slime-repl-mode-hook 'activate-lisp-minor-modes)
	 (or (featurep 'ergoemacs-keybindings)
	     (define-key slime-mode-map "\M-g"
	       'slime-complete-symbol)))))

;;; Clojure
(when-library
 nil clojure-mode			; from ELPA
 (eval-after-load "clojure-mode"
   '(add-hook 'clojure-mode-hook 'activate-lisp-minor-modes))
 (when-library
  nil slime
  (eval-after-load "slime"
    `(progn
       (when (file-exists-p
	      ,(win-or-nix (concat +home-path+ "Documents/javadoc")
			   "/usr/share/doc/java-sdk-docs-1.6.0.23"))
	 (defun slime-browse-local-javadoc (ci-name)
	   "Browse local JavaDoc documentation on class/interface CI-NAME."
	   (interactive
	    (list (slime-read-symbol-name "Class/Interface name: ")))
	   (or ci-name (error "No name given"))
	   (let ((name (replace-regexp-in-string "\\$" "." ci-name))
		 (path (concat
			,(win-or-nix
			  (concat +home-path+ "Documents/javadoc")
			  "/usr/share/doc/java-sdk-docs-1.6.0.23/html")
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

	 (byte-compile 'slime-browse-local-javadoc)
	 (define-key slime-mode-map "\C-cb" 'slime-browse-local-javadoc)
	 (define-key slime-repl-mode-map
	   "\C-cb" 'slime-browse-local-javadoc))

       (add-hook 'slime-repl-mode-hook
		 (lambda () "Clojure REPL hook."
		   (if (and (string-equal "clojure"
					  (slime-connection-name))
			    (require 'clojure-mode))
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
		      '(slime-use-autodoc-mode t)))))))))

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
   '(add-hook 'clips-mode-hook (byte-compile
				(lambda () (activate-lisp-minor-modes)
				  (setq indent-region-function nil)))))
 (eval-after-load "inf-clips"
   `(progn
      (setq inferior-clips-program
	    ,(win-or-nix
	      (eval-when-compile
		(concat +win-path+
			"Program Files/CLIPS/Bin/CLIPSDOS.exe"))
	      "clips"))
      (add-hook 'inferior-clips-mode-hook
		(byte-compile
		 (lambda () (activate-lisp-minor-modes)
		   (setq indent-region-function nil)))))))

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
 (hook-modes ((haskell-indentation-mode t))
	     haskell-mode-hook)

 (when-library
  nil ghc
  (autoload 'ghc-init "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)
				 (flymake-mode)))))

;;; cc-mode - hide functions
(add-hook 'c-mode-common-hook (lambda () (hs-minor-mode 1)
				(when-library nil hl-sexp
					      (hl-sexp-mode 1))
				(local-set-key [backtab]
					       'hs-toggle-hiding)))

;;; Emacs Speaks Statistics
(when-library
 nil ess-site
 (autoload 'R-mode "ess-site" "Major mode for editing R source." t)
 (autoload 'Rd-mode "ess-site"
   "Major mode for editing R documentation source files." t)
 (setq auto-mode-alist (nconc '(("\\.[rR]$" . R-mode)
				("\\.Rd$" . Rd-mode))
			      auto-mode-alist)))

;;; Emacs Code Browser
(when-library
 t semantic
 (eval-after-load "semantic"
   '(progn (global-semantic-decoration-mode 1)
	   (global-semantic-idle-summary-mode 1)))
 (when-library
  nil ecb
  (eval-after-load "ecb"
    `(progn (custom-set-variables '(ecb-options-version "2.40"))
	    (let ((prog-path ,(win-or-nix
			       (concat +home-path+ "Programs")
			       (eval-when-compile
				 (concat +home-path+ "Programs")))))
	      (ecb-add-source-path prog-path prog-path t))))))

;;; AutoComplete
(when (require 'auto-complete-config nil t)
  (setq ac-dictionary-directories
	(cons (win-or-nix
	       (concat user-emacs-directory
		       "elpa/auto-complete-20120304/dict")
	       (eval-when-compile
		 (concat user-emacs-directory
			 "elpa/auto-complete-20120304/dict")))
	      ac-dictionary-directories))
  (ac-config-default)
  (ac-flyspell-workaround))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Auxiliary extensions

;;; AUCTeX from ELPA
(when-library
 nil auctex-autoloads
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
	  "latex %s && dvips %s.dvi && ps2pdf -dEmbedAllFonts=true -dOptimize=true -dUseFlateCompression=true %s.ps"
	  TeX-run-command nil (latex-mode)
	  :help "Produce optimized pdf")))
      (or (featurep 'ergoemacs-keybindings)
	  (define-key TeX-mode-map "\M-g" 'TeX-complete-symbol)))))

;;; Ditaa
(let ((ditaa-path (win-or-nix
		   (concat +extras-path+ "bin/ditaa.jar")
		   (eval-when-compile
		     (concat +extras-path+ "bin/ditaa.jar")))))
  (when (file-exists-p ditaa-path)
    (when-library t org (setq org-ditaa-jar-path ditaa-path))

    (defun ditaa-generate ()
      "Invoke ditaa over current buffer."
      (interactive)
      (start-process "ditaa" "*Ditaa*" "java" "-jar"
		     org-ditaa-jar-path buffer-file-name)
      (display-buffer "*Ditaa*"))))

(if (executable-find "conkeror")
    (setq browse-url-browser-function 'browse-url-generic
	  browse-url-generic-program "conkeror"))

;;; w3m
(when (and (executable-find "w3m") (require 'w3m-load nil t))
  (setq ;; make w3m default for most URLs
   browse-url-browser-function
   `(("^ftp://.*" . browse-ftp-tramp)
     ("video" . ,browse-url-browser-function)
     ("\\.tv" . ,browse-url-browser-function)
     ("youtube" . ,browse-url-browser-function)
     ("." . w3m-browse-url)))

  (win-or-nix (setq w3m-imagick-convert-program nil))
  (autoload 'w3m-find-file "w3m" "Browse local file with w3m." t)

  (defun w3m-dired-open ()
    "Open a file with w3m."
    (interactive)
    (w3m-find-file (dired-get-filename)))

  (add-hook 'dired-load-hook
	    (lambda () "Add w3m key for opening files in dired."
	      (define-key dired-mode-map "\C-cw" 'w3m-dired-open)))

  ;; wget integration
  (when-library nil w3m-wget (if (executable-find "w3m")
				 (require 'w3m-wget nil t)))

  ;; Conkeror style anchor numbering on actions
  (add-hook 'w3m-mode-hook 'w3m-lnum-mode)

  (eval-after-load "w3m"
    `(progn
       (setq w3m-home-page ,(win-or-nix
			     (concat "file://" +home-path+
				     ".w3m/bookmark.html")
			     (eval-when-compile
			       (concat "file://" +home-path+
				       ".w3m/bookmark.html")))
	     w3m-use-cookies t)
       (define-keys w3m-mode-map
	 (if w3m-key-binding "t" "i") 'w3m-lnum-save-image
	 "z" 'w3m-horizontal-recenter
	 "\C-cs" 'w3m-session-select)
       ,(when-library
	 nil ergoemacs-keybindings
	 '(define-keys w3m-mode-map "\M-i" nil "\M-a" nil))

       (when (executable-find "curl")
	 (autoload 'thing-at-point-url-at-point "thingatpt")

	 (defun w3m-download-with-curl (arg)
	   "Download current w3m link or URL to DIR."
	   (interactive "P")
	   (let ((url (or (w3m-anchor)
			  (if arg
			      (read-string
			       "URI: " (thing-at-point-url-at-point))
			    (car (w3m-lnum-get-action
				  "Curl on: " 1))))))
	     (if (stringp url)
		 (let ((olddir default-directory))
		   (cd (read-directory-name
			"Save to: "
			,(win-or-nix '(concat +home-path+ "Downloads")
				     (concat +home-path+ "Downloads"))
			nil t))
		   (async-shell-command (concat "curl -O '" url "'")
					"*Curl*")
		   (cd olddir))
	       (w3m-message "No url specified"))))

	 (byte-compile 'w3m-download-with-curl)
	 (define-key w3m-mode-map "D" 'w3m-download-with-curl))

       (when browse-url-generic-program
	 (defun w3m-browse-url-generic (&optional arg)
	   "Open current link, link-number url with generic browser.
With optional prefix ARG ask for url."
	   (interactive "P")
	   (browse-url-generic
	    (if arg
		(read-string "URI: " (thing-at-point-url-at-point))
	      (or (w3m-anchor) (w3m-image)
		  (car (w3m-lnum-get-action
			(concat browse-url-generic-program
				" on link: ") 1))))))

	 (byte-compile 'w3m-browse-url-generic)
	 (define-key w3m-mode-map "m" 'w3m-browse-url-generic))

       (add-hook 'kill-emacs-hook (byte-compile (lambda () "Quit w3m."
						  (w3m-quit t))) t)))

  (eval-after-load "w3m-search"
    '(add-to-list 'w3m-search-engine-alist
		  '((car my-search) (concat (cdr my-search "%s"))
		    utf-8))))

;;; handle ftp with emacs, if not set above
(or (consp browse-url-browser-function)
    (setq browse-url-browser-function
	  `(("^ftp://.*" . browse-ftp-tramp)
	    ("." . ,browse-url-browser-function))))

;;; EMMS
(when (require 'emms-auto nil t)
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
	   (setq new-global-mode-string (cons (car global-mode-string)
					      new-global-mode-string)
		 global-mode-string (cdr global-mode-string)))
	 (setq global-mode-string
	       (nconc (nreverse new-global-mode-string)
		      '(emms-playing-time-string
			emms-mode-line-string))))
       (add-hook 'emms-player-started-hook 'emms-show)

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

       (byte-compile 'my-emms-default-info)
       (byte-compile 'my-emms-track-description-function)
       (byte-compile 'my-emms-covers)
       (setq emms-show-format "EMMS: %s"
	     emms-mode-line-format "%s"
	     emms-source-file-default-directory
	     ,(win-or-nix
	       (concat +home-path+ "Music/")
	       (eval-when-compile (concat +home-path+ "Music/")))
	     emms-track-description-function
	     'my-emms-track-description-function
	     emms-browser-covers 'my-emms-covers)

       (when (and (require 'my-secret "my-secret.el.gpg" t)
		  (require 'emms-lastfm-client nil t)
		  (let ((url-request-method "GET"))
		    (ignore-errors     ; check for internet connection
		      (url-retrieve-synchronously
		       "http://post.audioscrobbler.com"))))
	 (setq emms-lastfm-client-username "m00natic"
	       emms-lastfm-client-api-key
	       my-emms-lastfm-client-api-key
	       emms-lastfm-client-api-secret-key
	       my-emms-lastfm-client-api-secret-key)
	 (ignore-errors (emms-lastfm-scrobbler-enable)))

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

;;; Sauron
(when-library
 nit sauron
 (setq sauron-separate-frame nil)
 (eval-after-load "erc" '(sauron-start))

 (when-library
  nil notify
  (eval-after-load "sauron"
    '(add-hook 'sauron-event-added-functions
	       (lambda (origin prio msg &optional props)
		 (notify "Sauron"
			 (format "%s (%d): %s" origin prio msg)))))))

;;; Dictionary
(when-library nil dictionary
	      (global-set-key "\C-cd" 'dictionary-search))

;;; chess
(when-library nil chess			; from ELPA
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Specific OS extensions

(win-or-nix
;;; Cygwin
 (let ((cygwin-dir (eval-when-compile
		     (concat +win-path+ "cygwin/bin"))))
   (when (file-exists-p cygwin-dir)
     (setq shell-file-name "bash"
	   explicit-shell-file-name "bash")
     (setenv "SHELL" shell-file-name)
     (setenv "CYGWIN" "nodosfilewarning")
     (when (require 'cygwin-mount nil t)
       (cygwin-mount-activate)
       (setq w32shell-cygwin-bin cygwin-dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(win-or-nix ((server-start)		; using --daemon on *nix
	     (and *fullscreen-p* (window-system)
		  (w32-send-sys-command 61488))))

;;; init.el ends here
