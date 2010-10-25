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
;;   Prolog http://bruda.ca/emacs-prolog
;;   haskell-mode http://projects.haskell.org/haskellmode-emacs
;;   tuareg-mode http://tuareg.forge.ocamlcore.org
;;   Oz-mode http://www.mozart-oz.org
;;   Qi-mode http://code.google.com/p/qilang
;;   python-mode https://launchpad.net/python-mode
;;   CSharpMode https://code.google.com/p/csharpmode
;;   VisualBasicMode http://www.emacswiki.org/emacs/VisualBasicMode
;;   gtags http://www.gnu.org/software/global
;;   Yasnippet http://www.emacswiki.org/emacs/Yasnippet
;;   ECB http://ecb.sourceforge.net
;;  Lisp goodies:
;;   highlight-parentheses http://nschum.de/src/emacs/highlight-parentheses
;;   hl-sexp http://edward.oconnor.cx/elisp/hl-sexp.el
;;   ParEdit http://www.emacswiki.org/emacs/ParEdit
;;   autopair http://code.google.com/p/autopair
;;   Redshank http://www.foldr.org/~michaelw/emacs/redshank
;;  networking:
;;   emacs-w3m http://emacs-w3m.namazu.org
;;   emacs-wget http://pop-club.hp.infoseek.co.jp/emacs/emacs-wget
;;   MLDonkey-el http://www.emacswiki.org/emacs/MlDonkey
;;  misc:
;;   ErgoEmacs-mode http://xahlee.org/emacs/ergonomic_emacs_keybinding.html
;;   AUCTeX http://www.gnu.org/software/auctex
;;   Ditaa http://ditaa.sourceforge.net
;;   TabBar http://www.emacswiki.org/emacs/TabBarMode
;;   sml-modeline http://bazaar.launchpad.net/~nxhtml/nxhtml/main/annotate/head%3A/util/sml-modeline.el
;;   notify http://www.emacswiki.org/emacs/notify.el
;;   auto-install http://www.emacswiki.org/emacs/AutoInstall
;;   cygwin-mount http://www.emacswiki.org/emacs/cygwin-mount.el
;;   Dictionary http://www.myrkr.in-berlin.de/dictionary
;;   EMMS http://www.gnu.org/software/emms
;;   Emacs Chess http://github.com/jwiegley/emacs-chess
;;   sudoku http://www.columbia.edu/~jr2075/elisp
;;   GoMode http://www.emacswiki.org/emacs/GoMode

;;; Code:
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
(win-or-nix (defconst +win-path+ "C:/" "Windows root path."))
(defconst +home-path+
  (win-or-nix
   (cond ((string-match "\\(.*[/\\]home[/\\]\\)" exec-directory)
	  (match-string 0 exec-directory)) ; usb
	 ((file-exists-p (concat exec-directory "../../home"))
	  (file-truename (concat exec-directory "../../home/")))
	 (t (eval-when-compile (concat (getenv "HOME") "/"))))
   (eval-when-compile (concat (getenv "HOME") "/")))
  "Home path.")
(setq user-emacs-directory (win-or-nix
			    (concat +home-path+ ".emacs.d/")
			    (eval-when-compile
			      (concat +home-path+ ".emacs.d/"))))
(defconst +extras-path+ (win-or-nix
			 (concat user-emacs-directory "extras/")
			 (eval-when-compile
			   (concat user-emacs-directory "extras/")))
  "Elisp extensions' path.")

;; add `+extras-path+' and subdirs to `load-path'
(when (and (file-exists-p +extras-path+)
	   (not (member +extras-path+ load-path)))
  (push +extras-path+ load-path)
  (let ((default-directory +extras-path+))
    (normal-top-level-add-subdirs-to-load-path)))
;; add custom bin to path
(let ((bin-path (win-or-nix
		 (concat +extras-path+ "bin")
		 (eval-when-compile (concat +extras-path+ "bin")))))
  (if (file-exists-p bin-path) (add-to-list 'exec-path bin-path)))
;; set default directory for `*scratch*'
(setq default-directory (win-or-nix +home-path+
				    (eval-when-compile +home-path+)))
(win-or-nix
 nil (if (boundp 'Info-directory-list)
	 (add-to-list 'Info-directory-list "/usr/local/share/info")))

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
 '(frame-title-format "emacs - %b (%f)")
 '(gdb-many-windows t)
 '(global-highlight-changes-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode 1)
 '(highlight-changes-visibility-initial-state nil)
 '(icomplete-mode t)
 '(ido-enable-flex-matching t)
 '(ido-mode 'both)
 `(ido-save-directory-list-file ,(win-or-nix
				  (concat user-emacs-directory
					  ".ido.last")
				  (eval-when-compile
				    (concat user-emacs-directory
					    ".ido.last"))))
 '(inhibit-startup-screen t)
 '(initial-major-mode 'org-mode)
 '(initial-scratch-message nil)
 '(ispell-dictionary "en")
 '(line-number-mode nil)
 '(menu-bar-mode nil)
 '(proced-format 'medium)
 '(read-file-name-completion-ignore-case t)
 '(recentf-max-saved-items 100)
 '(recentf-mode t)
 `(recentf-save-file ,(win-or-nix
		       (concat user-emacs-directory "recentf")
		       (eval-when-compile
			 (concat user-emacs-directory "recentf"))))
 '(require-final-newline t)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(version-control t)
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
KEYS is alternating list of key-value."
  `(progn ,@(let ((res nil))
	      (while keys
		(push `(define-key ,mode ,(car keys) ,(cadr keys))
		      res)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; extension independent functions

(defun faces-generic ()
  "My prefered faces which differ from default."
  (condition-case nil
      (set-face-font 'default (win-or-nix "Consolas" "Inconsolata"))
    (error (ignore-errors (set-face-font 'default "terminus"))))
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
  (when-library
   nil tabbar
   (custom-set-faces
    '(tabbar-default ((t :inherit variable-pitch)))
    '(tabbar-selected
      ((default :inherit tabbar-default)
       (((class color) (min-colors 88) (background light))
	:background "white" :foreground "DeepSkyblue"
	:box (:line-width 1 :color "LightGray"))
       (((class color) (min-colors 88) (background dark))
	:background "black" :foreground "DeepSkyBlue"
	:box (:line-width 1 :color "black"))
       (((background dark)) :background "black"	:foreground "white")
       (t :background "white" :foreground "cyan")))
    '(tabbar-unselected
      ((default :inherit tabbar-default)
       (((class color) (min-colors 88) (background light))
	:background "gray" :foreground "DarkSlateGray"
	:box (:line-width 2 :color "white"))
       (((class color) (min-colors 88) (background dark))
	:background "#222" :foreground "DarkCyan"
	:box (:line-width 2 :color "#090909"))
       (((background dark)) :background "white" :foreground "black")
       (t :background "black" :foreground "cyan")))
    '(tabbar-button
      ((default (:inherit tabbar-default))
       (((background dark)) :background "black"
	:foreground "#0c0"
	:box (:line-width 2 :color "black"))
       (t :background "white" :foreground "black"
	  :box (:line-width 2 :color "LightGray"))))
    '(tabbar-separator ((t :background "#111")))))
  (when-library
   nil sml-modeline
   (custom-set-faces
    '(sml-modeline-vis-face ((default (:inherit hl-line))
			     (((class color) (min-colors 88))
			      :foreground "green"
			      :box (:line-width 1))
			     (t :foreground "SeaGreen")))
    '(sml-modeline-end-face ((t :inherit default
				:box (:line-width 1)))))))

(defun switch-faces (light)
  "Set dark faces.  With prefix, LIGHT."
  (interactive "P")
  (if light (custom-set-faces
	     '(default ((t (:foreground "black" :height 80
					:background "cornsilk")))))
    (custom-set-faces
     '(default ((default (:background "black" :height 80))
		(((class color) (min-colors 88))
		 (:foreground "wheat"))
		(t (:foreground "white"))))))
  ;; region has to be reset to change right away
  (custom-set-faces
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
	     (t :background "gray")))))

(defun solar-time-to-24 (time-str)
  "Convert solar type string TIME-STR to 24 hour format."
  (if (string-match "\\(.*\\)[/:-]\\(..\\)\\(.\\)" time-str)
      (format "%02d:%s"
	      (if (equal (match-string 3 time-str) "p")
		  (+ 12 (string-to-number (match-string 1 time-str)))
		(string-to-number (match-string 1 time-str)))
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

(defun activate-lisp-minor-modes ()
  "Activate some convenient minor modes for editing s-exp."
  (active-lisp-modes))

;;; fullscreen stuff
(defvar *fullscreen-p* nil "Check if fullscreen is on or off.")

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

(defun reset-frame-faces (frame)
  "Reset some faces which --daemon doesn't quite set and remove hook.
Execute once in the first graphical FRAME."
  (select-frame frame)
  (when (window-system)
    (remove-hook 'after-make-frame-functions 'reset-frame-faces)
    (if (require 'solar nil t) (my-colours-set))
    (faces-generic)
    (if *fullscreen-p*
	(set-frame-parameter nil 'fullscreen 'fullboth))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; appearance

(win-or-nix
 ((if (require 'solar nil t) (my-colours-set))
  (and *fullscreen-p* (window-system) (w32-send-sys-command 61488))
  (global-set-key "\C-x\C-c"
		  (lambda () "Keep emacs running hidden.
Use emacsclient -e '(make-frame-visible)' to restore it."
		    (interactive)
		    (server-edit)
		    (make-frame-invisible nil t))))
 (if (window-system)
     (progn (if (require 'solar nil t) (my-colours-set))
	    (if *fullscreen-p*
		(set-frame-parameter nil 'fullscreen 'fullboth)))
   (add-hook 'after-make-frame-functions 'reset-frame-faces)))

(faces-generic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; useful stuff

;;; goto-line
(global-set-key "\C-cl" 'goto-line)

;;; autoindent
(global-set-key (kbd "M-RET") (lambda () "Newline and indent."
				(interactive)
				(newline)
				(indent-according-to-mode)))

;;; jump through errors/results
(global-set-key (kbd "<C-M-prior>") 'previous-error)
(global-set-key (kbd "<C-M-next>") 'next-error)

;;; clipboard
(global-set-key "\C-cv" 'clipboard-yank)
(global-set-key "\C-cc" 'clipboard-kill-ring-save)

;;; Use y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region,copy current line."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line.")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region,kill current line."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Killed line.")
     (list (line-beginning-position) (line-beginning-position 2)))))

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

;; Battery
(when-library
 t battery
 (and (eq battery-status-function 'battery-linux-sysfs)
      (file-readable-p "/proc/acpi/thermal_zone")
      (defadvice battery-linux-sysfs (after battery-add-temperature
					    activate compile)
	"Add temperature to battery status when using `sysfs'."
	(push (cons ?d (or (battery-search-for-one-match-in-files
			    (mapcar (lambda (e) (concat e "/temperature"))
				    (ignore-errors
				      (directory-files
				       "/proc/acpi/thermal_zone/" t
				       "\\`[^.]")))
			    "temperature: +\\([0-9]+\\) C$" 1)
			   "N/A"))
	      ad-return-value))
      (setq battery-mode-line-format "[%b%p%%,%dC]")))

;;; highlight changes in documents
(when-library
 t hilit-chg
 ;; toggle changes visibility
 (global-set-key [f7] 'highlight-changes-visible-mode)
 ;; remove the change-highlight in region
 (global-set-key (kbd "S-<f7>") 'highlight-changes-remove-highlight)
 ;; alt-pgup/pgdown jump to the previous/next change
 (global-set-key (kbd "<M-prior>") 'highlight-changes-previous-change)
 (global-set-key (kbd "<M-next>") 'highlight-changes-next-change))

;;; tramp-ing
(when-library
 t tramp
 (defun tramping-mode-line ()
   "Change modeline when root or remote."
   (let ((host-name (if (file-remote-p default-directory)
			(tramp-file-name-host
			 (tramp-dissect-file-name
			  default-directory)))))
     (when host-name
       (make-local-variable 'mode-line-buffer-identification)
       (setq mode-line-buffer-identification
	     (cons
	      (propertize
	       (if (string-match "^/su\\(do\\)?:" default-directory)
		   (concat "su" (match-string 1 default-directory)
			   "@" host-name)
		 host-name)
	       'face 'font-lock-warning-face)
	      (default-value 'mode-line-buffer-identification))))))

 (hook-modes tramping-mode-line
	     find-file-hooks dired-mode-hook))

(when-library t epg
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
 t gnus
 (defvar *gnus-new-mail-count* "" "Unread messages count.")
 (put '*gnus-new-mail-count* 'risky-local-variable t)
 (add-to-list 'global-mode-string '*gnus-new-mail-count* t 'eq)

 (eval-after-load "gnus"
   `(progn
      (setq gnus-select-method '(nntp "news.gmane.org")
	    gnus-secondary-select-methods
	    `((nnimap "gmail" (nnimap-address "imap.gmail.com")
		      (nnimap-server-port 993) (nnimap-stream ssl)
		      (nnimap-authinfo-file ,(concat +home-path+
						     ".authinfo.gpg")))
	      (nnml "yahoo"))
	    message-send-mail-function 'smtpmail-send-it
	    smtpmail-starttls-credentials '(("smtp.gmail.com"
					     587 nil nil))
	    smtpmail-auth-credentials '(("smtp.gmail.com" 587
					 "m00naticus@gmail.com" nil))
	    smtpmail-default-smtp-server "smtp.gmail.com"
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
	      (let ((server (car server-status)))
		(and (gnus-check-backend-function
		      'request-scan (car server))
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
		    (if info (gnus-request-update-info info method))
		    (gnus-get-unread-articles-in-group info active)
		    (or (gnus-virtual-group-p group)
			(gnus-close-group group))
		    (if gnus-agent
			(gnus-agent-save-group-info
			 method (gnus-group-real-name group) active))
		    (gnus-group-update-group group))
		  (let ((unread (gnus-group-unread group)))
		    (and (numberp unread) (> unread 0)
			 (setq unread-count (+ unread-count unread)
			       unread-groups (concat unread-groups
						     ", " group)))))))
	    ;; show popup on new mail and change mode line
	    (setq *gnus-new-mail-count*
		  (if (null unread-groups) ""
		    ,(win-or-nix
		      nil
		      (when-library
		       nil notify
		       '(if (> unread-count
			       (string-to-number
				*gnus-new-mail-count*))
			    (notify
			     "Gnus"
			     (format
			      (concat "%d new mail%s in "
				      (substring unread-groups 2))
			      unread-count
			      (if (= unread-count 1) "" "s"))))))
		    (propertize (format "%d" unread-count)
				'face 'font-lock-warning-face))))))

      (byte-compile 'my-gnus-demon-scan-mail)
      ;; run (gnus-demon-init) to track emails
      (gnus-demon-add-handler 'my-gnus-demon-scan-mail 10 nil)
      (add-hook 'kill-emacs-hook (byte-compile
				  (lambda () "Quit Gnus."
				    (setq gnus-interactive-exit nil)
				    (gnus-group-exit)))))))

(when-library
 nil fortune
 (when (executable-find "fortune")
   (let ((fortune-d (concat user-emacs-directory "fortune/")))
     (if (file-exists-p fortune-d)
	 (setq fortune-dir fortune-d
	       fortune-file (concat fortune-dir "sigs"))))
   (add-hook 'message-signature-setup-hook 'fortune-to-signature)))

(when-library
 t browse-url
 (defconst +apropos-url-alist+
   '(("^s:? +\\(.*\\)" .
      "https://ssl.scroogle.org/cgi-bin/nbbwssl.cgi/search?q=\\1")
     ("^gw?:? +\\(.*\\)" .
      "http://www.google.com/search?q=\\1&ie=utf-8&oe=utf-8")
     ("^gs:? +\\(.*\\)" . "http://scholar.google.com/scholar?q=\\1")
     ("^g!:? +\\(.*\\)" .		; Google Lucky
      "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=\\1")
     ("^gl:? +\\(.*\\)" . "http://www.google.com/linux?q=\\1")
     ("^gi:? +\\(.*\\)" .		; Google Images
      "http://images.google.com/images?sa=N&tab=wi&q=\\1")
     ("^gv:? +\\(.*\\)" . "http://video.google.com/videosearch?q=\\1")
     ("^gg:? +\\(.*\\)" . "http://groups.google.com/groups?q=\\1")
     ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(\\w+://.*\\)" . ; Translate URL
      "http://translate.google.com/translate?langpair=\\1|\\2&u=\\3")
     ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(.*\\)" . ; Translate Text
      "http://translate.google.com/translate_t?langpair=\\1|\\2&text=\\3")
     ("^gd:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(.*\\)" . ; Google Dictionary
      "http://www.google.com/dictionary?aq=f&langpair=\\1|\\2&q=\\3&hl=\\1")
     ("^w:? +\\(.*\\)" .		; Wikipedia en
      "http://en.wikipedia.org/wiki/Special:Search?search=\\1")
     ("^bgw:? +\\(.*\\)" .		; Wikipedia bg
      "http://bg.wikipedia.org/wiki/Special:Search?search=\\1")
     ("^yt:? +\\(.*\\)" .		; YouTube
      "http://www.youtube.com/results?search_query=\\1")
     ("^/\\.:? +\\(.*\\)" .		; Slashdot search
      "http://www.osdn.com/osdnsearch.pl?site=Slashdot&query=\\1")
     ("^fm:? +\\(.*\\)" . "http://www.freshmeat.net/search?q=\\1")
     ("^rd:? +\\(.*\\)" . "http://m.reddit.com/r/\\1") ; sub Reddits
     ("^ewiki:? +\\(.*\\)" .		; Emacs Wiki Search
      "http://www.emacswiki.org/cgi-bin/wiki?search=\\1")
     ("^ewiki2:? +\\(.*\\)" .		; Google Emacs Wiki
      "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&q=\\1&sa=Search")
     ("^cliki:? +\\(.*\\)" .		; Common Lisp wiki
      "http://www.cliki.net/admin/search?words=\\1")
     ("^hayoo:? +\\(.*\\)" .		; Hayoo
      "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query=\\1")
     ("^imdb:? +\\(.*\\)" . "http://imdb.com/find?q=\\1")
     ("^ma:? +\\(.*\\)" .	       ; Encyclopaedia Metallum, bands
      ;;"http://www.metal-archives.com/search.php?type=band&string=\\1"
      "http://www.google.com/search?q=\\1&as_sitesearch=metal-archives.com")
     ("^aur:? +\\(.*\\)" .		; Arch Linux's Aur packages
      "http://aur.archlinux.org/packages.php?&K=\\1")
     ("^fp:? +\\(.*\\)" .		; FreeBSD's FreshPorts
      "http://www.FreshPorts.org/search.php?query=\\1&num=20")
     ("^nnm:? +\\(.*\\)" . "http://nnm.ru/search?q=\\1"))
   "Search engines and sites.")

 (autoload 'browse-url-interactive-arg "browse-url")
 (global-set-key
  [f6]
  (lambda (text &optional new-window)
    "Search for TEXT by some search engine.
Open in new tab if NEW-WINDOW."
    (interactive (browse-url-interactive-arg
		  (concat "Location" (if current-prefix-arg
					 " (new tab)")
			  ": ")))
    (let ((text (mapconcat (lambda (s) (encode-coding-string s 'utf-8))
			   (split-string text) " "))
	  (apropo-reg "^$"))
      (let ((url (assoc-default text +apropos-url-alist+
				(lambda (a b) (if (string-match a b)
					     (setq apropo-reg a)))
				text)))
	(browse-url (replace-regexp-in-string
		     " " "+"
		     (replace-regexp-in-string apropo-reg url text))
		    (not new-window)))))))

;;; LaTeX beamer allow for export=>beamer by placing
;; #+LaTeX_CLASS: beamer in org files
(when-library
 t org
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

;;; ELPA
(if (require 'package nil t) (package-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; external extensions

;;; sml-modeline
(when (require 'sml-modeline nil t)
  (scroll-bar-mode -1)
  (sml-modeline-mode 1))

;;; TabBar
(when (require 'tabbar nil t)
  (tabbar-mode 1)

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
     ((string-match "^*tramp" (buffer-name))
      (setq ad-return-value (list "Tramp")))
     ((string-match "^*anything" (buffer-name))
      (setq ad-return-value (list "Anything")))
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

  (global-set-key (kbd "C-<tab>")
		  (lambda (arg) "Go to next tab. With prefix, next group."
		    (interactive "P")
		    (if arg (tabbar-forward-group)
		      (tabbar-forward-tab))))
  (global-set-key (win-or-nix (kbd "C-S-<tab>")
			      (kbd "<C-S-iso-lefttab>"))
		  (lambda (arg)
		    "Go to previous tab. With prefix, previous group."
		    (interactive "P")
		    (if arg (tabbar-backward-group)
		      (tabbar-backward-tab))))

  (when-library
   t org
   (add-hook 'org-load-hook
	     (lambda () "Allow tabbar keys in org."
	       (define-keys org-mode-map
		 (kbd "C-<tab>") nil
		 (win-or-nix (kbd "C-S-<tab>")
			     (kbd "<C-S-iso-lefttab>")) nil))))
  ;; remove buffer name from modeline as it now becomes redundant
  (setq-default mode-line-buffer-identification ""))

;; Wget
(when-library
 nil wget
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
		 (wget-uri uri dir '("-krmnp" "-E" "-X/page,/message"
				     "--no-check-certificate"))))))))

;;; Anything
(when-library
 nil anything
 (autoload 'anything "anything" "Select anything.")
 (defalias 'my-anything 'anything)
 (global-set-key (kbd "<f5> m") 'my-anything)
 (when-library
  nil anything-config
  (autoload 'anything-for-files "anything-config"
    "Preconfigured `anything' for opening files.")
  (autoload 'anything-info-at-point "anything-config"
    "Preconfigured `anything' for searching info at point.")
  (autoload 'anything-show-kill-ring "anything-config"
    "Preconfigured `anything' for `kill-ring'.")
  (unless (featurep 'ergoemacs-mode)
    (global-set-key "\M-y" 'anything-show-kill-ring)
    (define-key minibuffer-local-map "\M-y" 'yank-pop))
  (global-set-key (kbd "<f5> f") 'anything-for-files)
  (global-set-key (kbd "<f5> a h i") 'anything-info-at-point)
  (win-or-nix
   nil (when (eval-when-compile
	       (string-match "gentoo\\|funtoo"
			     (shell-command-to-string "uname -r")))
	 (autoload 'anything-gentoo "anything-config"
	   "Preconfigured `anything' for gentoo linux.")
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
 nil ergoemacs-mode
 (setenv "ERGOEMACS_KEYBOARD_LAYOUT" "en")
 (when (require 'ergoemacs-mode nil t)
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
		  (if (equal ,layout "colemak")
		      (define-keys paredit-mode-map
			"\M-r" 'paredit-splice-sexp
			ergoemacs-next-line-key nil
			"\M-g" nil)))))
	  ,(when-library
	    nil slime
	    '(cond ((equal ergo-layout "colemak")
		    (eval-after-load "slime"
		      '(define-keys slime-mode-map
			 "\M-k" 'slime-next-note
			 "\M-K" 'slime-previous-note
			 "\M-n" nil
			 "\M-p" nil))
		    (eval-after-load "slime-repl"
		      '(define-key slime-repl-mode-map "\M-n" nil)))
		   ((equal ergo-layout "en")
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
     (unless (equal layout ergoemacs-keyboard-layout)
       (ergoemacs-mode 0)
       (setenv "ERGOEMACS_KEYBOARD_LAYOUT" layout)
       (setq ergoemacs-keyboard-layout layout)
       (load "ergoemacs-mode")
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
   (ergoemacs-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Lisp goodies

(when-library nil hl-sexp
	      (autoload 'hl-sexp-mode "hl-sexp"
		"Highlight s-expressions minor mode." t))

;;; autopairs
(when (require 'autopair nil t)
  (setq autopair-autowrap t
	autopair-blink nil)
  (autopair-global-mode 1))

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
(or (featurep 'ergoemacs-mode)
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

;;; Qi
(when-library
 nil qi-mode
 (autoload 'qi-mode "qi-mode" "Qi editing mode." t)
 (autoload 'run-qi "qi-mode" "Run an inferior Qi process." t)
 (add-to-list 'auto-mode-alist '("\\.qi\\'" . qi-mode))
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
		  qi-mode-hook inferior-qi-mode-hook))))

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
	 (if (file-exists-p ,(concat "file://" +home-path+
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
	 (or (featurep 'ergoemacs-mode)
	     (define-key slime-mode-map "\M-g"
	       'slime-complete-symbol)))))

;;; Clojure
(when-library
 nil clojure-mode			; from ELPA
 (eval-after-load "clojure-mode"
   '(add-hook 'clojure-mode-hook 'activate-lisp-minor-modes))
 (when-library
  nil (slime)
  (eval-after-load "slime"
    `(progn
       (when (file-exists-p
	      ,(win-or-nix (concat +home-path+ "Documents/javadoc")
			   "/usr/share/doc/java-sdk-docs-1.6.0.18"))
	 (defun slime-browse-local-javadoc (ci-name)
	   "Browse local JavaDoc documentation on class/interface CI-NAME."
	   (interactive
	    (list (slime-read-symbol-name "Class/Interface name: ")))
	   (or ci-name (error "No name given"))
	   (let ((name (replace-regexp-in-string "\\$" "." ci-name))
		 (path (concat
			,(win-or-nix
			  (concat +home-path+ "Documents/javadoc")
			  "/usr/share/doc/java-sdk-docs-1.6.0.18/html")
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
 (autoload 'clips-mode "clips-mode" "Clips editing mode." t)
 (autoload 'run-clips "inf-clips" "Run an inferior Clips process." t)
 (add-to-list 'auto-mode-alist '("\\.clp$" . clips-mode))
 (eval-after-load "clips-mode"
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

(when-library
 nil prog/prolog
 (fset 'run-prolog '(autoload "prog/prolog"
		      "Start a Prolog sub-process." t nil))
 (autoload 'prolog-mode "prog/prolog" "Prolog editing mode." t)
 (autoload 'mercury-mode "prog/prolog" "Mercury editing mode." t)
 (setq prolog-system 'swi
       auto-mode-alist (nconc '(("\\.pl$" . prolog-mode)
				("\\.m$" . mercury-mode))
			      auto-mode-alist)))

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
(if (load "haskell-site-file" t)
    (hook-modes ((haskell-indentation-mode t)
		 turn-on-haskell-doc-mode)
		haskell-mode-hook))

;;; Caml
(when-library
 nil tuareg
 (autoload 'tuareg-mode "tuareg" "(O)Caml editing mode." t)
 (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
 (add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode)))

;;; Python
(when-library
 nil python-mode
 (autoload 'python-mode "python-mode" "Python editing mode." t)
 (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
 (add-to-list 'interpreter-mode-alist '("python" . python-mode)))

;;; VB
(when-library nil visual-basic-mode
	      (autoload 'visual-basic-mode "visual-basic-mode"
		"Visual Basic mode." t)
	      (add-to-list 'auto-mode-alist
			   '("\\.\\(frm\\|bas\\|cls\\|rvb\\|vbs\\)$"
			     . visual-basic-mode)))

;;; C#
(when-library
 nil csharp-mode
 (autoload 'csharp-mode "csharp-mode" "C# editing mode." t)
 (add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode)))

;;; cc-mode - hide functions
(add-hook 'c-mode-common-hook (lambda () (hs-minor-mode 1)
				(local-set-key [backtab]
					       'hs-toggle-hiding)))

;;; Global tags
(when-library
 nil gtags
 (when (executable-find "global")
   (autoload 'gtags-mode "gtags"
     "Minor mode for utilizing global tags." t)

   (defun gtags-create-or-update ()
     "Create or update the gnu global tag file."
     (interactive)
     (if (= 0 (call-process "global" nil nil nil "-p"))
	 ;; tagfile already exists; update it
	 (start-process "global" "*GTags*" "global" "-u")
       (let ((olddir default-directory)
	     (topdir (read-directory-name "GTags:top of source tree: "
					  nil nil t)))
	 (cd topdir)
	 (start-process "gtags" "*GTags*" "gtags")
	 (cd olddir)))
     (display-buffer "*GTags*"))

   (add-hook 'gtags-mode-hook (lambda ()
				(local-set-key "\M-." 'gtags-find-tag)
				(local-set-key "\M-,"
					       'gtags-find-rtag)))
   (add-hook 'c-mode-common-hook
	     (lambda () ;; (gtags-create-or-update)
	       (if (= 0 (call-process "global" nil nil nil "-p"))
		   (gtags-mode t))))))

;;; Emacs Code Browser
(when-library
 t semantic
 (if (require 'ecb-autoloads nil t)
     (eval-after-load "ecb"
       `(progn (custom-set-variables '(ecb-options-version "2.40"))
	       (let ((prog-path ,(win-or-nix
				  (concat +home-path+ "Programs")
				  (eval-when-compile
				    (concat +home-path+ "Programs")))))
		 (ecb-add-source-path prog-path prog-path t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Auxiliary extensions

;;; AUCTeX from ELPA
(when-library
 nil auctex-autoloads
 (eval-after-load "latex"
   '(progn (load "preview" t)
	   (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)))
 (or (featurep 'ergoemacs-mode)
     (eval-after-load "tex"
       '(define-key TeX-mode-map "\M-g" 'TeX-complete-symbol))))

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

;;; Auto Install
(when-library
 nil auto-install
 (autoload 'auto-install-from-directory "auto-install"
   "Update elisp files under DIRECTORY from EmacsWiki." t)
 (eval-after-load "auto-install"
   `(setq auto-install-directory ,(win-or-nix
				   (concat +extras-path+
					   "auto-install/")
				   (eval-when-compile
				     (concat +extras-path+
					     "auto-install/"))))))

(if (executable-find "conkeror")
    (setq browse-url-browser-function 'browse-url-generic
	  browse-url-generic-program "conkeror"))

;;; w3m
(when (and (executable-find "w3m") (require 'w3m-load nil t))
  (setq ;; make w3m default for most URLs
   browse-url-browser-function
   `(("^ftp:/.*" . (lambda (url &optional nf)
		     (call-interactively
		      'find-file-at-point url)))
     ("video" . ,browse-url-browser-function)
     ("\\.tv" . ,browse-url-browser-function)
     ("youtube" . ,browse-url-browser-function)
     ("." . w3m-browse-url)))
  (autoload 'w3m-find-file "w3m" "Browse local file with w3m." t)
  (add-hook 'dired-load-hook
	    (lambda () "Add w3m key for opening files in dired."
	      (define-key dired-mode-map "\C-cw"
		(lambda () "Open a file with w3m."
		  (interactive)
		  (w3m-find-file (dired-get-filename))))))
  (when-library nil w3m-wget (if (executable-find "w3m")
				 (require 'w3m-wget nil t)))
  ;; Conkeror style anchor numbering on actions
  (add-hook 'w3m-mode-hook 'w3m-link-numbering-mode)

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
	 (if w3m-key-binding "t" "i") 'w3m-linknum-save-image
	 "z" 'w3m-horizontal-recenter
	 "\C-cs" 'w3m-session-select)
       ,(when-library
	 nil ergoemacs-mode
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
			    (car (w3m-linknum-get-action
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
		  (car (w3m-linknum-get-action
			(concat browse-url-generic-program
				" on link: ") 1))))))

	 (byte-compile 'w3m-browse-url-generic)
	 (define-key w3m-mode-map "m" 'w3m-browse-url-generic))

       (add-hook 'kill-emacs-hook (byte-compile (lambda () "Quit w3m."
						  (w3m-quit t))) t)))

  (eval-after-load "w3m-search"
    '(progn
       (add-to-list
	'w3m-search-engine-alist
	'("scroogle"
	  "https://ssl.scroogle.org/cgi-bin/nbbwssl.cgi/search?q=%s"
	  utf-8))
       (setq w3m-search-default-engine "scroogle"))))

;;; handle ftp with emacs, if not set above
(or (consp browse-url-browser-function)
    (setq browse-url-browser-function
	  `(("^ftp:/.*" . (lambda (url &optional nf)
			    (call-interactively
			     'find-file-at-point url)))
	    ("." . ,browse-url-browser-function))))

;;; mldonkey
(when-library
 nil mldonkey
 (autoload 'mldonkey "mldonkey" "Run the MlDonkey interface." t))

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
	   (push (car global-mode-string) new-global-mode-string)
	   (setq global-mode-string (cdr global-mode-string)))
	 (push 'emms-playing-time-string new-global-mode-string)
	 (push 'emms-mode-line-string new-global-mode-string)
	 (setq global-mode-string (nreverse new-global-mode-string)))
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

       (when (require 'emms-lastfm-client nil t)
	 (setq emms-lastfm-client-username "m00natic"
	       emms-lastfm-client-api-key
	       "very-secret"
	       emms-lastfm-client-api-secret-key
	       "very-secret")
	 (emms-lastfm-scrobbler-enable))

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

;;; Dictionary
(when-library nil dictionary
	      (global-set-key "\C-cd" 'dictionary-search))

;;; chess
(when-library nil chess			; from ELPA
	      ;;(autoload 'chess "chess" "Play a game of chess" t)
	      (setq chess-sound-play-function nil))

;;; go
(when-library nil gnugo
	      (if (executable-find "gnugo")
		  (autoload 'gnugo "gnugo" "Play Go." t)))

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
       (setq w32shell-cygwin-bin cygwin-dir))))

;;; Desktop notifications
 (when-library nil notify
	       (autoload 'notify "notify" "Notify TITLE, BODY.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(win-or-nix (server-start))	  ; using --daemon on *nix

;;; init.el ends here
