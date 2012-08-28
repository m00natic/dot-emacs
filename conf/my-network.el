;;; my-network.el --- Browse and network settings
;;; -*- lexical-bind: t -*-

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(require 'my-utils)

(custom-set-variables
 '(browse-url-new-window-flag t)
 '(tramp-shell-prompt-pattern
   "\\(?:^\\|\\)[^#$%>\n]*#?[#$%>] *\\(;?\\[[0-9;]*[a-zA-Z] *\\)*"))

;;; ERC
(when-library
 t erc (eval-after-load "erc" '(define-keys erc-mode-map
				 [f11] 'erc-previous-command
				 [f12] 'erc-next-command)))

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
	 (push (propertize
		(if (string-match "^/su\\(do\\)?:" default-directory)
		    (concat "su" (match-string 1 default-directory)
			    "@" host-name)
		  host-name)
		'face 'highlight)
	       mode-line-buffer-identification))))

 (hook-modes tramping-mode-line
	     find-file-hooks dired-mode-hook)

 (defun browse-ftp-tramp (url &optional new-window)
   "Open ftp URL in NEW-WINDOW with TRAMP."
   (if (string-match "\\(.*?\\)\\(\\/.*\\)" url 6)
       (find-file (concat "/ftp:anonymous@" (match-string 1 url)
			  ":" (match-string 2 url)))
     (find-file (concat "/ftp:anonymous@" (substring url 6) ":/")))))

;;; browse apropos
(when-library
 t browse-url
 (defvar my-search
   '("google" . "http://www.google.com/search?q=")
   "My default search engine.")

 (defconst +apropos-url-alist+
   '(("^s +\\(.*\\)" . "https://startingpage.com/do/search?query=\\1")
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
     ("^c\\+\\+ +\\(.*\\)" .		; C++
      "http://www.cplusplus.com/search.do?q=\\1")
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

;;; Wget
(when-library
 nil wget
 (when (executable-find "wget")
   (autoload 'wget-cd-download-dir "wget"
     "Change directory to wget download dir.")
   (autoload 'wget-uri "wget" "Wget URI asynchronously.")
   (setq
    wget-download-directory-filter 'wget-download-dir-filter-regexp
    wget-download-directory
    `(("\\.\\(jpe?g\\|png\\|gif\\|bmp\\)$"
       . ,(concat +home-path+ "Pictures"))
      ("." . ,(concat +home-path+ "Downloads"))))

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

;;; Default browser
(if (executable-find "conkeror")
    (setq browse-url-browser-function 'browse-url-generic
	  browse-url-generic-program "conkeror"))

;;; emacs-w3m
(when-library
 nil w3m
 (when (executable-find "w3m")
   (setq ;; make w3m default for most URLs
    browse-url-browser-function
    `(("^ftp://.*" . browse-ftp-tramp)
      ("video" . ,browse-url-browser-function)
      ("\\.tv" . ,browse-url-browser-function)
      ("youtube" . ,browse-url-browser-function)
      ("." . w3m-browse-url)))

   (win-or-nix (setq w3m-imagick-convert-program nil))

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
	(setq w3m-home-page ,(concat "file://" +home-path+
				     ".w3m/bookmark.html")
	      w3m-use-cookies t)
	(define-keys w3m-mode-map
	  (if w3m-key-binding "t" "i") 'w3m-lnum-save-image
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
			     (car (w3m-lnum-get-action
				   "Curl on: " 1))))))
	      (if (stringp url)
		  (let ((olddir default-directory))
		    (cd (read-directory-name
			 "Save to: " (concat +home-path+ "Downloads")
			 nil t))
		    (async-shell-command (concat "curl -kO '" url "'")
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
		     utf-8)))))

;;; handle ftp with emacs, if not set above
(or (consp browse-url-browser-function)
    (setq browse-url-browser-function
	  `(("^ftp://.*" . browse-ftp-tramp)
	    ("." . ,browse-url-browser-function))))

(provide 'my-network)

;;; my-network.el ends here
