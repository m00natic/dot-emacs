;;; my-misc.el --- Miscellaneous settings

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(require 'my-utils)

(custom-set-variables
 '(golden-ratio-exclude-modes
   '("calendar-mode" "gnus-summary-mode" "gnus-article-mode"
     "calc-mode" "calc-trail-mode" "wget-mode"))
 '(google-translate-enable-ido-completion t)
 '(helm-ff-auto-update-initial-value nil)
 `(org-default-notes-file ,(concat user-emacs-directory
				   ".notes.org")))

;;; goto line
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting \
for the line number input."
  (interactive)
  (unwind-protect
      (progn (linum-mode 1)
	     (let ((line (read-number "Goto line: ")))
	       (goto-char (point-min))
	       (forward-line (1- line))))
    (linum-mode -1)))

(global-set-key "\C-cl" 'goto-line-with-feedback)

;;; occur
(eval-after-load "replace"
  '(define-keys occur-mode-map
     "n" 'occur-next "p" 'occur-prev
     "o" 'occur-mode-display-occurrence
     "\C-c\C-c" 'occur-mode-goto-occurrence-other-window))

;;; Proced
(when-library t proced (global-set-key "\C-c^" 'proced))

;;; hide-show
(when-library
 t hideshow
 (eval-after-load "hideshow"
   '(define-key hs-minor-mode-map [backtab] 'hs-toggle-hiding)))

;;; Publishing
(when-library
 t org
 (define-key global-map "\C-cc" 'org-capture)

 (let ((my-site-dir (concat +home-path+ "Documents/my-site/")))
   (if (file-exists-p my-site-dir)
       (setq org-publish-project-alist
	     `(("my-site-org"
		:base-directory ,(concat my-site-dir "org/")
		:base-extension "org"
		:publishing-directory ,my-site-dir
		:recursive t
		:publishing-function org-publish-org-to-html
		:auto-preamble t
		:auto-sitemap t
		:sitemap-title "Sitemap")
	       ("my-site-static"
		:base-directory ,(concat my-site-dir "org/")
		:base-extension
		"css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
		:publishing-directory ,my-site-dir
		:recursive t
		:publishing-function org-publish-attachment)
	       ("my-site" :components ("my-site-org"
				       "my-site-static")))))))

;;; Ace Jump
(if (require 'ace-jump-mode nil t)
    (define-key global-map "\C-c " 'ace-jump-mode))

;;; golder ratio
(when-library
 nil golden-ratio
 (unless +old-emacs+
   (golden-ratio-mode 1)
   (when-library nil ecb
		 (add-hook 'ecb-before-activate-hook
			   (lambda () (golden-ratio-mode -1))))))

;;; helm
(if (and (not +old-emacs+)
	 (require 'helm-config nil t))
    (progn (or (featurep 'ergoemacs-mode)
	       (global-set-key "\M-y" 'helm-show-kill-ring))
	   (global-set-key "\M-." 'helm-etags-select)
	   (global-set-key "\C-co" 'helm-occur)
	   (helm-mode 1))
  (icomplete-mode 1))

;;; Ditaa
(let ((ditaa-path (executable-find "ditaa.jar")))
  (when ditaa-path
    (when-library t org (setq org-ditaa-jar-path ditaa-path))

    (defun ditaa-generate ()
      "Invoke ditaa over current buffer."
      (interactive)
      (start-process "ditaa" "*Ditaa*" "java" "-jar"
		     org-ditaa-jar-path buffer-file-name)
      (display-buffer "*Ditaa*"))))

;;; Dictionary
(when-library
 nil dictionary
 (global-set-key "\C-cd" 'dictionary-search)

 ;; workarround view-mode capturing <return>
 (eval-after-load "dictionary"
   '(define-key dictionary-mode-map [return] 'link-selected)))

;;; Google Translate
(when-library
 nil google-translate
 (autoload 'google-translate-at-point "google-translate" nil t)
 (autoload 'google-translate-query-translate "google-translate" nil t)

 (global-set-key "\C-ct" 'google-translate-query-translate)
 (global-set-key "\C-cT" 'google-translate-at-point))

;;; remove clutter from modeline
(when (require 'diminish nil t)
  (when-library nil golden-ratio (diminish 'golden-ratio-mode "φ"))
  (when-library nil helm (diminish 'helm-mode ""))
  (when-library nil company (diminish 'company-mode "⚯"))
  (when-library nil undo-tree (diminish 'undo-tree-mode "⎌")))

;;; large files
(if (require 'vlf-integrate nil t)
    (setq vlf-application 'dont-ask))

;;; end of working day
;; (run-with-timer 28800 1200 'animate-birthday-present "Go Home")

;;; zone matrix
;; (and (require 'zone-matrix nil t)
;;      (require 'zone-matrix-settings nil t)
;;      (require 'zone-settings nil t)
;;      (setq zone-programs [zone-matrix]))

(provide 'my-misc)

;;; my-misc.el ends here
