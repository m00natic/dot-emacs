;;; my-misc.el --- Miscellaneous settings  -*- lexical-binding: t -*-

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(require 'my-utils)

(custom-set-variables
 '(google-translate-enable-ido-completion t))

;;; Imenu
(when-library t imenu (global-set-key (kbd "C-`") 'imenu))

;;; Proced
(when-library t proced (global-set-key "\C-^" 'proced))

;;; Publishing
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
				      "my-site-static"))))))

;;; Ace Jump
(if (require 'ace-jump-mode nil t)
    (define-key global-map "\C-c " 'ace-jump-mode))

;;; browse kill ring
(when-library
 nil browse-kill-ring
 (unless (featurep 'ergoemacs-mode)
   (global-set-key "\M-y" 'browse-kill-ring)))

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
   '(add-hook 'view-mode-hook
	      (lambda () (if (dictionary-mode-p)
			(local-set-key [return] 'link-selected))))))

;;; Google Translate
(when-library
 nil google-translate
 (autoload 'google-translate-at-point "google-translate" nil t)
 (autoload 'google-translate-query-translate "google-translate" nil t)

 (global-set-key "\C-ct" 'google-translate-query-translate)
 (global-set-key "\C-cT" 'google-translate-at-point))

(provide 'my-misc)

;;; my-misc.el ends here
