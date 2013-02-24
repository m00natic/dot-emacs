;;; init-export.el --- .emacs alternative with clean org-publish state

;;; Commentary:
;; 

;;; Code:

(require 'org-publish)

(package-initialize)			; for htmlize

(load-theme 'wombat)

(let ((my-site-dir "~/Documents/my-site/"))
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

(org-publish "my-site")

(provide 'init-export)

;;; init-export.el ends here
