#!/usr/bin/emacs --script
;;; compile-extras.el --- byte compile all elisp files within directory tree

;;; Commentary:
;; Apply a function to name selected files in directory and subdirectories.

;;; Code:

(defun glob-to-regex (glob)
  "Turn a GLOB to a reg-exp."
  (concat
   "^" (replace-regexp-in-string
	"\*" ".*"
	(replace-regexp-in-string
	 "?" "." (replace-regexp-in-string "\\." "\\\\." glob)))
   "$"))

(defun filter-name (file-name patterns)
  "Check whether FILE-NAME is fully matched by any of the PATTERNS.
Return matching pattern if such, nil otherwise."
  (if patterns
      (let ((match (string-match (car patterns) file-name)))
	(or (and match (zerop match) (car patterns))
	    (filter-name file-name (cdr patterns))))))

(defun maprdir (fn dir &optional file-types subdir-p)
  "Apply FN over all files in DIR and its subdirectories.
FILE-TYPES determines file name patterns for calling FN upon.
Default is all files.  If SUBDIR-P is nil,
we are in the top level directory, otherwize we are lower.
This is used when recursing, when calling, must be nil."
  (or subdir-p                  ; executed only once, in top directory
      (setq file-types (mapcar 'glob-to-regex
			       (split-string (or file-types "*")
					     nil t))))
  (mapc (lambda (file)
	  (or (string-equal "." file)
	      (string-equal ".." file)
	      (let ((file-full (concat dir "/" file)))
		(cond ((file-directory-p file-full)
		       (maprdir fn (concat file-full "/")
				file-types t))
		      ((filter-name file file-types)
		       (funcall fn file-full))))))
	(directory-files dir)))

(maprdir 'byte-compile-file "~/.emacs.d/elpa/" "*.el")
(maprdir 'byte-compile-file "~/.emacs.d/conf/" "*.el")

(provide 'compile-extras)

;;; compile-extras.el ends here
