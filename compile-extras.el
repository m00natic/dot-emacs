#!/usr/bin/emacs --script
;;; compile-extras.el --- byte compile all elisp files within directory tree


;;; Commentary:
;;

;;; Code:

(defun replace-regex-str (word regex str)
  "In WORD replace REGEX with STR."
  (mapconcat 'identity (split-string word regex) str))

(defun glob-to-regex (glob)
  "Turn a GLOB to a reg-exp."
  (concat (replace-regex-str
	   (replace-regex-str (replace-regex-str glob "\\." "\\.")
			      "?" ".")
	   "\\*" ".*") "$"))

(defun filter-name (file-name patterns)
  "Check whether FILE-NAME is fully matched by any of the PATTERNS.
Return matching pattern if such, nil otherwise."
  (if patterns
      (let ((match (string-match (car patterns) file-name)))
	(or (and match (= 0 match) (car patterns))
	    (filter-name file-name (cdr patterns))))))

(defun maprdir (fn dir &optional file-types subdir-p)
  "Apply FN over all files in DIR and its subdirectories.
FILE-TYPES determines file name patterns for calling FN upon.
Default is all files.  If SUBDIR-P is nil,
we are in the top level directory, otherwize we are lower.
This is used when recursing, when calling, must be nil."
  (or subdir-p		 ;executed only once, in top directory
      (setq file-types (mapcar 'glob-to-regex
			       (split-string (or file-types "*")
					     nil t))))
  (dolist (file (directory-files dir))
    (or (equal "." file)
	(equal ".." file)
	(let ((file-full (concat dir file)))
	  (if (file-directory-p file-full)
	      (maprdir fn (concat file-full "/") file-types t)
	    (let ((match (filter-name file file-types)))
	      (if match
		  (funcall fn file-full))))))))

(maprdir 'byte-compile-file "~/.emacs.d/elpa/" "*.el")
(maprdir 'byte-compile-file "~/.emacs.d/extras/" "*.el")

(provide 'compile-extras)

;;; compile-extras.el ends here
