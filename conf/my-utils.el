;;; my-utils.el --- Help utilities

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

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
		(push `(define-key ,mode ,(car keys) ,(cadr keys))
		      res)
		(setq keys (cddr keys)))
	      (nreverse res))))

(provide 'my-utils)

;;; my-utils.el ends here
