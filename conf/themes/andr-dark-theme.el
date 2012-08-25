;;; andr-dark-theme.el --- andr-dark theme

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(deftheme andr-dark "My corrections over the default dark theme.")

(let ((class '((class color) (min-colors 88))))
  (custom-theme-set-faces
   'andr-dark
   `(default ((default :background "#242424")
	      (,class :foreground "#f6f3e8" :family "Inconsolata")
	      (t :foreground "white" :family "terminus")))
   `(mode-line ((default :width condensed :family "neep")
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
			       :width condensed)))))

(provide-theme 'andr-dark)

;;; andr-dark-theme.el ends here
