;;; andr-theme.el --- andr theme

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(deftheme andr "My corrections over the default theme.")

(let ((class '((class color) (min-colors 88))))
  (custom-theme-set-faces
   'andr
   `(default ((default :foreground "black")
	      (,class :background "cornsilk" :family "Inconsolata")
	      (t :background "white" :family "terminus")))
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
			       :width condensed)))))

(provide-theme 'andr)

;;; andr-theme.el ends here