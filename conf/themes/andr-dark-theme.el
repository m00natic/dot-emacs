;;; andr-dark-theme.el --- andr-dark theme

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(deftheme andr-dark "My corrections over the default dark theme.")

(let ((class '((class color) (min-colors 88))))
  (custom-theme-set-faces
   'andr-dark
   `(default ((default :background "#3f3f3f")
	      (,class :foreground "#dcdccc" :family "Inconsolata")
	      (t :foreground "white" :family "terminus")))
   `(mode-line ((default :inherit variable-pitch :width condensed)
		(,class :background "#2b2b2b" :foreground "#8fb28f")
		(t :background "cyan")))
   '(mode-line-inactive
     ((default :inherit mode-line :weight light
	:box (:line-width -1 :color "grey40")
	:background "#383838" :foreground "#5f7f5f")))
   `(tabbar-selected ((t :inherit default :weight bold
			 (,class :box (:line-width -1
				       :style pressed-button)))))
   `(tabbar-unselected ((default :inherit default)
			(,class :box (:line-width -1
				:style released-button))
			(t :inverse-video t)))
   '(tabbar-button ((t :inherit default)))
   '(sml-modeline-end-face ((t :family "neep" :inherit default
			       :width condensed)))))

(provide-theme 'andr-dark)

;;; andr-dark-theme.el ends here
