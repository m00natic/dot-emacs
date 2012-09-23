;;; andr-theme.el --- andr theme

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(deftheme andr "My corrections over the default theme.")

(let ((class '((class color) (min-colors 88))))
  (custom-theme-set-faces
   'andr
   `(default ((default :foreground "#232333")
	      (,class :background "#c0c0c0" :family "Inconsolata")
	      (t :background "white" :family "terminus")))
   `(mode-line
     ((default :inherit variable-pitch :width condensed)
      (,class :foreground "#704d70" :background "#c7c7c7")
      (t :background "cyan")))
   '(mode-line-inactive
     ((default :inherit mode-line :weight light
	:box (:line-width -1 :color "grey60")
	:foreground "#a080a0" :background "#c7c7c7")))
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

(provide-theme 'andr)

;;; andr-theme.el ends here
