;;; my-windows.el --- MS Windows speciffic settings  -*- lexical-binding: t -*-

;;; Commentary:
;; Author: Andrey Kotlarski <m00naticus@gmail.com>

;;; Code:

(defconst +win-path+ "C:/" "Windows root path.")

(setq user-emacs-directory (concat +home-path+ ".emacs.d/")
      default-directory +home-path+)

;;; Cygwin
(let ((cygwin-dir (concat +win-path+ "cygwin/bin")))
  (when (file-exists-p cygwin-dir)
    (setq shell-file-name "bash"
	  explicit-shell-file-name "bash")
    (setenv "SHELL" shell-file-name)
    (setenv "CYGWIN" "nodosfilewarning")

    (when (require 'cygwin-mount nil t)
      (cygwin-mount-activate)
      (setq w32shell-cygwin-bin cygwin-dir))))

;;; don't kill Emacs
(defun hide-frame ()
  "Keep Emacs running hidden.
Use emacsclient -e '(make-frame-visible)' to restore it."
  (interactive)
  (server-edit)
  (make-frame-invisible nil t))

(global-set-key "\C-x\C-c" 'hide-frame)

(server-start)

(provide 'my-windows)

;;; my-windows.el ends here
