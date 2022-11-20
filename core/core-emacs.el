(defun config/init-emacs () 
  (setq-default indent-tabs-mode nil)

  (use-package restart-emacs
    :config
    (general-define-key
     :states '(normal visual)
     :prefix "SPC"
     "qr" 'avg/restart-emacs-restore-frames
     "qR" 'restart-emacs)) 

  
  ;; NOTE: moves `.*~` and `#.*#` files to tmp dir
  (setq backup-directory-alist `((".*" . ,temporary-file-directory))) 
  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))


(defun avg/restart-emacs-restore-frames ()
  (interactive)
  (setq restart-emacs-restore-frames t)
  (restart-emacs))


(provide 'core-emacs)
