(require 'core-makeup)
(require 'core-emacs)
(require 'core-evil)
(require 'core-windows)
(require 'core-buffers)
(require 'core-help)


(defun config/core-init () 
  (config/init-makeup)
  (config/init-use-package)
  (config/init-emacs)
  (config/init-evil)
  (config/init-which-key)
  (config/init-winum)
  (config/init-buffers-keybindings)
  (config/init-help))


(defun config/init-use-package () 
  (setq package-archives '(("melpa"     . "https://melpa.org/packages/") 
			   ("org"       . "http://orgmode.org/elpa/") 
			   ("gnu"       . "http://elpa.gnu.org/packages/")))
  (unless (package-installed-p 'use-package) 
    (package-refresh-contents) 
    (package-install 'use-package)))


(defun config/init-which-key ()
  (use-package which-key
    :config (which-key-mode)))


(provide 'core)
