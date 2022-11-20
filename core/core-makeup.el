(defun config/init-makeup ()
  (setq inhibit-startup-message t)
  (setq ring-bell-function 'ignore)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (setq-default show-trailing-whitespace t)
  (defalias 'yes-or-no-p 'y-or-n-p)

  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode)

  (set-face-attribute 'default nil :font "Source Code Pro" :height 160)



  (use-package doom-themes
    :config
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    (load-theme 'doom-one t))


  (use-package zoom
    :custom
    ;; golden ratio
    (zoom-size '(0.618 . 0.618))
    :config
    (zoom-mode 1))


  ;; NOTE: launch emacs in fullscreen mode (emacsclient too)
  (add-to-list 'default-frame-alist '(fullscreen . fullboth))


  (use-package doom-modeline
    :config (doom-modeline-mode 1)))


(provide 'core-makeup)
