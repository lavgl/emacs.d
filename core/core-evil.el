(defun config/init-evil () 
  (use-package undo-fu)

  (use-package  evil 
    :init (setq evil-want-keybinding nil) 
    (setq evil-undo-system 'undo-fu)
    :config
    (evil-mode 1)
    (global-set-key (kbd "C-u") 'evil-scroll-up) 

    ;; TODO: check this out
    (setq evil-normal-state-modes
	  (append evil-normal-state-modes evil-motion-state-modes))))


(provide 'core-evil)



