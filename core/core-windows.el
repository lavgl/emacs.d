(defun config/init-winum ()
  (use-package winum
    :config
    (winum-mode)

    (general-define-key
     :states '(normal visual)
     :prefix "SPC"
     "w/" 'split-window-right
     "w-" 'split-window-below
     "wd" 'delete-window

     "0" 'winum-select-window-0
     "1" 'winum-select-window-1
     "2" 'winum-select-window-2
     "3" 'winum-select-window-3
     "4" 'winum-select-window-4
     "5" 'winum-select-window-5
     "6" 'winum-select-window-6
     "7" 'winum-select-window-7
     "8" 'winum-select-window-8
     "9" 'winum-select-window-9))

  (general-define-key
   :states 'normal
   :keymaps '(helpful-mode-map
	      help-mode-map
	      apropos-mode-map
	      ert-results-mode-map)
   "q" 'quit-window))


(provide 'core-windows)


