(defun config/init-buffers-keybindings ()
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "TAB" 'avg/alternate-buffer
   "fed" 'avg/find-dotfile
   "bd" 'avg/kill-this-buffer))


(defun avg/kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))


;; https://github.com/syl20bnr/spacemacs/blob/532ad2567cba1d57d09e102c385315e7cfa829ec/core/core-funcs.el#L331
(defun avg/alternate-buffer (&optional window)
  "To toggle in the current window between the current and the last buffers and vice versa"
  (interactive)
  (cl-destructuring-bind (buf start pos)
      (or (cl-find (window-buffer window) (window-prev-buffers)
		   :key #'car :test-not #'eq)
	  (list (other-buffer) nil nil))
    (if (not buf)
	(message "Last buffer not found.")
      (set-window-buffer-start-and-point window buf start pos))))  


(defun avg/find-dotfile ()
  (interactive)
  
  (find-file-existing "~/.emacs.my/init.el")) 


(provide 'core-buffers)
