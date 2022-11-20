(defun config/init-help ()
  (use-package helpful
    :bind (("C-h f" . 'helpful-callable)
           ("C-h v" . 'helpful-variable)
           ("C-h k" . 'helpful-key)
           ("C-c C-d" . 'helpful-at-point)))

  (use-package elisp-demos
    :config
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))


(provide 'core-help)
