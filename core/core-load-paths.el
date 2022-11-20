(defun add-to-load-path (dir)
  (add-to-list 'load-path dir))

;; TODO: get rid of this file?
(defconst core-directory (expand-file-name (concat user-emacs-directory "core/")))

(mapc 'add-to-load-path `(,core-directory))
