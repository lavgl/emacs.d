;; package management setup

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  (package-refresh-contents))

(require 'use-package)

(setq use-package-always-ensure t
      use-package-compute-statistics t
      use-package-verbose t)


;; no-littering setup
;; trying to not pollute my .emacs.d

(unless (package-installed-p 'no-littering)
  (package-install 'no-littering))

(require 'no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
      custom-file (no-littering-expand-etc-file-name "custom.el"))


;; some basic UI tweaks

(setq
 inhibit-startup-screen t
 ;; it's hard for me to track C-v/M-v with default 2 lines
 next-screen-context-lines 8
 ;; I don't like noises
 ring-bell-function 'ignore
 display-line-numbers-type 'relative)

 ;; I don't like trailing whitespace, so I prefer to see and remove them
(setq-default show-trailing-whitespace t)

(global-display-line-numbers-mode)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-face-attribute 'default nil :font "Source Code Pro" :height 160)

(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(load-theme 'modus-vivendi)

(use-package diminish)

;; improving help system

(use-package helpful
    :bind (("C-h f" . 'helpful-callable)
           ("C-h v" . 'helpful-variable)
           ("C-h k" . 'helpful-key)
           ("C-c C-d" . 'helpful-at-point)))

(use-package which-key
  :diminish
  :config (which-key-mode))

(use-package guru-mode
  :diminish
  :config (guru-global-mode))

(use-package command-log-mode
  :init
  (setq command-log-mode-is-global t
	command-log-mode-open-log-turns-on-mode t))


;; completion setup

(use-package vertico
  ;; TODO: why it's in init, not in config?
  :init (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; init lisp support

(use-package smartparens
  :hook (prog-mode . turn-on-smartparens-strict-mode)
  :config
  (require 'smartparens-config))


;; init yaml / ansible support

(use-package yaml-mode)

(use-package ansible
  :hook (yaml-mode . ansible))
(use-package ansible-doc
  :hook (yaml-mode . ansible-doc-mode))

(use-package highlight-indentation
  :hook (yaml-mode . highlight-indentation-current-column-mode))

