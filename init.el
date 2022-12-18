;; package management setup

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(setq use-package-compute-statistics t
      use-package-verbose t)

;; no-littering setup
;; trying to not pollute my .emacs.d

(straight-use-package 'no-littering)

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
 ;; don't ask if I go to git symlink in, for example, third-party elisp source code file
 vc-follow-symlinks t)

 ;; I don't like trailing whitespace, so I prefer to see and remove them
(setq-default
 show-trailing-whitespace t
 indent-tabs-mode nil
 tab-width 4)

(global-display-line-numbers-mode)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-face-attribute 'default nil :font "Source Code Pro" :height 160)

(load-theme 'modus-vivendi)

(use-package diminish)

;; window management

;; fullscreen frame on startup
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; golden ratio
(use-package zoom
  :custom
  (zoom-size '(0.618 . 0.618))
  :config
  (zoom-mode 1))

;; window keybindings
;;
;; I have considered:
;; 1. other-window
;; 2. ace-window
;; 3. bindings from the Xah Lee blogpost (xahlee.info/emacs/emacs/effective_emacs.html)
;; 4. finally, winum-mode, which I have been using in spacemacs
;;
;; With the default 'other-window command, I'm too tired to switch between windows,
;; if there is more than two windows.
;; I have at least two windows with vertical split usually, and I'm often switching between them.
;; So, I prefer to optimize window switching, not window splitting/killing
;; (that's the reason, why Xah Lee's Meta bindings don't work for me as well).
;;
;; winum-mode works great for this purpose.
;; ace-window looks interesting, but in practice, the default number-based window keys
;; are far away from the home row and uncomfortable,
;; and customizing 'aw-keys to the home row requires adaptation,
;; which seems unneccesary, as I already could just switch to winum

(use-package winum
  :init (winum-mode)
  :bind (("M-1" . 'winum-select-window-1)
         ("M-2" . 'winum-select-window-2)
         ("M-3" . 'winum-select-window-3)
         ("M-4" . 'winum-select-window-4)))

;; improving help & discoverability

(use-package helpful
  :defer t
  :bind (("C-h f" . 'helpful-callable)
         ("C-h v" . 'helpful-variable)
         ("C-h k" . 'helpful-key)
         ("C-c C-d" . 'helpful-at-point)))

(use-package info+
  :defer t)

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

(use-package keyfreq
  :init
  (keyfreq-mode)
  (keyfreq-autosave-mode)
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command)))


;; completion setup

(use-package vertico
  :init (vertico-mode)
  :custom (vertico-cycle t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :after vertico
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; TODO: bind 'recentf-open-fles to the key?
(use-package recentf
  :init
  (recentf-mode))

;; NOTE: persists minibuffer history over emacs restarts
(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :custom
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (read-extended-command-predicate
   #'command-completion-default-include-p)

  (completion-cycle-threshold 3))


(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-popupinfo
                              corfu-history-mode))
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-preview-current nil)    ;; Disable current candidate preview

  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode))


;; search & navigation

(use-package ctrlf
  :init
  (ctrlf-mode))


(use-package projectile
  :defer t
  :init
  (projectile-mode)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;;

(use-package hl-todo
  :config
  (global-hl-todo-mode))

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
