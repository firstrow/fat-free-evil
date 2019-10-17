;;; init.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;
;;; Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight manager by default
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;
;;; Packages
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
(use-package hydra)
(use-package major-mode-hydra
  :bind
  ("M-SPC" . major-mode-hydra))
(use-package expand-region)
(use-package diminish)
(use-package smex
  :config
  (smex-initialize))
(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x))
(use-package counsel-projectile)
(use-package goto-last-change)
(use-package swiper
  :bind ("C-s" . 'swiper))
(use-package magit)
(use-package yasnippet
  :init (setq yas-snippet-dirs '("~/code/yasnippet-go"))
  :config (yas-global-mode 1))
(use-package recentf
  :init
  (setq recentf-max-menu-items 100
        recentf-max-saved-items 500)
  :config
  (recentf-mode +1)
  (run-at-time nil (* 5 60) 'recentf-save-list))
(use-package ido)
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode))
(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1))
(use-package which-key
  :defer 2
  :diminish which-key-mode
  :config
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))
(use-package company
  :diminish company-mode
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-idle-delay .7)                          ; decrease delay before autocompletion popup shows
  (setq company-echo-delay .7)                          ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command))  ; start autocompletion only after typing
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "<tab>") 'company-select-next-if-tooltip-visible-or-complete-selection)
  (add-hook 'after-init-hook 'global-company-mode))
(use-package flycheck
  :diminish flycheck-mode
  :config
  ;; (global-flycheck-mode)
  (add-hook 'go-mode-hook #'flycheck-mode)

  ;; Fix flycheck error messages for golang
  ;; https://github.com/flycheck/flycheck/issues/1523#issuecomment-469402280
  (let ((govet (flycheck-checker-get 'go-vet 'command)))
    (when (equal (cadr govet) "tool")
      (setf (cdr govet) (cddr govet)))))
(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode))
(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all t))
(use-package saveplace
  :hook (after-init . save-place-mode))
(use-package ibuffer
  :commands (ibuffer-find-file
             ibuffer-current-buffer)
  :bind ("C-x C-b" . ibuffer))
(use-package jump-char)
(use-package whitespace
  :config
    (add-hook 'prog-mode-hook (lambda() (setf show-trailing-whitespace t))))

(use-package org
  :config
  (setq org-default-notes-file (concat org-directory "/gtd.org"))
  (setq org-agenda-files (list "~/org/gtd.org")))

(require 'init-core)
(require 'init-ui)
(require 'init-go)
(require 'init-lisp)
(require 'init-elisp)
(require 'init-fatfree-evil)
(require 'init-projects)
(require 'init-orgbabel)
(require 'init-hydras)
