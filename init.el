;;; init.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;
;;; Initialize package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
(straight-use-package 'use-package)

;;
;;; Packages
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
(use-package hydra :ensure t)
(use-package major-mode-hydra
  :ensure t
  :bind
  ("M-SPC" . major-mode-hydra))
(use-package expand-region :ensure t)
(use-package diminish :ensure t)
(use-package smex
  :ensure t
  :config
  (smex-initialize))
(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x))
(use-package counsel-projectile :ensure t)
(use-package goto-last-change :ensure t)
(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper))
(use-package magit :ensure t)
(use-package yasnippet
  :ensure t
  :init (setq yas-snippet-dirs '("~/code/yasnippet-go"))
  :config (yas-global-mode 1))
(use-package recentf
  :ensure t
  :init
  (setq recentf-max-menu-items 100
        recentf-max-saved-items 500)
  :config
  (recentf-mode +1)
  (run-at-time nil (* 5 60) 'recentf-save-list))
(use-package ido)
(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode))
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1))
(use-package which-key
  :ensure t
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
  :ensure t
  :diminish company-mode
  :config
  (progn
    (setq company-minimum-prefix-length 2)
    (setq company-show-numbers t)
    (setq company-tooltip-limit 10)
    (setq company-idle-delay .7)                          ; decrease delay before autocompletion popup shows
    (setq company-echo-delay .7)                          ; remove annoying blinking
    (setq company-begin-commands '(self-insert-command))  ; start autocompletion only after typing
    (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
    (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
    (define-key company-active-map (kbd "<tab>") 'company-select-next-if-tooltip-visible-or-complete-selection)
    (add-hook 'after-init-hook 'global-company-mode)))
(use-package flycheck
  :ensure t
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
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode))
(use-package multiple-cursors
  :ensure t
  :config
  (setq mc/always-run-for-all t))
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))
(use-package ibuffer
  :ensure nil
  :commands (ibuffer-find-file
             ibuffer-current-buffer)
  :bind ("C-x C-b" . ibuffer))
(use-package jump-char :ensure t)
(use-package whitespace
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook (lambda() (setf show-trailing-whitespace t)))))

(require 'init-core)
(require 'init-ui)
(require 'init-go)
(require 'init-elisp)
(require 'init-fatfree-evil)
(require 'init-projects)
(require 'init-orgbabel)
(require 'init-hydras)
