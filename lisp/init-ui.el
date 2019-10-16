;;; ui.el -*- lexical-binding: t; -*-

(defconst EMACS26+ (> emacs-major-version 25))
(defconst EMACS27+ (> emacs-major-version 26))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

(add-to-list 'default-frame-alist '(font .  "Iosevka Semibold 14" ))
(set-face-attribute 'default t :font  "Iosevka Semibold 14" )

(setq inhibit-startup-message t
      inhibit-startup-screen t
      scroll-step 1
      scroll-margin 3
      visible-bell nil
      ring-bell-function 'ignore
      backup-by-copying t
      make-backup-files nil
      confirm-kill-emacs 'yes-or-no-p
      create-lockfiles nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;;
;;; Scrolling
(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 10
      scroll-margin 3
      scroll-preserve-screen-position t
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

(when IS-MAC
  ;; sane trackpad/mouse scroll settings
  (setq mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil))

;;
;; Vim-like scrolling
(defun scroll-line-up (count)
  "Scrolls the window COUNT lines upwards."
  (interactive "p")
  (let ((scroll-preserve-screen-position nil))
    (scroll-up count)))
(global-set-key (kbd "C-l") 'scroll-line-up)

;;
;;; Cursor

;; Don't blink the cursor, it's too distracting.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)
(setq visible-cursor nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

(global-hl-line-mode 0)
;; (set-face-attribute hl-line-face nil :underline t)

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

(show-paren-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-auto-revert-mode 1)
(prefer-coding-system 'utf-8)
(fringe-mode 1)
(electric-pair-mode)
(put 'dired-find-alternate-file 'disabled nil)

;;
;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

(unless EMACS27+  ; We already do this in early-init.el
  ;; Disable tool and scrollbars; Doom encourages keyboard-centric workflows, so
  ;; these are just clutter (the scrollbar also impacts Emacs' performance).
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(setq window-combination-resize t)

;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; _while_ we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer, like vim does. Any feedback after
;; typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
;; They're generalsly unhelpful and only add confusing visual clutter.
(setq mode-line-default-help-echo nil
      show-help-function nil)

;; y/n is easier to type than yes/no
(fset #'yes-or-no-p #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Reset theme color before loading new theme
(defadvice load-theme (before disable-before-load)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))
(ad-activate 'load-theme)

(use-package solarized-theme
  :config
  (load-theme 'solarized-light t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    ))

(use-package minions
  :config
  (setq minions-mode-line-lighter "~")
  (minions-mode))

(use-package moody
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)

  ;; Will it work on linux without a patch?
  ;; pl functions copied from powerline
  ;; https://github.com/milkypostman/powerline/blob/master/powerline-separators.el
  (defun pl/color-xyz-to-apple-rgb (X Y Z)
    "Convert CIE X Y Z colors to Apple RGB color space."
    (let ((r (+ (* 3.2404542 X) (* -1.5371385 Y) (* -0.4985314 Z)))
          (g (+ (* -0.9692660 X) (* 1.8760108 Y) (* 0.0415560 Z)))
          (b (+ (* 0.0556434 X) (* -0.2040259 Y) (* 1.0572252 Z))))
      (list (expt r (/ 1.8)) (expt g (/ 1.8)) (expt b (/ 1.8)))))

  (defun pl/color-srgb-to-apple-rgb (red green blue)
    "Convert RED GREEN BLUE colors from sRGB color space to Apple RGB.
RED, GREEN and BLUE should be between 0.0 and 1.0, inclusive."
    (apply 'pl/color-xyz-to-apple-rgb (color-srgb-to-xyz red green blue)))

  (defun linus/hex-to-apple-rgb-hex (hex)
    (apply #'color-rgb-to-hex
       (apply #'pl/color-srgb-to-apple-rgb (color-name-to-rgb hex))))

  (defun linus/moody-slant (direction c1 c2 c3 &optional height)
    (apply
     #'moody-slant
     direction
     (linus/hex-to-apple-rgb-hex c1)
     (linus/hex-to-apple-rgb-hex c2)
     (linus/hex-to-apple-rgb-hex c3)
     height))

  (setq moody-slant-function 'linus/moody-slant)
  ;; end fix

  (setq-default x-underline-at-descent-line t
                column-number-mode t))

(provide 'init-ui)
