;;
;; Ryo modal configuration
(use-package ryo-modal
  :straight (ryo-modal :type git :host github :repo "Kungsgeten/ryo-modal"
                       :fork (:host github :repo "firstrow/ryo-modal"))
  :config

  (defun start-from-new-line ()
    (interactive)
    (move-end-of-line nil)
    (newline)
    (indent-for-tab-command))

  (defun start-from-new-top-line ()
    (interactive)
    (previous-line)
    (start-from-new-line))

  (defun insert-mode ()
    "Kill active region if active"
    (interactive)
    (if mark-active (kill-region (region-beginning) (region-end)))
    (message "Insert mode actived"))

  (defun spacemacs/show-and-copy-buffer-filename ()
    "Show and copy the full path to the current file in the minibuffer."
    (interactive)
    ;; list-buffers-directory is the variable set in dired buffers
    (let ((file-name (or (buffer-file-name) list-buffers-directory)))
      (if file-name
          (message (kill-new file-name))
    (error "Buffer not visiting a file"))))

  (ryo-modal-keys
   ;; Movement
   ("i" insert-mode :exit t)
   ("a" forward-char :exit t)
   ("b" backward-word)
   ("f" jump-char-forward)
   ("F" jump-char-backward)
   ("x" delete-char)
   ("j" next-line)
   ("h" backward-char)
   ("w" forward-word)
   ("A" move-end-of-line)
   ("k" previous-line)
   ("l" forward-char)
   ;; Editing
   ("X" backward-delete-char-untabify)
   ("y" kill-ring-save)
   ("p" yank)
   ("^" back-to-indentation)
   ("o" start-from-new-line :exit t)
   ("O" start-from-new-top-line :exit t)
   ("u" undo)
   ;; Searching
   ("*" swiper-thing-at-point)
   ("/" counsel-projectile-rg)
   (";" counsel-projectile-switch-to-buffer)
   ;; Visual selection
   ("e" er/expand-region)
   ("(" er/mark-inside-pairs)
   ("'" er/mark-inside-quotes)
   ;; Definitions
   ("." xref-find-definitions)
   (">" xref-find-definitions-other-window)
   ("," xref-pop-marker-stack))

  (ryo-modal-keys
   (:norepeat t)
   ("0" "M-0")
   ("1" "M-1")
   ("2" "M-2")
   ("3" "M-3")
   ("4" "M-4")
   ("5" "M-5")
   ("6" "M-6")
   ("7" "M-7")
   ("8" "M-8")
   ("9" "M-9"))

  (add-hook 'find-file-hook 'ryo-modal-mode)
  (global-set-key (kbd "C-c C-SPC") 'ryo-modal-mode)

  ;;
  ;;; Modeline hooks
  (defun add-ryo-modeline-status (&rest _)
    (interactive)
    (let ((win (frame-selected-window)))
      (unless (minibuffer-window-active-p win)
        (add-to-list 'global-mode-string '(:eval (if (bound-and-true-p ryo-modal-mode)
                                                     (propertize "<R>" 'face 'lazy-highlight)
                                                   (propertize "<I>" 'face 'fringe))))
        (add-to-list 'global-mode-string '(:eval (format " #%s " (safe-persp-name (get-frame-persp)))))
        )))

  (add-hook 'after-focus-change-function 'add-ryo-modeline-status)
  (add-hook 'window-configuration-change-hook 'add-ryo-modeline-status)
  (add-hook 'focus-in-hook                    'add-ryo-modeline-status))

(provide 'init-fatfree-evil)
