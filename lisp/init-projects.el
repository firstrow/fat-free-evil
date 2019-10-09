
(defun spacemacs/layout-switch-by-pos (pos)
  "Switch to perspective of position POS."
  (interactive)
  (let ((persp-to-switch
         (nth pos (persp-names-current-frame-fast-ordered))))
    (if persp-to-switch
        (persp-switch persp-to-switch)
      (when (y-or-n-p
             (concat "Perspective in this position doesn't exist. Do you want to create one? "))
        (let ((persp-reset-windows-on-nil-window-conf t))
          (persp-switch nil))))))

;; Define all `spacemacs/persp-switch-to-X' functions
(dolist (i (number-sequence 9 0 -1))
  (eval `(defun ,(intern (format "spacemacs/persp-switch-to-%s" i)) nil
           ,(format "Switch to layout %s." i)
           (interactive)
           (spacemacs/layout-switch-by-pos ,(if (eq 0 i) 9 (1- i))))))

(use-package persp-mode
  :ensure t
  :config
  (with-eval-after-load "persp-mode-autoloads"
    (setq wg-morph-on nil) ;; switch off animation
    (setq persp-auto-resume-time 0)
    (add-hook 'after-init-hook #'(lambda () (persp-mode 1)))

    (setq persp-nil-name "home")

    (ryo-modal-key
     "SPC l" :hydra
     '(hydra-layout ()
		                "Layout"
		                ("s" persp-frame-switch "switch project")
                        ("n" persp-next "next")
                        ("p" persp-prev "prev")
                        ("r" persp-rename "rename")
                        ("a" persp-add-new "add new")
		                ("q" nil "cancel" :color blue)))))

(provide 'init-projects)
