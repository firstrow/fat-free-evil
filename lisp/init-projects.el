(use-package persp-mode
  :config
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
                  ("q" nil "cancel" :color blue))))

(provide 'init-projects)
