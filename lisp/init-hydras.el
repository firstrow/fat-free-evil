;;
;;; Hydras

(defhydra hydra-expand (global-map "C-c")
  "expand-region"
  ("e" er/expand-region "Expand")
  ("c" er/contract-region "Contract" :bind nil))

(ryo-modal-key
 "SPC f" :hydra
 '(hydra-files ()
		       "Files"
		       ("r" counsel-recentf "recent")
		       ("f" counsel-projectile-find-file "find file")
		       ("y" spacemacs/show-and-copy-buffer-filename "copy filename")
		       ("q" nil "cancel" :color blue)))
(ryo-modal-key
 "SPC g" :hydra
 '(hydra-git ()
	         "Git"
	         ("g" magit-status "status")
	         ("q" nil "cancel" :color blue)))
(ryo-modal-key
 "SPC w" :hydra
 '(hydra-windows ()
		         "Windows"
		         ("w" other-window "other")
		         ("d" delete-window "delete")
		         ("s" split-window-right "split")
		         ("q" nil "cancel" :color blue)))
(ryo-modal-key
 "SPC p" :hydra
 '(hydra-projectile ()
		            "Projectile"
		            ("p" projectile-switch-project "switch project")
		            ("q" nil "cancel" :color blue)))
(ryo-modal-key
 "g" :hydra
 '(hydra-goto ()
		      "Goto"
		      (";" goto-last-change "last change")
              ("l" goto-last-change-reverse "undo goto change")
              ("g" beginning-of-buffer "beginning")
		      ("q" nil "cancel" :color blue)))

(provide 'init-hydras)
