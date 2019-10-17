
(use-package slime-company)
(use-package slime
             :config
             (setq inferior-lisp-program "/usr/local/bin/sbcl"
                   lisp-indent-function 'common-lisp-indent-function
                   slime-complete-symbol-function 'slime-fuzzy-complete-symbol
                   slime-startup-animation nil)

             (defvar electrify-return-match
               "[\]}\)\"]"
               "If this regexp matches the text after the cursor, do an \"electric\" return.")

             (defun electrify-return-if-match (arg)
               "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
               (interactive "P")
               (let ((case-fold-search nil))
                 (if (looking-at electrify-return-match)
	                 (save-excursion (newline-and-indent)))
                 (newline arg)
                 (indent-according-to-mode)))
             ;; Using local-set-key in a mode-hook is a better idea.
             (global-set-key (kbd "RET") 'electrify-return-if-match)

             (slime-setup '(slime-fancy
                            slime-autodoc
                            slime-indentation
                            slime-company
                            ))

             (setq slime-net-coding-system 'utf-8-unix
                   slime-truncate-lines nil)

             (setq lisp-lambda-list-keyword-parameter-alignment t
                   lisp-lambda-list-keyword-alignment t)

             (setq slime-net-coding-system 'utf-8-unix))

(provide 'init-lisp)
