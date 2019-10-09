(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (shell . t)
   ))

;; Syntax highlight in #+BEGIN_SRC blocks
(setq org-src-fontify-natively t)
;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)

(provide 'init-orgbabel)
