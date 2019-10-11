;; Install requirements:

;; Works fater
;; go get -u github.com/visualfc/gocode

;; go get -u -v github.com/stamblerre/gocode
;; go get -u -v github.com/rogpeppe/godef
;; go get -u -v golang.org/x/tools/cmd/guru
;; go get -u -v golang.org/x/tools/cmd/gorename
;; go get -u -v golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/gopls

(defun go-search-func-at-cursor (sym)
  (interactive (list (thing-at-point 'symbol)))
  (counsel-git-grep (format "func %s" sym)))

(use-package lsp-mode
  :init
  (setq lsp-before-save-edits nil)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-prefer-flymake :none)
  :commands lsp)

(add-hook 'go-mode-hook #'lsp)

(use-package company-lsp :commands company-lsp)
(use-package go-eldoc)

(add-hook 'go-mode-hook
          (lambda ()
            (setq gofmt-command "goimports")
            (setq go-packages-function 'go-packages-go-list)
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq truncate-lines t)
            (setq indent-tabs-mode t)
            (setq tab-width 4)))

(major-mode-hydra-define go-mode nil
  ("Golang"
   (("a" go-import-add "add import")
    ("i" go-search-func-at-cursor "search func")
    ("e" flycheck-next-error "next error"))))

(provide 'init-go)
