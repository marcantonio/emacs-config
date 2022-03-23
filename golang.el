;; go-mode
(use-package go-mode
  :config
  ;(add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook #'lsp-deferred)
  :custom
  (gofmt-command "goimports"))
