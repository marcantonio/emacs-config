(require 'mas-lsp)

(use-package go-mode
  :ensure
  :config
  ;(add-hook 'before-save-hook 'gofmt-before-save)
  ;(add-hook 'go-mode-hook #'lsp-deferred)
  :custom
  (gofmt-command "goimports"))

(load-dev-mode 'go-mode)

(provide 'mas-golang)
