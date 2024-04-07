;; go-mode

(use-package go-mode
  :ensure
  :config
  ;(add-hook 'before-save-hook 'gofmt-before-save)
  ;(add-hook 'go-mode-hook #'lsp-deferred)
  :custom
  (gofmt-command "goimports"))

(require 'mas-lsp)
(load-dev-mode 'go-mode)
(lsp-deferred)

(provide 'mas-golang)
