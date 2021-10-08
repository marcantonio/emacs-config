;; go-mode
(use-package go-mode
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  :custom
  (gofmt-command "goimports"))
