;;; -*- lexical-binding: t; -*-

(require 'mas/dev)
(require 'mas/lsp)

;; go install golang.org/x/tools/gopls@latest

(mas/load-dev-mode 'go-ts-mode)

;; (setq tramp-default-method "ssh")
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setq lsp-go-gopls-server-path "gopls")

(provide 'mas/golang)
