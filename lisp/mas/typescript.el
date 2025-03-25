;;; -*- lexical-binding: t; -*-

;; https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config

(require 'mas/dev)
(require 'mas/lsp)

(mas/load-dev-mode 'tsx-ts-mode)
(mas/load-dev-mode 'typescript-ts-mode)
(mas/load-dev-mode 'js-ts-mode)

(setq typescript-ts-mode-indent-offset 4)

(use-package yaml-mode :ensure)

(provide 'mas/typescript)
