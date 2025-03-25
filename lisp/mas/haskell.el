;;; -*- lexical-binding: t; -*-

(require 'mas/dev)
(require 'mas/lsp)

(use-package lsp-haskell :ensure)
(mas/load-dev-mode 'haskell-mode)

(provide 'mas/haskell)
