(use-package lsp-haskell :ensure)

(require 'mas-lsp)
(load-dev-mode 'haskell-mode)
(lsp-deferred)

(provide 'mas-haskell)
