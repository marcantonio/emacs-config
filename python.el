;pip install "python-lsp-server[all]"
(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (flycheck-disable-checker 'python-pylint)
            (flycheck-select-checker 'python-flake8)))
(setq flycheck-flake8rc "~/.emacs.d/flake8.cfg")

(require 'mas-setup-lsp)
(load-dev-mode 'python-mode)
(lsp-deferred)
