;; When not using an old version of python, try directives `like
;; lsp-pylsp-plugins-flake8-exclude` instead:
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/#lsp-pylsp-plugins-flake8-config

;(setq lsp-pylsp-server-command "pyls")
(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (flycheck-disable-checker 'python-pylint)
            (flycheck-select-checker 'python-flake8)))
(setq flycheck-flake8rc "~/.emacs.d/flake8.cfg")

(require 'setup-lsp)
(load-dev-mode 'python-mode)
(lsp-deferred)
