;;; -*- lexical-binding: t; -*-

(require 'mas/dev)
(require 'mas/lsp)

;; https://blog.serghei.pl/posts/emacs-python-ide/
;; npm -g --prefix /home/mas/.emacs.d/.cache/lsp/npm/basedpyright install basedpyright
(use-package lsp-pyright
  :ensure
  :config
  (setq lsp-pyright-langserver-command "basedpyright")
  (setq lsp-pyright-venv-path "")
  (setq lsp-pyright-extra-paths [])
  (setq lsp-pyright-type-checking-mode "off"))

;; (setq flycheck-flake8rc "~/.emacs.d/flake8.cfg")

(mas/load-dev-mode 'python-ts-mode)

(provide 'mas/python)
