;;; -*- lexical-binding: t; -*-

(require 'mas/dev)
(require 'mas/lsp)

;; https://blog.serghei.pl/posts/emacs-python-ide/
;; npm -g --prefix /home/mas/.emacs.d/.cache/lsp/npm/basedpyright install basedpyright
(use-package lsp-pyright
  :ensure
  :config
  (setq lsp-pyright-langserver-command "basedpyright")
  (setq lsp-pyright-venv-path "/opt/pyenv/versions/3.12.7")
  (setq lsp-pyright-extra-paths ["/usr/local/ddg-index/lib"])
  (setq lsp-pyright-type-checking-mode "off"))

;;(require 'tramp)
;; this makes everything work...
;; (setq lsp-log-io t)
;; (setq tramp-default-method "ssh")
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;; (add-to-list 'tramp-remote-path "/home/mas/.pyenv/versions/index/bin")

;; pip install "python-lsp-server[all]"
;; (add-hook 'lsp-managed-mode-hook
;;           (lambda ()
;;             (flycheck-disable-checker 'python-pylint)
;;             ;;(flycheck-select-checker 'python-flake8)))
;;             (flycheck-disable-checker 'python-flake8)))
;; (setq flycheck-flake8rc "~/.emacs.d/flake8.cfg")

(mas/load-dev-mode 'python-ts-mode)

(provide 'mas/python)
