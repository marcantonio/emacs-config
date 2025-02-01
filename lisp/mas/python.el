(require 'mas/dev)
(require 'mas/lsp)

;;(require 'tramp)

;; this makes everything work...
;; (setq lsp-log-io t)
;; (setq tramp-default-method "ssh")
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;; (add-to-list 'tramp-remote-path "/home/mas/.pyenv/versions/index/bin")
;; (setq lsp-pylsp-server-command "pylsp")
;; (setq lsp-pylsp-plugins-pydocstyle-ignore ["D100" "D101" "D102" "D103" "D107"])
;; (setq lsp-pylsp-plugins-flake8-ignore ["E302" "E501"])

;; pip install "python-lsp-server[all]"
(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (flycheck-disable-checker 'python-pylint)
            ;;(flycheck-select-checker 'python-flake8)))
            (flycheck-disable-checker 'python-flake8)))
(setq flycheck-flake8rc "~/.emacs.d/flake8.cfg")
(mas/load-dev-mode 'python-ts-mode)

(provide 'mas/python)
