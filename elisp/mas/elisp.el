(require 'mas/dev)

(mas/load-dev-mode 'emacs-lisp-mode)
(use-package paredit
  :ensure
  :hook
  ((emacs-lisp-mode . enable-paredit-mode)))

(global-set-key (kbd "C-x C-g") 'eval-buffer)
(setq debug-on-error t)

(provide 'mas/elisp)
