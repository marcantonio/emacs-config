(require 'mas/dev)

(mas/load-dev-mode 'emacs-lisp-mode)

(global-set-key (kbd "C-x C-g") 'eval-buffer)
(setq debug-on-error t)

(provide 'mas/elisp)
