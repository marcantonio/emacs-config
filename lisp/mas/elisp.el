;;; -*- lexical-binding: t; -*-

;; Call this early to prevent a recursive require loop when used with
;; `emacs-lisp-mode-hook'. This will happen when a package is loaded on start via
;; `use-package' because `loaddefs-generate' loads `emacs-lisp-mode' which then tries to
;; load this again, etc. Must be a better way...
(provide 'mas/elisp)

(require 'mas/dev)

(use-package paredit
  :ensure
  :hook
  ((emacs-lisp-mode . enable-paredit-mode)))

(global-set-key (kbd "C-x C-g") 'eval-buffer)

(mas/load-dev-mode 'emacs-lisp-mode)
