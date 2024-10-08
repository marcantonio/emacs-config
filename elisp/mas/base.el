(defun mas/reload-config ()
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun mas/c-x-c-c (&optional arg)
  "Allow C-u C-x C-c to shutdown the daemon"
  (interactive "P")
  (if arg
      (save-buffers-kill-emacs))
  (delete-frame))

(defun mas/visible-bell ()
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit
                                exit-minibuffer keyboard-quit))
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(defun mas/maybe-disable-bars ()
  (if (boundp 'menu-bar-mode)
      (menu-bar-mode 0))
  (if (boundp 'tool-bar-mode)
      (tool-bar-mode 0))
  (if (boundp 'scroll-bar-mode)
      (scroll-bar-mode 0)))

;; dev mode hooks
(defun mas/config-cxx-mode ()
  (require 'mas/lsp)
  (require 'mas/cc)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (lsp-deferred))

(defun mas/config-rustic-mode ()
  (require 'mas/lsp)
  (require 'mas/rust)
  (lsp-deferred))

(defun mas/config-python-mode ()
  (require 'mas/lsp)
  (require 'mas/python)
  (lsp-deferred))

(defun mas/config-go-mode ()
  (require 'mas/lsp)
  (require 'mas/golang)
  (lsp-deferred))

(defun mas/config-typescript-mode ()
  (require 'mas/lsp)
  (require 'mas/typescript)
  (lsp-deferred))

(defun mas/config-haskell-mode ()
  (require 'mas/lsp)
  (require 'mas/haskell)
  (lsp-deferred))

(defun mas/config-perl-mode ()
  (require 'mas/lsp)
  (require 'mas/perl)
  (lsp-deferred))

(add-hook 'c-mode-hook 'mas/config-cxx-mode)
(add-hook 'c++-mode-hook 'mas/config-cxx-mode)
(add-hook 'go-ts-mode-hook 'mas/config-go-mode)
(add-hook 'haskell-mode-hook 'mas/config-haskell-mode)
(add-hook 'js-ts-mode-hook 'mas/config-typescript-mode)
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook 'mas/config-perl-mode)
(add-hook 'python-mode-hook 'mas/config-python-mode)
(add-hook 'rustic-mode-hook 'mas/config-rustic-mode)
(add-hook 'tsx-ts-mode-hook 'mas/config-typescript-mode)
(add-hook 'typescript-ts-mode-hook 'mas/config-typescript-mode)

(add-to-list 'auto-mode-alist '("\\.go" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts" . typescript-ts-mode))

(provide 'mas/base)
