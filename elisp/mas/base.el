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
(defun mas/cxx-mode ()
  (require 'mas/cc)
  (lsp-deferred))

(defun mas/elisp-mode ()
  (require 'mas/elisp))

(defun mas/go-mode ()
  (require 'mas/golang)
  (lsp-deferred))

(defun mas/haskell-mode ()
  (require 'mas/haskell)
  (lsp-deferred))

(defun mas/perl-mode ()
  (require 'mas/perl)
  (lsp-deferred))

(defun mas/python-mode ()
  (require 'mas/python)
  (lsp-deferred))

(defun mas/rust-mode ()
  (require 'mas/rust)
  (lsp-deferred))

(defun mas/typescript-mode ()
  (require 'mas/typescript)
  (lsp-deferred))

;; load this early so the hooks below work right
(use-package treesit-auto
  :ensure
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(add-hook 'c-mode-hook 'mas/cxx-mode)
(add-hook 'c++-mode-hook 'mas/cxx-mode)
(add-hook 'emacs-lisp-mode-hook 'mas/elisp-mode)
(add-hook 'go-ts-mode-hook 'mas/go-mode)
(add-hook 'haskell-mode-hook 'mas/haskell-mode)
(add-hook 'js-ts-mode-hook 'mas/typescript-mode)
(add-hook 'cperl-mode-hook 'mas/perl-mode)
(defalias 'perl-mode 'cperl-mode)
(add-hook 'python-ts-mode-hook 'mas/python-mode)
(add-hook 'rust-ts-mode-hook 'mas/rust-mode)
(add-hook 'tsx-ts-mode-hook 'mas/typescript-mode)
(add-hook 'typescript-ts-mode-hook 'mas/typescript-mode)

(provide 'mas/base)
