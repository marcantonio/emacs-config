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

(defun mas/config-ts-mode ()
  (require 'mas/lsp)
  (require 'mas/ts)
  (lsp-deferred))

(defun mas/config-haskell-mode ()
  (require 'mas/lsp)
  (require 'mas/haskell)
  (lsp-deferred))

(defun mas/config-perl-mode ()
  (require 'mas/lsp)
  (require 'mas/perl)
  (lsp-deferred))

(provide 'mas/base)
