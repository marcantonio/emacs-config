;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; general
(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq column-number-indicator-zero-based nil)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 90)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(set-face-attribute 'default nil :height 140)
(setq visible-bell t)

;; use normal emacs regexes in builder
(require 're-builder)
(setq reb-re-syntax 'string)

;; highlight and remove trailing whitespace
(require 'whitespace)
(setq-default show-trailing-whitespace t)
(set-face-attribute 'trailing-whitespace nil :underline t :background "black")
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ispell
(setq ispell-program-name "/usr/bin/ispell")
(setq ispell-dictionary "american")

;; balance pairs
(electric-pair-mode t)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

;; macos specific
(when (string-equal system-type "darwin")
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)

  ;; start in a reasonable position
  (setq default-frame-alist
        '((top + 25) (left + 40)))

  ;; with reasonable dimensions
  (when window-system
    (set-frame-size (selected-frame) 200 60))

  ;; bring initial frame to the foreground
  (select-frame-set-input-focus (selected-frame))
  (setq ispell-program-name "/opt/homebrew/bin/ispell")

  ;; on a mac the default visible bell is obnoxious
  (setq ring-bell-function
        (lambda ()
          (unless (memq this-command
                        '(isearch-abort abort-recursive-edit
                                        exit-minibuffer keyboard-quit))
            (invert-face 'mode-line)
            (run-with-timer 0.1 nil 'invert-face 'mode-line))))

  (when (equal emacs-version "27.2")
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))

;; save custom stuff elsewhere
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p (expand-file-name "custom.el" user-emacs-directory))
    (load-file (expand-file-name "custom.el" user-emacs-directory)))

;; icons for doom-modeline
;; run `nerd-icons-install-fonts` after
(use-package nerd-icons
  :ensure
  :if (display-graphic-p))

;; modeline candy
(use-package doom-modeline
  :ensure
  :config
  (doom-modeline-mode)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-version nil))

;; menu for minor modes
(use-package minions
  :ensure
  :after (doom-modeline)
  :config
  (setq doom-modeline-minor-modes t)
  (minions-mode))

;; better package interface
(use-package paradox
  :ensure
  :config
  (paradox-enable)
  (setq paradox-execute-asynchronously nil)
  (setq paradox-github-token t))

;; selection goodness -- counsel/ivy
(use-package counsel
  :ensure
  :config
  (counsel-mode t))

;; install smex but don't use it -- gives history to counsel-M-x
(use-package smex :ensure)

;; best git UI ever
(use-package magit
  :ensure
  :bind ("C-c m" . magit-status)
  :config
  ;; magit should use the whole window unless purpose is handling it
  (setq magit-display-buffer-function
        (lambda (buffer)
          (interactive)
          (if (bound-and-true-p mas-purpose-active)
              (magit-display-buffer-traditional buffer)
            (magit-display-buffer-fullframe-status-v1 buffer)))))

;; markdown-mode
(use-package markdown-mode
  :ensure
  :custom
  (auto-mode-alist (cons '("\\.md" . gfm-mode) auto-mode-alist)))

;; json-mode
(use-package json-mode
  :ensure
  :custom
  (json-reformat:pretty-string? t)) ; don't escaping strings

;; highlights all symbol occurences
(use-package highlight-symbol :ensure)

;; hints for keybindings
(use-package which-key
  :ensure
  :config
  (which-key-mode))

;; select windows easier
(use-package winum
  :ensure
  :bind
  (:map winum-keymap
        ("M-1" . winum-select-window-1)
        ("M-2" . winum-select-window-2)
        ("M-3" . winum-select-window-3))
  :config
  (setq winum-auto-setup-mode-line nil)
  (winum-mode))

;; project interaction -- I don't know if this is useful yet
;; todo: look at counsel-projectile-mode
;; (use-package projectile
;;   :ensure
;;   :bind-keymap ("C-c p" . projectile-command-map)
;;   :config
;;   (projectile-mode t))

;; load special configs
(load-file (expand-file-name "decorations.el" user-emacs-directory))

(defun config-rustic-mode ()
  (load-file (expand-file-name "lsp.el" user-emacs-directory))
  (load-file (expand-file-name "rust.el" user-emacs-directory)))
(add-hook 'rustic-mode-hook 'config-rustic-mode)

(defun config-cxx-mode ()
  (load-file (expand-file-name "lsp.el" user-emacs-directory))
  (load-file (expand-file-name "cc.el" user-emacs-directory))
  (lsp))
(add-hook 'c-mode-hook 'config-cxx-mode)
(add-hook 'c++-mode-hook 'config-cxx-mode)

(defun config-haskell-mode ()
  (load-file (expand-file-name "lsp.el" user-emacs-directory))
  (load-file (expand-file-name "haskell.el" user-emacs-directory)))
(add-hook 'haskell-mode-hook 'config-haskell-mode)

(defun config-python-mode ()
  (load-file (expand-file-name "lsp.el" user-emacs-directory))
  (load-file (expand-file-name "python.el" user-emacs-directory)))
(add-hook 'python-mode-hook 'config-python-mode)

(defun config-go-mode ()
  (load-file (expand-file-name "lsp.el" user-emacs-directory))
  (load-file (expand-file-name "golang.el" user-emacs-directory)))
(add-hook 'go-mode-hook 'config-go-mode)

(defun config-perl-mode ()
  (load-file (expand-file-name "lsp.el" user-emacs-directory))
  (load-file (expand-file-name "perl.el" user-emacs-directory)))
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook 'config-perl-mode)

(with-eval-after-load "scheme-mode"
  (load-file (expand-file-name "scheme.el" user-emacs-directory)))

(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'light-mode)
