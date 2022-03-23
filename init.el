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
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

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

;; balance pairs
(electric-pair-mode t)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

;; macos specific
(when (string-equal system-type "darwin")
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (when window-system
    (set-frame-size (selected-frame) 200 60))
  (when (equal emacs-version "27.2")
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
  (set-face-attribute 'default nil :height 130))

;; save custom stuff elsewhere
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p (expand-file-name "custom.el" user-emacs-directory))
    (load-file (expand-file-name "custom.el" user-emacs-directory)))

;; icons for doom-modeline
(use-package all-the-icons
  :ensure
  :if (display-graphic-p))

;; modeline candy
;; run `all-the-icons-install-fonts` after
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
  :bind ("C-c m" . magit-status))

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

;; let's try treemacs again
;; (use-package treemacs
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :bind
;;   (:map global-map
;;         ("C-x t t" . treemacs)))

;; (use-package treemacs-magit
;;   :after (treemacs magit))

;; (use-package lsp-treemacs
;;   :config
;;   (lsp-treemacs-sync-mode t))

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

;; window purposes
(use-package window-purpose
  :ensure
  :bind
  (("C-c p r" . purpose-load-rust-dev)
   :map purpose-mode-map
   ("C-x b" . nil)
   ("C-x C-f" . nil))
  :config
  (defun purpose-load-rust-dev ()
    (interactive)
    (purpose-load-window-layout 'rust-dev)
    (rustic-cargo-test)
    (flycheck-list-errors)
    (winum-mode))
  (purpose-mode)
  (add-to-list 'purpose-user-mode-purposes '(rustic-mode . main))
  (add-to-list 'purpose-user-regexp-purposes '("^magit.*" . main))
  (add-to-list 'purpose-user-mode-purposes '(rustic-cargo-test-mode . cargo-run-test))
  (add-to-list 'purpose-user-mode-purposes '(rustic-cargo-run-mode . cargo-run-test))
  (add-to-list 'purpose-user-mode-purposes '(rustic-cargo-plain-run-mode . cargo-run-test))
  (add-to-list 'purpose-user-mode-purposes '(fundamental-mode . cargo-run-test))
  (add-to-list 'purpose-user-mode-purposes '(flycheck-error-list-mode . flycheck))
  (add-to-list 'purpose-user-mode-purposes '(xref--xref-buffer-mode . flycheck))
  (purpose-compile-user-configuration))

;; load special configs
(load-file (expand-file-name "decorations.el" user-emacs-directory))
(with-eval-after-load "rustic"
  (load-file (expand-file-name "lsp.el" user-emacs-directory))
  (load-file (expand-file-name "rust.el" user-emacs-directory)))
(with-eval-after-load "scheme-mode"
  (load-file (expand-file-name "scheme.el" user-emacs-directory)))
(with-eval-after-load "go-mode"
  (load-file (expand-file-name "lsp.el" user-emacs-directory))
  (load-file (expand-file-name "golang.el" user-emacs-directory)))

(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'light-mode)
