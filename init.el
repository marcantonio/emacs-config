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
(setq-default fill-column 100)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode t)
(electric-pair-mode t)
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
(setq ispell-program-name "/usr/local/bin/ispell")

;; better M-x
(use-package smex
  :ensure
  :bind (("M-x" . smex)
         ("C-c C-c M-x" . execute-extended-command))) ; original M-x

;; macos specific
(when (string-equal system-type "darwin")
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (when window-system
    (tool-bar-mode 0)
    (set-frame-size (selected-frame) 200 60)))

;; best git UI ever
(global-set-key (kbd "C-c m") 'magit-status)
(use-package magit
  :ensure
  :bind (("C-c m" . magit-status)))

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

;; save custom stuff elsewhere
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; load special configs
(load-file (expand-file-name "decorations.el" user-emacs-directory))
(load-file (expand-file-name "rust.el" user-emacs-directory))
(load-file (expand-file-name "go.el" user-emacs-directory))
