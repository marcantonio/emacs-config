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
    (set-frame-size (selected-frame) 200 60)))

;; save custom stuff elsewhere
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p (expand-file-name "custom.el" user-emacs-directory))
    (load-file (expand-file-name "custom.el" user-emacs-directory)))

;; hide minor modes from modeline
(use-package diminish :ensure)

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
  :diminish
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
  :diminish
  :config
  (which-key-mode))

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
  :diminish
  :bind
  (("C-c p r" . purpose-load-rust-dev)
   :map purpose-mode-map
   ("C-x b" . nil)
   ("C-x C-f" . nil))
  :config
  (defun purpose-load-rust-dev ()
    (interactive)
    (purpose-load-window-layout 'rust-dev1))
  (purpose-mode)
  (add-to-list 'purpose-user-mode-purposes '(rustic-mode . main))
  (add-to-list 'purpose-user-mode-purposes '(magit-status-mode . main))
  (add-to-list 'purpose-user-mode-purposes '(rustic-cargo-test-mode . cargo-run-test))
  (add-to-list 'purpose-user-mode-purposes '(rustic-cargo-run-mode . cargo-run-test))
  (add-to-list 'purpose-user-mode-purposes '(flycheck-error-list-mode . flycheck))
  (purpose-compile-user-configuration))

;; load special configs
; todo: do this conditionally
(load-file (expand-file-name "decorations.el" user-emacs-directory))
(load-file (expand-file-name "rust.el" user-emacs-directory))
(load-file (expand-file-name "golang.el" user-emacs-directory))
(load-file (expand-file-name "scheme.el" user-emacs-directory))
