(add-to-list 'after-init-hook
             (lambda ()
               (message (concat "emacs (" (number-to-string (emacs-pid)) ") started in " (emacs-init-time)))))

;; minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; lower threshold back to 8MB
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; my libs
(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'mas/base)

;; general
(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq column-number-indicator-zero-based nil)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 90)
(mas/maybe-disable-bars)
(blink-cursor-mode 0)
(show-paren-mode t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(set-face-attribute 'default nil :height 140)
(setq visible-bell t)
(global-set-key (kbd "M-r") 'mas/reload-config)
(setq inhibit-compacting-font-caches t)

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
(setq electric-pair-delete-adjacent-pairs nil)
(setq electric-pair-inhibit-predicate
      ;; Also inhibit if the pair would balanced
      (lambda (char)
        (or (electric-pair-conservative-inhibit char)
            (electric-pair-inhibit-if-helps-balance char))))

;; macOS specific
(when (string-equal system-type 'darwin)
  ;; swap meta and super
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (global-set-key (kbd "M-`") 'other-frame)

  ;; for all frames start with better position and dimensions and with no scrollbar
  (setq default-frame-alist
        '((top + 25) (left + 40)
          (width . 200) (height . 60)
          (vertical-scroll-bars . nil)))

  ;; bring initial frame to the foreground
  (select-frame-set-input-focus (selected-frame))

  ;; different path on my old intel mac
  (if (string-match "aarch" system-configuration)
      (setq ispell-program-name "/opt/homebrew/bin/ispell")
    (setq ispell-program-name "/usr/local/bin/ispell"))

  ;; on a mac the default visible bell is obnoxious
  (setq ring-bell-function 'mas/visible-bell))

;; save custom stuff elsewhere
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load-file custom-file))

;; icons for doom-modeline
;; run `nerd-icons-install-fonts` after
(use-package nerd-icons
  :ensure
  :defer t
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

;; selection goodness -- counsel/ivy
(use-package counsel
  :ensure
  :defer t
  :bind (("M-x" . 'counsel-M-x)
         ("C-x C-f" . 'counsel-find-file))
  :config
  (counsel-mode t))

;; install smex but don't use it -- gives history to counsel-M-x
(use-package smex
  :ensure
  :defer t)

;; best git UI ever
(use-package magit
  :ensure
  :bind ("C-c m" . magit-status)
  :config
  ;; magit should use the whole window unless purpose is handling it
  (setq magit-display-buffer-function
        (lambda (buffer)
          (interactive)
          (if (bound-and-true-p mas/purpose-active)
              (magit-display-buffer-traditional buffer)
            (magit-display-buffer-fullframe-status-v1 buffer)))))

;; markdown-mode
(use-package markdown-mode
  :ensure
  :defer t
  :custom
  (auto-mode-alist (cons '("\\.md" . gfm-mode) auto-mode-alist)))

;; json-mode
(use-package json-mode
  :ensure
  :defer t
  :custom
  ;; don't escape strings
  (json-reformat:pretty-string? t))

;; highlights all symbol occurences
(use-package highlight-symbol
  :ensure
  :defer t)

;; hints for keybindings
(use-package which-key
  :ensure
  :config
  (which-key-mode))

(use-package gptel
  :ensure
  :bind ("C-<return>" . gptel-send)
  :config
  (setq gptel-model "gpt-4o"))

;; load special configs
(load-file (expand-file-name "elisp/decorations.el" user-emacs-directory))
