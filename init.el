;; general
(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 100)
(menu-bar-mode -1)
(blink-cursor-mode 0)
(show-paren-mode 1)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; use normal emacs regexes in builder
(require 're-builder)
(setq reb-re-syntax 'string)

;; whitespace
(require 'whitespace)
(setq-default show-trailing-whitespace t)
(set-face-attribute 'trailing-whitespace nil :underline t :background "black")
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ispell
(setq ispell-program-name "/usr/local/bin/ispell")

;; decorations
;pick colors with C-u C-x = over spot
(set-foreground-color "white")
(set-background-color "black")
(set-face-attribute 'default nil :height 140)
(set-face-attribute 'font-lock-comment-face nil :foreground "Firebrick")
(set-face-attribute 'font-lock-string-face nil :foreground "SpringGreen4")
(set-face-attribute 'font-lock-keyword-face nil :foreground "RoyalBlue")
(set-face-attribute 'font-lock-function-name-face nil :foreground "DarkOrchid")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "GoldenRod")
(set-face-attribute 'font-lock-type-face nil :foreground "DarkGoldenRod")
(setq font-lock-maximum-decoration t)

;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
(setq package-selected-packages
      (quote
       (paradox highlight-symbol neotree smex magit json-mode go-mode rust-mode markdown-mode)))

;; paradox
(setq paradox-execute-asynchronously nil)
(setq paradox-github-token nil)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;original M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; magit
(global-set-key (kbd "C-c m") 'magit-status)

;; go
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; markdown
(setq auto-mode-alist
      (cons '("\\.md" . gfm-mode) auto-mode-alist))

;; json
;no escaping strings
(setq json-reformat:pretty-string? t)

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; mac specific
(when (string-equal system-type "darwin")
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (when window-system
    (tool-bar-mode -1)
    (set-frame-size (selected-frame) 120 40)))

;; use magit diff colors for ediff -- better over tmux
(add-hook 'ediff-load-hook
          (lambda ()
            (set-face-foreground ediff-current-diff-face-A "#aa2222")
            (set-face-background ediff-current-diff-face-A "#eecccc")
            (set-face-background ediff-fine-diff-face-A "#ffbbbb")
            (set-face-foreground ediff-current-diff-face-B "#22aa22")
            (set-face-background ediff-current-diff-face-B "#cceecc")
            (set-face-background ediff-fine-diff-face-B "#aaffaa")
            (set-face-foreground ediff-even-diff-face-A "grey30")
            (set-face-background ediff-even-diff-face-A "grey80")
            (set-face-foreground ediff-even-diff-face-B "grey30")
            (set-face-background ediff-even-diff-face-B "grey80")
            (set-face-foreground ediff-odd-diff-face-A "grey50")
            (set-face-background ediff-odd-diff-face-A "grey95")
            (set-face-foreground ediff-odd-diff-face-B "grey50")
            (set-face-background ediff-odd-diff-face-B "grey95")))
