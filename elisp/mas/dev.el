;; window purposes
(use-package window-purpose
  :ensure
  :bind (("C-c p c" . purpose-load-dev)
         :map purpose-mode-map
         ("C-x b" . nil)
         ("C-x C-f" . nil))
  :config
  (defun load-treemacs-symbols ()
    (let ((main-win (selected-window)))
      (setq treemacs-position 'right)
      (setq lsp-treemacs-symbols-position-params
            `((side . right)
              (slot . 2)
              (window-width . ,treemacs-width)))
      ;;(treemacs)
      ;;(lsp-treemacs-symbols)
      (select-window main-win)))
  (defun purpose-load-dev ()
    (interactive)
    (purpose-load-window-layout 'dev)
    (load-treemacs-symbols)
    (flycheck-list-errors)
    (setq mas/purpose-active t))
  (purpose-mode)
  (add-to-list 'purpose-user-regexp-purposes '("^magit.*" . main))
  (add-to-list 'purpose-user-mode-purposes '(flycheck-error-list-mode . flycheck))
  (add-to-list 'purpose-user-mode-purposes '(xref--xref-buffer-mode . flycheck))
  (purpose-compile-user-configuration))

;; load a dev mode by ensuring the main window selects the right major mode
(defun mas/load-dev-mode (mode)
  (require 'window-purpose)
  (add-to-list 'purpose-user-mode-purposes `(,mode . main))
  (purpose-compile-user-configuration))

;; code formatting for typescript, python, and go
(use-package apheleia
  :ensure
  :config
  (apheleia-global-mode))

;; still not sure about this
(use-package projectile
  :ensure
  :bind (:map projectile-mode-map
              ("C-c C-c p" . projectile-command-map))
  :config
  (projectile-mode))

;; auto-complete
(use-package company
  :ensure
  :bind (("M-RET" . company-complete)
         (:map company-active-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               ("M-<" . company-select-first)
               ("M->" . company-select-last)))
  :config
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 2)
  (company-mode))

;; code snippets
(use-package yasnippet
  :ensure
  :demand
  :bind (:map yas-minor-mode-map
              ("M-'" . yas-expand)
              ([(tab)] . nil)
              ("TAB" . nil))
  :config
  (yas-reload-all)
  (yas-global-mode))

(use-package treemacs
  :ensure
  :bind (:map global-map
              ("M-0" . treemacs-select-window)
              ("C-x t t" . treemacs)))

(use-package treemacs-projectile
  :ensure
  :after (treemacs projectile))

;; code folding
(use-package treesit-fold
  :after treesit
  :load-path "~/.emacs.d/elisp/vendor/treesit-fold"
  :config
  (global-treesit-fold-mode))

(use-package treesit-fold-indicators
  :after (treesit treesit-fold)
  :load-path "~/.emacs.d/elisp/vendor/treesit-fold"
  :config
  (global-treesit-fold-indicators-mode))

(provide 'mas/dev)
