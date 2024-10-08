;; common lsp-mode config

(use-package lsp-mode
  :ensure
  :commands lsp
  :bind (:map lsp-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-." . lsp-ui-peek-find-definitions)
              ("M-?" . lsp-ui-peek-find-references)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c f" . treesit-fold-toggle)
              ("C-c C-c h" . lsp-ui-doc-show)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c z" . lsp-ui-doc-focus-frame)
              ("C-c C-c C-j" . flycheck-next-error)
              ("C-c C-c C-k" . flycheck-previous-error))
  :custom
  (lsp-idle-delay 0.1)
  (lsp-signature-render-documentation nil)
  (lsp-lens-enable nil)
  :config
  (setq lsp-eldoc-render-all nil)
  (setq gc-cons-threshold (* 100 1024 1024))
  (setq read-process-output-max (* 3 1024 1024))
  (setq undo-limit (* 2 1024 1024))
  (setq undo-strong-limit (* 2 1024 1024))
  (setq undo-outer-limit (* 24 1024 1024))
  (setq compilation-scroll-output t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-sideline-show-code-actions t)
  ;(lsp-ui-sideline-diagnostic-max-lines 20)
  (lsp-ui-sideline-show-hover t))

(use-package lsp-ivy
  :ensure
  :defer t
  :after (lsp-mode ivy))

;; inline errors
(use-package flycheck
  :ensure
  :defer t)

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
  (setq company-minimum-prefix-length 2))

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
              ("C-x t t" . treemacs)))

(use-package lsp-treemacs
  :ensure
  :config
  (setq lsp-treemacs-error-list-expand-depth t)
  (lsp-treemacs-sync-mode t))

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
      (setq lsp-treemacs-symbols-position-params
            `((side . right)
              (slot . 2)
              (window-width . ,treemacs-width)))
      (lsp-treemacs-symbols)
      (define-key global-map (kbd "M-0") #'lsp-treemacs-symbols)
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

(defun mas/load-dev-mode (mode)
  (require 'window-purpose)
  (add-to-list 'purpose-user-mode-purposes `(,mode . main))
  (purpose-compile-user-configuration))

;; https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
;; todo: https://robbmann.io/posts/emacs-treesit-auto/
;; todo: lsp -> dev, and move this there
(use-package treesit
  :mode (("\\.cjs\\'" . typescript-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.go\\'" . go-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
      :preface
      (defun mas/setup-install-grammars ()
        "Install Tree-sitter grammars if they are absent."
        (interactive)
        (dolist (grammar
                 '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                   (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                   (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.23.1"))
                   (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0"))
                   (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                   (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                   (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                   (prisma "https://github.com/victorhqc/tree-sitter-prisma")
                   (toml "https://github.com/tree-sitter/tree-sitter-toml")
                   (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                   (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                   (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
          (add-to-list 'treesit-language-source-alist grammar)
          (unless (treesit-language-available-p (car grammar))
            (treesit-install-language-grammar (car grammar)))))

      (dolist (mapping
               '((bash-mode . bash-ts-mode)
                 (go-mode . go-ts-mode)
                 (js-mode . typescript-ts-mode)
                 (js-json-mode . json-ts-mode)
                 (js2-mode . typescript-ts-mode)
                 (json-mode . json-ts-mode)
                 (sh-base-mode . bash-ts-mode)
                 (sh-mode . bash-ts-mode)
                 (typescript-mode . typescript-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))
      :config
      (mas/setup-install-grammars))

;; Code folding
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

(provide 'mas/lsp)
