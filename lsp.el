;; common lsp-mode config

(use-package lsp-mode
  :ensure
  :commands lsp
  :bind (:map lsp-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance)
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
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-sideline-show-code-actions t)
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
  :bind
  (("M-RET" . company-complete)
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
  :bind
  (:map yas-minor-mode-map
        ("C-'". yas-expand)
        ([(tab)] . nil)
        ("TAB" . nil))
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (yas-minor-mode))
