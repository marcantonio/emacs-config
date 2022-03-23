(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  (lsp-idle-delay 0.7)
  (lsp-signature-doc-lines 1)
  (lsp-lens-enable nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (if (boundp 'tramp-remote-path)
      (progn
        (add-to-list 'tramp-remote-path "/home/mas/go/bin")
        (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "gopls")
                    :major-modes '(go-mode)
                    :remote? t
                    :server-id 'electron.soda.fm)))

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
         ("M->" . company-select-last))))

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
  (add-hook 'prog-mode-hook 'yas-minor-mode))
