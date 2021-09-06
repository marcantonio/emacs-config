;; rust-mode + enhancements
;; much of this comes from https://github.com/rksm/emacs-rust-config
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  (setq rustic-format-on-save t))

;; for rust-analyzer
(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package lsp-treemacs
  :ensure
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1)
  (lsp-treemacs-symbols))

(use-package lsp-ivy
  :ensure
  :after (lsp-mode ivy))

;; inline errors
(use-package flycheck :ensure)

;; auto-complete and code snippets
(use-package company
  :ensure
  :bind ("M-RET". company-complete)
  (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("M-<" . company-select-first)
              ("M->" . company-select-last)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;; cargo toml
(use-package toml-mode :ensure)

;; for debugging
;; haven't palyed with this yet
;(use-package exec-path-from-shell
;  :ensure
;  :init (exec-path-from-shell-initialize))
;
;(when (executable-find "lldb-mi")
;  (use-package dap-mode
;    :ensure
;    :config
;    (dap-ui-mode)
;    (dap-ui-controls-mode 1)
;
;    (require 'dap-lldb)
;    (require 'dap-gdb-lldb)
;    ;; installs .extension/vscode
;    (dap-gdb-lldb-setup)
;    (dap-register-debug-template
;     "Rust::LLDB Run Configuration"
;     (list :type "lldb"
;           :request "launch"
;           :name "LLDB::Run"
;	   :gdbpath "rust-lldb"
;           ;; uncomment if lldb-mi is not in PATH
;           ;; :lldbmipath "path/to/lldb-mi"
;           ))))
