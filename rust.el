;; rust-mode + enhancements
;; much of this comes from https://github.com/rksm/emacs-rust-config
;;
;; great ref for lsp ui elements:
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

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
              ("C-c C-c h" . lsp-ui-doc-glance)
              ("C-c C-c C-e" . rustic-run-shell-command)
              ("C-c C-c C-y" . rustic-cargo-test-rerun)
              ("C-c C-c C-j" . flycheck-next-error)
              ("C-c C-c C-k" . flycheck-previous-error))
  :config
  (setq lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer"))
  (setq lsp-eldoc-render-all nil)
  (setq compilation-scroll-output t)) ;auto scroll compilation buffers

;; lsp for rust-analyzer
(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-idle-delay 0.7)
  (lsp-signature-render-documentation nil)
  (lsp-lens-enable nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-rust-analyzer-proc-macro-enable t))

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

;; inline errors
(use-package flycheck
  :ensure
  :defer t)

;; auto-complete
(use-package company
  :ensure
  :bind
  (("M-RET". company-complete)
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
