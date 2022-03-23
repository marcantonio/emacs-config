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
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-proc-macro-enable t)
  (setq lsp-eldoc-render-all nil)
  (setq compilation-scroll-output t)) ;auto scroll compilation buffers

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
