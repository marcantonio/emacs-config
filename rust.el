;; rust-mode + enhancements
;; much of this comes from https://github.com/rksm/emacs-rust-config
;;
;; great ref for lsp ui elements:
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c C-e" . rustic-run-shell-command)
              ("C-c C-c C-y" . rustic-cargo-test-rerun))
  :config
  (setq lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer"))
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-proc-macro-enable t))

;; cargo toml
(use-package toml-mode :ensure)

;; for debugging
;; (defun find-rust-exec ()
;;   (let ((exec-name (f-base (lsp-workspace-root))))
;;     (concat "target/debug/" exec-name)))

;; (use-package dap-mode
;;   :ensure
;;   :defer
;;   :config
;;   (require 'dap-cpptools)
;;   (dap-auto-configure-mode t)
;;   ;;(setq dap-default-terminal-kind "integrated")
;;   (dap-cpptools-setup)
;;   (dap-register-debug-template "Rust::CppTools Run Configuration"
;;                                (list :type "cppdbg"
;;                                      :request "launch"
;;                                      :name "Rust::Run"
;;                                      :MIMode "gdb"
;;                                      :miDebuggerPath "rust-gdb"
;;                                      :environment []
;;                                      :cwd "${workspaceFolder}"
;;                                      :program (find-rust-exec)
;;                                      :dap-compilation "cargo build"
;;                                      :dap-compilation-dir "${workspaceFolder}")))
;; (use-package dap-mode
;;   :ensure
;;   :defer
;;   :config
;;   (dap-ui-mode)
;;   ;;(dap-auto-configure-mode t)
;;   (require 'dap-lldb)
;;   (require 'dap-gdb-lldb)
;;   (setq dap-lldb-debug-program '("/usr/lib/llvm-14/bin/lldb-vscode"))
;;   ;; (setq dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug.")))
;;   (setq dap-lldb-debugged-program-function (lambda () (find-rust-exec)))
;;   ;; installs .extension/vscode
;;   (dap-gdb-lldb-setup)
;;   (dap-register-debug-template
;;    "Rust::LLDB Run Configuration"
;;    (list :type "lldb-vscode"
;;          :request "launch"
;;          :name "LLDB::Run"
;;          :cwd (lsp-workspace-root)
;;          :args nil)));:gdbpath "rust-lldb")))
