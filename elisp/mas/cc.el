(require 'mas/lsp)

;; I think this is a bug in c++-mode. You need to set the local buffer variable and the
;; global variable or things get messed up. Troubleshoot with M-x debug-watch
(setq c-basic-offset 4)
(setq-default c-basic-offset 4)
(mas/load-dev-mode 'c-mode)
(mas/load-dev-mode 'c++-mode)

(provide 'mas/cc)

;; $ cat > .clangd
;; CompileFlags:
;;   Add: [-std=c++11]
