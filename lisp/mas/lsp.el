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
  (lsp-idle-delay 0.5)
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

(use-package lsp-treemacs
  :ensure
  :config
  (setq lsp-treemacs-error-list-expand-depth t)
  (lsp-treemacs-sync-mode t))

;; lsp-booster stuff
;;
;; cd lisp/vendor/emacs-lsp-booster
;; cargo build --release
;; ln -s `pwd`/target/release/emacs-lsp-booster ~/.local/bin

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(provide 'mas/lsp)
