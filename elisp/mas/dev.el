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

;; todo: https://robbmann.io/posts/emacs-treesit-auto/
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

(provide 'mas/dev)
