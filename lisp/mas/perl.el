(require 'mas/dev)
(require 'mas/lsp)

;; cpanm Perl::LanguageServer
(setq perl-indent-parens-as-block t)
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))
(mas/load-dev-mode 'perl-mode)

(provide 'mas/perl)
