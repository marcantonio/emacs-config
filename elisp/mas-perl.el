; cpanm Perl::LanguageServer

(setq perl-indent-parens-as-block t)
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))

(require 'setup-lsp)
(load-dev-mode 'perl-mode)
(lsp-deferred)

(provide 'mas-perl)
