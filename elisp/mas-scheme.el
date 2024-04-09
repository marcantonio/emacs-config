(use-package geiser
  :mode ("\\.scm\\'" . scheme-mode))

(use-package geiser-mit
  :after (geiser)
  :mode ("\\.scm\\'" . scheme-mode)
  :config
  (setq geiser-active-implementations '(mit))
  (setq geiser-mit-binary "/usr/bin/mit-scheme"))
