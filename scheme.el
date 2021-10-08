(use-package geiser)

(use-package geiser-mit
  :after (geiser)
  :custom
  (geiser-active-implementations '(mit))
  (geiser-mit-binary "/usr/local/bin/mit-scheme"))
