(use-package geiser :ensure)

(use-package geiser-mit
  :ensure
  :after (geiser)
  :custom
  (geiser-active-implementations '(mit))
  (geiser-mit-binary "/usr/bin/mit-scheme"))
