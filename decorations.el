;; decorations
; hint: pick colors with C-u C-x = over spot
;(set-foreground-color "white")
;(set-background-color "black")
;(set-face-attribute 'default nil :height 140)
;(set-face-attribute 'font-lock-comment-face nil :foreground "Firebrick")
;(set-face-attribute 'font-lock-string-face nil :foreground "SpringGreen4")
;(set-face-attribute 'font-lock-keyword-face nil :foreground "RoyalBlue")
;(set-face-attribute 'font-lock-function-name-face nil :foreground "DarkOrchid")
;(set-face-attribute 'font-lock-variable-name-face nil :foreground "GoldenRod")
;(set-face-attribute 'font-lock-type-face nil :foreground "DarkGoldenRod")
;(setq font-lock-maximum-decoration t)

(load-theme 'leuven)
(set-cursor-color "#335ea8")

;; slightly bigger font
(set-face-attribute 'default nil :height 130)

;; use magit diff colors for ediff -- better over tmux
(add-hook 'ediff-load-hook
          (lambda ()
            (set-face-foreground ediff-current-diff-face-A "#aa2222")
            (set-face-background ediff-current-diff-face-A "#eecccc")
            (set-face-background ediff-fine-diff-face-A "#ffbbbb")
            (set-face-foreground ediff-current-diff-face-B "#22aa22")
            (set-face-background ediff-current-diff-face-B "#cceecc")
            (set-face-background ediff-fine-diff-face-B "#aaffaa")
            (set-face-foreground ediff-even-diff-face-A "grey30")
            (set-face-background ediff-even-diff-face-A "grey80")
            (set-face-foreground ediff-even-diff-face-B "grey30")
            (set-face-background ediff-even-diff-face-B "grey80")
            (set-face-foreground ediff-odd-diff-face-A "grey50")
            (set-face-background ediff-odd-diff-face-A "grey95")
            (set-face-foreground ediff-odd-diff-face-B "grey50")
            (set-face-background ediff-odd-diff-face-B "grey95")))
