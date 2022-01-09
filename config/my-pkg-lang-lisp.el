;;;; -*- lexical-binding: t; -*-

(setup (:pkg evil-cleverparens)
  (:disable)
  (setq evil-cleverparens-swap-move-by-word-and-symbol t
        evil-cleverparens-use-additional-bindings nil
        evil-cleverparens-use-additional-movement-keys nil
        evil-cleverparens-use-regular-insert t)
  (:load-after (evil smartparens evil-smartparens)
    (:hook-into emacs-lisp-mode
                eval-expression-minibuffer-setup
                ielm-mode
                lisp-interaction-mode
                lisp-mode
                sly-mrepl-mode)
    (:hide-mode)))

(setup (:pkg evil-smartparens)
  (:disable)
  (:load-after (evil smartparens)
    (:hook-into smartparens-enabled-hook)
    (:hide-mode)))

(setup (:pkg rainbow-delimiters)
  (setq rainbow-delimiters-max-face-count 8)
  (:when-loaded
    (set-face-foreground 'rainbow-delimiters-depth-1-face "dark orange")
    (set-face-foreground 'rainbow-delimiters-depth-2-face "deep pink")
    (set-face-foreground 'rainbow-delimiters-depth-3-face "chartreuse")
    (set-face-foreground 'rainbow-delimiters-depth-4-face "deep sky blue")
    (set-face-foreground 'rainbow-delimiters-depth-5-face "yellow")
    (set-face-foreground 'rainbow-delimiters-depth-6-face "orchid")
    (set-face-foreground 'rainbow-delimiters-depth-7-face "spring green")
    (set-face-foreground 'rainbow-delimiters-depth-8-face "sienna1"))
  (:hook-into emacs-lisp-mode-hook
              eval-expression-minibuffer-setup-hook
              ielm-mode-hook
              lisp-interaction-mode-hook
              lisp-mode-hook
              sly-mrepl-mode-hook)
  (:hide-mode))

(setup (:pkg smartparens)
  (:require smartparens)
  (setq sp-cancel-autoskip-on-backward-movement nil
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil
        sp-max-pair-length 2
        sp-max-prefix-length 32
        sp-message-width nil
        sp-navigate-consider-sgml-tags nil
        sp-navigate-skip-match nil
        sp-show-pair-from-inside t
        blink-matching-paren 'jump-offscreen
        show-paren-when-point-inside-paren t)
  (set-face-attribute 'show-paren-match nil :background nil :weight 'ultra-bold)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (sp-pair "(" nil :unless '(:rem sp-point-before-word-p))
  (:with-hook (emacs-lisp-mode-hook
               eval-expression-minibuffer-setup-hook
               ielm-mode-hook
               lisp-interaction-mode-hook
               lisp-mode-hook
               sly-mrepl-mode-hook)
    (:hook smartparens-strict-mode))
  (:hide-mode))

(define-local-keys (emacs-lisp-mode-map sly-mrepl-mode-map lisp-mode-map)
  :infix "l"
  "" '(:ignore t :wk "lisp")
  "a" '(sp-absorb-sexp :wk "absorb")
  "b" '(sp-forward-barf-sexp :wk "barf forward")
  "B" '(sp-backward-barf-sexp :wk "barf backward")
  "c" '(sp-convolute-sexp :wk "convolute")
  "d" '(sp-kill-sexp :wk "kill sexp")
  "e" '(sp-splice-sexp-killing-backward :wk "splice killing backward")
  "E" '(sp-splice-sexp-killing-forward :wk "splice killing forward")
  "j" '(sp-join-sexp :wk "join")
  "r" '(sp-raise-sexp :wk "raise")
  "s" '(sp-forward-slurp-sexp :wk "slurp forward")
  "S" '(sp-backward-slurp-sexp :wk "slurp backward")
  "t" '(sp-transpose-sexp :wk "transpose")
  "w" '(sp-wrap-round :wk "wrap")
  "W" '(sp-unwrap-sexp :wk "unwrap")
  "y" '(sp-copy-sexp :wk "yank"))

(define-local-keys (lisp-mode-map emacs-lisp-mode-map)
  "m" '(macrostep-expand :wk "macro expand"))

(provide 'my-pkg-lang-lisp)
