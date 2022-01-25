;;;; -*- lexical-binding: t; -*-

(setup (:pkg ace-window)
  (setq aw-background t)
  (setq aw-scope 'frame)
  (ace-window-display-mode 1)
  (:hide-mode))

(setup (:pkg popper)
  (:global-bind
   "<M-tab>" popper-toggle-latest
   "<M-SPC>" popper-cycle)
  (setq popper-display-control nil)
  (setq popper-group-function nil)
  (setq popper-mode-line nil)
  (setq popper-reference-buffers
        '(help-mode
          helpful-mode
          sly-mrepl-mode
          term-mode))
  (popper-mode 1))

(setup (:pkg shackle)
  (setq shackle-rules
        `((compilation-mode
           :select t :align right :size 0.33)
          (magit-status-mode
           :select t :align right :size 0.5)
          ((help-mode helpful-mode)
           :select t :align right :size 0.4)
          ((sly-xref-mode "\\*\\(sly-mrepl\\|ielm\\)")
           :regexp t :noselect t :align below :size 0.24)))
  (shackle-mode 1))

(setup (:pkg windmove)
  ;; (windmove-default-keybindings)
  )

(setup (:pkg winner)
  (winner-mode 1))

(provide 'my-pkg-window)
