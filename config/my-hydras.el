;;;; -*- lexical-binding: t; -*-

(setup my/hydras
  (:disable)
  (:require hydra)

  (defhydra my/hydra-orgtodos (:hint nil)
    "
Sexps (quit with _q_)
^Nav^              ^Barf/Slurp^             ^Depth^
^---^--------------^----------^-------------^-----^-----------------
_l_: forward       _C-l_: slurp forward     _R_: splice
_h_: backward      _C-L_: barf forward      _r_: raise
_H_: backward ↑    _C-h_: slurp backward    _C-j_: raise backward
_L_: forward ↓     _C-H_: barf backward     _C-k_: raise forward
_j_: backward ↓
_k_: forward ↑
^Misc 1^           ^Misc 2^                 ^Wrap^
^------^-----------^------^-----------------^----^------------------
_y_: copy          _M-j_: join              _(_: wrap with ( )
_D_: kill          _M-s_: split             _{_: wrap with { }
_u_: undo          _M-t_: transpose         _'_: wrap with ' '
_C-r_: redo        _M-c_: convolute         _\"_: wrap with \" \"
^^                 _i_: indent defun        _W_: unwrap"
    ("q" nil)
    ;; Wrapping
    ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
    ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
    ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
    ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
    ("W" (lambda (_) (interactive "P") (sp-unwrap-sexp)))
    ;; Navigation
    ("l" sp-forward-sexp )
    ("h" sp-backward-sexp)
    ("H" sp-backward-up-sexp)
    ("L" sp-down-sexp)
    ("j" sp-backward-down-sexp)
    ("k" sp-up-sexp)
    ;; Kill/copy/etc
    ("y" sp-copy-sexp)
    ("D" sp-kill-sexp)
    ("u" undo)
    ("C-r" undo-redo)
    ;; Misc
    ("M-t" sp-transpose-sexp)
    ("M-j" sp-join-sexp)
    ("M-s" sp-split-sexp)
    ("M-c" sp-convolute-sexp)
    ("i" sp-indent-defun)
    ;; Depth changing
    ("R" sp-splice-sexp)
    ("r" sp-splice-sexp-killing-around)
    ("C-j" sp-splice-sexp-killing-backward)
    ("C-k" sp-splice-sexp-killing-forward)
    ;; Barfing/slurping
    ("C-l" sp-forward-slurp-sexp)
    ("C-L" sp-forward-barf-sexp)
    ("C-h" sp-backward-barf-sexp)
    ("C-H" sp-backward-slurp-sexp))

  (defhydra my/hydra-smartparens (:hint nil)
    "
Sexps (quit with _q_)
^Nav^              ^Barf/Slurp^             ^Depth^
^---^--------------^----------^-------------^-----^-----------------
_l_: forward       _C-l_: slurp forward     _R_: splice
_h_: backward      _C-L_: barf forward      _r_: raise
_H_: backward ↑    _C-h_: slurp backward    _C-j_: raise backward
_L_: forward ↓     _C-H_: barf backward     _C-k_: raise forward
_j_: backward ↓
_k_: forward ↑
^Misc 1^           ^Misc 2^                 ^Wrap^
^------^-----------^------^-----------------^----^------------------
_y_: copy          _M-j_: join              _(_: wrap with ( )
_D_: kill          _M-s_: split             _{_: wrap with { }
_u_: undo          _M-t_: transpose         _'_: wrap with ' '
_C-r_: redo        _M-c_: convolute         _\"_: wrap with \" \"
^^                 _i_: indent defun        _W_: unwrap"
    ("q" nil)
    ;; Wrapping
    ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
    ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
    ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
    ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
    ("W" (lambda (_) (interactive "P") (sp-unwrap-sexp)))
    ;; Navigation
    ("l" sp-forward-sexp )
    ("h" sp-backward-sexp)
    ("H" sp-backward-up-sexp)
    ("L" sp-down-sexp)
    ("j" sp-backward-down-sexp)
    ("k" sp-up-sexp)
    ;; Kill/copy/etc
    ("y" sp-copy-sexp)
    ("D" sp-kill-sexp)
    ("u" undo)
    ("C-r" undo-redo)
    ;; Misc
    ("M-t" sp-transpose-sexp)
    ("M-j" sp-join-sexp)
    ("M-s" sp-split-sexp)
    ("M-c" sp-convolute-sexp)
    ("i" sp-indent-defun)
    ;; Depth changing
    ("R" sp-splice-sexp)
    ("r" sp-splice-sexp-killing-around)
    ("C-j" sp-splice-sexp-killing-backward)
    ("C-k" sp-splice-sexp-killing-forward)
    ;; Barfing/slurping
    ("C-l" sp-forward-slurp-sexp)
    ("C-L" sp-forward-barf-sexp)
    ("C-h" sp-backward-barf-sexp)
    ("C-H" sp-backward-slurp-sexp)))

(provide 'my-hydras)
