;;;; -*- lexical-binding: t; -*-

(setup (:pkg company)
  (global-company-mode 1)
  (company-tng-mode 1)
  (setq company-backends (remove 'company-dabbrev company-backends))
  (setq company-idle-delay nil)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (:with-state insert
    (:bind
     [tab] company-complete-common-or-cycle))
  (:with-map company-active-map
    (:bind
     [tab] company-select-next
     [backtab] company-select-previous))
  (:hide-mode))

(setup (:pkg consult)
  (setq consult-async-min-input 2)      ;
  (setq consult-preview-key 'any)
  (setq xref-show-definitions-function #'consult-xref)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple)
  (:load-after projectile
    (setq consult-project-root-function #'projectile-project-root))
  (:load-after vertico
    (setq completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args)))))

(setup (:pkg consult-dir)
  (:load-after (consult)
    (defun consult-dir-maybe ()
      (interactive)
      (let* ((full-category (completion-metadata-get (embark--metadata) 'category))
             (category (pcase full-category
                         ('consult-multi (car (get-text-property
                                               0 'consult-multi
                                               (vertico--candidate))))
                         (_ full-category))))
        (if (member category '(file))
            (call-interactively #'consult-dir)
          (call-interactively (lookup-key global-map (kbd "C-M-d"))))))
    (:global-bind
     "C-x C-d" consult-dir)
    (:with-map minibuffer-local-completion-map
      (:bind
       "C-M-d" consult-dir-maybe
       "H-M-d" consult-dir-maybe
       "C-M-j" consult-dir-jump-file
       "M-s f" consult-dir-jump-file
       "H-M-j" consult-dir-jump-file))
    (:with-map vertico-map
      (:bind
       "C-M-d" consult-dir-maybe
       "H-M-d" consult-dir-maybe
       "M-s f" consult-dir-jump-file
       "C-M-j" consult-dir-jump-file
       "H-M-j" consult-dir-jump-file))
    ))

(setup (:pkg embark)
  (:load-after which-key
    (defun embark-which-key-indicator ()
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (plist-get (car targets) :type) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))
    (setq prefix-help-command #'embark-prefix-help-command)
    (setq embark-indicators '(embark-which-key-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator))
    (:advise embark-completing-read-prompter :around (fn &rest args)
      (when-let ((win (get-buffer-window which-key--buffer 'visible)))
        (quit-window 'kill-buffer win)
        (let ((embark-indicators (delq #'embark-which-key-indicator
                                       embark-indicators)))
          (apply fn args))))
    (:global "C-," embark-act)))

(setup (:pkg embark-consult)
  (:load-after (embark consult)
    (:with-mode embark-collect-mode
      (:hook consult-preview-at-point-mode))))

(setup (:pkg orderless)
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion))))
  (setq completion-styles '(orderless)))

(setup (:pkg (vertico :files (:defaults "extensions/*")))
  (:also-load vertico-repeat vertico-reverse vertico-grid vertico-quick vertico-buffer vertico-multiform vertico-unobtrusive vertico-flat)

  ;; Vertico-grid

  (define-minor-mode my/vertico-grid-mode
    "Vertico-grid display with modified row count."
    :global t :group 'vertico
    (cond
     (my/vertico-grid-mode
      (setq my/vertico-count-orig vertico-count)
      (setq vertico-count 4)
      (vertico-grid-mode 1))
     (t (vertico-grid-mode 0)
        (setq vertico-count my/vertico-count-orig))))
  (setq vertico-grid-separator "    ")
  (setq vertico-grid-lookahead 50)

  ;; Vertico-multiform
  (setq vertico-multiform-categories
        '((file my/vertico-grid-mode reverse)
          (project-file my/vertico-grid-mode reverse)
          (imenu buffer)
          (consult-location buffer)
          (consult-grep buffer)
          (notmuch-result reverse)
          (minor-mode reverse)
          (reftex-label reverse)
          (bib-reference reverse)
          (t unobtrusive)))
  (setq vertico-multiform-commands
        '((load-theme my/vertico-grid-mode reverse)
          (my/toggle-theme my/vertico-grid-mode reverse)
          (consult-dir-maybe reverse)
          (consult-dir reverse)
          (consult-history reverse)
          (consult-completion-in-region reverse)
          (completion-at-point reverse)
          (org-roam-node-find reverse)
          (embark-completing-read-prompter reverse)
          (embark-act-with-completing-read reverse)
          (embark-prefix-help-command reverse)
          (tmm-menubar reverse)))

  ;; Vertico-quick
  (defun vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))
  (:with-map vertico-map
    (:bind
     "s-<return>" vertico-unobtrusive-mode
     "s-l" vertico-quick-exit
     "s-e" vertico-quick-embark))

  ;; Vertico base
  (setq vertico-buffer-display-action 'display-buffer-reuse-window)

  ;; Vertico base
  (setq vertico-count 15)
  (setq vertico-cycle t)
  (setq vertico-resize t)
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (:with-hook minibuffer-setup-hook
    (:hook vertico-repeat-save)))

(provide 'my-pkg-completion)
