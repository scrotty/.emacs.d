;;;; -*- lexical-binding: t; -*-

(setq org-directory "~/orgmode")
(defun my/org-path (path)
  (expand-file-name path org-directory))

(setup (:pkg deft)
  (:load-after org
    (setq deft-default-extension "org"
          deft-directory org-directory
          deft-recursive t
          deft-use-filter-string-for-filename t)))

(setup (:pkg evil-org)
  (:load-after (evil org)
    (:hook-into org-mode org-agenda-mode)
    (:require evil-org-agenda)
    (evil-org-set-key-theme
     '(navigation insert textobjects additional calendar))
    (evil-org-agenda-set-keys)
    (:hide-mode)))

(setup (:pkg org)
  (setq org-capture-bookmark nil
        org-catch-invisible-edits 'show-and-error
        org-cycle-separator-lines 2
        org-edit-src-content-indentation 2
        org-ellipsis " ▾"
        org-export-coding-system 'utf-8-unix
        org-export-headline-levels 8
        org-export-with-section-numbers nil
        org-export-with-smart-quotes t
        org-export-with-sub-superscripts t
        org-export-with-toc t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-block-startup nil
        org-hide-emphasis-markers t
        org-html-coding-system 'utf-8-unix
        org-html-todo-kwd-class-prefix "keyword "
        org-outline-path-complete-in-steps nil
        org-pretty-entities t
        org-return-follows-link t
        org-src-fontify-natively t
        org-src-preserve-indentation nil
        org-src-tab-acts-natively t
        org-startup-folded 'content
        org-startup-indented t
        org-startup-with-inline-images t
        org-image-actual-width '(0.8))

  (:load-after hl-fill-column
    (:hook (fn (auto-fill-mode 0)
               (hl-fill-column-mode 0)
               (visual-line-mode 1)))))

(setup (:pkg org-appear)
  (:load-after org
    (setq org-appear-autolinks t)
    (setq org-appear-autoemphasis t)
    (setq org-appear-autoentities t)
    (setq org-appear-autokeywords t)
    (:hook-into org-mode)))

(setup org-faces
  (:load-after org
    (dolist (face-cons '((org-document-title . 1.75)
                         (org-level-1 . 1.5)
                         (org-level-2 . 1.25)
                         (org-level-3 . 1.12)
                         (org-level-4 . 1.05)
                         (org-level-5 . 1.0)
                         (org-level-6 . 1.0)
                         (org-level-7 . 1.0)
                         (org-level-8 . 1.0)))
      (cl-destructuring-bind (face . height) face-cons
        (set-face-attribute face
                            nil
                            :weight 'bold
                            :font "Iosevka Aile"
                            :height height)))))

(setup org-indent
  (:load-after (org evil)
    (setq evil-auto-indent nil)
    (org-indent-mode 1)
    (:hide-mode)))

(setup (:pkg org-superstar)
  (:load-after org
    (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
    (setq org-superstar-remove-leading-stars t)
    (:hook-into org-mode)))

(setup org-tempo
  (:load-after org
    (add-to-list 'org-structure-template-alist '("cl" . "src common-lisp"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
    (add-to-list 'org-structure-template-alist '("sh" . "src sh"))))

(setup (:pkg toc-org)
  (:load-after org
    (:hook-into org-mode)))

(define-local-keys org-mode-map
  "i" '(org-id-get-create :wk "add id")
  "t" '(org-set-tags-command :wk "add tags"))

(provide 'my-pkg-lang-org-base)
