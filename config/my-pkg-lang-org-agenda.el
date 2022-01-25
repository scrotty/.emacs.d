;;;; -*- lexical-binding: t; -*-

(setup org-agenda
  (:load-after org
    (setq triage-path (my/org-path "triage.org"))
    (setq gtd-path (my/org-path "gtd.org"))
    (setq archive-path (my/org-path "archive.org"))
    (setq org-archive-location "archive.org::datetree/")
    (setq org-id-link-to-org-use-id 'create-if-interactive)
    (setq org-id-locations-file (expand-file-name ".orgids" triage-path))
    (setq org-agenda-files
          (list triage-path archive-path gtd-path))
    (setq org-log-done 'time)

    (setq org-todo-keyword-faces
          '(("TODO" :foreground "dodger blue" :weight bold)
            ("NEXT" :foreground "orange red" :weight bold)
            ("INPROGRESS" :foreground "spring green" :weight bold)
            ("WAITING" :foreground "yellow" :weight bold)
            ("HOLD" :foreground "yellow" :weight bold)
            ("DONE" :foreground "forest green" :weight bold)
            ("CANCELLED" :foreground "forest green" :weight bold)))
    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS(i)" "|" "VERIFY(v)" "DONE(d)")
            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

    (setq org-capture-templates
          '(("t" "Todo"
             entry (file triage-path)
             "* TODO %?\n  %U\n  %i"
             :kill-buffer t)
            ("l" "Todo with link"
             entry (file triage-path)
             "* TODO %?\n  %U\n  %a\n  %i"
             :kill-buffer t)))

    (setq org-refile-targets
          '((nil :maxlevel . 1)
            (org-agenda-files :maxlevel . 3)))
    (setq org-refile-use-outline-path t)
    (advice-add 'org-refile :after 'org-save-all-org-buffers)
    (advice-add 'org-archive-subtree :after 'org-save-all-org-buffers)))

(provide 'my-pkg-lang-org-agenda)
