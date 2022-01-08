;;;; -*- lexical-binding: t; -*-

(require 'recentf)

;;; ============================================================================
;;; Variables
;;; ============================================================================

(defvar my/ignored-directories
  `(,user-emacs-directory
    "eln-cache"))

(defvar my/ignored-suffixes
  '(".7z" ".bz2" ".db" ".dll" ".dmg" ".elc" ".exe" ".fasl" ".gz" ".iso" ".jar"
    ".o" ".pyc" ".rar" ".so" ".sql" ".sqlite" ".tar" ".tgz" ".xz" ".zip"))

;;; ============================================================================
;;; Macros
;;; ============================================================================

(defmacro fn (&rest body)
  `(lambda () ,@body))

(defmacro fn! (&rest body)
  `(lambda () (interactive) ,@body))

(defmacro quiet! (&rest forms)
  `(cond
    (noninteractive
     (let ((old-fn (symbol-function 'write-region)))
       (cl-letf ((standard-output (lambda (&rest _)))
                 ((symbol-function 'load-file)
                  (lambda (file) (load file nil t)))
                 ((symbol-function 'message) (lambda (&rest _)))
                 ((symbol-function 'write-region)
                  (lambda (start end filename &optional append visit lockname
                                 mustbenew)
                    (unless visit (setq visit 'no-message))
                    (funcall old-fn start end filename append visit lockname
                             mustbenew))))
         ,@forms)))
    ((or debug-on-error debug-on-quit)
     ,@forms)
    ((let ((inhibit-message t)
           (save-silently t))
       (prog1 ,@forms (message ""))))))

;;; ============================================================================
;;; Functions
;;; ============================================================================

(defun my/show-startup-time ()
  (message "Emacs startup time: %.2fs (%d GCs)"
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(defun my/etc-file (file-name)
  (expand-file-name (format "etc/%s" file-name) user-emacs-directory))

(defun my/cache-dir-p (path)
  (string-prefix-p (getenv "XDG_CACHE_HOME") (expand-file-name path)))

(defun my/smarter-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun my/yank-primary-selection ()
  (interactive)
  (let ((primary (or (gui-get-primary-selection) (gui-get-selection))))
    (when primary
      (push-mark (point))
      (insert-for-yank primary))))

(defun my/delete-file (filename)
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (delete-file filename)))

(defun my/rename-file ()
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir))
                            (yes-or-no-p (format "Create directory '%s'?"
                                                 dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (message "File '%s' successfully renamed to '%s'" name
                        (file-name-nondirectory new-name))))))))

(defun my/html-open-link-in-brave (&optional @fullpath &rest dummy)
  "open url under cursor in Brave browser. Work in Mac OS only Version 2019-02-17"
  (interactive)
  (let ($path)
    (if @fullpath
        (progn (setq $path @fullpath))
      (let (($inputStr
             (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (let ($p0 $p1 $p2
                         ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
                 (setq $p0 (point))
                 (skip-chars-backward $pathStops)
                 (setq $p1 (point))
                 (goto-char $p0)
                 (skip-chars-forward $pathStops)
                 (setq $p2 (point))
                 (goto-char $p0)
                 (buffer-substring-no-properties $p1 $p2)))))
        (setq $path (replace-regexp-in-string
                     "^file:///" "/"
                     (replace-regexp-in-string
                      ":\\'" "" $inputStr)))))
    (cond
     ((string-equal system-type "darwin")
      (shell-command (format "open -a 'Brave Browser.app' \"%s\"" $path)))
     ((string-equal system-type "windows-nt")
      ;; "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" 2019-11-09
      (let ((process-connection-type nil))
        (start-process "" nil "powershell" "start-process" "brave" $path )))
     ((string-equal system-type "gnu/linux")
      (shell-command (format "brave \"%s\"" $path))))))

(provide 'my-util)
