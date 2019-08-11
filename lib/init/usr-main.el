;;; usr-main.el --- main-init-script                    -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(defvar config-registry '()
  "Profiling and initialization status data")

(defconst config-scope '()
  "The current config breadcrumb trail.")

(defmacro config-segment (name &rest body)
  "Run a timed config operation BODY with name NAME."
  `(progn
    (let* ((start-time (current-time))
           (error-state nil)
           (config-scope (cons (symbol-name ,name) config-scope))
           (config-name (string-join (reverse config-scope) "::")))
      (condition-case the-problem
        (progn ,@body)
        (error (setq error-state the-problem))
        (warn (setq error-state the-problem)))
      (if error-state
        (let* ((end-time (current-time))
               (delta (time-subtract end-time start-time))
               (rethrow (lambda () (funcall #'signal
                                            (car error-state)
                                            (cdr error-state))))
               (registry-data (record 'config-err
                                       config-scope
                                       `(,start-time . ,end-time)
                                       error-state)))
          (message (concat config-name " failed after %.3fs")
                   (float-time delta))
          (add-to-list 'config-registry `(,config-name . ,registry-data))
          (if noninteractive (funcall rethrow)))
        (let* ((end-time (current-time))
               (delta (time-subtract end-time start-time))
               (registry-data (record 'config-ok
                                       config-scope
                                       `(,start-time . ,end-time)
                                       delta)))
          (message (concat config-name " finished in %.3fs")
                   (float-time delta))
          (add-to-list 'config-registry `(,config-name . ,registry-data)))))))

(defmacro config-package (package &rest args)
  "Load a package PACKAGE by calling use-package, with ARGS."
  (let ((name (cond ((symbolp package) package)
                    ((stringp package) (make-symbol package))
                    (t nil))))
    `(config-segment ',name
                     (use-package ,name ,@args))))

(defun config-errors ()
  (let ((error-registry (seq-remove (lambda (c) (eq (type-of (cdr c)) 'config-ok)) config-registry)))
    (mapcar (lambda (c)
              `(,(car c) ,@(cdr (aref (cdr c) 3))))
            error-registry)))

(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "locutus")
(setq initial-buffer-choice t)
(setq initial-scratch-message "")
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 1)
(fringe-mode 1) ; pixels
(blink-cursor-mode 0)
(show-paren-mode 1)
(setq show-paren-delay 0)
;(global-hl-line-mode 1)
(column-number-mode 1)
(line-number-mode 1)
(global-display-line-numbers-mode 1)
(size-indication-mode 1)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(setq-default indent-tabs-mode nil)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(fset 'yes-or-no-p 'y-or-n-p)
(progn
  ;; Put backup files neatly away
  (let ((backup-dir "~/.emacs.d/backups")
        (auto-saves-dir "~/.emacs.d/autosaves/"))
    (dolist (dir (list backup-dir auto-saves-dir))
      (when (not (file-directory-p dir))
        (make-directory dir t)))
    (setq backup-directory-alist `(("." . ,backup-dir))
          auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
          auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
          tramp-backup-directory-alist `((".*" . ,backup-dir))
          tramp-auto-save-directory auto-saves-dir))
  (setq backup-by-copying t    ; Don't delink hardlinks
        delete-old-versions t  ; Clean up the backups
        version-control t      ; Use version numbers on backups,
        kept-new-versions 5    ; keep some new versions
        kept-old-versions 2))  ; and some old ones, too
(global-auto-revert-mode t)
(add-hook 'before-save-hook 'whitespace-cleanup)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(config-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(cl-flet ((config-custom-arg (switch)
  (let ((found-switch (member switch command-line-args)))
    (setq command-line-args (delete switch command-line-args))
    found-switch)))
         (let ((short-arg (config-custom-arg "-n"))
               (long-arg (config-custom-arg "--new")))
           (setq config-arg-new (or long-arg short-arg))))

(config-package server
  :straight t
  :config
  (unless (or noninteractive config-arg-new)
    (if (server-running-p)
      (progn
        (with-demoted-errors "Server Error: %S"
                             (if (daemonp)
                               (error "Abort: Another running Emacs server detected")
                               (let ((args (append '("emacsclient" "-a" "\"\"" "-c")
                                                   (cdr command-line-args))))
                                 (make-frame-invisible (selected-frame) t)
                                 (shell-command (substring (format "%S" args) 1 -1)))))
        (kill-emacs))
      (server-start))))

(let ((config-batch (lambda (sym) (let ((config-dir (cdr (assoc sym config-dirs)))
                                        (config-re (concat "^usr-" (symbol-name sym) "-.+.el")))
                                    (config-segment sym
                                                    (mapc
                                                      (lambda (fp)
                                                        (config-segment
                                                          (make-symbol
                                                            (car (last (split-string (file-name-base fp) "-"))))
                                                          (load-file fp)))
                                                      (directory-files config-dir t config-re t)))))))
  (mapc (lambda (d) (funcall config-batch d))
        `(,'crit ,'tool ,'util ,'lang))
  (config-end))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
