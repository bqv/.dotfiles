;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(progn ; startup-pre
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-full-name "Tony Olagbaiye")
  (setq user-mail-address "frony0@gmail.com")
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (setq gc-cons-threshold 50000000)
  (setq large-file-warning-threshold 100000000)
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  (setq inhibit-startup-message t)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
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
  (global-set-key (kbd "C-x k") 'kill-this-buffer))

(progn ; straight
  (defvar bootstrap-version)
  (let ((bootstrap-file
          (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
        (url-retrieve-synchronously
          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
          'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'use-package)
  (setq use-package-verbose t)
  (use-package auto-compile
    :straight t
    :demand t
    :config
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)
    (setq auto-compile-display-buffer               nil)
    (setq auto-compile-mode-line-counter            t)
    (setq auto-compile-source-recreate-deletes-dest t)
    (setq auto-compile-toggle-deletes-nonlib-dest   t)
    (setq auto-compile-update-autoloads             t))
  (use-package diminish
    :straight t
    :demand t))

(progn ; startup
  (message "Basic init...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

(defconst config-scope '()
  "The current config breadcrumb trail.")

(defmacro config-segment (name &rest body)
  "Run a timed config operation BODY with name NAME."
  `(progn
    (let* ((start-time (current-time))
           (config-scope (cons (symbol-name ,name) config-scope))
           (config-name (string-join (reverse config-scope) "::")))
      (with-demoted-errors "Configuration failure: %s"
        ,@body)
      (message (concat config-name " finished in %.3fs")
               (float-time (time-subtract (current-time)
                                          start-time))))))

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  "Load a FILE in current user's configuration directory."
  (interactive "f")
  (load-file (expand-file-name file user-init-dir)))

(defun config-init (name)
  "Load config module NAME from the user configuration directory."
  (config-segment name (load-user-file (concat (symbol-name name) ".el"))))

(config-init 'main) ; startup

(progn ; startup-post
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (setq debug-on-error t)
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
