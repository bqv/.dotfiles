;;; usr-init.el --- emacs-init-script                    -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(progn ; startup-pre
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `current-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq current-file (or load-file-name (buffer-file-name)))
  (setq current-path (file-name-directory current-file))
  (message "Initializing...")
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  (setq load-prefer-newer t)
  (let ((config-lib-dir (file-name-directory
                          (directory-file-name current-path))))
    (defconst config-crit-dir
              (expand-file-name "crit" config-lib-dir)
              "Critical feature scripts path")
    (defconst config-tool-dir
              (expand-file-name "tool" config-lib-dir)
              "Priority feature scripts path")
    (defconst config-util-dir
              (expand-file-name "util" config-lib-dir)
              "Dispensable feature scripts path")
    (defconst config-lang-dir
              (expand-file-name "lang" config-lib-dir)
              "Modal feature scripts path"))
  (defconst config-dirs `((,'crit . ,config-crit-dir)
                          (,'tool . ,config-tool-dir)
                          (,'util . ,config-util-dir)
                          (,'lang . ,config-lang-dir))
            "Featue directories mapped by symbols"))

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
  (use-package gcmh
    :straight t
    :config
    (gcmh-mode t))
  (use-package diminish
    :straight t
    :demand t)
  (use-package epkg
    :straight t)
  (use-package cask
    :straight t
    :config
    (progn
      (ignore-errors (delete-directory cask-directory t))
      (make-symbolic-link
       (expand-file-name "../../repos/cask/" cask-directory)
       cask-directory t))))

(progn ; startup
  (message "Bootstrapping...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (defun config-end ()
    (message
      "Configuring...done (%.3fs) [after-init]"
      (float-time (time-subtract (current-time)
                                 before-user-init-time)))
    (fmakunbound 'config-end)
    (setq debug-on-error t))
  (load-file (expand-file-name "usr-main.el" current-path))
  (message "Loading...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (makunbound 'before-user-init-time))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
