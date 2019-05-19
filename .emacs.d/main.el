;;; main.el --- config-main                    -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :straight t
  :config
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
    (server-start)))

(config-init 'wm)
(config-init 'evil)
(config-init 'helm)
(config-init 'smex)
(config-init 'avy)
(config-init 'syntax)
(config-init 'theme)
(config-init 'shell)
(config-init 'latex)
(config-init 'org)
(config-init 'highlight)
(config-init 'flymake)
(config-init 'bufmgmt)
(config-init 'magit)
(config-init 'vcs)
(config-segment 'lang
    (config-init 'rust)
    (config-init 'racket))
(config-init 'misc)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
