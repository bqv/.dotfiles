;;; bufmgmt.el --- config-bufmgmt                    -*- lexical-binding: t -*-

; TODO: Use workgroups
; ...or don't? exwm and desktop.el are kinda good enough

(config-package persp-mode
  :straight t
  :init
  (defun fixme-errors (old-fn &rest args)
    (with-demoted-errors "Fixme: %S"
      (apply old-fn args)))
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-auto-save-fname "autosave"
        persp-auto-save-opt 1
        persp-nil-hidden t
        persp-nil-name "nil"
        persp-save-dir "~/.emacs.d/workspaces/"
        persp-init-frame-behaviour nil)
  (advice-add #'window-state-put :around #'fixme-errors)
  (advice-add #'persp-add-buffer :around #'fixme-errors)
  ;ehh maybe not;(persp-mode t)
  )

(defun advice-list (sym)
  (let ((l nil))
    (advice-mapc (lambda (a &rest ignored)
                   (push a l))
                 sym)
    l))

(config-package neotree
  :straight t
  :after (persp-mode projectile)
  :config
  (defun local/persp-neo ()
    "Make Neotree follow perspective"
    (interactive)
    (let ((cw (selected-window))
          (path (buffer-file-name)))
      (progn
        (when (and (fboundp 'projectile-project-p)
                   (projectile-project-p)
                   (fboundp 'projectile-project-root))
          (neotree-dir (projectile-project-root)))
        (neotree-find path))
      (select-window cw)))
  :hook
  (persp-switch . local/persp-neo))

(config-package persp-projectile
  :straight t
  :after (projectile persp-mode))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
