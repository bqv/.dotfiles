;;; bufmgmt.el --- config-bufmgmt                    -*- lexical-binding: t -*-

; TODO: Use workgroups

(use-package persp-mode
  :straight t
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-auto-save-fname "autosave"
        persp-auto-save-opt 1
        persp-nil-hidden t
        persp-nil-name "nil"
        persp-save-dir ".emacs.d/workspaces/")
  (persp-mode t))

(use-package neotree
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

;(use-package persp-projectile
;  :straight t
;  :after (projectile persp-mode))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
