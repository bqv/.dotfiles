;;; vcs.el --- config-vcs                    -*- lexical-binding: t -*-

(config-package magit
  :straight t
  :bind (("C-x g" . magit-status)))

(config-package git-gutter
  :straight t
  :config
  (global-git-gutter-mode 't)
  :diminish git-gutter-mode)

(config-package git-timemachine
  :straight t)

(config-package projectile
  :straight t
  :diminish projectile-mode
  :demand t
  :bind
  (("C-c p f" . projectile-find-file)
   ("C-c p p" . projectile-switch-project)
   ("C-c p s" . projectile-save-project-buffers))
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (setq projectile-enable-caching t)
  (projectile-mode t))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
