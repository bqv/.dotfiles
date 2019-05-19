;;; vcs.el --- config-vcs                    -*- lexical-binding: t -*-

(use-package magit
  :bind (("C-M-g" . magit-status)))

(use-package projectile
  :straight t
  :diminish projectile-mode
  :demand t
  :bind
  (("C-c p f" . helm-projectile-find-file)
   ("C-c p p" . helm-projectile-switch-project)
   ("C-c p s" . projectile-save-project-buffers))
  :config
  (setq projectile-enable-caching t)
  (projectile-mode t))

(use-package helm-projectile
  :straight t
  :after (projectile helm)
  :init
  (setq projectile-completion-system 'helm
        helm-projectile-fuzzy-match t)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
