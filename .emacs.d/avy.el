;;; avy.el --- config-avy                    -*- lexical-binding: t -*-

(use-package avy
  :straight t
  :bind ("C-=" . avy-goto-char)
  :config
  (setq avy-background t))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
