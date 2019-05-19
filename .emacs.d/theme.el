;;; theme.el --- config-theme                    -*- lexical-binding: t -*-

(use-package solarized-theme
             :straight t)

(use-package zenburn-theme
             :straight t)

(use-package doom-themes
             :straight t)

(if window-system
  (load-theme 'zenburn t)
  (load-theme 'zenburn t))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
