;;; theme.el --- config-theme                    -*- lexical-binding: t -*-

(config-package solarized-theme
             :straight t)

(config-package zenburn-theme
             :straight t)

(config-package doom-themes
             :straight t)

(if window-system
  (load-theme 'zenburn t)
  (load-theme 'zenburn t))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
