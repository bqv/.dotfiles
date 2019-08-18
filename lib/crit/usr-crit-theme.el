;;; theme.el --- config-theme                    -*- lexical-binding: t -*-

(config-package solarized-theme
             :straight t)

(config-package zenburn-theme
             :straight t)

(config-package hc-zenburn-theme
             :straight t)

(config-package material-theme
             :straight t)

(config-package doom-themes
             :straight t)

(if window-system
  (load-theme 'hc-zenburn t)
  (load-theme 'zenburn t))

(config-package misc-cmds
                :straight t)

(config-package palette
                :straight t)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
