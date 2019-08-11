;;; avy.el --- config-avy                    -*- lexical-binding: t -*-

(config-package avy
  :straight t
  :bind ("C-=" . avy-goto-char)
  :config
  (setq avy-background t))

;(config-package ace-window
;  :straight t
;  :bind ("M-m" . ace-window)
;  :config
;  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
;        aw-dispatch-always t))

(config-package switch-window
  :straight t
  :after (exwm exwm-init)
  :config
  (exwm-input-set-key (kbd "s-m") 'switch-window)
  (setq switch-window-shortcut-style 'qwerty
        switch-window-input-style 'minibuffer
        switch-window-multiple-frames t))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
