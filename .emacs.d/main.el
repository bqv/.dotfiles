;;; main.el --- config-main                    -*- lexical-binding: t -*-

(let ((start-time (current-time)))
  (progn ; UI
    (when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))
    (defalias 'yes-or-no-p 'y-or-n-p))
  (progn ; Editor
    (show-paren-mode 1)
    (setq show-paren-delay 0)
    (column-number-mode 1)
    (setq-default indent-tabs-mode nil))
  (message "*:pre finished in %.3fs"
           (float-time (time-subtract (current-time)
                                      start-time))))

(defun config-init (name)
  (progn
    (let ((start-time (current-time))
          (config-name (symbol-name name)))
      (with-demoted-errors "Configuration failure: %s"
        (load-user-file (concat config-name ".el")))
      (message (concat "*:" config-name " finished in %.3fs")
               (float-time (time-subtract (current-time)
                                          start-time))))))

(config-init 'evil)
(config-init 'ido)
(config-init 'smex)
(config-init 'theme)
(config-init 'shell)
(config-init 'racket)
(config-init 'latex)
(config-init 'org)
(config-init 'highlight)
(config-init 'flymake)
;(config-init 'misc)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
