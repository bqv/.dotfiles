;;; smex.el --- config-smex                    -*- lexical-binding: t -*-

(use-package smex
             :straight t
             :init
             (smex-initialize)
             (global-set-key (kbd "M-x") 'smex)
             (global-set-key (kbd "M-X") 'smex-major-mode-commands)
             (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; old M-x
             )

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
