;;; shell.el --- config-racket                    -*- lexical-binding: t -*-

(defun zsh ()
  (interactive)
  (ansi-term "/usr/bin/zsh"))
(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")

(config-package xterm-color
  :straight t
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'shell-mode-hook
          (lambda ()
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
  (require 'eshell)
  (add-hook 'eshell-before-prompt-hook
          (lambda ()
            (setq xterm-color-preserve-properties t)))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setq compilation-environment '("TERM=xterm-256color"))
  (add-hook 'compilation-start-hook
          (lambda (proc)
            ;; We need to differentiate between compilation-mode buffers
            ;; and running as part of comint (which at this point we assume
            ;; has been configured separately for xterm-color)
            (when (eq (process-filter proc) 'compilation-filter)
              ;; This is a process associated with a compilation-mode buffer.
              ;; We may call `xterm-color-filter' before its own filter function.
              (set-process-filter
               proc
               (lambda (proc string)
                 (funcall 'compilation-filter proc
                          (xterm-color-filter string))))))))

(config-package eterm-256color
  :straight t
  :config
  (add-hook 'term-mode-hook #'eterm-256color-mode))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
