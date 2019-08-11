;;; evil.el --- config-evil                    -*- lexical-binding: t -*-

(config-package evil
             :straight t
             :diminish undo-tree-mode
             :init
             (setq evil-default-cursor t)
             ;(setq evil-default-state 'emacs)
             :config
             ;(advice-add 'global-set-key :after
             ;            (lambda (old-fn &rest args)
             ;                     (apply 'evil-global-set-key args)))
             (evil-mode 1)
             (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
             (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
             (define-key evil-insert-state-map (kbd "C-w") 'kill-region)
             (define-key evil-visual-state-map (kbd "C-e") 'move-end-of-line)
             (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
             (define-key evil-normal-state-map (kbd "C-k") 'kill-line)
             (define-key evil-normal-state-map (kbd "C-y") 'yank)
             (define-key evil-insert-state-map (kbd "C-y") 'yank)
             (define-key evil-normal-state-map (kbd "C-w") 'kill-region)
             (define-key evil-visual-state-map (kbd "C-w") 'kill-region))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
