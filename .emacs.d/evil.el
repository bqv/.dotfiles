;;; evil.el --- config-evil                    -*- lexical-binding: t -*-

(use-package evil
             :straight t
             :diminish undo-tree-mode
             :init
             (setq evil-default-cursor t)
             :config
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
