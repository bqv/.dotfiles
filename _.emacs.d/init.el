;; ELPA
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; UI
(setq inhibit-startup-message t)
(when (fboundp 'windmove-default-keybindings)
        (windmove-default-keybindings))
(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode 0)
(menu-bar-mode 1)

;; Editor
(show-paren-mode 1)
(setq show-paren-delay 0)
(column-number-mode 1)
(setq-default indent-tabs-mode nil)

;; Ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Evil-mode
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
(define-key evil-visual-state-map (kbd "C-w") 'kill-region)

;; Smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; old M-x

;; Theming
;(if window-system
;  (load-theme 'solarized-light t)
;  (load-theme 'tango-2 t))
(load-theme 'tango-2 t)

;; Racket scheme
(setq scheme-program-name "racket")

;; Zsh terminal
(defun zsh ()
  (interactive)
  (ansi-term "/usr/bin/zsh"))

;; LaTeX
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(setq TeX-command-default "LaTeX")
(setq TeX-engine 'lualatex)
(add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)
(setq pdf-latex-command "lualatex")
(add-hook 'LaTeX-mode-hook
      (lambda ()
        (visual-line-mode t)
        (turn-on-reftex))
      t)
(setq TeX-PDF-mode t)

;; Make doc-view-mode scroll sanely with the mouse wheel
(add-hook 'doc-view-mode-hook (lambda () (define-key doc-view-mode-map [wheel-down] 'doc-view-next-line-or-next-page) (define-key doc-view-mode-map [double-wheel-down] (lambda () (interactive) (doc-view-next-line-or-next-page 2))) (define-key doc-view-mode-map [triple-wheel-down] (lambda () (interactive) (doc-view-next-line-or-next-page 3))) (define-key doc-view-mode-map [wheel-up] 'doc-view-previous-line-or-previous-page) (define-key doc-view-mode-map [double-wheel-up] (lambda () (interactice) (doc-view-previous-line-or-previous-page 2))) (define-key doc-view-mode-map [triple-wheel-up] (lambda () (interactive) (doc-view-previous-line-or-previous-page 3)))) t)

;; Code Browser
(setq ecb-options-version "2.40")
(setq ecb-source-path '(("/home/bao/src/java/MyDulwich/" "MyDulwich")))
(setq ecb-examples-bufferinfo-buffer-name nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("bad832ac33fcbce342b4d69431e7393701f0823a3820f6030ccc361edd2a4be4" "f32dd8e7b3a508874eded03d5be43d2bdfffe81c199eea72de06ce3e653db720" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
