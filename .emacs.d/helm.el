;;; helm.el --- config-helm                    -*- lexical-binding: t -*-

(use-package helm
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  :straight t
  :demand t
  :bind (("M-x" . helm-M-x)
         ("C-c h o" . helm-occur)
         ("<f1> SPC" . helm-all-mark-rings) ; I modified the keybinding
         ("M-y" . helm-show-kill-ring)
         ("C-c h x" . helm-register)    ; C-x r SPC and C-x r j
         ("C-c h g" . helm-google-suggest)
         ("C-c h M-:" . helm-eval-expression-with-eldoc)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)      ; *<major-mode> or /<dir> or !/<dir-not-desired> or @<regexp>
         :map helm-map
         ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
         ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
         ("C-z" . helm-select-action) ; list actions using C-z
         :map shell-mode-map
         ("C-c C-l" . helm-comint-input-ring) ; in shell mode
         :map minibuffer-local-map
         ("C-c C-l" . helm-minibuffer-history))
  :init
  (setq helm-command-prefix-key "C-c h")
  (setq recentf-save-file "~/.emacs.d/recentf" ; customize yours
        recentf-max-saved-items 50)
  (require 'helm-eshell)        ; question
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))
  :config
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq helm-completion-in-region-fuzzy-match t
        helm-mode-fuzzy-match t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)
  (setq helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match    t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (require 'helm-config)
  (setq helm-split-window-in-side-p         t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (helm-adaptive-mode t)
  (helm-mode 1))

(use-package helm-descbinds
  :straight t
  :after helm
  :config
  (helm-descbinds-mode))

(use-package helm-flx
  :straight t
  :after helm
  :config
  (helm-flx-mode t))

(use-package helm-ag
  :straight t
  :after helm-projectile
  :commands helm-do-ag)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
