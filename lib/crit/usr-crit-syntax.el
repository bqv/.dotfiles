;;; syntax.el --- config-syntax                    -*- lexical-binding: t -*-

(config-package idle-highlight-mode
             :straight t
             :config
             (add-hook 'prog-mode-hook 'idle-highlight-mode))

(config-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode))

(config-package company
  :straight t
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 1))

(config-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(config-package company-lsp
  :straight t
  :config
  (push 'company-lsp company-backends))

(config-package lsp-mode
  :straight t
  :commands lsp
  :config
  (require 'lsp-clients))

(config-package lsp-ui
  :straight t)

(config-package yasnippet
  :straight t)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
