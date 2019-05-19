;;; syntax.el --- config-syntax                    -*- lexical-binding: t -*-

(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode))

(use-package company
  :straight t
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 1))

(use-package lsp-mode
  :straight t
  :commands lsp
  :config
  (require 'lsp-clients))

(use-package lsp-ui
  :straight t)

(use-package yasnippet
  :straight t)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
