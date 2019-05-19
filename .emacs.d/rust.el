;;; rust.el --- config-rust                    -*- lexical-binding: t -*-

(use-package toml-mode
  :straight t)

(use-package rust-mode
  :straight t
  :after (lsp)
  :hook (rust-mode . lsp))

(use-package cargo
  :straight t
  :after (rust-mode)
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :straight t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
