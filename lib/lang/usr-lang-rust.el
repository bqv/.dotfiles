;;; rust.el --- config-rust                    -*- lexical-binding: t -*-

(config-package toml-mode
  :straight t)

(config-package rust-mode
  :straight t
  :after (lsp)
  :hook (rust-mode . lsp))

(config-package cargo
  :straight t
  :after (rust-mode)
  :hook (rust-mode . cargo-minor-mode))

(config-package flycheck-rust
  :straight t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
