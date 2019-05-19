;;; latex.el --- config-latex                    -*- lexical-binding: t -*-

(use-package auctex-lua
             :straight t
             :after (auctex))

(use-package auctex-latexmk
             :straight t
             :after (auctex))

(use-package auctex
             :straight t
             :defer t
             :after (tex latex)
             :config
             (TeX-global-PDF-mode t))

(use-package latex
             :mode ("\\.tex\\'" . latex-mode)
             :config
             (add-hook 'doc-view-mode-hook 'auto-revert-mode)
             (add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)
             (add-hook 'LaTeX-mode-hook
                       (lambda ()
                         (visual-line-mode t)
                         (turn-on-reftex))
                       t)
             (setq pdf-latex-command "lualatex")
             (setq-default TeX-command-default "LaTeX"
                           TeX-PDF-mode t
                           Tex-engine 'lualatex))

(use-package doc-view
             :defer t
             :config
             (add-hook 'doc-view-mode-hook
                       (lambda ()
                         (define-key doc-view-mode-map [wheel-down] 'doc-view-next-line-or-next-page)
                         (define-key doc-view-mode-map [double-wheel-down]
                                     (lambda ()
                                       (interactive)
                                       (doc-view-next-line-or-next-page 2)))
                         (define-key doc-view-mode-map [triple-wheel-down]
                                     (lambda ()
                                       (interactive)
                                       (doc-view-next-line-or-next-page 3)))
                         (define-key doc-view-mode-map [wheel-up] 'doc-view-previous-line-or-previous-page)
                         (define-key doc-view-mode-map [double-wheel-up]
                                     (lambda ()
                                       (interactive)
                                       (doc-view-previous-line-or-previous-page 2)))
                         (define-key doc-view-mode-map [triple-wheel-up]
                                     (lambda ()
                                       (interactive)
                                       (doc-view-previous-line-or-previous-page 3))))
                       t)
             )

(use-package latex-preview-pane
             :straight t
             :after (tex latex))

(use-package latex-pretty-symbols
             :straight t
             :after (tex latex))

(use-package latex-extra
             :straight t
             :after (tex latex))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
