(setq inhibit-startup-screen t)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-master nil)
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(setq reftex-plug-into-AUCTeX t)

(defun count-words-region (start end)
 (interactive "r")
 (save-excursion
  (let ((n 0))
   (goto-char start)
   (while (< (point) end) (if (forward-word 1) (setq n (1+ n))))
   (message "Region has %d words" n) n)))

(define-key global-map (kbd "C-x c") 'count-words-region)

(require 'w3m-load)

(add-to-list 'load-path (expand-file-name "~/emacs/site/cedet/common"))

(load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")

(require 'cedet)

(set-language-environment "Japanese")
(require 'mozc)  ; or (load-file "/path/to/mozc.el")
(setq default-input-method "japanese-mozc")
