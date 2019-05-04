;;; latex-preview-pane-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (latex-preview-pane-mode latex-preview-pane-update-p
;;;;;;  latex-preview-pane-update latex-preview-update init-latex-preview-pane
;;;;;;  latex-preview-pane-enable) "latex-preview-pane" "latex-preview-pane.el"
;;;;;;  (21252 36446 506405 533000))
;;; Generated autoloads from latex-preview-pane.el

(autoload 'latex-preview-pane-enable "latex-preview-pane" "\
Enable `latex-preview-pane-mode' in `latex-mode'.

\(fn)" nil nil)

(autoload 'init-latex-preview-pane "latex-preview-pane" "\


\(fn)" nil nil)

(autoload 'latex-preview-update "latex-preview-pane" "\


\(fn)" t nil)

(autoload 'latex-preview-pane-update "latex-preview-pane" "\


\(fn)" t nil)

(autoload 'latex-preview-pane-update-p "latex-preview-pane" "\


\(fn)" nil nil)

(autoload 'latex-preview-pane-mode "latex-preview-pane" "\
Toggle Latex Preview Pane Mode.
     Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  From Lisp, argument omitted or nil enables
     the mode, `toggle' toggles the state.
     
     When Latex Preview Pane mode is enabled, saving a latex file will cause 
     a PDF preview pane of your document to appear.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("latex-preview-pane-pkg.el") (21252 36446
;;;;;;  751648 961000))

;;;***

(provide 'latex-preview-pane-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; latex-preview-pane-autoloads.el ends here
