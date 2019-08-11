;;; org.el --- config-org                    -*- lexical-binding: t -*-

(config-package org
  :straight t
  :config
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-switchb)
  (setq bookmark-save-flag t)
  (setq org-log-done 'time)
  (setq org-log-done 'note)
  (setq org-directory "~/var/org/"
        org-agenda-files (list org-directory))
  (defmacro .org (name &rest ignored)
    `,(expand-file-name (concat name ".org") org-directory)))

(config-package oauth2
  :straight t)

(config-package auth-source
  :straight t
  :config
  (setq auth-sources '(default
                        "secrets:session"
                        "secrets:Login"
                        "~/.authinfo"
                        "~/.authinfo.gpg")))

(config-package org-gcal
  :straight t
  :after (oauth2)
  :init
  (defun bqv/gcal-sync (&optional val)
    (interactive "i")
    (org-gcal-fetch)
    (org-gcal-sync))
  (defun bqv/org-gcal-idle-sync (&optional secs)
    (let ((delay (* 1 (or secs 300))))
      (run-with-idle-timer delay nil
                           'bqv/gcal-sync)))
  :config
  (setq plstore-cache-passphrase-for-symmetric-encryption t)
  (setq org-gcal-client-id "848970098357-565dckem2o5i4h1tbp0th2hgo68usnah.apps.googleusercontent.com"
        org-gcal-client-secret "tkMV-EHXD29Sqocf0TcQXT6G")
  (setq org-gcal-file-alist
        `(("frony0@gmail.com" . ,(.org "Calendar"))
          ("family04403277236973536823@group.calendar.google.com" . ,(.org "Family"))
          ("tony.olagbaiye@answerdigital.com" . ,(.org "Schedule"))))
  (add-hook 'after-save-hook 'bqv/org-gcal-idle-sync)
  (add-hook 'org-agenda-mode-hook 'bqv/org-gcal-idle-sync)
  (add-hook 'org-capture-after-finalize-hook 'bqv/org-gcal-idle-sync)
  (bqv/gcal-sync))

(config-package calfw
  :straight t)

(config-package calfw-org
  :straight t
  :after (org calfw)
  :config
  (setq calendar-week-start-day 1))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
