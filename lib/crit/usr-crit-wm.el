;;; wm.el --- config-wm                    -*- lexical-binding: t -*-

(config-package exwm
  :straight t
  :init
  (progn
    (defun bqv/exwm-reliable-class-p ()
      "Return t if application's class is suitable for naming."
      (and (not (string-prefix-p "sun-awt-X11-" exwm-instance-name))
           ;; gimp has several windows with the same class:
           (not (string= "gimp" exwm-instance-name))))

    (defun bqv/exwm-class-updated ()
      "Use class names if `bqv/exwm-reliable-class-p'."
      (when (bqv/exwm-reliable-class-p)
        (exwm-workspace-rename-buffer
         (case exwm-class-name
           (("chromium-browser-chromium") "chromium")
           (otherwise exwm-class-name)))))

    (defun bqv/exwm-title-updated ()
      "Use title unless `bqv/exwm-reliable-class-p'."
      (unless (bqv/exwm-reliable-class-p)
        (exwm-workspace-rename-buffer
         (case exwm-class-name
           (("chromium-browser-chromium") "chromium")
           (otherwise exwm-class-name)))))

    (setq bqv/exwm-screens
          '(("DisplayPort-1" ("1920x1080" "0x270" nil))
            ("HDMI-A-4" ("1920x1080" "1920x0" "1.25x1.25"))
            ("HDMI-A-3" ("1920x1080" "4320x270" nil))))

    (defun bqv/exwm-setup ()
      "Setup exwm instance"
      (with-demoted-errors "Exwm: %S"
        (setq display-time-day-and-date t)
        (display-time-mode t))
      (with-demoted-errors "Exwm: %S"
        (with-temp-buffer
          (shell-command
            (format "xrandr %s"
                    (mapconcat (lambda (scr)
                                 (let ((output (car scr))
                                       (mode (car (cadr scr)))
                                       (pos (cadr (cadr scr)))
                                       (style (caddr (cadr scr))))
                                   (format "%s %s %s %s"
                                           (format "--output %s" output)
                                           (format "--mode %s" mode)
                                           (format "--pos %s" pos)
                                           (if (null style)
                                               "--auto"
                                             (format "--scale %s" style)))))
                               bqv/exwm-screens
                               " ")))))
      nil)

    (defun bqv/exwm-init-advice (old-function &rest args)
      (flet ((rawdisplay
              (str)
              (let ((dsp (replace-regexp-in-string "^:\\|\\.0$" "" str)))
                (string-to-number dsp))))
        (unless (>= 50 (rawdisplay
                        (frame-parameter (selected-frame) 'display)))
            (apply old-function args)))))
  :config
  ;(advice-add 'exwm-init
  ;            :around 'bqv/exwm-init-advice)
  (add-hook 'exwm-init-hook 'bqv/exwm-setup)
  (add-hook 'exwm-update-class-hook 'bqv/exwm-class-updated)
  (add-hook 'exwm-update-title-hook 'bqv/exwm-title-updated)
  (setq exwm-debug-on t)
  (exwm-enable))

(config-package exwm-config
  :after (exwm))

(config-package map)

(config-package cl)

(config-package exwm-input
  :after (exwm map cl)
  :init
  (progn ;;; TODO: Use sd-bus for all of this
    (defstruct service
      "Systemd Service"
      session
      name description
      loadstate activestate substate
      followed path
      jobid jobtype jobpath)

    (defconst bqv/exwm-applications
      (let ((map (make-hash-table)))
        (setf (map-elt map 'firefox)
              (cons "Firefox" (list "firefox")))
        (setf (map-elt map 'chromium)
              (cons "Chromium" (list "chromium" "--remote-debugging-port=9222")))
        (setf (map-elt map 'qutebrowser)
              (cons "QuteBrowser" (list "qutebrowser")))
        (setf (map-elt map 'slack)
              (cons "Slack" (list "slack")))
        (setf (map-elt map 'discord)
              (cons "Discord" (list "discord")))
        map)
      "Maps an application name symbol to a pair (BUFFER-NAME . INVOCATION).")
    (defconst bqv/exwm-services
      (let ((map (make-hash-table)))
        (setf (map-elt map 'ckb)
              (cons "ckb" (list "ckb" "-b")))
        (setf (map-elt map 'dunst)
              (cons "dunst" (list "dunst")))
        map)
      "Maps an service name symbol to a pair (PROCESS-NAME . INVOCATION).")

    (defun bqv/systemd-scopes ()
      (mapcar (lambda (scp)
                (cons (reverse (substring (reverse (car scp)) 6))
                      (make-service :session 'user
                                    :name (nth 1 scp)
                                    :description (nth 2 scp)
                                    :loadstate (nth 3 scp)
                                    :activestate (nth 4 scp)
                                    :substate (nth 5 scp)
                                    :followed (nth 6 scp)
                                    :path (nth 7 scp)
                                    :jobid (nth 8 scp)
                                    :jobtype (nth 9 scp)
                                    :jobpath (nth 10 scp))))
              (let ((user-mode t))
                (seq-filter (lambda (srv) (string-suffix-p ".scope" (car srv)))
                            (dbus-call-method
                             (if user-mode :session :system)
                             "org.freedesktop.systemd1"
                             "/org/freedesktop/systemd1"
                             "org.freedesktop.systemd1.Manager"
                             "ListUnits"
                             :timeout 1000)))))

    (defun bqv/systemd-scope-running-p (name)
      (ecase (intern (service-activestate
                      (cdr (assoc name (bqv/systemd-scopes)))))
        (failed nil)
        (running t)))

    (defun bqv/exwm-desktop-invocation (desktop-filename)
      "Launch the application pointed to by DESKTOP-FILENAME."
      (assert (stringp desktop-filename))
      (list "gtk-launch" desktop-filename))

    (defun bqv/exwm-launch (name executable &rest args)
      (let* ((command (cons executable args))
             (invocation (append `("systemd-run" "--scope" "--user"
                                   ,(concat "--unit=" (downcase name))
                                   "--collect")
                                 command)))
        (apply #'start-process
               (append `(,name ,(get-buffer-create (concat "*" name "*")))
                       invocation))))

    (defun bqv/exwm-harvest (name)
      (let ((invocation `("systemctl" "--user" "stop"
                          ,(concat (downcase name) ".scope"))))
        (apply #'call-process
               (append `(,(car invocation) nil nil nil)
                       (cdr invocation)))))

    (defun bqv/exwm-scan (name)
      (let ((invocation `("systemctl" "--user" "show"
                          ,(concat (downcase name) ".scope")
                          "--no-page")))
        (with-temp-buffer
          (apply #'call-process
                 (append `(,(car invocation) nil t nil)
                         (cdr invocation)))
          (goto-char (point-min))
          (not (null (save-excursion (search-forward "Transient=yes" nil t)))))))

    (defun bqv/exwm-screenshot (&rest args)
      (let ((incantation `("systemd-run" "--scope" "--user" "--user=scrot" "--collect"
                           "scrot" ,@args "-e" "mv $f ~/var/cap/")))
        (apply #'start-process
               (append '("scrot" nil) incantation))))

    (defun bqv/exwm-screenshot-all ()
      (interactive)
      (bqv/exwm-screenshot-all))

    (defun bqv/exwm-screenshot-current ()
      (interactive)
      (bqv/exwm-screenshot-all "-u"))

    (defun bqv/exwm-screenshot-region ()
      (interactive)
      (bqv/exwm-screenshot-all "-s"))

    (defun executables-list ()
      "Returns a list of all files available in the directories of the $PATH variable."
      (remove-if-not #'file-executable-p
                     (remove-if #'file-directory-p
                                (apply #'append
                                       (mapcar (lambda (p) (directory-files p t))
                                               (remove-if-not #'file-exists-p
                                                              (split-string (getenv "PATH") ":")))))))

    (defun bqv/exwm-svc-start (name)
      "Start NAME unless already started. NAME is a key of `bqv/exwm-services'."
      (interactive (list (intern (completing-read "Service" (map-keys bqv/exwm-services) nil t))))
      (let* ((service (map-elt bqv/exwm-services name))
             (buffer-name (concat "*" (car service) "*")))
        (assert (not (null service)))
        (if-let* ((buffer (get-buffer buffer-name)))
          (when (y-or-n-p (format "%s was started already.  Start it again? " (car service)))
            (apply #'bqv/exwm-launch service))
          (apply #'bqv/exwm-launch service))))

    (defun bqv/exwm-app-join (name)
      "Switch to application NAME if already started, start it otherwise.
NAME is a key of `bqv/exwm-applications'."
      (interactive (list (intern (completing-read "Application" (map-keys bqv/exwm-applications) nil t))))
      (let* ((application (map-elt bqv/exwm-applications name))
             (buffer-name (car application)))
        (assert (not (null application)))
        (if (and (bqv/exwm-scan (car (cdr application)))
                 (y-or-n-p (format "%s is already started.  Kill it? " buffer-name)))
            (bqv/exwm-harvest (car (cdr application)))
          (apply #'bqv/exwm-launch application))))

    (defun bqv/exwm-exec (cmd)
      "Launch a shell command CMD."
      (interactive (list (completing-read "Command" (mapcar #'file-name-base (executables-list)))))
      (let* ((executable (car (split-string cmd " ")))
             (buffer-name (concat "*" executable "*")))
        (assert (not (null executable)))
        (bqv/exwm-launch executable "sh" "-c" cmd)))

    (defun app/qutebrowser ()
      (interactive)
      (bqv/exwm-app-join 'qutebrowser))

    (defun app/slack ()
      (interactive)
      (bqv/exwm-app-join 'slack))

    (defun app/discord ()
      (interactive)
      (bqv/exwm-app-join 'discord))

    (defun bqv/exwm-setup-desktop ()
      "Setup exwm desktop"
      (with-demoted-errors "Exwm: %S"
        (bqv/exwm-svc-start 'ckb)
        (bqv/exwm-svc-start 'dunst)))

    (add-hook 'exwm-init-hook 'bqv/exwm-setup-desktop)
    (bind-key  "C-. s f" (lambda () (interactive) (bqv/exwm-app-join 'firefox)))
    (bind-key  "C-. s c" (lambda () (interactive) (bqv/exwm-app-join 'chromium)))
    (bind-key  "C-. s k" (lambda () (interactive) (bqv/exwm-app-join 'slack))))

  :config
  (progn
    ;; Key bindings accessible from everywhere:
    (exwm-input-set-key (kbd "s-r") #'exwm-reset)
    (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
    (exwm-input-set-key (kbd "s-;") #'other-frame)

    (exwm-input-set-key (kbd "<s-tab>") #'bqv/swap-last-buffers)
    (exwm-input-set-key (kbd "C-x w") #'bqv/switch-to-window)
    (exwm-input-set-key (kbd "C-;") #'other-window)
    (exwm-input-set-key (kbd "s-!") #'counsel-linux-app)
    (exwm-input-set-key (kbd "C-M-'") #'shell-switcher-new-shell)
    (exwm-input-set-key (kbd "C-'") #'shell-switcher-switch-buffer)
    (exwm-input-set-key (kbd "C-M-v") #'scroll-other-window)

    ;; Bind C-q so that the next key is sent literally to the
    ;; application
    (add-to-list 'exwm-input-prefix-keys ?\C-q)
    (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

    (add-to-list 'exwm-input-prefix-keys ?\C-.)
    (add-to-list 'exwm-input-prefix-keys ?\C-,)

    (setq exwm-input-simulation-keys
          `(
            ;; movement
            ([?\C-b] . [left])
            ([?\M-b] . [C-left])
            ([?\C-f] . [right])
            ([?\M-f] . [C-right])
            ([?\C-p] . [up])
            ([?\C-n] . [down])
            ([?\C-a] . [home])
            ([S-left] . [C-home])
            ([S-right] . [C-end])
            ([?\C-e] . [end])
            ([?\M-v] . [prior])
            ([?\C-v] . [next])
            ([?\C-d] . [delete])
            ([?\C-k] . [S-end ?\C-x])
            ;; cut/paste, selection
            ([?\C-w] . [?\C-x])
            ([?\M-w] . [?\C-c])
            ([?\C-y] . [?\C-v])
            ([?\M-d] . [C-S-right ?\C-x])
            ([M-backspace] . [C-S-left ?\C-x])
            ;; search
            ([?\C-s] . [?\C-f])
            ;; escape
            ([?\C-g] . [escape])))))

(config-package desktop-environment
  :straight t
  :after exwm-input
  :config
  (progn
    (desktop-environment-mode)))

(config-package buffer-move
  :straight t
  :after exwm-input
  :config
  (progn
    (exwm-input-set-key (kbd "<s-up>") #'buf-move-up)
    (exwm-input-set-key (kbd "<s-down>") #'buf-move-down)
    (exwm-input-set-key (kbd "<s-left>") #'buf-move-left)
    (exwm-input-set-key (kbd "<s-right>") #'buf-move-right)))

(config-package exwm-manage
  :after exwm
  :init
  (progn
    ;; https://github.com/ch11ng/exwm/issues/574
    (add-to-list 'exwm-manage-configurations
                 '((equal exwm-class-name "Slack") managed t))))

(config-package exwm-randr
  :after (exwm exwm-workspace exwm-manage)
  :init
  (progn
    (defun bqv/exwm-xrandr ()
      "Configure screen with xrandr."
      (setq xrandr-current
            (shell-command-to-string "xrandr")))
    (defun bqv/exwm-pinning ()
      (ignore-errors
        (let* ((wmap exwm-workspace-window-assignments)
               (target (cdr (assoc exwm-class-name wmap))))
          (exwm-workspace-move-window target)))))
  (progn
    (defvar exwm-workspace-window-assignments
      '(("chromium-browser-chromium" . 2)
        ("ansi-term" . 0))
      "An alist of windows and what workspace to put them on.")
    (setq exwm-randr-workspace-monitor-plist
          '(0 "DisplayPort-1" 1 "DVI-D-0" 2 "HDMI-A-3" 3 "HDMI-A-4")))
  :config
  (progn
    (add-hook 'exwm-randr-screen-change-hook 'bqv/exwm-xrandr)
    (add-hook 'exwm-manage-finish-hook 'bqv/exwm-pinning)
    (exwm-randr-enable)))

(config-package exwm-systemtray
  :after exwm
  :config
  (progn
    (exwm-systemtray-enable)))

(config-package exwm-workspace
  :after exwm
  :init
  (progn
    (setq exwm-workspace-number 4)
    (setq exwm-workspace-show-all-buffers t)
    (setq exwm-layout-show-all-buffers t)
    (setq exwm-workspace-minibuffer-position 'top)
    (defun bqv/exwm-minibuffer-advice (old-function &rest args)
      (flet ((rawdisplay
              (str)
              (replace-regexp-in-string "\\.0$" "" str)))
        (if (ignore-errors
              (string-equal
               (rawdisplay (slot-value exwm--connection 'display))
               (rawdisplay (frame-parameter (selected-frame) 'display))))
            (apply old-function args)))))
  :config
  (advice-add 'exwm-workspace--on-minibuffer-setup
              :around 'bqv/exwm-minibuffer-advice))

(config-package exwm-edit
  :straight t
  :after exwm)

(config-package pinentry
  :straight t
  :after exwm
  :init
  (progn
    ;; Use gpg as ssh agent (for this to work in shells, also set the
    ;; ENV var in $HOME/.profile or similar.
    (setenv "SSH_AUTH_SOCK"
            (string-trim
             (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket"))))
  :config
  (progn
    (pinentry-start)

    (defun bqv/pinentry-restart ()
      "Kill and restart gpg-agent and pinentry."
      (interactive)
      (delete-process "pinentry")
      (shell-command "gpgconf --kill gpg-agent")
      (pinentry-start)
      (message "gpg-agent and pinentry restarted successfully."))))

(config-package ivy-exwm
  :straight (ivy-exwm :type git :host github :repo "pjones/ivy-exwm")
  :after (exwm ivy)
  :config
  (ivy-exwm-mode t))

(config-package all-the-icons
  :straight t)

(config-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

Process xrandr finished
