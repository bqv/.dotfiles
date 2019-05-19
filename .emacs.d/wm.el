;;; wm.el --- config-wm                    -*- lexical-binding: t -*-

(use-package exwm
  :straight t
  :preface
  (progn
    (defun bqv/exwm-reliable-class-p ()
      "Return t if application's class is suitable for naming."
      (and (not (string-prefix-p "sun-awt-X11-" exwm-instance-name))
           ;; gimp has several windows with the same class:
           (not (string= "gimp" exwm-instance-name))))

    (defun bqv/exwm-class-updated ()
      "Use class names if `bqv/exwm-reliable-class-p'."
      (when (bqv/exwm-reliable-class-p)
        (exwm-workspace-rename-buffer exwm-class-name)))

    (defun bqv/exwm-title-updated ()
      "Use title unless `bqv/exwm-reliable-class-p'."
      (unless (bqv/exwm-reliable-class-p)
        (exwm-workspace-rename-buffer exwm-class-name)))

    (defun bqv/exwm-setup ()
      "Setup exwm instance"
      (with-demoted-errors "Exwm: %S"
        (display-time-mode 1))))
  :init
  (setq display-time-default-load-average nil)
  :config
  (add-hook 'exwm-init-hook 'bqv/exwm-setup)
  (add-hook 'exwm-update-class-hook 'bqv/exwm-class-updated)
  (add-hook 'exwm-update-title-hook 'bqv/exwm-title-updated)
  (exwm-enable))

(use-package exwm-config
  :after (exwm))

(use-package map)

(use-package exwm-input
  :after (exwm map)
  :preface
  (progn
    (defconst bqv/exwm-applications
      (let ((map (make-hash-table)))
        (setf (map-elt map 'firefox)
              (cons "Firefox"
                    (lambda () (bqv/exwm-launch-desktop "firefox.desktop"))))
        (setf (map-elt map 'chromium)
              (cons "Chromium-browser"
                    (lambda ()
                      (start-process
                       "chromium-browser"
                       (generate-new-buffer "chromium-process")
                       "chromium-browser"
                       "--remote-debugging-port=9222"))))
        (setf (map-elt map 'pulseaudio)
              (cons "Pavucontrol"
                    (lambda () (bqv/exwm-launch-desktop "pavucontrol.desktop"))))
        (setf (map-elt map 'vbox)
              (cons "VirtualBox Machine"
                    (lambda ()
                      (start-process
                       "virtualbox"
                       (generate-new-buffer "virtualbox")
                       "VBoxManage"
                       "startvm"
                       "Windows 10 (v4)"))))
        (setf (map-elt map 'slack)
              (cons "Slack"
                    (lambda () (bqv/exwm-launch-desktop "com.slack.Slack.desktop"))))
        map)
      "Maps an application name symbol to a pair (BUFFER-NAME . DESKTOP-FILENAME).")

    (defun bqv/exwm-launch-desktop (desktop-filename)
      "Launch the application pointed to by DESKTOP-FILENAME."
      (call-process "gtk-launch" nil 0 nil desktop-filename))

    (defun bqv/exwm-app-switch/launch (name)
      "Switch to application NAME if already started, start it otherwise.
NAME is a key of `bqv/exwm-applications'."
      (interactive (list (intern (completing-read "Application" (map-keys bqv/exwm-applications) nil t))))
      (let* ((application (map-elt bqv/exwm-applications name))
             (buffer-name (car application))
             (start-application (cdr application)))
        (if-let* ((buffer (get-buffer buffer-name)))
            (if (display-buffer-reuse-window buffer '((reusable-frames . visible)))
                (pop-to-buffer buffer
                               (cons
                                #'display-buffer-reuse-window
                                '((reusable-frames . visible))))
              (switch-to-buffer buffer))
          (when (y-or-n-p (format "%s not started yet. Start it?" buffer-name))
            (funcall start-application)))))

    (bind-key  "C-. s f" (lambda () (interactive) (bqv/exwm-app-switch/launch 'firefox)))
    (bind-key  "C-. s c" (lambda () (interactive) (bqv/exwm-app-switch/launch 'chromium)))
    (bind-key  "C-. s p" (lambda () (interactive) (bqv/exwm-app-switch/launch 'pulseaudio)))
    (bind-key  "C-. s v" (lambda () (interactive) (bqv/exwm-app-switch/launch 'vbox)))
    (bind-key  "C-. s k" (lambda () (interactive) (bqv/exwm-app-switch/launch 'slack)))
    (bind-key  "C-. s r" (lambda () (interactive) (bqv/exwm-app-switch/launch 'riot))))

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

(use-package desktop-environment
  :straight t
  :after exwm-input
  :config
  (progn
    (desktop-environment-mode)))

(use-package buffer-move
  :straight t
  :after exwm-input
  :config
  (progn
    (exwm-input-set-key (kbd "<s-up>") #'buf-move-up)
    (exwm-input-set-key (kbd "<s-down>") #'buf-move-down)
    (exwm-input-set-key (kbd "<s-left>") #'buf-move-left)
    (exwm-input-set-key (kbd "<s-right>") #'buf-move-right)))

(use-package exwm-manage
  :after exwm
  :init
  (progn
    ;; https://github.com/ch11ng/exwm/issues/574
    (add-to-list 'exwm-manage-configurations
                 '((equal exwm-class-name "Slack") managed t))))

(use-package exwm-randr
  :after (exwm)
  :preface
  (progn
    (defun bqv/exwm-xrandr ()
      "Configure screen with xrandr."
      (setq xrandr-current
            (shell-command-to-string "xrandr"))))
  :init
  (progn
    (setq exwm-randr-workspace-monitor-plist
          '(0 "DisplayPort-1" 1 "HDMI-A-4" 2 "HDMI-A-3" 3 "DVI-D-0")))
  :config
  (add-hook 'exwm-randr-screen-change-hook 'bqv/exwm-xrandr)
  (progn
    (exwm-randr-enable)))

(use-package exwm-systemtray
  :after exwm
  :config
  (progn
    (exwm-systemtray-enable)))

(use-package exwm-workspace
  :after exwm
  :init
  (progn
    (setq exwm-workspace-number 4)
    (setq exwm-workspace-show-all-buffers t)
    (setq exwm-layout-show-all-buffers t)))

(use-package exwm-edit
  :straight t
  :after exwm)

(use-package pinentry
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

(use-package helm-exwm
  :straight t
  :after (exwm helm))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
