;;; usr-early.el --- early-init-script                    -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(setq debug-on-error t)

(progn ; user
  (setq user-full-name "Tony Olagbaiye")
  (setq user-mail-address "frony0@gmail.com"))

(progn ; performance
  (setq gc-cons-threshold 50000000)
  (setq large-file-warning-threshold 100000000))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
