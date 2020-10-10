;;; early-init.el --- Artisanal Emacs. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author: Christo <http://github/critocrito>
;; Maintainer: Christo <christo@cryptodrunks.net>
;; Created: 2020
;; Modified: 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/critocrito/artisinal-emacs
;; Package-Requires: ((emacs 27))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;;  before package and UI initialization happens.
;;
;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Enforce Emacs 27.
(when (version< emacs-version "27")
  (error "Detected Emacs %s.  Artisinal Emacs only supports Emacs 27 and higher"
         emacs-version))

;; `file-name-handler-alist' is consulted on every `require', `load' and various
;; path/io functions. You get a minor speed up by nooping this.
(defvar ae/file-name-handler-alist-original file-name-handler-alist)
(defvar ae/initial-file-name-handler-alist '())

(unless noninteractive
  (setq file-name-handler-alist nil)

  ;; Restore `file-name-handler-alist', because it is needed for handling
  ;; encrypted or compressed files, among other things.
  (defun ae/reset-file-handler-alist-h ()
    ;; Re-add rather than `setq', because file-name-handler-alist may have
    ;; changed since startup, and we want to preserve those.
    (dolist (handler file-name-handler-alist)
      (add-to-list 'ae/initial-file-name-handler-alist handler))
    (setq file-name-handler-alist ae/initial-file-name-handler-alist))

  (add-hook 'emacs-startup-hook #'ae/reset-file-handler-alist-h))

;; In Emacs 27+, package initialization occurs before `user-init-file'
;; is loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

(provide 'early-init)
;;; early-init.el ends here
