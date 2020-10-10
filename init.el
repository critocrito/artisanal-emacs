;;; init.el --- Artisanal Emacs. -*- lexical-binding: t; -*-
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
;;  This Emacs configuration works for me.  That is the purpose and the goal.
;;
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

(defvar better-gc-cons-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'.

If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)))

;; Ensure Emacs is running out of this file's directory.
(setq user-init-file (or load-file-name (buffer-file-name))
      user-emacs-directory (file-name-directory user-init-file))

;; Determin host environment.
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-bsd* (or *is-mac* (eq system-type 'berkeley-unix)))

(defvar ae/initial-load-path load-path)
(defvar ae/initial-process-environment process-environment)
(defvar ae/initial-exec-path exec-path)

;; Just the bare necessities
(require 'subr-x)
(require 'cl-lib)

(defun doom-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defmacro ae/hook! (hook &rest body)
  "Shorten declaration of HOOK by adding BODY to it."
  (declare (indent 1) (debug t))
  `(add-hook ,hook (lambda () ,@body)))

(defmacro ae/hook-modes! (modes &rest body)
  "Add to a list of MODES the hook BODY."
  (declare (indent 1) (debug t))
  `(dolist (mode ,modes)
     (ae/hook! (intern (format "%s-hook" mode)) ,@body)))

(defvar ae/transient-counter 0)
(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.
FORMS are evaluated once, when that function/hook is first
invoked, then never again.  HOOK-OR-FUNCTION can be a quoted hook
or a sharp-quoted function (which will be advised)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        ;; Avoid `make-symbol' and `gensym' here because an interned symbol is
        ;; easier to debug in backtraces (and is visible to `describe-function')
        (fn (intern (format "ae/transient-%d-h" (cl-incf ae/transient-counter)))))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (doom-unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append))))))

(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.
ARGLIST is as in `defun'.  WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.
\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (doom-enlist ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(if (fboundp 'with-eval-after-load)
    (defalias 'ae/after-load! 'with-eval-after-load)
  (defmacro ae/after-load! (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defun ae/keyword-name (keyword)
  "Return the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defmacro ae/log (format-string &rest args)
  "Log FORMAT-STRING to *Messages* if `ae/debug-p' is on.
Does not interrupt the minibuffer if it is in use, but still logs to *Messages*.
Accepts the same rest arguments ARGS as `message'."
  `(when ae/debug-p
     (let ((inhibit-message (active-minibuffer-window)))
       (message
        ,(concat (propertize "EMD " 'face 'font-lock-comment-face)
                 format-string)
        ,@args))))

;;
;;; Global Variables
(defvar ae/init-time nil
  "The time it took, in seconds, for Emacs to initialize.")

(defvar ae/debug-p (or (getenv "DEBUG") init-file-debug)
  "If non-nil, `Emacs, my dear!' will log more.")

;;; Directories/files
(defconst ae/emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory.  Must end with a slash.")

(defconst ae/local-dir
  (if-let (localdir (getenv "EMACSLOCALDIR"))
      (expand-file-name (file-name-as-directory localdir))
    (concat ae/emacs-dir ".local/"))
  "Root directory for local storage.
Use this as a storage location for this system's installation of EMD Emacs.
These files should not be shared across systems.  By default, it is used by
`ae/etc-dir' and `ae/cache-dir'.  Must end with a slash.")

(defconst ae/etc-dir (concat ae/local-dir "etc/")
  "Directory for non-volatile local storage.
Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data.  Must end with a slash.")

(defconst ae/cache-dir (concat ae/local-dir "cache/")
  "Directory for volatile local storage.
Use this for files that change often, like cache files.  Must end with a slash.")

(defconst ae/autoloads-file (concat ae/local-dir "autoloads.el")
  "Where `ae/reload-core-autoloads' stores its core autoloads.
This file is responsible for informing Emacs where to find all
autoloaded core functions (in core/autoload/*.el).")

(defconst ae/env-file (concat ae/local-dir "env")
  "The location of your envvar file.
This file contains environment variables scraped from your shell environment,
which is loaded at startup (if it exists).  This is helpful if Emacs can't
\(easily) be launched from the correct shell session (particularly for MacOS
users).")

;; Create all our core directories to quell file errors.
;; (mapc (apply-partially #'make-directory 'parents)
;;       (list ae/local-dir
;;             ae/etc-dir
;;             ae/cache-dir))

;;
;;; Emacs core configuration

;; lo', longer logs ahoy, so to reliably locate lapses in the logic later
(setq message-log-max 8192)

;; Reduce debug output, well, unless we've asked for it.
(setq debug-on-error ae/debug-p
      jka-compr-verbose ae/debug-p)

;; Contrary to what many Emacs users have in their configs, you really don't
;; need more than this to make UTF-8 the default coding system:
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please

;; Disable warnings from legacy advice system. They aren't useful, and what can
;; we do about them, besides changing packages upstream?
(setq ad-redefinition-action 'accept)

;; Make `apropos' et co search more extensively. They're more useful this way.
(setq apropos-do-all t)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (or that the user needs to stop relying on case
;; insensitivity).
(setq auto-mode-case-fold nil)

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Shave seconds off startup time by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
      ;; pull in a ton of packages.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be at
;; least a little more discerning.
(setq gnutls-verify-error (not (getenv "INSECURE"))
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not (version< emacs-version "26.3"))
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with `gnutls' by default, so `tls-program' would not be
      ;; used in that case. Otherwise, people have reasons to not go with
      ;; `gnutls', we use `openssl' instead. For more details, see
      ;; https://redd.it/8sykl1
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))

;; Emacs stores `authinfo' in $HOME and in plain-text. Let's not do that, mkay?
;; This file stores usernames, passwords, and other such treasures for the
;; aspiring malicious third party.
(setq auth-sources (list (concat ae/etc-dir "authinfo.gpg")
                         "~/.authinfo.gpg"))


;; Don't litter `ae/emacs-dir'. We don't use `no-littering' because it's a
;; mote too opinionated for our needs.
(setq abbrev-file-name             (concat ae/local-dir "abbrev.el")
      async-byte-compile-log-file  (concat ae/etc-dir "async-bytecomp.log")
      bookmark-default-file        (concat ae/etc-dir "bookmarks")
      custom-file                  (concat ae/emacs-dir "custom.el")
      desktop-dirname              (concat ae/etc-dir "desktop")
      desktop-base-file-name       "autosave"
      desktop-base-lock-name       "autosave-lock"
      pcache-directory             (concat ae/cache-dir "pcache/")
      request-storage-directory    (concat ae/cache-dir "request")
      shared-game-score-directory  (concat ae/etc-dir "shared-game-score/")
      tramp-auto-save-directory    (concat ae/cache-dir "tramp-auto-save/")
      tramp-backup-directory-alist backup-directory-alist
      tramp-persistency-file-name  (concat ae/cache-dir "tramp-persistency.el")
      url-cache-directory          (concat ae/cache-dir "url/")
      url-configuration-directory  (concat ae/etc-dir "url/"))

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1.0)

;;
;;; Optimizations

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless *is-mac*   (setq command-line-ns-option-alist nil))
(unless *is-linux* (setq command-line-x-option-alist nil))

;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason; inexplicably doubling startup time for terminal Emacs. Keeping
;;      it disabled will have nasty side-effects, so we simply delay it until
;;      later in the startup process and, for some reason, it runs much faster
;;      when it does.
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (ae/hook! 'window-setup-hook
    (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t)))

(defun ae/display-benchmark-h (&optional return-p)
  "Display a benchmark including number of packages and modules loaded.
If RETURN-P, return the message as a string instead of displaying it."
  (funcall
   (if return-p #'format #'message)
   "Emd loaded %d packages in %.03fs"
   (- (length load-path) (length ae/initial-load-path))
   (or ae/init-time
       (setq ae/init-time (float-time (time-subtract (current-time) before-init-time))))))

(add-hook 'window-setup-hook #'ae/display-benchmark-h 'append)

;;
;;;; Package Management
;;;;

;; Ensure that, if we do need package.el, it is configured correctly. You really
;; shouldn't be using it, but it may be convenient for quick package testing.
(setq package-enable-at-startup nil
      package-user-dir (concat ae/local-dir "elpa/")
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir)
      ;; I omit Marmalade because its packages are manually submitted rather
      ;; than pulled, so packages are often out of date with upstream.
      package-archives
      (let ((proto (if gnutls-verify-error "https" "http")))
        (list (cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
              (cons "melpa" (concat proto "://melpa.org/packages/"))
              (cons "org"   (concat proto "://orgmode.org/elpa/")))))

;; package.el has no business modifying the user's init.el
(advice-add #'package--ensure-init-file :override #'ignore)

;; Refresh package.el the first time you call `package-install', so it can still
;; be used (e.g. to temporarily test packages). Remember to run 'doom sync' to
;; purge them; they can conflict with packages installed via straight!
(add-transient-hook! 'package-install (package-refresh-contents))

(setq straight-base-dir ae/local-dir
      straight-repository-branch "develop"
      straight-cache-autoloads nil ; we already do this, and better.
      ;; Doom doesn't encourage you to modify packages in place. Disabling this
      ;; makes 'doom sync' instant (once everything set up), which is much nicer
      ;; UX than the several seconds modification checks.
      straight-check-for-modifications nil
      ;; We handle package.el ourselves (and a little more comprehensively)
      straight-enable-package-integration nil
      ;; Before switching to straight, `doom-local-dir' would average out at
      ;; around 100mb with half Doom's modules at ~230 packages. Afterwards, at
      ;; around 1gb. With shallow cloning, that is reduced to ~400mb. This has
      ;; no affect on packages that are pinned, however (run 'doom purge' to
      ;; compact those after-the-fact). Some packages break when shallow cloned
      ;; (like magit and org), but we'll deal with that elsewhere.
      straight-vc-git-default-clone-depth 1
      ;; Prefix declarations are unneeded bulk added to our autoloads file. Best
      ;; we don't have to deal with them at all.
      autoload-compute-prefixes nil
      ;; We handle it ourselves
      straight-fix-org nil)

(defvar bootstrap-version)
(let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(straight-use-package 'bind-key)
(straight-use-package
 '(blackout :host github :repo "raxod502/blackout"))

(use-package gcmh
  :straight t
  :blackout
  :hook (after-init . gcmh-mode)
  :commands (gcmh-set-high-threshold)
  :config
  ;; Adopt a sneaky garbage collection strategy of waiting until idle time to
  ;; collect; staving off the collector while the user is working.
  (setq gcmh-idle-delay 5
	gcmh-high-cons-threshold (* 16 1024 1024)  ; 16mb
	gcmh-verbose ae/debug-p))

(straight-use-package 'auto-minor-mode)
(straight-use-package
 '(explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode"))
(straight-use-package 'restart-emacs)

;;
;;; UI
;;;

;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

;; Typing yes/no is obnoxious when y/n will do
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;
;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(ae/hook-modes! '(eshell-mode-hook term-mode-hook)
  (setq hscroll-margin 0))

;;
;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;;
;;; Cursor

;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;;
;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;;
;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Artisinal Emacs")
      icon-title-format frame-title-format)

;; Don't resize windows & frames in steps; it's prohibitive to prevent the user
;; from resizing it to exact dimensions, and looks weird.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; These are disabled directly through their frame parameters, to avoid the
;; extra work their minor modes do, but we have to unset these variables
;; ourselves, otherwise users will have to cycle them twice to re-enable them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

(ae/hook! 'window-setup-hook
  (window-divider-mode))

;; always avoid GUI
(setq use-dialog-box nil)
;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
;; ...especially on linux
(when *is-linux*
  (setq x-gtk-use-system-tooltips nil))

 ;; Favor vertical splits over horizontal ones. Screens are usually wide.
(setq split-width-threshold 160
      split-height-threshold nil)

;; Highlight the current line.
(use-package hl-line
  :straight t
  :config
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost. I also don't need to see it in other buffers.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  (ae/hook-modes! '(prog-mode text-mode conf-mode special-mode)
    (hl-line-mode))

  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  (defvar ae/hl-line-mode nil)

  (ae/hook! 'activate-mark-hook
    (when hl-line-mode
      (setq-local ae/hl-line-mode t)
      (hl-line-mode -1)))

  (ae/hook! 'deactivate-mark-hook
    (when ae/hl-line-mode
      (hl-line-mode +1))))

;; undo/redo changes to Emacs' window layout
(use-package winner
  :straight t
  :preface (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
  :hook (after-init . winner-mode)
  :config
  (appendq! winner-boring-buffers
            '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
              "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
              "*esh command on file*")))

;; highlight matching delimiters
(use-package paren
  :straight t
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;;;###package whitespace
(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline newline-mark
        trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [?› ?\t])
        (newline-mark ?\n [?¬ ?\n])
        (space-mark ?\  [?·] [?.])))

(straight-use-package 'hide-mode-line)

;; Many major modes do no highlighting of number literals, so we do it for them
(use-package highlight-numbers
  :straight t
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package rainbow-delimiters
  :config
  (setq rainbow-delimiters-max-face-count 3))

(set-frame-font "Operator Mono Book 12")

;;
;;; Line numbers

;; Explicitly define a width to reduce computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions makes it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Enable line numbers in most text-editing modes. We avoid
;; `global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want them.
(ae/hook-modes! '(prog-mode-hook text-mode-hook conf-mode-hook)
  (display-line-numbers-mode))

(use-package doom-themes
    :straight t
    :config
    ;; Global settings (defaults)
    (setq
     ;; if nil, bold is universally disabled
     doom-themes-enable-bold t
     ;; if nil, italics is universally disabled
     doom-themes-enable-italic t
     ;; Underline looks a bit better when drawn lower
     x-underline-at-descent-line t)

    (load-theme 'doom-one t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Enable custom neotree theme (all-the-icons must be installed!)
    (doom-themes-neotree-config)
    ;; or for treemacs users
    (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
    (doom-themes-treemacs-config)

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

;; Don't display messages in the minibuffer when using the minibuffer
(defmacro ae/silence-motion-key (command key)
  "Avoid displaying KEY for COMMAND when in the minibuffer."
  (let ((key-command (intern (format "emd/silent-%s" command))))
    `(progn
       (defun ,key-command ()
         (interactive)
         (ignore-errors (call-interactively ',command)))
       (define-key minibuffer-local-map (kbd ,key) #',key-command))))
(ae/silence-motion-key backward-delete-char "<backspace>")
(ae/silence-motion-key delete-char "<delete>")

;;
;;;; Editor
;;;;

;;
;;; Clipboard / kill-ring

;; Cull duplicates in the kill ring to reduce bloat and make the kill ring
;; easier to peruse (with `counsel-yank-pop' or `helm-show-kill-ring'.
(setq kill-do-not-save-duplicates t)

;; Allow UTF or composed text from the clipboard, even in the terminal or on
;; non-X systems (like Windows or macOS), where only `STRING' is used.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; When popping a yank with M-y select what to pop.
(defun ae/yank-pop (&optional arg)
 "Call `yank-pop' with ARG when appropriate, or offer completion."
 (interactive "*P")
 (if arg (yank-pop arg)
   (let* ((old-last-command last-command)
          (selectrum-should-sort-p nil)
          (enable-recursive-minibuffers t)
          (text (completing-read
                 "Yank: "
                 (cl-remove-duplicates
                  kill-ring :test #'string= :from-end t)
                 nil t nil nil))
          ;; Find `text' in `kill-ring'.
          (pos (cl-position text kill-ring :test #'string=))
          ;; Translate relative to `kill-ring-yank-pointer'.
          (n (+ pos (length kill-ring-yank-pointer))))
     (unless (string= text (current-kill n t))
       (error "Could not setup for `current-kill'"))
     ;; Restore `last-command' over Selectrum commands.
     (setq last-command old-last-command)
     ;; Delegate to `yank-pop' if appropriate or just insert.
     (if (eq last-command 'yank)
         (yank-pop n) (insert-for-yank text)))))

(global-set-key [remap yank-pop] #'ae/yank-pop)

;;; Make window splitting bit more useful,
(defun ae/vsplit-same-buffer ()
  "Split the window vertically and switch to it."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))

(defun ae/vsplit-last-buffer ()
  "Split the window vertically and switch to next buffer."
  (interactive)
  (ae/vsplit-same-buffer)
  (switch-to-next-buffer))

(defun ae/hsplit-same-buffer ()
  "Split the window horizontally and switch to it."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))

(defun ae/hsplit-last-buffer ()
  "Split the window horizontally and switch to next buffer."
  (interactive)
  (ae/hsplit-same-buffer)
  (switch-to-next-buffer))

(bind-keys
 ;; Window management
 ("M-0" . delete-window)
 ("M-1" . delete-other-windows)
 ("M-2" . ae/vsplit-same-buffer)
 ("M-3" . ae/hsplit-same-buffer)
 ("C-x 2" . ae/vsplit-last-buffer)
 ("C-x 3" . ae/hsplit-last-buffer))

;; I use Selectrum over Ido, Ivy or Helm for incremental narrowing. I
;; combine it with prescient for better sorting and filtering.
(use-package selectrum
  :straight t
  :config
  (selectrum-mode +1))

(use-package prescient
  :straight t)

(use-package selectrum-prescient
  :straight t
  :after (selectrum prescient)
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

;; Use amx over the builtin M-x.
(use-package amx
  :straight t
  :hook (after-init . amx-mode)
  :after (selectrum)
  :config
  (setq amx-backend 'selectrum
	amx-save-file (concat ae/cache-dir "amx-items")))

;; a better *help* buffer
(use-package helpful
  :straight t
  :commands helpful--read-symbol
  :init
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol)

  ;; (defun ae/use-helpful-a (orig-fn &rest args)
  ;;   "Force ORIG-FN to use helpful instead of the old describe-* commands."
  ;;   (letf! ((#'describe-function #'helpful-function)
  ;;           (#'describe-variable #'helpful-variable))
  ;;     (apply orig-fn args)))
  )

;; patch apropos buttons to call helpful instead of help
;; (use-package apropos
;;   :after (helpful)
;;   :config
;;   (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
;;     (button-type-put
;;      fun-bt 'action
;;      (lambda (button)
;;        (helpful-callable (button-get button 'apropos-symbol)))))
;;   (dolist (var-bt '(apropos-variable apropos-user-option))
;;     (button-type-put
;;      var-bt 'action
;;      (lambda (button)
;;        (helpful-variable (button-get button 'apropos-symbol))))))

;;;###package imenu
(ae/hook! 'imenu-after-jump-hook
  (recenter))

(defvar indent-sensitive-modes '(python-mode
                                 slim-mode
                                 haskell-mode
                                 purescript-mode)
  "A group of modes that are sensitive to indentation.")

(defvar progish-modes '(prog-mode
                        css-mode)
  "A group of modes that are for programming.")

(defvar lispy-modes '(lisp-mode
                      lisp-interaction-mode
                      emacs-lisp-mode
                      ielm-mode
                      scheme-mode
                      racket-mode
                      clojure-mode
                      eval-expression-minibuffer-setup)
  "A group of modes that are LISP.")

(defvar writing-modes '(org-mode
                        text-mode
                        message-mode
                        markdown-mode)
  "A group of modes that are for writing.")

(use-package smartparens
  :straight t
  :hook (after-init . smartparens-global-mode)
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("M-F" . sp-forward-symbol)
              ("M-B" . sp-backward-symbol)
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              ("C-<down>" . sp-down-sexp)
              ("C-<up>" . sp-up-sexp)
              ("M-<down>" . sp-backward-down-sexp)
              ("M-<up>" . sp-backward-up-sexp)
              ("C-<right>" . sp-forward-slurp-sexp)
              ("C-<left>" . sp-backward-slurp-sexp)
              ("M-<right>" . sp-forward-barf-sexp)
              ("M-<left>" . sp-backward-barf-sexp)
              ("C-M-S-w" . sp-mark-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-k" . sp-kill-hybrid-sexp)
              ("M-k" . sp-backward-kill-sexp)
              ("M-<delete>" . sp-unwrap-sexp)
              ("M-<backspace>" . sp-backward-unwrap-sexp)
              ("C-M-t" . sp-transpose-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("M-r" . sp-raise-sexp)
              ("C-\"" . sp-change-inner)
              ("C-]" . sp-select-next-thing-exchange)
              ("C-M-]" . sp-select-next-thing)
              ("C-<left_bracket>" . sp-select-previous-thing))
  :config
  (defun ae/smartparens-add-space-after-sexp-insertion (id action _context)
    (when (eq action 'insert)
      (save-excursion
        (forward-char (sp-get-pair id :cl-l))
        (when (or (eq (char-syntax (following-char)) ?w)
                  (looking-at (sp--get-opening-regexp)))
          (insert " ")))))

  (defun ae/smartparens-add-space-before-sexp-insertion (id action _context)
    (when (eq action 'insert)
      (save-excursion
        (backward-char (length id))
        (when (or (eq (char-syntax (preceding-char)) ?w)
                  (and (looking-back (sp--get-closing-regexp) nil)
                       (not (eq (char-syntax (preceding-char)) ?'))))
          (insert " ")))))

  (require 'smartparens-config)

  ;;; lisp modes
  (sp-with-modes sp-lisp-modes
    (sp-local-pair "(" nil
                   :wrap "C-("
                   :pre-handlers '(ae/smartparens-add-space-before-sexp-insertion)
                   :post-handlers '(ae/smartparens-add-space-after-sexp-insertion)))

  (setq sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil
        sp-navigate-reindent-after-up nil
	;; Overlays are too distracting and not terribly helpful. show-parens does
	;; this for us already (and is faster), so...
	sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil
	;; No pair has any business being longer than 4 characters; if they must, set
	;; it buffer-locally. It's less work for smartparens.
	sp-max-pair-length 4
	;; The default is 100, because smartparen's scans are relatively expensive
	;; (especially with large pair lists for some modes), we reduce it, as a
	;; better compromise between performance and accuracy.
	sp-max-prefix-length 25)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  ;; Enable `smartparens-mode' in the minibuffer, during
  ;; `eval-expression' or `pp-eval-expression'.
  (ae/hook! 'minibuffer-setup-hook
    (and (memq this-command '(eval-expression pp-eval-expression))
           smartparens-global-mode
           (smartparens-mode)))

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)

  (show-smartparens-global-mode)

  (ae/hook-modes! lispy-modes
    (smartparens-strict-mode))

  (sp-with-modes sp-lisp-modes
    ;; disable ', it's the quote character!
    (sp-local-pair "'" nil :actions nil)
    ;; also only use the pseudo-quote inside strings where it serve as hyperlink.
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p)))

  (sp-with-modes '(emacs-lisp-mode)
    (sp-local-pair "[" nil :actions nil)))

(use-package which-key
  :straight t
  :blackout which-key-mode
  :commands which-key-mode
  :hook ((after-init . which-key-mode)
         (which-key-init-buffer-hook . (lambda () (setq line-spacing 3))))
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-idle-delay 1.0)
  :config
  ;; general improvements to which-key readability
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom))

;; a less intrusive `delete-trailing-whitespaces' on save
(use-package ws-butler
  :straight (:host github :repo "hlissner/ws-butler")
  :hook (after-init . ws-butler-global-mode))

;; Select windows without the use of the mouse or spatial navigation.
(use-package ace-window
  :straight t
  :defer t
  :bind* (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
	aw-scope 'frame
        aw-background t))

;; Highlight TODO/FIXME/NOTE tags in programming major-modes.
(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold))))

(use-package crux
  :straight t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c C-x r" . crux-rename-file-and-buffer)
         ("C-c C-x d" . crux-delete-file-and-buffer)
         ("C-c f" . crux-recentf-find-file)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c t" . crux-visit-term-buffer)
         ("C-^" . crux-top-join-line)))

;;
;;;; Syntax Checking

;;;###autoload
(defun +syntax-init-popups-h ()
  "Activate `flycheck-popup-tip-mode'.
Do nothing if `lsp-ui-mode' is active and `lsp-ui-sideline-enable' is non-nil."
  (unless (and (bound-and-true-p lsp-ui-mode)
               lsp-ui-sideline-enable)
    (flycheck-popup-tip-mode +1)))

(use-package flycheck
  :straight t
  :commands flycheck-list-errors flycheck-buffer
  :hook (after-init . global-flycheck-mode)
  :config
  (setq
   ;; Display errors a little quicker (default is 0.9s)
   flycheck-display-errors-delay 0.25
   ;; Check only when saving or opening files. Newline & idle checks are a mote
   ;; excessive and can catch code in an incomplete state, producing false
   ;; positives, so we removed them.
   flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch)
   ;; For the above functionality, check syntax in a buffer that you switched to
   ;; only briefly. This allows "refreshing" the syntax check state for several
   ;; buffers quickly after e.g. changing a config file.
   flycheck-buffer-switch-check-intermediate-buffers t))

(use-package flycheck-popup-tip
  :straight t
  :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
  :hook (flycheck-mode . +syntax-init-popups-h)
  :config
  (setq flycheck-popup-tip-error-prefix "X "))

;;
;;;; Completion

;;;###autoload
(defvar +company-backend-alist
  '((text-mode company-dabbrev company-yasnippet company-ispell)
    (prog-mode company-capf company-yasnippet)
    (conf-mode company-capf company-dabbrev-code company-yasnippet))
  "An alist matching modes to company backends.  The backends for any mode is built from this.")

;;;###autodef
(defun set-company-backend! (modes &rest backends)
  "Prepend BACKENDS (in order) to `company-backends' in MODES.
MODES should be one symbol or a list of them, representing major or minor modes.
This will overwrite backends for MODES on consecutive uses.
If the car of BACKENDS is nil, unset the backends for MODES.
Examples:
  (set-company-backend! 'js2-mode
    'company-tide 'company-yasnippet)
  (set-company-backend! 'sh-mode
    '(company-shell :with company-yasnippet))
  (set-company-backend! '(c-mode c++-mode)
    '(:separate company-irony-c-headers company-irony))
  (set-company-backend! 'sh-mode nil)  ; unsets backends for `sh-mode'"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (if (null (car backends))
        (setq +company-backend-alist
              (delq (assq mode +company-backend-alist)
                    +company-backend-alist))
      (setf (alist-get mode +company-backend-alist)
            backends))))

(use-package company
  :straight t
  :commands company-complete-common company-manual-begin company-grab-line
  :hook (after-init . global-company-mode)
  :init
  (setq company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not erc-mode message-mode help-mode gud-mode)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)

        ;; Buffer-local backends will be computed when loading a major mode, so
        ;; only specify a global default here.
        company-backends '(company-capf)

        ;; These auto-complete the current selection when
        ;; `company-auto-complete-chars' is typed. This is too magical. We
        ;; already have the much more explicit RET and TAB.
        company-auto-complete nil
        company-auto-complete-chars nil

        ;; Only search the current buffer for `company-dabbrev' (a backend that
        ;; suggests text your open buffers). This prevents Company from causing
        ;; lag once you have a lot of buffers open.
        company-dabbrev-other-buffers nil
        ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
        ;; domain-specific words with particular casing.
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil))

;;
;;;; Search
(use-package ctrlf
  :straight t
  :hook (after-init . ctrlf-mode))

;;
;;;; Projects and Navigation
;;;;
(defvar ae/projectile-cache-limit 10000
  "If any project cache surpasses this many files it is purged when quitting Emacs.")

(defvar ae/projectile-cache-blacklist '("~" "/tmp" "/")
  "Directories that should never be cached.")

(use-package projectile
  :straight t
  :after (selectrum)
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-cache-file (concat ae/cache-dir "projectile.cache")
        ;; Auto-discovery is slow to do by default. Better to update the list
        ;; when you need to (`projectile-discover-projects-in-search-path').
        projectile-auto-discover nil
        ;; projectile-enable-caching doom-interactive-p
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file (concat ae/cache-dir "projectile.projects")
        projectile-ignored-projects '("~/" "/tmp")
	projectile-completion-system 'default)
  :config
  (add-transient-hook! 'projectile-relevant-known-projects
    (projectile-cleanup-known-projects)
    (projectile-discover-projects-in-search-path))

  ;; In the interest of performance, we reduce the number of project root marker
  ;; files/directories projectile searches for when resolving the project root.
  (setq projectile-project-root-files-bottom-up
        (append '(".projectile"  ; projectile's root marker
                  ".git")        ; Git VCS root dir
                (when (executable-find "hg")
                  '(".hg"))      ; Mercurial VCS root dir
                (when (executable-find "bzr")
                  '(".bzr")))    ; Bazaar VCS root dir
        ;; This will be filled by other modules. We build this list manually so
        ;; projectile doesn't perform so many file checks every time it resolves
        ;; a project's root -- particularly when a file has no project.
        projectile-project-root-files '()
        projectile-project-root-files-top-down-recurring '("Makefile"))

  ;; Treat current directory in dired as a "file in a project" and track it
  (ae/hook! 'dired-before-readin-hook
    (projectile-track-known-projects-find-file-hook))

  ;; Accidentally indexing big directories like $HOME or / will massively bloat
  ;; projectile's cache (into the hundreds of MBs). This purges those entries
  ;; when exiting Emacs to prevent slowdowns/freezing when cache files are
  ;; loaded or written to.
  ;; Purge projectile cache entries that:
  ;; a) have too many files (see `ae/projectile-cache-limit'),
  ;; b) represent blacklisted directories that are too big, change too often or are
  ;;    private. (see `ae/projectile-cache-blacklist'),
  ;; c) are not valid projectile projects.
  (ae/hook! 'kill-emacs-hook
    (when (and (bound-and-true-p projectile-projects-cache)
               projectile-enable-caching)
      (projectile-cleanup-known-projects)
      (projectile-serialize-cache)))

  (projectile-mode +1)

  (ae/after-load! 'projectile
    (setq-default
     projectile-mode-line
     '(:eval
       (if (file-remote-p default-directory)
           " Proj"
         (format " Proj[%s]" (projectile-project-name)))))))

;;
;;;; Programming
;;;;

(use-package highlight-quoted
  :straight t
  :commands (highlight-quoted-mode)
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;;
;;;; Formatting code buffers

(use-package apheleia
  :straight (:host github :repo "raxod502/apheleia")
  :hook (prog-mode . apheleia-global-mode)
  :config
  (defun +format/buffer (&optional arg)
  "Reformat the current buffer using LSP or `format-all-buffer'."
  (interactive "P")
  (call-interactively
   (if (and (bound-and-true-p lsp-mode)
            (lsp-feature? "textDocument/formatting"))
       #'lsp-format-buffer
     #'apheleia-format-buffer))))

;;
;;;; Lookup things

(use-package dumb-jump
  :straight t
  :hook (dumb-jump-after-jump . better-jumper-set-jump)
  :commands dumb-jump-result-follow
  :config
  (setq dumb-jump-default-project ae/emacs-dir
        dumb-jump-prefer-searcher 'rg
        dumb-jump-aggressive nil
        ;; dumb-jump-selector ('popup)
	))

;;
;;;; Language Server Protocol

(defun ae/lsp-init-optimizations-h ()
  "Deploys universal optimizations for `lsp-mode'."
  (when (bound-and-true-p lsp-mode)
    ;; `read-process-output-max' is only available on recent development
    ;; builds of Emacs 27 and above.
    (setq-local read-process-output-max (* 1024 1024))
    ;; REVIEW LSP causes a lot of allocations, with or without Emacs 27+'s
    ;;        native JSON library, so we up the GC threshold to stave off
    ;;        GC-induced slowdowns/freezes. We use `gcmh' to enforce its GC
    ;;        strategy, so we modify its variables rather than
    ;;        `gc-cons-threshold' directly.
    (setq-local gcmh-high-cons-threshold (* 2 (default-value 'gcmh-high-cons-threshold)))
    (gcmh-set-high-threshold)))

(use-package lsp-mode
  :straight t
  :commands lsp-install-server
  :hook (lsp-mode . ae/lsp-init-optimizations-h)
  :init
  (setq
   lsp-session-file (concat ae/etc-dir "lsp-session")
   ;; For `lsp-clients'
   lsp-server-install-dir (concat ae/etc-dir "lsp/")
   ;; Don't auto-kill LSP server after last workspace buffer is killed, because I
   ;; will do it for you, after `+lsp-defer-shutdown' seconds.
   lsp-keep-workspace-alive nil
   ;; Disable features that have great potential to be slow.
   lsp-enable-file-watchers nil
   lsp-enable-folding nil
   lsp-enable-text-document-color nil
   ;; Disable features that modify our code without our permission.
   lsp-enable-indentation nil
   lsp-enable-on-type-formatting nil))

(use-package lsp-ui
  :straight t
  :defer t
  :config
  (setq
   lsp-ui-doc-max-height 8
   lsp-ui-doc-max-width 35
   lsp-ui-sideline-ignore-duplicate t
   ;; lsp-ui-doc is redundant with and more invasive than
   ;; `+lookup/documentation'
   lsp-ui-doc-enable nil
   ;; Don't show symbol definitions in the sideline. They are pretty noisy,
   ;; and there is a bug preventing Flycheck errors from being shown (the
   ;; errors flash briefly and then disappear).
   lsp-ui-sideline-show-hover nil))

;; Rust
(use-package rustic
  :straight t
  :after (flycheck lsp-mode)
  :mode ("\\.rs$" . rustic-mode)
  :commands rustic-run-cargo-command rustic-cargo-outdated
  :config
  (setq rustic-lsp-client 'lsp-mode)
  (add-to-list 'flycheck-checkers 'rustic-clippy))

(provide 'init)
;;; init.el ends here
