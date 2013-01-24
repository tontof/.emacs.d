;; turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq inhibit-startup-message t)   ;; remove start message
(setq initial-scratch-message nil) ;; show nothing in *scratch* when started
(setq visible-bell t)              ;; replace bell by blink

;; disable version control and magic mode
(setq vc-handled-backends nil)
(setq magic-mode-alist nil)

;; disable local variables -*-
(setq enable-local-variables nil)

(column-number-mode 1)             ;; show column number
(defalias 'yes-or-no-p 'y-or-n-p)  ;; replace yes/no by y/n

;; keep just one window when multiple file opening
(add-hook 'emacs-startup-hook 'delete-other-windows)

;; save temporary/backup file in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; set utf-8 coding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; do not use tab
(setq-default indent-tabs-mode nil)
(setq c-default-style "bsd"
      c-basic-offset 4)

;; set background colors
(set-background-color "black")
(set-foreground-color "white")

;; set/load some paths 
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(setq setup-dir (expand-file-name "setup" user-emacs-directory))  
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path setup-dir)

(require 'mode-mappings)
(require 'key-bindings)