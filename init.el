;; turn off mouse interface early in startup to avoid momentary display
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq inhibit-startup-message t)          ;; remove start message
(setq initial-scratch-message nil)        ;; show nothing in *scratch* when started
(setq visible-bell t)                     ;; replace bell by blink
(setq-default frame-title-format '("%b")) ;; add buffer file name to emacs window

;; disable version control and magic mode
(setq vc-handled-backends nil)
(setq magic-mode-alist nil)

;; disable local variables -*-
(setq enable-local-variables nil)

;; (global-linum-mode 1)
;; (require 'linum)
;; (defun linum-update-window-scale-fix (win)
;;   "fix linum for scaled text"
;;   (set-window-margins win
;; 		      (ceiling (* (if (boundp 'text-scale-mode-step)
;; 				      (expt text-scale-mode-step
;; 					    text-scale-mode-amount) 1)
;; 				  (if (car (window-margins))
;; 				      (car (window-margins)) 1)
;; 				  ))))
;; (advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

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

;; auto complete: auto code completion
(setq auto-complete-dir (expand-file-name "auto-complete" user-emacs-directory))
(add-to-list 'load-path auto-complete-dir)
(require 'auto-complete-config)
(ac-config-default)
;; remove ac-comphist.dat file
(setq ac-comphist-file "")

;; highlight parentheses
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; do not automatically add new line at buffer end
(setq mode-require-final-newline nil)

;; dracula theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

(set-face-foreground 'font-lock-comment-face "light pink")

(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

;; custom mode + shortcuts
(require 'mode-mappings)
(require 'key-bindings)

