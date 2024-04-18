
;; LISP
(eval-after-load "lisp-mode"
  '(progn
     ;; http://www.opensource.apple.com/source/emacs/emacs-39/emacs/lisp/emacs-lisp/bytecomp.el
     (defcustom emacs-lisp-file-regexp (if (eq system-type 'vax-vms)
                                           "\\.EL\\(;[0-9]+\\)?$"
                                         "\\.el$")
       "*Regexp which matches Emacs Lisp source files.
You may want to redefine the function `byte-compile-dest-file'
if you change this variable."
       :group 'bytecomp
       :type 'regexp)

     (defun byte-compiler-base-file-name (filename)
       (let ((handler (find-file-name-handler filename
                                              'byte-compiler-base-file-name)))
         (if handler
             (funcall handler 'byte-compiler-base-file-name filename)
           filename)))

     (defun byte-compile-dest-file (filename)
       "Convert an Emacs Lisp source file name to a compiled file name."
       (setq filename (byte-compiler-base-file-name filename))
       (setq filename (file-name-sans-versions filename))
       (cond ((eq system-type 'vax-vms)
              (concat (substring filename 0 (string-match ";" filename)) "c"))
             ((string-match emacs-lisp-file-regexp filename)
              (concat (substring filename 0 (match-beginning 0)) ".elc"))
             (t (concat filename ".elc"))))

     (defun byte-compile-current-buffer ()
       "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
       (interactive)
       (when (and (eq major-mode 'emacs-lisp-mode)
                  (file-exists-p (byte-compile-dest-file buffer-file-name)))
         (byte-compile-file buffer-file-name)))
     ;; auto compile lisp file
     ;; http://ergoemacs.org/emacs/organize_your_dot_emacs.html
     (add-hook 'after-save-hook 'byte-compile-current-buffer)))

;; PHP
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(eval-after-load "web-mode"
  '(progn
     ((lambda ()
        (require 'emmet-mode)
        (add-hook 'web-mode-hook  'emmet-mode)
        ))))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html" . mhtml-mode))
(eval-after-load "mhtml-mode"
  '(progn
     ((lambda ()
        (require 'yasnippet)
        (require 'emmet-mode)
        (add-hook 'html-mode-hook
                  (lambda ()
                    (emmet-mode 1)
                    ))
        ))))

;; Less
(require 'less-css-mode)

;; CSS
(eval-after-load "css-mode"
  '(progn
     (setq css-indent-offset 2)))


;; JAVASCRIPT
(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(eval-after-load "js3-mode"
  '(progn
    (add-to-list 'ac-modes 'js3-mode)
    (setq js3-auto-indent-p t)
    (setq js3-enter-indents-newline t) 
    (setq js3-indent-on-enter-key t)))

(eval-after-load "js-mode"
  '(progn
    (setq js-indent-level 2)))
  
  
;; COFFEESCRIPT
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(eval-after-load "coffee-mode"
  '(progn
     (setq coffee-tab-width 2)))

;; ORG
(eval-after-load "org"
  '(progn
     ((lambda ()
        (require 'ox-reveal)
        (require 'htmlize)
        (setq org-ditaa-jar-path "~/.emacs.d/ditaa/ditaa.jar")
        (org-babel-do-load-languages
         'org-babel-load-languages
         '((ditaa . t))) ; this line activates ditaa
        (setq org-html-htmlize-output-type `nil)))))

;; OCaml
(add-to-list 'auto-mode-alist '("\\.ml[ip]?\\'" . (lambda ()
                                                    ;; add major mode setting here, if needed, for example:
                                                    (require 'tuareg)
                                                    (tuareg-mode)
                                                    )))

;; Rust
(add-to-list 'auto-mode-alist '("\\.rs\\'" . (lambda ()
                                               ;; add major mode setting here, if needed, for example:
                                               (require 'rust-mode)
                                               (rust-mode)
                                               )))

;; Text mode
(add-hook 'text-mode-hook 'auto-complete-mode)

(provide 'mode-mappings)

