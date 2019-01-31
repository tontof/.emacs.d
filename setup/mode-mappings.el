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

;; SGML
(add-to-list 'auto-mode-alist '("\\.tpl.php[345]?" . sgml-mode))
(eval-after-load "sgml-mode"
  '(progn
     ((lambda ()
        (require 'rename-sgml-tag)
        (define-key sgml-mode-map (kbd "C-c C-e") 'sgml-close-tag)
        (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)
        (add-hook 'sgml-mode-hook
	  (lambda ()
            (require 'yasnippet)
	    (require 'zencoding-mode)
	    (zencoding-mode 1)
	    (define-key zencoding-mode-keymap (kbd "C-c C-j") 'zencoding-expand-line)))))))

;; PHP
(autoload 'php-mode "php-mode" t)
(add-to-list 'auto-mode-alist '("\\.php[345]?" . php-mode))
(eval-after-load "php-mode"
  '(progn
     ((lambda ()
        (require 'multi-web-mode)
        (define-key php-mode-map (kbd "C-c /") 'comment-or-uncomment-region)
        (setq mweb-default-major-mode 'html-mode)
        (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                          (js-mode "<script\\( +type=\"text/javascript\"\\| *\\)[^>]*>" "</script>")
                          (css-mode "<style\\( +type=\"text/css\"\\| *\\)[^>]*>" "</style>")))
        (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
        (multi-web-global-mode 1)))))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html" . sgml-mode))
(eval-after-load "sgml-mode"
  '(progn
     ((lambda ()
        (require 'multi-web-mode)
        (setq mweb-default-major-mode 'html-mode)
        (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                          (js-mode "<script\\( +type=\"text/javascript\"\\| *\\)[^>]*>" "</script>")
                          (css-mode "<style\\( +type=\"text/css\"\\| *\\)[^>]*>" "</style>")))
        (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
        (multi-web-global-mode 1)))))

;; CSS
(eval-after-load "css-mode"
  '(progn
    (setq css-indent-offset 2)))


;; JAVASCRIPT
(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(eval-after-load "js3-mode"
  (progn
    (add-to-list 'ac-modes 'js3-mode)
    (setq js3-auto-indent-p t)
    (setq js3-enter-indents-newline t) 
    (setq js3-indent-on-enter-key t)))

(eval-after-load "js-mode"
  (progn
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
        (setq org-html-htmlize-output-type 'css)))))

(provide 'mode-mappings)
