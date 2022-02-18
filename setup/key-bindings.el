;; Replace rectangle-text with multiple-cursors
(require 'mc-edit-lines)
(global-set-key (kbd "C-x r t") 'mc/edit-lines)

;; Multiple mark with multiple-cursors
(require 'mc-mark-more)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Use ace jump mode
(require 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; Move windows, even in org-mode
(global-set-key (kbd "<M-right>") 'windmove-right)
(global-set-key (kbd "<M-left>") 'windmove-left)
(global-set-key (kbd "<M-up>") 'windmove-up)
(global-set-key (kbd "<M-down>") 'windmove-down)

;; Camel to underscore : TestTest <-> test_test
(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun camelcase  (s) (mapconcat 'capitalize (split-name s) ""))
(defun underscore (s) (mapconcat 'downcase   (split-name s) "_"))

(defun camelscore (s)
  (cond ((string-match-p "_" s)(camelcase s))
        (t                     (underscore s))))

(defun camelscore-word-at-point ()
  (interactive)
  (let* ((case-fold-search nil)
         (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
         (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
         (txt (buffer-substring beg end))
         (cml (camelscore txt)) )
    (if cml (progn (delete-region beg end) (insert cml))) ))

(global-set-key (kbd "M-C") 'camelscore-word-at-point)

;; Mark current word
(defun mark-current-word (&optional arg allow-extend)
  "Put point at beginning of current word, set mark at end."
  (interactive "p\np")
  (setq arg (if arg arg 1))
  (if (and allow-extend
           (or (and (eq last-command this-command) (mark t))
               (region-active-p)))
      (set-mark
       (save-excursion
         (when (< (mark) (point))
           (setq arg (- arg)))
         (goto-char (mark))
         (forward-word arg)
         (point)))
    (let ((wbounds (bounds-of-thing-at-point 'word)))
      (unless (consp wbounds)
        (error "No word at point"))
      (if (>= arg 0)
          (goto-char (car wbounds))
        (goto-char (cdr wbounds)))
      (push-mark (save-excursion
                   (forward-word arg)
                   (point)))
      (activate-mark))))

(global-set-key (kbd "M-*") 'mark-current-word)

(require 'dired-sidebar)
(global-set-key [f8] 'dired-sidebar-toggle-sidebar)
;; list all file except . directory and dotfile but .. and directory first
(setq dired-custom-ls "-al --ignore=\. --ignore=\.[^\.]* --group-directories-first")
;; list all file except . directory and directory first
(setq dired-custom-ls-dotfile "-al --ignore=. --group-directories-first")
(setq dired-custom-switch 1)

(setq dired-listing-switches dired-custom-ls)
(setq dired-sidebar-theme 'nerd)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "M-o")
              (lambda ()
                "Toggle between hide and show."
                (interactive)
                (setq dired-custom-switch (- dired-custom-switch))
                (if (= dired-custom-switch 1)
                    (dired-sort-other dired-custom-ls)
                  (dired-sort-other dired-custom-ls-dotfile))))))

(require 'zoom-frm)
(global-set-key [C-mouse-4] 'zoom-in)
(global-set-key [C-mouse-5] 'zoom-out)
(global-set-key (kbd "C-+") 'zoom-in)
(global-set-key (kbd "C--") 'zoom-out)
(global-set-key (kbd "C-0") 'zoom-frm-unzoom)

(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key [C-tab] 'hs-toggle-hiding)

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

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

(global-set-key (kbd "C-x /") 'window-split-toggle)

;; Inspired by https://stackoverflow.com/questions/7362625/
(defun print-to-pdf-with-lines ()
  "Print the current buffer to pdf with number lines"
  (interactive)
  (print-to-pdf t))

(defun print-to-pdf (&optional with-lines)
  "Print the current buffer to pdf"
  (interactive)
  (let ((wbuf (generate-new-buffer (concat (format-time-string "%Y-%m-%d") ":" (buffer-name (current-buffer)))))
        (sbuf (current-buffer)))
    ;; (jit-lock-fontify-now)
    (load-theme 'print t)
    (save-current-buffer
      (set-buffer wbuf)
      (insert-buffer sbuf)
      (visual-line-mode t)
      ;; Customize header/footer ps https://www.emacswiki.org/emacs/PsPrintPackage-23
      (setq ps-print-header t
            ps-print-header-frame t
            ps-header-lines 1
            ps-left-header (list (lambda () (buffer-name sbuf)))
            ps-right-header (list (lambda () (format-time-string "%Y-%m-%d")))
            ps-print-footer t
            ps-print-footer-frame nil
            ps-footer-lines 1
            ps-right-footer nil
            ps-left-footer (list (concat "{pagenumberstring dup stringwidth pop"                                         " 2 div PrintWidth 2 div exch sub 0 rmoveto}"))
            ;;ps-number-of-columns 2
            ps-line-number (if with-lines t nil)
            )
      ;; (if with-lines (rectangle-number-lines (region-beginning) (region-end) 1))
      (ps-spool-buffer-with-faces)
      (kill-buffer wbuf)
      (switch-to-buffer "*PostScript*")
      (write-file (concat (buffer-name sbuf) ".ps"))
      ;;(kill-buffer (current-buffer))
      )
    (load-theme 'dracula t)
    (call-process "ps2pdf" nil nil nil
                  (concat (buffer-name sbuf) ".ps") (concat (buffer-name sbuf) ".pdf"))
    (delete-file (concat (buffer-name sbuf) ".ps"))
    (message (concat "PDF saved to " (concat (buffer-name sbuf) ".pdf")))))

(global-set-key (kbd "C-c p p") 'print-to-pdf)
(global-set-key (kbd "C-c p n") 'print-to-pdf-with-lines)

(provide 'key-bindings)