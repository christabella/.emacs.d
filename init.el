;; Enable package archives
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize)
  )

;; Set user details
(setq user-full-name "Christabella Irwanto"
      user-mail-address "christabella.irwanto@gmail.com")

;; Copy path from shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Remove UI cruft
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Set sentence detection to just one space
(setq sentence-end-double-space nil)

;; To follow links with RET
(setq org-return-follows-link t)

;; Turns - [X] into ☑ and - [ ] into ☐ for html export??

(defun sacha/org-html-checkbox (checkbox)
  "Format CHECKBOX into HTML."
  (case checkbox (on "<span class=\"check\">&#x2611;</span>") ; checkbox (checked)
        (off "<span class=\"checkbox\">&#x2610;</span>")
        (trans "<code>[-]</code>")
        (t "")))
(defadvice org-html-checkbox (around sacha activate)
  (setq ad-return-value (sacha/org-html-checkbox (ad-get-arg 0))))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (defvar use-package-verbose t)
  (require 'cl)
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish)
  (setq use-package-always-ensure t))

;; y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Keep customizations in separate file
(setq custom-file "~/.emacs.d/custom.el")

;; Change backup directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Overwrite highlighted text
(delete-selection-mode +1)

;; Show matching parentheses
(show-paren-mode +1)

;; Default font
(add-to-list 'default-frame-alist
             '(font . "Fira Code-12"))

;; Set 2 spaces indentation

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Set theme
;; (use-package tao-theme
;; :init
;; (load-theme 'tao-yang t))


(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))

;; Install packages
;; Window management
(use-package ace-window
  :bind (("M-q" . ace-window)))

;; Automatic indentation
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config (add-hook 'prog-mode-hook 'aggressive-indent-mode))

;; ANaconda
(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

;; Disable aggressive-indent-mode in Python
(add-hook 'python-mode-hook (lambda () (aggressive-indent-mode -1)))

;; Go-to-char functionality
(use-package avy
  :bind (("C-," . avy-goto-char)
         ("C-." . avy-goto-char-2)))

;; Beacon
(use-package beacon
  :diminish beacon-mode
  :config (progn
            (beacon-mode 1)
            (setq beacon-push-mark 10)))

;; Code completion
(use-package company
  :defer 5
  :diminish company-mode
  :init (progn
          (add-hook 'after-init-hook 'global-company-mode)
          (setq company-dabbrev-ignore-case nil
                company-dabbrev-code-ignore-case nil
                company-dabbrev-downcase nil
                company-idle-delay 0
                company-begin-commands '(self-insert-command)
                company-transformers '(company-sort-by-occurrence))
          (use-package company-quickhelp
            :config (company-quickhelp-mode 1))))

;; 
(use-package counsel)

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-blame))
  :init (setq magit-auto-revert-mode nil)
  :config
  (add-hook 'magit-mode-hook 'hl-line-mode))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c a" . mc/mark-all-like-this)))

(use-package nyan-mode
  :config
  (add-hook 'after-init-hook 'nyan-mode))

(use-package swiper
  :bind*
  (("C-s" . counsel-grep-or-swiper)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c h f" . counsel-describe-function)
   ("C-c h v" . counsel-describe-variable)
   ("C-c i u" . counsel-unicode-char)
   ("M-i" . counsel-imenu)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-rg)
   ("C-c k" . counsel-ag)
   ("C-c l" . counsel-locate))
  :config
  (setq counsel-grep-swiper-limit 20000)
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (define-key read-expression-map (kbd "C-r") #'counsel-expression-history)
    (ivy-set-actions
     'counsel-find-file
     '(("d" (lambda(x) (delete-file (expand-file-name x)))
        "delete"
        )))
    (ivy-set-actions
     'ivy-switch-buffer
     '(("k"
        (lambda (x)
          (kill-buffer x)
          (ivy--reset-state ivy-last))
        "kill")
       ("j"
        ivy--switch-buffer-other-window-action
        "other window")))))

(use-package which-key
  :diminish which-key-mode
  :config (add-hook 'after-init-hook 'which-key-mode))

(use-package yasnippet
  :defer 5
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config (setq yas-snippet-dirs '("~/.emacs.d/snippets/")))

;; Move Region
(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-region-up)
(global-set-key (kbd "M-<down>") 'move-region-down)

;; End of packages
(load custom-file)
