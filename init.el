;; Enable package archives
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize))

;; Set user details
(setq user-full-name "Christabella Irwanto"
      user-mail-address "christabella.irwanto@gmail.com")

;; Remove UI cruft
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

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
      '((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      '((".*" ,temporari-file-directory t)))

;; Overwrite highlighted text
(delete-selection-mode +1)

;; Default font
(add-to-list 'default-frame-alist
	     '(font . "Fira Code-12"))

;; Set 2 spaces indentation
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Set theme
(use-package tao-theme
  :init
  (load-theme 'tao-yang t))

;; Install ace-window
(use-package ace-window
  :bind (("M-q" . ace-window)))

(use-package avy
  :bind (("C-q" . avy-goto-char)
	 ("C-w" . avy-goto-char-2)))

;; Install Ivy, Counsel, and Swiper
(use-package counsel)
(use-package swiper
  :bind*
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resumqe)
   ("M-a" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c h f" . counsel-describe-function)
   ("C-c h v" . counsel-describe-variable)
   ("C-c i u" . counsel-unicode-char)
   ("M-i" . counsel-imenu)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-c l" . counsel-locate))
   :config
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
	 "other window"))))))

(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-blame))
  :init (setq magit-auto-revert-mode nil)
  :config
  (add-hook 'magit-mode-hook 'hl-line-mode))

(show-paren-mode +1)

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config (add-hook 'prog-mode-hook 'aggressive-indent-mode))

(load custom-file)
