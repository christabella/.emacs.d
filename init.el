;;; package --- Summary
;;; Bella's init file

;; Enable package archives
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (package-initialize)
  )

(setq-default explicit-shell-file-name "/bin/bash")
(setq-default shell-file-name "/bin/bash")

;; Fullscreen
(setq ns-use-native-fullscreen nil)

;; Org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c C-g" . org-capture)
         ("C-c C-a" . org-agenda)
         ("M-n" . outline-next-visible-heading)
         ("M-p" . outline-previous-visible-heading)
         ("M-u" . outline-up-heading)
         )
  )

;; Make dired less verbose
;; (require 'dired-details)
;; (setq-default dired-details-hidden-string "-- ")
;; (dired-details-install)

;; Dired emoji https://github.com/jtbm37/all-the-icons-dired
;; (Note: Had to `M-x all-the-icons-install-fonts` and `M-x unicode-fonts-setup`)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Move to trash instead of delete
(setq delete-by-moving-to-trash t)

;; List directories first in dired
;; First, gotta use GNU's gls (Note: `brew install coreutils`)
(let ((gls "/usr/local/bin/gls"))
  (if (file-exists-p gls)
      (setq insert-directory-program gls)))
(setq dired-listing-switches "-aBhl  --group-directories-first")

;; Enable recursive copies and deletes
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

;; Preview files in dired
(use-package peep-dired
  :bind (:map peep-dired-mode-map
              ("SPC" . nil)
              ("<backspace>" . nil))
  (setq peep-dired-cleanup-eagerly t))

(use-package projectile
  :demand t
  :init (add-hook 'after-init-hook 'projectile-global-mode)
  :config
  (setq projectile-keymap-prefix (kbd "C-x p"))
  (use-package counsel-projectile
    :bind (("s-p" . counsel-projectile)
           ("s-f" . counsel-projectile-find-file)
           ("s-b" . counsel-projectile-switch-to-buffer)))
  (setq projectile-use-git-grep t)
  (setq projectile-completion-system 'ivy))

;; Emoji
(use-package emojify
  :bind (("C-c e" . emojify-insert-emoji))
  :init (add-hook 'after-init-hook 'global-emojify-mode))

;; Set user details
(setq user-full-name "Christabella Irwanto"
      user-mail-address "christabella.irwanto@gmail.com")

;; Copy path from shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize)
  )

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

;; M-x without a caret
(setq ivy-initial-inputs-alist nil)

;; Globally set Org tags
(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("question" . ?q)))

;;; syntax highlighting for org
(setq org-src-fontify-natively t)

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
;; (add-to-list 'default-frame-alist
;;              '(font . "Fira Code-12"))

;; Set 2 spaces indentation

(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default js-switch-indent-offset js-indent-level)
(setq-default indent-tabs-mode nil)

;; Set theme
;; (use-package tao-theme
;; :init
;; (load-theme 'tao-yang t))


;; (use-package zenburn-theme
;;   :init
;;   (load-theme 'zenburn t))

;; Doom theme
(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))



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

;; Pyflakes
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")

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
         ("C-c a" . mc/mark-all-like-this-dwim)
         ("C-c A" . mc/mark-all-like-this)))

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

;; ------------------------------ Little Helpers ------------------------------

;; Copy lines
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(global-set-key (kbd "C-c C-l") 'copy-line)

;; Move text
(use-package move-text
  :bind ("C-c t" . hydra-move-text/body)
  :config
  ;; Move Text
  (defhydra hydra-move-text ()
    "Move text"
    ("<up>" move-text-up "Up")
    ("<down>" move-text-down "Down")
    ("q" nil "Quit" :color blue)))

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;; Crux
(use-package crux 
  :bind (("C-c o" . crux-open-with)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-a" . crux-move-beginning-of-line)
         ("s-r" . crux-rename-file-and-buffer)
         ("s-d" . crux-duplicate-current-line-or-region)
         ("s-D" . crux-duplicate-and-comment-current-line-or-region)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-<backspace>" . crux-kill-whole-line)
         ("S-<return>" . crux-smart-open-line-above)
         ("C-<return>" . crux-smart-open-line)
         ("s-/" . comment-or-uncomment-region))
  :config (crux-with-region-or-line comment-or-uncomment-region))

(global-set-key (kbd "C-S-<backspace>") 'fixup-whitespace)

(global-set-key (kbd "C-c C-f") 'toggle-frame-fullscreen)

(global-set-key (kbd "C-x j") 'org-journal-new-entry)

;; ---------------------------------------- Go ----------------------------------

(require 'auto-complete-config)
(ac-config-default)

;; Define function to call when go-mode loads
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v; and go test -v; and go vet"))

  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers

  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  (electric-pair-mode)
  (electric-indent-mode)
  (flycheck-mode))

;; ;; Misc go stuff
;; (auto-complete-mode 1))                         ; Enable auto-complete mode

;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Ensure the go specific autocomplete is active in go-mode.
(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

;; If the go-guru.el and go-rename.el files are in the load path, this will load it.
(require 'go-guru)
(require 'go-rename)

;; Protobuf
(use-package protobuf-mode :ensure t
  :mode "\\.proto\\'"
  :config
  (setq-local c-basic-offset 4))

;; -------------------------------------- React ----------------------------------
(defun jethro/setup-rjsx-mode ()
  (setq-local emmet-expand-jsx-className? t)
  (setq-local web-mode-enable-auto-quoting nil)
  (setq js2-strict-missing-semi-warning nil))

(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.react.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\index.android.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . rjsx-mode))
  (add-to-list 'magic-mode-alist '("/\\*\\* @jsx React\\.DOM \\*/" . rjsx-mode))
  (add-to-list 'magic-mode-alist '("import React" . rjsx-mode))
  (add-to-list 'magic-mode-alist '("^/*Enable flow type*/" . rjsx-mode))
  (add-to-list 'magic-mode-alist '("^// @flow" . rjsx-mode))
  (add-hook 'rjsx-mode-hook 'jethro/setup-rjsx-mode)
  (add-hook 'rjsx-mode-hook 'tern-mode)
  (add-hook 'rjsx-mode-hook 'emmet-mode)
  :config
  (with-eval-after-load 'flycheck
    (dolist (checker '(javascript-eslint javascript-standard))
      (flycheck-add-mode checker 'rjsx-mode)))
  (defun jethro/line-align-closing-bracket ()
    "Workaround sgml-mode and align closing bracket with opening bracket"
    (save-excursion
      (beginning-of-line)
      (when (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))
  (advice-add #'js-jsx-indent-line
              :after
              #'jethro/line-align-closing-bracket))


;; End of packages
(load custom-file)

(provide 'init)
;;; init.el ends here
