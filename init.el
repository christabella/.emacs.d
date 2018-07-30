;;; package --- Summary
;;; Bella's init file

;; Enable package archives
(require 'package)
(setq package-archives
      '(("elpy" . "http://jorgenschaefer.github.io/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq-default explicit-shell-file-name "/usr/local/bin/fish")
(setq-default shell-file-name "/usr/local/bin/fish")

;; Pixel scroll mode
(with-eval-after-load "postpone"
  (when (version<= "26.1" emacs-version)
    (pixel-scroll-mode 1)))

;; Fullscreen
(setq ns-use-native-fullscreen nil)

;; Set fonts
(set-face-attribute 'default nil :family "Iosevka" :height 130)
(set-face-attribute 'fixed-pitch nil :family "Iosevka")
;; So that org-mode will be the pretty Edward Tufte (EtBembo) serif font.
(set-face-attribute 'variable-pitch nil :family "ETBembo" :height 170)

;; Variable pitch from https://xiangji.me/2015/07/13/a-few-of-my-org-mode-customizations/
(defun set-buffer-variable-pitch ()
  (interactive)
  (variable-pitch-mode t)
  ;; (setq line-spacing 3)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  )

(add-hook 'org-mode-hook 'set-buffer-variable-pitch)
(add-hook 'markdown-mode-hook 'set-buffer-variable-pitch)
(add-hook 'eww-mode-hook 'set-buffer-variable-pitch)
(add-hook 'Info-mode-hook 'set-buffer-variable-pitch)

;; Org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init (add-hook 'org-mode-hook
		  '(lambda nil
		     (setq-local truncate-lines nil)
		     (org-indent-mode)
		     (setq org-support-shift-select t)
		     ;; Hardcode image width in in pixels
		     ;; We can also to half of the screen width: (setq org-image-actual-width (/ (display-pixel-width) 2))
		     (setq org-image-actual-width '(600))
		     (org-display-inline-images) ;; Display inline images
		     (org-bullets-mode 1)        ;; For UTF-8 bullets
		     (olivetti-mode 1)           ;; Centers text in the buffer
		     ;; (flyspell-mode 1)           ;; Catch Spelling mistakes
		     (typo-mode 1)               ;; Good for symbols like em-dash
		     (blink-cursor-mode 0)       ;; Reduce visual noise
		     (add-to-list 'org-babel-load-languages '("python" . t))
		     (org-babel-do-load-languages
		      'org-babel-load-languages
		      '((python . t)
			(js . t)))
		     ;; Maybe I'll try the below when the highlight bug is fixed
		     ;; (highlighting is invisible)
		     ;; (load-theme-buffer-local
		     ;;  'doom-solarized-light
		     ;;  (current-buffer))
		     ))
  :bind (("C-c C-g" . org-capture)
         ("C-c C-a" . org-agenda)
	 ("C-<up>" . org-move-subtree-up)
	 ("C-<down>" . org-move-subtree-down)
	 ("M-n" . outline-next-visible-heading)
	 ("M-p" . outline-previous-visible-heading)
	 ("M-u" . outline-up-heading)))

(setq org-directory "~/.org")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (lambda () (concat org-directory "/todo/todo.org")) "Tasks")
	 "* TODO %?\n %i\n %a")
	("s" "Shopping" entry (file+headline (lambda () (concat org-directory "/todo/shopping.org")) "List")
	 "* TODO %?\n %i")
	("l" "Learnings" entry (file+headline (lambda () (concat org-directory "/learnings/TIL.org")) "Learnings")
	 "* %?\n %i")))

;; For inserting images/screenshots into org files
(use-package org-download :after org
  :custom
  (org-download-screenshot-method "screencapture -i %s")
  (org-download-image-dir "~/.org/img")
  :bind
  (:map org-mode-map
	(("s-y" . org-download-yank)
	 ("s-Y" . org-download-screenshot)
	 ("C-c l" . org-store-link))))

;; For exporting from .org to Github-flavored Markdown (`org-gfm-export-to-markdown`)
(use-package ox-gfm :after org)
(use-package ox-hugo
  :after ox)

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
  :defer t ;; don't access `dired-mode-map' until `peep-dired' is loaded
  :init
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-enable-on-directories t)
  :bind (:map dired-mode-map
	      ("P" . peep-dired)))

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

;; Make the prompt selectable
(setq ivy-use-selectable-prompt t)

;; Globally set Org tags
(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("question" . ?q)))

;; Syntax highlighting for org
(setq org-src-fontify-natively t)

;; Enable upcase-region
(put 'upcase-region 'disabled nil)

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

;; (setq-default tab-width 2)
(setq-default css-indent-offset 2)

;; Display column number in modeline
(setq column-number-mode t)

;; Add /themes to custom theme load path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes" )

;; Set theme
;; (use-package tao-theme
;; :init
(load-theme 'dracula t)


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

;; ---------------------------------------- Python ----------------------------------

;; Anaconda
(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

;; Disable aggressive-indent-mode in Python
(add-hook 'python-mode-hook (lambda () (aggressive-indent-mode -1)))

;; Define function to call when python-mode loads
(defun my-python-mode-hook ()
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  ;; Flycheck for Python!
  (flycheck-mode))

;; Connect go-mode-hook with the function we just defined
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Pyflakes
;; (require 'flymake-python-pyflakes)
;; (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
;; (setq flymake-python-pyflakes-executable "flake8")

;; ------------------------------ Java ------------------------------
(use-package autodisass-java-bytecode
  :ensure t
  :defer t)

(use-package google-c-style
  :defer t
  :ensure t
  :commands
  (google-set-c-style))

(use-package meghanada
  :defer t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (setq-local c-basic-offset 4)
              (setq c-basic-offset 4)
              (add-to-list 'c-offsets-alist '(annotation-top-cont . 0))
              ;; (google-set-c-style)
              ;; (google-make-newline-indent)
              (meghanada-mode t)
              (smartparens-mode t)
              (rainbow-delimiters-mode t)
              (highlight-symbol-mode t)
              ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
              (add-hook 'before-save-hook 'meghanada-import-all)
              ))

  :config
  (use-package realgud
    :ensure t)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq meghanada-server-remote-debug t)
  (setq meghanada-javac-xlint "-Xlint:all,-processing")
  :bind
  (:map meghanada-mode-map
        ("C-S-t" . meghanada-switch-testcase)
        ("M-RET" . meghanada-local-variable)
        ("C-M-." . helm-imenu)
        ("M-r" . meghanada-reference)
        ("M-t" . meghanada-typeinfo)
        ("C-z" . hydra-meghanada/body))
  :commands
  (meghanada-mode))

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
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c a" . mc/mark-all-like-this-dwim)
         ("C-c A" . mc/mark-all-like-this)
	 ;; Make <return> insert a newline instead of disabling multiple-cursors-mode
         :map mc/keymap
      	 ("<return>" . 'newline-and-indent)))

(use-package nyan-mode
  :config
  (add-hook 'after-init-hook 'nyan-mode))

(use-package swiper
  :bind*
  (("C-s" . counsel-grep-or-swiper)
   ("C-c C-r" . ivy-resume)
   ("C-j" . ivy-immediate-done)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c h f" . counsel-describe-function)
   ("C-c h v" . counsel-describe-variable)
   ("C-c i u" . counsel-unicode-char)
   ("M-i" . counsel-imenu)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-rg) ;; Faster than counsel-ag
   ("C-c k" . counsel-ag))
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

;; Override major modes
(bind-key* "M-l" 'copy-line)

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

(bind-keys*
 ("C-M-p" . move-text-up)
 ("C-M-n" . move-text-down)
 ("M-<up>" . move-text-up)
 ("M-<down>" . move-text-down))

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
         ("s-/" . comment-or-uncomment-region)
	 ("M-*" . crux-switch-to-previous-buffer)
	 )
  :config (crux-with-region-or-line comment-or-uncomment-region))

(global-set-key (kbd "C-S-<backspace>") 'fixup-whitespace)

(global-set-key (kbd "C-c C-f") 'toggle-frame-fullscreen)

(global-set-key (kbd "C-x j") 'org-journal-new-entry)

;; ---------------------------------------- Go ----------------------------------

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
  (flycheck-mode)

  ;; Show function signatures in company-go autocompletion dropdown
  (setq company-go-show-annotation t)
  ;; Align function signatures to the right
  (setq company-tooltip-align-annotations t)

  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode))
;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Ensure the go specific autocomplete is active in go-mode.
(with-eval-after-load 'go-mode
  (require 'company-go) ; load company mode go backend
  ;; If the go-guru.el and go-rename.el files are in the load path, this will load it.
  (require 'go-guru)
  (require 'go-rename))


;; Protobuf
(use-package protobuf-mode :ensure t
  :mode "\\.proto\\'"
  :config
  (setq-local c-basic-offset 4))

;; ------------------------------------ Dumb jump --------------------------------
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-." . dumb-jump-go)
         ("M-," . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure)

;; -------------------------------------- React ----------------------------------
(defun my-rjsx-mode-hook ()
  (setq-local emmet-expand-jsx-className? t)
  (setq-local web-mode-enable-auto-quoting nil)
  (setq js2-strict-missing-semi-warning nil)
  (emmet-mode t)
  ;; Disable aggressive-indent-mode in rjsx
  (aggressive-indent-mode -1)
  ;; Override js2-jump-to-definition in rjsx-mode-map
  (bind-key* "M-." 'dumb-jump-go)
  )

(use-package rjsx-mode
  :ensure t
  :pin melpa-stable
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.react.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\index.android.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . rjsx-mode))
  (add-to-list 'magic-mode-alist '("/\\*\\* @jsx React\\.DOM \\*/" . rjsx-mode))
  (add-to-list 'magic-mode-alist '("import React" . rjsx-mode))
  (add-to-list 'magic-mode-alist '("^/*Enable flow type*/" . rjsx-mode))
  (add-to-list 'magic-mode-alist '("^// @flow" . rjsx-mode))
  (add-hook 'rjsx-mode-hook 'my-rjsx-mode-hook)
  :config
  (with-eval-after-load 'flycheck
    (dolist (checker '(javascript-eslint javascript-standard))
      (flycheck-add-mode checker 'rjsx-mode))))

(use-package prettier-js
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (setq-default js-indent-level 2)
  (setq-default js2-indent-level 2)
  (setq-default jsx-indent-level 2)
  (setq-default sgml-basic-offset 2)
  (setq-default js2-basic-offset 2))

(use-package add-node-modules-path
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'prettier-js-mode-hook 'add-node-modules-path))

(add-hook 'js-mode-hook (lambda () (aggressive-indent-mode -1)))

;; End of packages
(load custom-file)

(provide 'init)
;;; init.el ends here

