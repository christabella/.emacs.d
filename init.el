;;; package --- Summary
;;; Bella's init file 2

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

;; Save on frame focus loss
;; (add-hook 'focus-out-hook 'save-buffer)

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

;; Install use-package (needs to come before any use-package use)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Global keybinding or overrides
(global-set-key (kbd "S-s-<up>") 'flymake-goto-prev-error)
(global-set-key (kbd "S-s-<down>") 'flymake-goto-next-error)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-c b") 'browse-url-at-point)

;; Smart parens
(use-package smartparens-config
  :ensure smartparens
  :config
  (sp-local-pair 'org-mode "_" "_" )
  (sp-local-pair 'org-mode "*" "*" )
  (sp-local-pair 'org-mode "~" "~" )
  (sp-local-pair 'org-mode "\[" "\]" )
  (sp-local-pair 'org-mode "$" "$" )
  (sp-local-pair 'prog-mode "{" "}" )
  (sp-local-pair 'latex-mode "$" "$" )
  (sp-local-pair 'latex-mode "\\left(" "\\right)" :trigger "\\l(")
  (sp-local-pair '(js-mode js2-mode html-mode) "<span>" "</span>")
  (sp-local-pair '(js-mode js2-mode html-mode) "<div className=\"{_}\"" "</div>")
  (progn
    (show-smartparens-global-mode t)))

;; From https://ebzzry.io/en/emacs-pairs/
(bind-keys
 :map smartparens-mode-map
 ("C-M-s" . smartparens-mode)
 ("C-M-<down>" . sp-backward-down-sexp)
 ("C-M-<up>"   . sp-backward-up-sexp)
 ("M-]" . sp-forward-sexp)
 ("M-[" . sp-backward-sexp)
 ;; ("M-{" . sp-beginning-of-sexp)
 ;; ("M-}" . sp-end-of-sexp)

 ("C-S-f" . sp-forward-symbol)
 ("C-S-b" . sp-backward-symbol)

 ("C-]" . sp-rewrap-sexp)

 ;; ("C-<right>" . sp-forward-slurp-sexp)
 ;; ("M-<right>" . sp-forward-barf-sexp)
 ;; ("C-<left>"  . sp-backward-slurp-sexp)
 ;; ("M-<left>"  . sp-backward-barf-sexp)

 ("C-M-t" . sp-transpose-sexp)
 ("C-M-k" . sp-kill-sexp)
 ("C-k"   . sp-kill-hybrid-sexp)
 ("M-k"   . sp-backward-kill-sexp)
 ("C-M-w" . sp-copy-sexp)

 ("M-<backspace>" . backward-kill-word)
 ("C-<backspace>" . sp-backward-kill-word)
 ([remap sp-backward-kill-word] . backward-kill-word)

 ;; ("M-[" . sp-backward-unwrap-sexp)
 ;; ("M-]" . sp-unwrap-sexp)

 ("C-x C-t" . sp-transpose-hybrid-sexp))

;; Strict mode enforces that pairs are always balanced, so commands like kill-line keep your code well-formed.
(add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
(add-hook 'org-mode-hook 'turn-on-smartparens-mode)
(add-hook 'html-mode-hook 'turn-on-smartparens-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)


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
  :bind (("C-M-S-s-c" . org-capture)
         ("C-M-S-s-a" . org-agenda)
	 ("C-<up>" . org-move-subtree-up)
	 ("C-<down>" . org-move-subtree-down)
	 ("M-n" . outline-next-visible-heading)
	 ("M-p" . outline-previous-visible-heading)
	 ("M-u" . outline-up-heading)))

(setq org-directory "~/.org")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (lambda () (concat org-directory "/todo/todo.org")) "Tasks")
	 "* TODO %?\n %i")
	("s" "Shopping" entry (file+headline (lambda () (concat org-directory "/todo/shopping.org")) "List")
	 "* TODO %?\n %i")
	("l" "Learnings" entry (file+headline (lambda () (concat org-directory "/learnings/TIL.org")) "Learnings")
	 "* %?\n %i")))

;; For inserting images/screenshots into org files
(use-package org-download :after org
  :custom
  (org-download-screenshot-method "screencapture -i %s")
  :bind
  (:map org-mode-map
	(("s-y" . org-download-yank)
	 ("s-Y" . org-download-screenshot)
	 ("C-c l" . org-store-link))))

(defun my-org-download-method (link)
  "This is a helper function for org-download.

It creates a folder in the root directory (~/.org/img/) named after the
org filename (sans extension) and puts all images from that file in there.

Inspired by https://github.com/daviderestivo/emacs-config/blob/6086a7013020e19c0bc532770e9533b4fc549438/init.el#L701"
  (let ((filename
         (file-name-nondirectory
          (car (url-path-and-query
                (url-generic-parse-url link)))))
        ;; Create folder name with current buffer name, and place in root dir
        (dirname (concat "~/.org/img/"
                         (file-name-sans-extension (buffer-name)))))

    ;; Add timestamp to filename
    (setq filename-with-timestamp (format "%s%s.%s"
                                          (file-name-sans-extension filename)
                                          (format-time-string org-download-timestamp)
                                          (file-name-extension filename)))
    ;; Create folder if necessary
    (unless (file-exists-p dirname)
      (make-directory dirname))
    (expand-file-name filename-with-timestamp dirname)))
(setq org-download-method 'my-org-download-method)

(use-package org-noter
  :after org
  :config
  (setq org-noter-auto-save-last-location t)
  (lambda() org-noter-set-doc-split-fraction 0.6)
  )

(use-package org-ref
  :after org
  :ensure t
  :init
  (setq org-latex-listings 'minted)
  (setq org-latex-custom-lang-environments
        '(
	  (sh "\\begin{minted}[style=friendly,bgcolor=friendlybg]{shell}
		 %s\\end{minted}
		 ")	  ;; For some reason minted python lexer is not working on my mac :(
	  (python "\\begin{minted}[style=friendly,bgcolor=friendlybg]{shell}
		 %s\\end{minted}
		 ")))
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("fontsize" "0.8\\scriptsize")
          ("linenos" "")))
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))
  (add-hook 'bibtex-mode-hook ;; Sort bibliography before every save
	    (lambda () (add-hook 'before-save-hook 'bibtex-sort-buffer nil t)))

  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
	org-ref-pdf-directory "~/repos/deep-probabilistic-models/papers/"
	;; org-ref-pdf-directory "~/Documents/Papers/"
	org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib"))
  :bind (("C-c c" . org-ref-insert-link)
	 ("C-c x" . arxiv-add-bibtex-entry)
	 ("C-c C-x x" . arxiv-get-pdf-add-bibtex-entry)
	 ))

(use-package ox-hugo-org-ref-overrides
  :after org org-ref ox-hugo
  :load-path "./lisp/")

;; https://emacs.stackexchange.com/a/41685/19739
(use-package org
  :ensure org-plus-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

;; Underscores and carets don't cause text to be sub- or superscripted
(setq org-export-with-sub-superscripts nil)

;; For exporting from .org to Github-flavored Markdown (`org-gfm-export-to-markdown`)
(use-package ox-gfm :after org)
(use-package ox-hugo
  :after org)

(use-package ox-reveal
  :after org
  :ensure ox-reveal
  :config (require 'htmlize))

(require 's)  ;; For s-join
(use-package deft
  :after org
  :init
  (defun close-deft ()
    (interactive)
    (quit-window)
    (deft-filter-clear))
  :bind
  ("C-M-S-s-d" . deft)
  ("C-M-S-s-l" . jethro/get-linked-files)
  ("C-M-S-s-z" . org-insert-zettel)
  ("C-M-S-s-k" . close-deft)
  ("C-M-S-s-?" . jethro/deft-insert-boilerplate)
  :custom
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/.org")
  (deft-extensions '("org"))
  (deft-use-filename-as-title t)
  (deft-recursive t)
  :config
  (setq zettel-indicator "§")
  (defun jethro/deft-insert-boilerplate ()
    (interactive)
    (when (= (buffer-size (current-buffer)) 0)
      (let ((title (s-join " " (-map #'capitalize (split-string (file-name-sans-extension (buffer-name)) "_")))))
        (insert "#+SETUPFILE:./hugo_zettel.setup\n")
        (insert "#+TITLE: ")
        (insert title)
	(insert "\n#+hugo_tags: ml\n\n\n\n")  ;; Very likely to be ML :shrug:
        (insert "* \nbibliography:")
        (insert (car org-ref-default-bibliography))
	(goto-char (point-max))
        )))

  (defun org-insert-zettel (file-name)
    "Finds a file, inserts it as a link with the base file name as the link name, and adds the zd-link-indicator I use to the front."
    (interactive (list (completing-read "File: " (deft-find-all-files-no-prefix))))
    (let ((org-link-file-type 'relative))
      (org-insert-link nil (concat "file:" (concat deft-directory file-name))
                       (concat zettel-indicator (file-name-base file-name)))))

  (defun jethro/get-linked-files ()
    "Show links to this file."
    (interactive)
    (let* ((search-term (file-name-nondirectory buffer-file-name))
           (files deft-all-files)
	   (tnames (mapcar #'file-truename files)))
      (multi-occur
       (mapcar (lambda (x)
	         (with-current-buffer
		     (or (get-file-buffer x) (find-file-noselect x))
		   (widen)
		   (current-buffer)))
	       files)
       search-term
       3))))

;;https://emacs.stackexchange.com/a/22591
(use-package pdf-tools
  :ensure t
  :pin melpa-stable
  :init (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
  :config
  (setq TeX-PDF-mode t)
  ;; More fine-grained zooming with steps of 10%
  (setq pdf-view-resize-factor 1.1)
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  :bind
  (:map pdf-view-mode-map
	(("C-s" . isearch-forward))))
(pdf-tools-install)

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
  :init
  (add-hook 'after-init-hook 'global-emojify-mode)
  ;; Disable plain text emojis (no 'ascii' option)
  (setq emojify-emoji-styles '(github unicode)) 
  :bind (("C-c e" . emojify-insert-emoji)))

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

;; Globally set Org tags
(setq org-tag-alist '(("@work" . ?w)
		      ("@home" . ?o)
		      ("question" . ?q)
		      ("export" . ?e)
		      ("gratitude" . ?g)
		      ("to-read" . ?r)
		      ))

;; Syntax highlighting for org
(setq org-src-fontify-natively t)

;; Enable upcase-region
(put 'upcase-region 'disabled nil)

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

;; Set 2 spaces indentation
;; (setq-default tab-width 2)
(setq-default css-indent-offset 2)

;; Display column number in modeline
(setq column-number-mode t)

;; Natural title bar options from `brew info emacs-plus`
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Add /themes to custom theme load path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes" )

;; Set theme
;; (load-theme 'doom-challenger-deep t)
(load-theme 'doom-dracula t)
;; (load-theme 'doom-solarized-light t)

;; Doom theme
;; (use-package doom-themes
;;   :init
;;   (load-theme 'doom-one t)
;;   :config
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)

;;   ;; Enable custom neotree theme
;;   (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; Super pretty but doesn't have line numbers, mc support etc
;; (use-package doom-modeline
;;   :ensure t
;;   :defer t
;;   :hook (after-init . doom-modeline-init))

;; Install packages
;; Window management
(use-package ace-window
  ;; :init (add-to-list 'golden-ratio-extra-commands 'ace-window)
  :bind (("M-q" . ace-window) ("M-Q" . ace-delete-window)))

;; Automatic indentation
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config (add-hook 'prog-mode-hook 'aggressive-indent-mode))

(use-package flycheck
  :config (setq flycheck-highlighting-mode 'lines))

(use-package which-key
  :diminish which-key-mode
  :init
  (add-hook 'after-init-hook 'which-key-mode))

;; ---------------------------------------- Python ----------------------------------

;; Anaconda
(use-package anaconda-mode
  :defer t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  )

(use-package python
  :ensure t
  :defer t
  :config
  (use-package elpy
    :ensure t
    :config
    (elpy-enable)
    :bind (:map elpy-mode-map
		("M-." . elpy-goto-definition)
		("M-," . pop-tag-mark)))
  :bind (:map python-mode-map
	      ("M-n" . python-nav-forward-block)
	      )
  )

;; Disable aggressive-indent-mode in Python
(add-hook 'python-mode-hook (lambda () (aggressive-indent-mode -1)))

(add-hook 'python-mode-hook ;; YAPF-ify before every save
	  (lambda () (add-hook 'before-save-hook 'yapfify-buffer nil t)))

;; Define function to call when python-mode loads
(defun my-python-mode-hook ()
  ;; (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  ;; (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg
  (bind-key "M-p" 'python-nav-backward-block python-mode-map)
  (bind-key "s-]" 'python-nav-end-of-block python-mode-map)
  (bind-key "s-[" 'python-nav-beginning-of-block python-mode-map)
  (bind-key "s-}" 'python-nav-forward-defun python-mode-map)
  (bind-key "s-{" 'python-nav-backward-defun python-mode-map)
  ;; No need for below since we have elpy now
  (bind-key "C-<return>" 'elpy-shell-send-region-or-buffer-and-step-and-go elpy-mode-map)
  ;; (define-key elpy-mode-map (kbd "<S-return>") 'elpy-shell-send-statement-and-step)

  ;; Flycheck for Python linting!
  ;; (flycheck-mode)
  )

;; Connect python-mode-hook with the function we just defined
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; ------------------------------ C++ ------------------------------

(use-package clang-format
  :ensure t
  :config
  (progn
    (setq clang-format-executable "/usr/local/bin/clang-format")
    (add-hook 'c++-mode-hook (lambda () (add-hook 'before-save-hook 'clang-format-buffer nil t)))
    (add-hook 'c-mode-hook (lambda () (add-hook 'before-save-hook 'clang-format-buffer nil t)))))


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
            :config (company-quickhelp-mode 1)))
  :bind
  (:map company-active-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ))

;;
(use-package counsel)

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-blame))
  :init (setq magit-auto-revert-mode nil)
  ;; https://emacs.stackexchange.com/questions/28496/magit-status-always-split-vertically
  (setq split-height-threshold nil) (setq split-width-threshold 200)
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
   ("M-y" . counsel-yank-pop)
   ("C-x C-f" . counsel-find-file)
   ("C-c h f" . counsel-describe-function)
   ("C-c h v" . counsel-describe-variable)
   ("C-c i u" . counsel-unicode-char)
   ("M-i" . counsel-imenu)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-rg) ;; Faster than counsel-ag
   ("C-c k" . counsel-ag))
  :config
  (setq counsel-grep-swiper-limit 50000)
  ;; Speeding up with rg https://oremacs.com/2017/08/04/ripgrep/
  (setq counsel-grep-base-command
	"rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  ;; (setq counsel-grep-base-command "grep -E -i -n -e %s %s")
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    ;; M-x without a caret
    (setq ivy-initial-inputs-alist nil)
    ;; Make the prompt selectable
    (setq ivy-use-selectable-prompt t)
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

(use-package deadgrep
  :if (executable-find "rg")
  :bind* (("C-'" . deadgrep)))

(use-package which-key
  :diminish which-key-mode
  :config (add-hook 'after-init-hook 'which-key-mode))

(use-package yasnippet
  :defer 5
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
  ;; Disable aggressive-indent-mode in yasnippet-mode
  (aggressive-indent-mode -1)
  )

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
         ("S-s-<return>" . crux-smart-open-line-above)
         ("s-<return>" . crux-smart-open-line)
         ("s-/" . comment-or-uncomment-region)
	 ("M-*" . crux-switch-to-previous-buffer)
	 )
  :config (crux-with-region-or-line comment-or-uncomment-region))

(global-set-key (kbd "C-S-<backspace>") 'fixup-whitespace)

(global-set-key (kbd "C-c C-f") 'toggle-frame-fullscreen)

;; Hyper-J for journal
(global-set-key (kbd "C-M-S-s-j") 'org-journal-new-entry)

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

;; Shut up Emacs!
(setq visible-bell 1)

;; End of packages
(load custom-file)

(provide 'init)
;;; init.el ends here
