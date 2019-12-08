(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1E2029" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(custom-safe-themes
   (quote
    ("add84a254d0176ffc2534cd05a17d57eea4a0b764146139656b4b7d446394a54" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "bbb4a4d39ed6551f887b7a3b4b84d41a3377535ccccf901a3c08c7317fad7008" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "72ef6008c5981a5f9f8760fad021e2801c19b28514ff46d3ddea663c5e61613d" "242ed4611e9e78142f160e9a54d7e108750e973064cee4505bfcfc22cc7c61b1" "013c62a1fcee7c8988c831027b1c38ae215f99722911b69e570f21fc19cb662e" "4597d1e9bbf1db2c11d7cf9a70204fa42ffc603a2ba5d80c504ca07b3e903770" "723e48296d0fc6e030c7306c740c42685d672fd22337bc84392a1cf92064788a" "53d1bb57dadafbdebb5fbd1a57c2d53d2b4db617f3e0e05849e78a4f78df3a1b" "891debfe489c769383717cc7d0020244a8d62ce6f076b2c42dd1465b7c94204d" "c5d320f0b5b354b2be511882fc90def1d32ac5d38cccc8c68eab60a62d1621f2" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "aa0a998c0aa672156f19a1e1a3fb212cdc10338fb50063332a0df1646eb5dfea" "5715d3b4b071d33af95e9ded99a450aad674e308abb06442a094652a33507cd2" "3fb5d37cdafa4aa86eeb63fdbf3a82c446d90204429c9886393e7e9261a1e3dd" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(debug-on-error t)
 '(doom-dracula-brighter-modeline t)
 '(doom-solarized-light-brighter-modeline t)
 '(electric-indent-mode nil)
 '(emojify-display-style (quote image))
 '(fci-rule-color "#6272a4")
 '(jdee-db-active-breakpoint-face-colors (cons "#191C25" "#80A0C2") t)
 '(jdee-db-requested-breakpoint-face-colors (cons "#191C25" "#A2BF8A") t)
 '(jdee-db-spec-breakpoint-face-colors (cons "#191C25" "#434C5E") t)
 '(line-spacing 0.2)
 '(magit-dispatch-arguments nil)
 '(nyan-mode t)
 '(olivetti-body-width 100)
 '(org-download-image-dir "~/.org/img")
 '(org-download-screenshot-method "screencapture -i %s" t)
 '(org-fontify-done-headline t)
 '(org-format-latex-options
   (quote
    (:foreground "White" :background "Transparent" :scale 1.6 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-hide-emphasis-markers t)
 '(org-indent-indentation-per-level 1)
 '(org-latex-packages-alist
   (quote
    (("" "mathpazo" nil)
     ("" "upquote" nil)
     ("" "csquotes" nil)
     ("" "minted" nil)
     "\\usepackage[left=1in,top=1in,right=1in,bottom=1.5in]{geometry}" "\\usepackage{sectsty}" "\\sectionfont{\\fontsize{12}{15}\\selectfont}" "\\subsectionfont{\\fontsize{10}{10}\\selectfont}" "\\usepackage{algorithm}" "\\usepackage{esdiff}" "\\usepackage{algpseudocode}")))
 '(org-latex-pdf-process (quote ("latexmk -shell-escape -bibtex -f -pdf %f")))
 '(package-selected-packages
   (quote
    (string-inflection multi-line prog-mode elisp-mode lisp-mode pandoc kaolin-themes elpy sos org-ref ox-reveal org-tree-slide pdf-tools doom-modeline ox-hugo typo org-bullets olivetti material-theme poet-theme load-theme-buffer-local auctex deft smooth-scrolling org-noter occur-x add-node-modules-path browse-kill-ring org ox-gfm browse-kill-ring+ zenburn-theme yapfify which-key wgrep use-package unicode-fonts tern smartparens rjsx-mode realgud ranger rainbow-delimiters pyvenv py-isort protobuf-mode prettier-js phoenix-dark-pink-theme peep-dired parsebib org-journal org-download org-attach-screenshot nyan-mode neotree multiple-cursors move-text meghanada markdown-mode+ magit lua-mode key-chord json-mode isend-mode hydra highlight-symbol highlight-indentation google-c-style golden-ratio flymake-python-pyflakes flymake-go flycheck-pyflakes fish-mode find-file-in-project expand-region exec-path-from-shell emojify emmet-mode dumb-jump dracula-theme doom-themes dockerfile-mode direnv dired-ranger dired-details+ crux counsel-projectile corral company-quickhelp company-go biblio beacon autodisass-java-bytecode auto-complete anaconda-mode all-the-icons-dired aggressive-indent ace-window)))
 '(pdf-tools-handle-upgrades nil)
 '(rcirc-colors pink-bliss-foreground-colors)
 '(safe-local-variable-values
   (quote
    ((org-ref-pdf-directory . "/Users/christabellairwanto/Repos/deep-probabilistic-models"))))
 '(tramp-remote-path
   (quote
    (tramp-own-remote-path tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin")) nil (tramp))
 '(vc-annotate-background "#282a36")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50fa7b")
    (cons 40 "#85fa80")
    (cons 60 "#bbf986")
    (cons 80 "#f1fa8c")
    (cons 100 "#f5e381")
    (cons 120 "#face76")
    (cons 140 "#ffb86c")
    (cons 160 "#ffa38a")
    (cons 180 "#ff8ea8")
    (cons 200 "#ff79c6")
    (cons 220 "#ff6da0")
    (cons 240 "#ff617a")
    (cons 260 "#ff5555")
    (cons 280 "#d45558")
    (cons 300 "#aa565a")
    (cons 320 "#80565d")
    (cons 340 "#6272a4")
    (cons 360 "#6272a4")))
 '(vc-annotate-very-old-color nil)
 '(which-function-mode t)
 '(word-wrap t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:inherit fixed-pitch :background "#23242f" :foreground "#bd93f9" :height 0.75))))
 '(org-block-begin-line ((t (:background "#23242f" :foreground "#6272a4" :height 0.7 :family "Iosevka"))))
 '(org-block-end-line ((t (:family "Iosevka"))))
 '(org-hide ((t (:foreground "#282c34" :family "Iosevka"))))
 '(org-indent ((t (:inherit org-hide))))
 '(org-level-1 ((t (:height 1.01))))
 '(org-meta-line ((t (:foreground "#83898d" :height 0.7 :family "Iosevka"))))
 '(org-property-value ((t (:foreground "LightGoldenrod4"))) t)
 '(org-quote ((t (:background "#F2E6CE" :height 10 :family "Iosevka"))))
 '(org-table ((t (:foreground "#6c71c4" :height 0.75 :family "Iosevka")))))
