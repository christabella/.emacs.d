;; -------------------------------------
;; -- PDF
;; -------------------------------------
;; 'djcb-org-article' for export org documents to the LaTex 'article', using
;; XeTeX and some fancy fonts; requires XeTeX (see org-latex-to-pdf-process)
;; -----------------------------------------------------------------------------
;; http://emacs-fu.blogspot.com/2011/04/nice-looking-pdfs-with-org-mode-and.html
;; http://comments.gmane.org/gmane.emacs.orgmode/40221
;; -----------------------------------------------------------------------------
;; Install Packages:
;; + texlive-all  
;; + texlive-xetex
;; + ttf-sil-gentium
;; + ttf-sil-gentium-basic
;; + ttf-sil-charis
;; + ttf-dejavu
;; -----------------------------------------------------------------------------
;; Make sure to include the latex class in you header:
;; #+LaTeX_CLASS: djcb-org-article
;; -----------------------------------------------------------------------------
(eval-after-load 'org-ref
  '(progn
     (add-to-list 'org-export-latex-classes
		  '("djcb-org-article"
		    "\\documentclass[11pt,a4paper]{article}
             \\usepackage{minted}
             \\usemintedstyle{emacs}
             \\newminted{common-lisp}{fontsize=10}
                     \\usepackage[T1]{fontenc}
                     \\usepackage{hyperref}
                     \\usepackage{fontspec}
                     \\usepackage{graphicx} 
                     \\defaultfontfeatures{Mapping=tex-text}
                     \\setromanfont{Gentium}
                     \\setromanfont [BoldFont={Gentium Basic Bold},
                                     ItalicFont={Gentium Basic Italic}]{Gentium Basic}
                     \\setsansfont{Charis SIL}
                     \\setmonofont[Scale=0.8]{DejaVu Sans Mono}
                     \\usepackage{geometry}
                     \\geometry{a4paper, textwidth=6.5in, textheight=10in,
                                 marginparsep=7pt, marginparwidth=.6in}
                     \\pagestyle{empty}
                     \\title{}
                           [NO-DEFAULT-PACKAGES]
                           [NO-PACKAGES]"
		    ("\\section{%s}" . "\\section*{%s}")
		    ("\\subsection{%s}" . "\\subsection*{%s}")
		    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		    ("\\paragraph{%s}" . "\\paragraph*{%s}")
		    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))
;; -----------------------------------------------------------------------------
;; Added Syntax Highlighting Support
;; http://orgmode.org/worg/org-tutorials/org-latex-export.html
;; #+LaTeX_HEADER: \usepackage{minted}
;; #+LaTeX_HEADER: \usemintedstyle{emacs}
;; #+LaTeX_HEADER: \newminted{common-lisp}{fontsize=\footnotesize}
;; -----------------------------------------------------------------------------
;; Install Packages:
;; + python-pygments
;; -----------------------------------------------------------------------------
(setq org-export-latex-listings 'minted)
(setq org-export-latex-custom-lang-environments
      '(
    (emacs-lisp "common-lispcode")
       ))
(setq org-export-latex-minted-options
      '(("frame" "lines")
        ("fontsize" "\\scriptsize")
    ("linenos" "")
))
(setq org-latex-to-pdf-process 
      '("xelatex --shell-escape -interaction nonstopmode %f"
    "xelatex --shell-escape -interaction nonstopmode %f")) ;; for multiple passes
;; Not sure if this is actually setting the export class correctly.
(setq org-export-latex-class "djcb-org-article")
;;
;;
