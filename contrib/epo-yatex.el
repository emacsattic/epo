;;; -*- Emacs-Lisp -*-
;;; EPO symbol table for LaTeX adopted from YaTeX
;;; epo.el rev. 1.1c
;;; (c)2001 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Wed Feb  7 23:33:18 2001 on firestorm



(defvar epo-latex-section-table
  '(("part") ("chapter") ("chapter*") ("section") ("section*")
    ("subsection") ("subsection*")
    ("subsubsection") ("paragraph") ("subparagraph")
    ("author") ("thanks") ("documentstyle") ("pagestyle") ("thispagestyle")
    ("title") ("underline") ("label") ("makebox")
    ("footnote") ("footnotetext") ("index")
    ("hspace*") ("vspace*") ("bibliography") ("bibitem") ("cite")
    ("input") ("include") ("includeonly") ("mbox") ("hbox") ("caption")
    ("newlength") ("setlength" 2) ("addtolength" 2) ("settowidth" 2)
    ("setcounter" 2) ("addtocounter" 2) ("stepcounter" 2)
    ("newcommand" 2) ("renewcommand" 2)
    ("setcounter" 2) ("newenvironment" 3) ("newtheorem" 2)
    ("cline") ("framebox") ("savebox" 2) ("sbox" 2) ("newsavebox") ("usebox")
    ("date") ("put") ("ref") ("pageref")
    ("multicolumn" 3) ("shortstack")
    ;; for mathmode accent
    ("tilde") ("hat") ("check") ("bar") ("dot") ("ddot") ("vec")
    ("widetilde") ("widehat") ("overline") ("overrightarrow")
    ;; section types in mathmode
    ("frac" 2) ("sqrt") ("mathrm") ("mathbf") ("mathit")
    ("documentclass") ("usepackage")
    ("textbf") ("textgt") ("textit") ("textmc") ("textmd") ("textnormal")
    ("textrm") ("textsc") ("textsf") ("textsl") ("texttt") ("textup")
    ("mathbf") ("mathcal") ("mathit") ("mathnormal") ("mathrm")
    ("mathsf") ("mathtt")
    ("scalebox" 1)			;is faking of argument position
    ("rotatebox" 2) ("resizebox" 2) ("reflectbox")
    ("colorbox" 2) ("fcolorbox" 3) ("textcolor" 2) ("color")
    ("includegraphics") ("includegraphics*")
    )
  "Default completion table for section-type completion.")

; Set tex-environment possible completion
(defvar epo-latex-env-table
  '(("quote") ("quotation") ("center") ("verse") ("document")
    ("verbatim") ("itemize") ("enumerate") ("description")
    ("list") ("tabular") ("tabular*") ("table") ("tabbing") ("titlepage")
    ("sloppypar") ("picture") ("displaymath")
    ("eqnarray") ("figure") ("equation") ("abstract") ("array")
    ("thebibliography") ("theindex") ("flushleft") ("flushright")
    ("minipage"))
  "Default completion table for begin-type completion.")

; Set {\Large }-like completion
(defvar epo-latex-fontsize-table
  '(("rm") ("em") ("bf") ("boldmath") ("it") ("sl") ("sf") ("sc") ("tt")
    ("dg") ("dm")
    ("tiny") ("scriptsize") ("footnotesize") ("small")("normalsize")
    ("large") ("Large") ("LARGE") ("huge") ("Huge")
    ("rmfamily") ("sffamily") ("ttfamily")
    ("mdseries") ("bfseries") ("upshape")
    ("itshape") ("slshape") ("scshape"))
  "Default completion table for large-type completion.")

(defvar LaTeX2e-fontstyle-alist
  '(("rm" . "rmfamily")
    ("sf" . "sffamily")
    ("tt" . "ttfamily")
    ("md" . "mdseries")
    ("bf" . "bfseries")
    ("up" . "upshape")
    ("it" . "itshape")
    ("sl" . "slshape")
    ("sc" . "scshape")))

(defvar epo-latex-singlecmd-table
   '(("maketitle") ("makeindex") ("sloppy") ("protect")
     ("LaTeX") ("TeX") ("item") ("item[]") ("appendix") ("hline") ("kill")
     ;;("rightarrow") ("Rightarrow") ("leftarrow") ("Leftarrow")
     ("pagebreak") ("nopagebreak") ("tableofcontents")
     ("newpage") ("clearpage") ("cleardoublepage")
     ("footnotemark") ("verb") ("verb*")
     ("linebreak") ("pagebreak") ("noindent") ("indent")
     ("left") ("right") ("dots") ("smallskip") ("medskip") ("bigskip")
     ("alpha") ("beta") ("gamma") ("delta") ("epsilon")
     ("varepsilon") ("zeta") ("eta") ("theta")("vartheta")
     ("iota") ("kappa") ("lambda") ("mu") ("nu") ("xi") ("pi")
     ("varpi") ("rho") ("varrho") ("sigma") ("varsigma") ("tau")
     ("upsilon") ("phi") ("varphi") ("chi") ("psi") ("omega")
     ("Gamma") ("Delta") ("Theta") ("Lambda")("Xi") ("Pi")
     ("Sigma") ("Upsilon") ("Phi") ("Psi") ("Omega"))
   "Default completion table for maketitle-type completion.")
