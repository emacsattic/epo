;;; -*- Emacs-Lisp -*-
;;; EPO TeX dependent settings
;;; (c)1999-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Mon Feb  4 13:12:41 2002 on firestorm

;;[Commentary]
;;	
;;	EPOの(La)TeX依存設定
;;	
;;[Abstract]
;;	

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-tex-structure-alist
  '((?b (type . begin)
	(structure "\\begin{" keyword "}" argument "\n" indent cursor "\n"
		   "\\end{" keyword "}" indent)
	(table . epo-latex-env-table)
	(argsep "" "{" nil "" "}" "")
	(mustmatch . nil) (arg-reader . epoi-tex-arg-reader-env))
    (?s (type . section)
	(structure "\\" keyword "{" argument "}")
	(table . epo-latex-section-table)
	(arg-reader . epoi-tex-arg-reader-section))
    (?m (type . maketitle)
	(structure "\\" keyword " ")
	(table . epo-latex-singlecmd-table))
    (?l (type . large)
	(structure "{\\" keyword " " cursor "}")
	(table . epo-latex-fontsize-table))))

(defun epoi-tex-arg-reader-env (env argp)
  "Argument reader works fine with YaTeX :)"
  (if (featurep 'yatex)
      (if (and (boundp 'type) (equal type "begin"))
	  (YaTeX-addin env)
	(if (fboundp (intern-soft (concat "YaTeX::" env)))
	    (funcall (intern (concat "YaTeX::" env)) argp)
	  nil))
    (read-string (format "arg#%d for %s: " argp env))))

(defun epoi-tex-arg-reader-section (cmd argp)
  "Argument reader for section-type macros"
  (if (featurep 'yatex)
      (if (and (intern-soft (concat "YaTeX::" cmd))
	       (fboundp (intern (concat "YaTeX::" cmd))))
	  (funcall (intern (concat "YaTeX::" cmd)) argp)
	"")
    ""))

(defvar epo-tex-iteration-alist
  '((?i (type . itemize)
	(opener . ((pattern "\\\\begin{itemize}" (!before . comment-start))))
	(closer . ((pattern "\\\\end{itemize}" (!before . comment-start))))
	(iterator . ("\\item ")))
    (?e (type . enumerate)
	(opener . ((pattern "\\\\begin{enumerate}"
			    (!before . comment-start))))
	(closer . ((pattern "\\\\end{enumerate}" (!before . comment-start))))
	(iterator . ("\\item ")))
    (?d (type . description)
	(opener . ((pattern "\\\\begin{description}"
			    (!before . comment-start))))
	(closer . ((pattern "\\\\end{description}"
		   (!before . comment-start))))
	(iterator . (("\\item[" cursor "]"))))
    (?t (type . tabular)
	(opener . ((pattern "\\\\begin{tabular}" (!before . comment-start))))
	(closer . ((pattern "\\\\end{tabular}" (!before . comment-start))))
	(iterator . epoi-tex-enviroment-iterators))
    ))

(defun epoi-tex-enviroment-iterators (opener)
  (let*((env (substring
	      opener 
	      (1+ (string-match "{" opener))
	      (string-match "}" opener)))
	(f (intern-soft (concat "YaTeX-intelligent-newline-" env))))
    (if (and f (fboundp f))
	(funcall f)
      (epoi*insert-structure-list '("&" cursor "&& \\")))))

;;;
;; Variables for Relation Resolver(EPOR)
;;;
(defvar epo-tex-relation-alist
  '((?l (type . reference)
	(idpattern . "\\w+")
	(relation
	 (labeldef
	  (pattern "\\\\label{\\(%i\\)}" (!before . comment-start))
	  (group . 1)
	  (structure "\\label{" keyword "}"))
	 (labelref
	  (pattern "\\\\ref{\\(%i\\)}" (!before . comment-start))
	  (group . 1))))
    (?c (type . reference)
	(idpattern . "\\w+")
	(relation
	 (bibitem
	  (pattern "\\\\bibitem\\(\\[.*\\]\\)?{\\(%i\\)}"
		   (!before . comment-start))
	  (group . 2)
	  (structure "\\bibitem[" cursor "]{" keyword "}"))
	 (cite
	  (pattern "\\\\cite{\\(%i\\)}" (!before . comment-start))
	  (group . 1))))
    (?b (type . box)
	(idpattern . "\\w+")
	(relation
	 (begin (pattern "\\\\begin{\\(%i\\)}") (group . 1))
	 (end (pattern "\\\\end{\\(%i\\)}") (group . 1)))
	(table . (epo-tex-structure-alist . ?b)))
    (?b (type . file)
	(idpattern . "[^ \t()'\"\n\r#/]+")
	(relation
	 (include
	  (pattern "\\\\\\(include\\|input\\){\\(%i\\)}")
	  (group . 2))))))

;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-tex-process-alist
  '((?j (type . compile) (command "typeset" "jlatex" filename)
	(builtin . "#!"))
    (?r (type . run) (prompt . t)
	(command "preview" "xdvi" rootname))
    (?l (type . run)
	(command "printing" "dvips" (basename "\\(.*\\)\\.tex$" "\\1.dvi"))
	(prompt . t)))
  "*(La)TeX dependent process alists")

(defvar epo-tex-tagjump-alist
  '(("typeset" (type . paren) (pattern . "^l\\.%l "))))

(defvar epo-tex-file-alist
  '((extension . ".tex")
    (search . ".")
    (recursive-search . t)
    ))

(load "yatex" t)
(load "yatexadd" t)
(if (featurep 'yatex)
    (progn
      (defvar epo-latex-section-table section-table)
      (defvar epo-latex-env-table env-table)
      (defvar epo-latex-fontsize-table fontsize-table)
      (defvar epo-latex-singlecmd-table singlecmd-table))
  (load "epo-yatex"))

(provide 'epo-tex)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 

