;;; -*- Emacs-Lisp -*-
;;; EPO Emacs-Lisp dependent settings
;;; (c)2000-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Mon Feb  4 13:12:06 2002 on firestorm

;;[Commentary]
;;	
;;	EPO の Emacs-Lisp 依存設定
;;	
;;	

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-elisp-structure-alist
  '((?b (type . block)
	(structure "(" keyword cursor "\n)" indent)
	(table ("if") ("while") ("save-excursion") ("save-restriction")
	       ("progn") ("condition-case") ("unwind-protect")))
    (?d (type . defun)
	(structure "(defun " keyword " (" cursor ")\n\"\"" indent "\n)"))
    (?v (type . defvar)
	(structure "(defvar " keyword " " cursor "\n\"\"" indent "\n)"))))

(defvar epo-elisp-relation-alist
  '((?v (type . reference)
	(idpattern . "[^ \t()'\"\n\r#]+")
	(table . (epo-elisp-complete-vars . boundp))
	(relation
	 (defvars (pattern "(def\\(var\\|custom\\)\\s +\\(%i\\)\\>")
	   (group . 2) (structure . (epo-elisp-structure-alist . ?v)))
	 (varref
	  (pattern "\\(%i\\)"
		   (or ((!before . "(") (!before . "'")
			(!before . "(\\(def\\(un\\|macro\\|subst\\|alias\\|advice\\)\\|fset\\|require\\)"))
		       ((upparen -2 "(cond"))))
	  (group . 1))))
    (?f (type . reference)
	(idpattern . "[^ \t\n()'\"#]+")
	(table . (obarray . fboundp))
	(relation
	 (function-definition
	   (pattern
	    "(\\(def\\(un\\|macro\\|subst\\|alias\\|advice\\)\\|fset\\)\\s +'?\\(%i\\)\\>"
	    (after . "(\\|nil\\|'"))
	   (group . 3) (structure . (epo-elisp-structure-alist . ?d))
	   (partner . functionref1))
	 (functionref1
	  (pattern "(\\(%i\\)\\(\\s \\|$\\|[()]\\)")
	  (group . 1) (partner . function-definition))
	 (functionref2
	  (pattern "'\\(%i\\)\\(\\s \\|$\\|[()]\\)"
		   (before . "autoload\\|funcall\\|apply"))
	  (group . 1) (partner . function-definition))
	 ))
    (?r (type . file)
	(idpattern . "[^ \t()'\"\n\r#/]+")
	(relation
	 (require1 (pattern "(require\\s +'\\(%i\\))") (group . 1))
	 (require2 (pattern "(require\\s +'\\S +\"\\(%i\\)\")") (group . 1))))
    ))

(defun epo-elisp-collect-local-vars ()
  "Collect local variables in current fuction."
  (let ((p (point)) top vars (lim (progn (beginning-of-defun) (point))))
    (goto-char p)
    (catch 'stop
      (while (and (<= lim (point)) (not (bobp)))
	(condition-case nil
	    (up-list -1)
	  (error (throw 'stop t)))
	(cond
	 ((eq (char-after (1- (point))) ?\()
	  nil)				; "((let .." may be let variable itself
	 ((looking-at "(let\\*?")
	  (setq top (point))
	  (goto-char (match-end 0))
	  (skip-chars-forward " \t\n\r")
	  (or (looking-at "(")
	      (error "Invalid `let' form found on point %d" (point)))
	  (condition-case nil
	      (progn
		(forward-sexp 1)
		(down-list -1)
		(catch 'end
		  (while t
		    (condition-case nil
			(backward-sexp 1)
		      (error (throw 'end t)))
		    (if (looking-at "(?\\([^() \t\n\r'\"#]+\\)")
			(setq vars
			      (cons (cons (epo*match-string 1) nil) vars)))))
		(goto-char top))
	    (error (goto-char top)))))))
    (goto-char p)
    vars))

(defun epo-elisp-complete-vars (word predicate listall)
  "Complete Emacs-Lisp variables including local bindings by `let'."
  (let ((localvars (epo-elisp-collect-local-vars)))
    (if localvars
	(eval (list 'let (mapcar '(lambda (s) (intern (car s)))
				 localvars)
		    (list (if listall 'all-completions 'try-completion)
			  word 'obarray (list 'quote predicate))))
      (funcall (if listall 'all-completions 'try-completion)
	       word obarray predicate))))

(defvar epo-elisp-file-alist
  '((extension . ".el")
    (search . load-path)
    (recursive-search . nil)
    ))

;;;
;; Variables for Process Handler(EPOP)
;;;
;;- Nothing needed for Emacs-Lisp...

(provide 'epo-elisp)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
