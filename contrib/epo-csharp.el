;;; -*- Emacs-Lisp -*-
;;; EPO C# dependent settings
;;; (c)2002 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2002 Feb.1
;;; $Lastupdate: Sat Feb 02 08:19:03 2002 $ on inspire.

;;[Commentary]
;;	
;;	EPO -- C#
;;	
;;[Abstract]
;;	

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-csharp-structure-alist
  '((?b (type . block)
	(structure keyword " (" argument ") {\n" indent cursor "\n" "}" indent)
	(table ("if") ("for") ("foreach") ("while") ("switch"))
	(mustmatch . t) (arg-reader . epoi-csharp-block))
    (?t (type . try)
	(structure "try {\n" indent cursor
		   "\n" "} catch() {" indent
		   "\n" "} finally {" indent "\n" "}" indent)
	(mustmatch . t) (arg-reader . epo-csharp-block))
    (?s (type . identifer)
	(structure keyword)
	(table . epo-csharp-freq-identifer))
    (?f (type . function)
	(structure keyword "(" argument ");")
	(argsep "" "" nil ", " "" "")
	(table . epo-csharp-freq-funcs)) ))

(defun epo-csharp-block (keyword argp)
  (cond
   ((= argp 1) (read-string (format "%s, condition: " keyword)))))

(defvar epo-csharp-freq-identifer ;;; for csharp-font-lock-keywords-3
  '(("#if")           ("#elif")         ("#endif")        ("#else")      
    ("namespase")     ("using")         ("class")         ("interface")
    ("struct")        ("enum")          ("virtual")       ("delegate")
    ("override")      ("abstract")      ("extern")        ("static")
    ("public")        ("protected")     ("private")       ("internal")    
    ("in")            ("out")           ("ref")           ("sealed")   
    ("implicit")      ("explicit")      ("operator")
    ("bool")          ("byte")          ("char")          ("double")       
    ("float")         ("int")           ("long")          ("object")       
    ("sbyte")         ("short")         ("string")        ("uint")
    ("ulong")         ("ushort")        ("void")          ("base")
    ("const")         ("checked")
    ("switch")        ("case :")        ("default :")
    ("do")            ("while")         ("for")           ("foreach")
    ("if")            ("break")         ("continue")
    ("trow")          ("try")           ("catch")         ("finally")
    ("false")         ("true")          ("null")
    ("fixed")         ("lock")          ("new")           ("return")
    ("sizeof")        ("this")          ("typeof")        ("unchecked")))

(defvar epo-csharp-freq-funcs
  '(("System.Console.WriteLine")))

(defvar epo-csharp-iteration-alist
  '((?s (type . switch-case)
	(opener . ((pattern "\\<switch\\s *(.*)\\s *{"
			    (!before . comment-start))))
	(closer . ((pattern "}")))
	(iterator . ("case " cursor ":")))
    ))

;;;
;; Variables for Reference Handler(EPOR)
;;;
(defvar epo-csharp-relation-alist
  '((?f (type . reference)
	(idpattern . "[A-Za-z$_][0-9A-Za-z$_]*")
	(relation
	 (definition (pattern . epo-csharp-crexp-deffunc) (group . 0))
	 (reference (pattern . epo-csharp-crexp-reffunc) (group . 0))))
    ))

(defvar epo-csharp-crexp-deffunc
  '("\\<%i\\>" (after . "(") (paren 1 "{")))
(defvar epo-csharp-crexp-reffunc
  '("\\<%i\\>" (after . "(") (!paren 1 "{")))

;;;
;; Variables for Process Handler(EPOP)
;;;

(defvar epo-csharp-process-alist
  '((?j (type . compile) (command "Compile" "cs" basename))
    (?r (type . run) (prompt . t) (command "run" magic)))
  "*Csharp dependent process alists")

(defvar epo-csharp-tagjump-alist
  '(("Compile" (type . inline) (pattern . "\\.cs:%l:"))))


(provide 'epo-csharp)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
