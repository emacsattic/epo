;;; -*- Emacs-Lisp -*-
;;; EPO CORBA/IDL dependent settings
;;; (c)2000, 2001 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2001 Feb.25
;;; $Lastupdate: Sun Nov 11 20:19:17 2001 $ on inspire.

;;[Commentary]
;;	
;;	EPO -- CORBA/IDL
;;

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-idl-structure-alist
  '((?b (type . block)
	(structure keyword " " argument " {\n" indent cursor "\n" "};" indent)
	(table . epo-idl-freq-block)
	(mustmatch . t) (arg-reader . epo-idl-block))
    (?s (type . identifer)
	(structure keyword)
	(table . epo-idl-freq-identifer))
    (?u (type . union)
	(structure
	 "union " argument " switch() {\n" indent
	 "case " cursor ":\n" indent
	 "};" indent)
	(mustmatch . t) (arg-reader . epo-idl-block)) ))

(defun epo-idl-block (keyword argp)
  (cond
   ((= argp 1)
    (if (equal nil keyword)
	(read-string (format "union condition: "))
	(read-string (format "%s condition: " keyword))))))

(defvar epo-idl-freq-block
  '( ("enum") ("struct") ("module") ("interface") ("exception")
     ("operation")))

(defvar epo-idl-freq-identifer
  '(("boolean") ("short") ("long") ("long long")
    ("unsigned short") ("unsigned long") ("unsigned long long")
    ("flort") ("double") ("long double") ("char") ("wchar")
    ("fixed") ("octet") ("string") ("wstring")
    ("any") ("void") ; not int
    ("sequence") ("constant")
    ("readonly attribute") ("readwrite attribute")
    ("typedef") ("in") ("out") ("inout")

    ("case :") ("default:") ("raises();") )) ;;

(defvar epo-idl-keyword-cpp
  "#\\s *\\(define\\|else\\|endif\\|error\\|file\\|\\(el\\)?if\\(n?def\\)?\\|include\\|line\\|pragma\\|undef\\)")
(defvar epo-idl-keyword-type
  "\\(boolean\\|unsigned\\|char\\|string\\|short\\|long\\|float\\|double\\|void\\|any\\|constant\\|w\\(char\\|string\\)\\)\\*?")
(defvar epo-idl-keyword-storage
  "enum\\|struct\\|union\\|read\\(only\\|write\\)\\|attribute\\|typedef")

(defvar epo-idl-keywords
  (concat
   epo-idl-keyword-cpp	"\\|"
   epo-idl-keyword-type	"\\|"
   epo-idl-keyword-storage))

(defvar epo-idl-keyword-define
  (concat
   epo-idl-keyword-type	"\\|"
   epo-idl-keyword-storage))

(defvar epo-idl-keywords
  (concat
   "\\<\\("
   epo-idl-keyword-cpp	"\\|"
   epo-idl-keyword-type
   "\\)\\>"))

;;;
;; Variables for Relation Resolver(EPOR)
;;;
(defvar epo-idl-relation-alist
  '((?f (type . reference)
	(idpattern . "[A-Za-z_][0-9A-Za-z_]*")
	(relation
	 (definition
	   (pattern . epo-idl-crexp-deffunc) (group . 0))
	 (reference (pattern . epo-idl-crexp-reffunc) (group . 0)))
	(table . epo-idl-libc-functions))
    (?v (type . reference)
	(idpattern . "[A-Za-z_][0-9A-Za-z_]*")
	(relation
	 (definition
	   (pattern
	    "\\(%i\\)"
	    (repeat-from epo-idl-keyword-type
			 epo-idl-vardef-ccre))
	   (group . 0))
	 (reference (pattern . epo-idl-crexp-refvar) (group . 0)))
	(table . epo-idl-libc-functions))
    ))

(defvar epo-idl-crexp-deffunc
  '("\\<%i\\>" (after . "(") (paren 1 "{")))
(defvar epo-idl-crexp-reffunc
  '("\\<%i\\>" (after . "(") (!paren 1 "{")))
(defvar epo-idl-crexp-refvar
  '("\\<%i\\>" (!after . "(")))
(defvar epo-idl-vardef-ccre
  '("\\*?[A-Za-z_][A-Za-z_]*\\s *\\(\\[.*\\]+\\)?\\s *=?"
    (or ((after . ",") (return))
	((after . "{") (paren 1 ",") (return))
	((after . "[^;,{}]+,") (return))))
  "Initial variable setting ccre")

(defvar epo-idl-file-alist
  '((extension . ("\\.idl"))
    (search . (".")) ))

;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-idl-process-alist
  '((?j (type . compile) (command "Compile" "make" rootname)
	(builtin . "#make#"))
    (?r (type . run) (prompt . t)
	(command "run-it" (basename "\\(.*\\)\\.idl" "./\\1"))
	(builtin . "#!")))
  "*C dependent process alists")

(defvar epo-idl-tagjump-alist
  '(("Compile" (type . inline) (pattern . "\\(\\S +\\.idl\\):\\(%l\\):")
     (matchinfo 1 . 2))))

(provide 'epo-idl)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
