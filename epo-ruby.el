;;; -*- Emacs-Lisp -*-
;;; EPO Ruby dependent settings
;;; (c)2000-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Mon Feb  4 13:12:37 2002 on firestorm

;;[Commentary]
;;	
;;	EPOのRuby依存設定
;;	
;;[Abstract]
;;	

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-ruby-structure-alist
  '((?c (type . comment-block)
	(structure "=begin\n" indent cursor "\n=end" indent)
	(table . nil)
	(argsep . nil))
    (?e (type . ensure)
	(structure "begin\n" indent cursor "\nensure" indent
		   "\n" indent "\nend" indent))
    (?r (type . rescue)
	(structure "begin\n" indent cursor "\nrescue" indent
		   "\n" indent "\nend" indent))
    (?i (type . if)
	(structure "if " cursor " then\nelse" indent "\nend" indent))
    (?w (type . while)
	(structure "while " cursor " \nend" indent))
    (?b (type . block)
	(structure "{|" cursor "|\n" indent "\n}" indent))
    ))

(defvar epo-ruby-relation-alist
  '((?f (type . reference)
	(idpattern . "[A-Za-z_@\$][0-9A-Za-z_]*")
	(relation
	 (definition
	   (pattern "\\<def\\s +\\(%i\\)\\>\\|\\(%i\\)\\s *=")
	   (group 1 2)
	   (structure "def " keyword indent "(" cursor ")\n"
		      indent "\nend" indent))
	 (reference (pattern "\\<%i\\>") (group . 0)))
	(table . epo-ruby-default-symbols))
    ))

(defvar epo-ruby-process-alist
  '((?r (type . run) (prompt . t)
	(command "run" magic)))
  "*Ruby dependent process alists")

(defvar epo-ruby-file-alist
  '((extension . ".rb")
    (search . epo-ruby-load-path)
    (recursive-search . nil)))

(defvar epo-ruby-load-path
  (epo*process-output-list
   "ruby -e 'print $LOAD_PATH.join(\"\\n\")'" " *EPO ruby tmp*")
  "*List of each $LOAD_PATH of ruby")

(defvar epo-ruby-global-variables
  (epo*process-output-list
   "ruby -e 'print global_variables.join(\"\\n\")'"
   " *EPO ruby tmp*" nil nil 'alist)
  "*Alist of global_variables of ruby")

(defvar epo-ruby-misc-methods
  (let ((rp (locate-library "ruby-methods")))
    (epo*process-output-list
     (format "ruby %s | sort -u" rp)
     " *EPO ruby tmp*" nil nil 'alist))
    "*Alist of methods of ruby")

(defvar epo-ruby-reserved-words
  '(("BEGIN")("class")("ensure")("nil")("self")("when")("END")("def")("false")
    ("not")("super")("while")("alias")("defined")("for")("or")("then")("yield")
    ("and")("do")("if")("redo")("true")("begin")("else")("in")("rescue")
    ("undef")("break")("elsif")("module")("retry")("unless")("case")("end")
    ("next")("return")("until"))
  "*Reserved words of ruby")

(defvar epo-ruby-default-symbols
  (delq nil
	(append epo-ruby-global-variables
		epo-ruby-misc-methods
		epo-ruby-reserved-words))
  "Default ruby symbols(variables and methods)")

(defvar epo-ruby-file-alist
  '((extension . ".rb")
    (search . epo-ruby-load-path)
    (recursive-search . nil)
    ))

(defvar epo-ruby-tagjump-alist
  '(("run" (type . inline) (pattern . "^\\(\\S +\\.rb\\):\\(%l\\):")
     (matchinfo 1 . 2)))
  "TAG jump alist suggested by MIYOSHI Masanori")

(provide 'epo-ruby)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 

