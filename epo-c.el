;;; -*- Emacs-Lisp -*-
;;; EPO C dependent settings
;;; (c)2000-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Sun Apr 21 00:09:53 2002 on balius

;;[Commentary]
;;	
;;	EPOのC依存設定
;;	
;;	

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-c-structure-alist
  '((?b (type . block)
	(structure keyword " (" argument ") {\n" indent cursor "\n" "}" indent)
	(table ("if" "condition") ("for") ("while") ("switch"))
	(mustmatch . t) (arg-reader . epoi-java-block))
    (?f (type . function)
	(structure keyword "(" argument ");")
	;;(argsep "" "" nil ", " "" "")
	(argsep . c-function)
	(table . epo-c-libc-functions))))

(defvar epo-c-freq-funcs
  '(("printf" "Format" "Rest args..")
    ("fprintf" "FILE" "Format" "Rest args..")))

(defvar epo-c-keyword-cpp		;from font-lock.el
  "#\\s *\\(define\\|else\\|endif\\|error\\|file\\|\\(el\\)?if\\(n?def\\)?\\|include\\|line\\|pragma\\|undef\\)")
(defvar epo-c-keyword-type		;from font-lock.el
  "\\(char\\|short\\|int\\|long\\|\\(un\\)?signed\\|float\\|double\\|void\\|complex\\)\\*?")
(defvar epo-c-keyword-storage
  "extern\\|auto\\|register\\|static\\|volatile\\|const\\|restrict\\|enum\\|struct\\|union")

(defvar epo-c-keyword-misc
  "break\\|continu\\|do\\|else\\|if\\|for\\|return\\|switch\\|case\\|while\\|sizeof\\|typedef")

(defvar epo-c-keywords
  (concat
   epo-c-keyword-cpp	"\\|"
   epo-c-keyword-type	"\\|"
   epo-c-keyword-storage "\\|"
   epo-c-keyword-misc))

(defvar epo-c-keyword-define
  (concat
   epo-c-keyword-type	"\\|"
   epo-c-keyword-storage))

(defvar epo-c-keywords
  (concat
   "\\<\\("
   epo-c-keyword-cpp	"\\|"
   epo-c-keyword-type	"\\|"
   epo-c-keyword-misc
   "\\)\\>"))

(defvar epo-c-relation-alist
  '((?f (type . reference)
	(idpattern . "[A-Za-z_][0-9A-Za-z_]*")
	(relation
	 (definition
	   (pattern . epo-c-crexp-deffunc) (group . 0))
	 (reference (pattern . epo-c-crexp-reffunc) (group . 0)))
	(table . epo-c-libc-functions))
    (?v (type . reference)
	(idpattern . "[A-Za-z_][0-9A-Za-z_]*")
	(relation
	 (definition
	   (pattern
	    "\\(%i\\)"
	    (repeat-from epo-c-keyword-type
			 epo-c-vardef-ccre))
	   (group . 0))
	 (reference (pattern . epo-c-crexp-refvar) (group . 0)))
	(table . epo-c-libc-functions))
    ))

(defvar epo-c-crexp-deffunc
  '("\\<%i\\>" (after . "(") (paren 1 "{")))
(defvar epo-c-crexp-reffunc
  '("\\<%i\\>" (after . "(") (!paren 1 "{")))
(defvar epo-c-crexp-refvar
  '("\\<%i\\>" (!after . "(")))
(defvar epo-c-vardef-ccre
  '("\\*?[A-Za-z_][A-Za-z_]*\\s *\\(\\[.*\\]+\\)?\\s *=?"
    (or ((after . ",") (return))
	((after . "{") (paren 1 ",") (return))
	((after . "[^;,{}]+,") (return))))
  "Initial variable setting ccre")

(defvar epo-c-file-alist
  '((extension . ("\\.c" "\\.C" "\\.h"))
    (search . ("." "/usr/include" "/usr/X11R6/include/X11"
	       "/usr/local/include"))
    ))

(defvar epo-c-libc-functions nil
  "Default completion list of libc function names.")

(let*((dirlist '("/usr/share/man" "/usr/man" "/usr/share/catman/p_man"))
      (b " *epo-c man tmp*")
      (m (message "Collecting libc functions..."))
      d f
      (files
       (catch 'files
	 (while (setq d (car dirlist))
	   (cond
	    ((file-directory-p (expand-file-name "man3" d))
	     (throw 'files
		    (epo*process-output-list "echo man3*/*.*|xargs ls" b d)))
	    ((file-directory-p (expand-file-name "cat3" d))
	     (throw 'files
		    (epo*process-output-list "echo cat3*/*.*|xargs ls" b d))))
	   (setq dirlist (cdr dirlist))))))
  (if files
      (progn
	(while files
	  (if (not (string< "" (car files)))
	      nil
	    (setq f (substring (car files) (1+ (string-match "/" (car files))))
		  f (substring f 0 (string-match "\\." f)))
	    (or (assoc f epo-c-libc-functions)
		(setq epo-c-libc-functions
		      (cons (cons f nil) epo-c-libc-functions))))
	  (setq files (cdr files))))
    (setq epo-c-libc-functions
	  '(("printf") ("fprintf") ("fputc") ("putc"))))
  (message "%s%s" m "Done"))

;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-c-process-alist
  '((?j (type . compile) (command "Compile" "make" rootname)
	(builtin . "#make#"))
    (?m (type . make) (command "Make in toplevel" "make")
	(makefile . "^[Mm]akefile") (builtin . "/make/"))
    (?r (type . run) (prompt . t)
	(command "run-it" (basename "\\(.*\\)\\.c" "./\\1"))
	(builtin . "#!")))
  "*C dependent process alists")

(defvar epo-c-tagjump-alist
  '(("Compile\\|^Make" (type . inline) (pattern . "^\\(\\S +\\.c\\):\\(%l\\):")
     (matchinfo 1 . 2))))

(provide 'epo-c)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
