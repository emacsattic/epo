;;; -*- Emacs-Lisp -*-
;;; EPO Xi dependent settings
;;; (c) 2001 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2001 Aug 11
;;; $Lastupdate: Sat Apr 06 17:48:21 2002 $ on inspire.

(require 'epo-xml)
;;[Commentary]
;;	
;;	EPO -- Xi (Extend it! Xi for XML)
;;      http://www.baykit.org/xi/index-en.html
;;      http://www.baykit.org/xi/index-jp.html
;;

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-xi-structure-alist
  (append 
   '((?b (type . block)
	 (structure indent "<" keyword argument ">\n" indent cursor "\n"
		    "</" keyword ">" indent)
	 (table . epo-xi-block-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?s (type . inline)
	 (structure indent "<" keyword argument " />" cursor)
	 (table . epo-xi-block-table)
;;;      (table . epo-xi-inline-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil)))
  epo-xml-basic-alist))

(defvar epo-xi-version-aligns
  '(("1.0")))

(defvar epo-xi-xmlns-aligns
  '(("http://www.baykit.org/Xi/1.0")))

(defvar epo-xi-yes-no '(("yes") ("no")))
(defvar epo-xi-data-type '(("text") ("number")))
(defvar epo-xi-order '(("acsending") ("descending")))
(defvar epo-xi-method '(("html" "xml")))

(defvar epo-xi-block-table
  '( ("xi:program"
      ("version" . epo-version-aligns)
      ("xmlns:xi" . epo-xi-xmlns-aligns))
     ("xi:var" ("name") ("select"))
     ("xi:variable" ("name") ("select"))
     ("xi:value-of"
      ("select")
      ("disable-output-escaping" . epo-xi-yes-no))
     ("xi:eval" ("select"))
     ("xi:exec" ("select"))
     ("xi:set" ("select") ("out"))
     ("xi:copy-of" ("select"))
     ("xi:text"
      ("disable-output-escaping" . epo-xi-yes-no)
      ("cdata-section" . epo-xi-yes-no))
     ("xi:include" ("href" . epo-xml-file-name))
     ("xi:element" ("name"))
     ("xi:attribute" ("name"))
     ("xi:comment" . 0)
     ("xi:processing-instruction" ("name"))
     ("xi:block" . 0)
     ("xi:if" ("test"))
     ("xi:choose" . 0)
     ("xi:when" ("test"))
     ("xi:otherwise" . 0)
     ("xi:sort"
      ("select")
      ("data-type" . epo-xi-data-type)
      ("order" . epo-xi-order))
     ("xi:while" ("test"))
     ("xi:raise" ("select"))
     ("xi:try" . 0)
     ("xi:exception" ("name"))
     ("xi:finally" . 0)
     ("xi:for-each" ("select") ("item"))
     ("xi:message" ("select") ("terminate" . epo-xi-yes-no))
     ("xi:function" ("name") ("return"))
     ("xi:param" ("name") ("select"))
     ("xi:call-function" ("name"))
     ("xi:with-param" ("name") ("select"))
     ("xi:output" ("method" . epo-xi-method)
      ("encoding" . epo-xml-encoding)))
  "Default block type elements")

(defvar epo-xi-relation-alist
  (append
   '((?f (type . file)
	 (idpattern . "[^ \t\n\r]+")
	 (relation
	  (href-file
	   (pattern "\\<include\\s +href=\\s *"
		    (after . "\\(\"\\)?\\([^\"]*\\)\\1") (return))
	   (group . 2)))))
   epo-xml-relation-alist))

;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-xi-command-line "xi" "*xi command name")
(defvar epo-xi-process-alist
  '((?j (type . compile) (prompt . t)
	(command "xi" epo-xi-command-line basename)))
  "*dependent process alists")

(provide 'epo-xi)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 