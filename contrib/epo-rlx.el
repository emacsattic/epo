;;; -*- Emacs-Lisp -*-
;;; EPO Relax dependent settings
;;; (c)2001-2002 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2001 Feb.25
;;; $Lastupdate: Sat Apr 06 17:39:00 2002 $ on inspire.

(require 'epo-xml)
;;[Commentary]
;;	
;;	EPO -- RELAX (Regular Language description for XML)
;;      http://www.xml.gr.jp/relax/
;;

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-rlx-structure-alist
  (append
   '((?b (type . block)
	 (structure indent "<" keyword argument ">\n" indent cursor "\n"
		    "</" keyword ">" indent)
	 (table . epo-rlx-block-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?s (type . inline)
	 (structure indent "<" keyword argument " />" cursor)
	 (table . epo-rlx-block-table)
;;;	(table . epo-rlx-inline-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil)))
   epo-xml-basic-alist))

(defvar epo-rlx-moduleVersion-aligns
  '(("1.0")))

(defvar epo-rlx-relaxCoreVersion-aligns
  '(("1.0")))

(defvar epo-rlx-targetNamespace-aligns
  '(("http://www.xml.gr.jp/xmlns/relaxCore")))

(defvar epo-rlx-targetXmlns-aligns
  '(("http://www.xml.gr.jp/xmlns/relaxCore")))

(defvar epo-rlx-type-aligns ;; rf. XML Press Vol.1 P26
  '(("NMTOKEN") ("NMTOKENS") ("ENTITY") ("ENTITYS")
    ("ID") ("IDREF") ("IDREFS") ("NOTATION")
    ("string") ("boolean") ("float") ("double")
    ("binary") ("uri-reference")  ("language") 
    ("Name")  ("QName")  ("NCName") ("integer")
    ("non-positive-integer") ("positive-integer")
    ("non-negative-integer") ("negative-integer") 
    ("long") ("int") ("short") ("byte") ("decimal")
    ("unsigned-long") ("unsigned-int") ("unsigned-short")
    ("unsigned-byte") ("unsigned-integer")
    ("date") ("time") ("timeInstant") ("timeDuration")
    ("recuurringInstant")
    ("emptyString") ("none") ))

(defvar epo-rlx-block-table
  '( ("module"
      ("moduleVersion" . epo-rlx-moduleVersion-aligns)
      ("relaxCoreVersion" . epo-rlx-relaxCoreVersion-aligns)
      ("targetNamespace" . epo-rlx-targetNamespace-aligns)
      ("xmlns" . epo-rlx-targetXmlns-aligns))
     ("interface" . 0)
     ("export" ("label"))
     ("include" ("moduleLocation" . epo-xml-file-name))
     ("annotation" . 0)
     ("documentation" . 0)
     ("elementRule" ("label") ("role") ("type" . epo-rlx-type-aligns))
     ("div" . 0)
     ("empty" . 0)
     ("ref" ("label") ("occurs"))
     ("choice" ("occurs"))
     ("sequence" ("occurs"))
     ("none" . 0)
     ("mixed" . 0)
     ("tag" ("name"))
     ("attribute"
      ("name")
      ("required" . epo-xml-boolean)
      ("type" . epo-rlx-type-aligns))
     ("hedgeRef" ("label"))
     ("hedgeRule" ("label"))
     ("attPool" ("role"))
     ("enumeration" ("value")))
  "Default block type elements")

(defvar epo-rlx-iteration-alist
  '((?i (type . interface)
	(opener . ((pattern "<interface\\>" (!before . comment-start))))
	(closer . ((pattern "</interface>" (!before . comment-start))))
	(iterator . ((indent "<export label=\"" cursor "\"/>"))))
    (?t (type . tag)
	(opener . ((pattern "<tag\\>" (!before . comment-start))))
	(closer . ((pattern "</tag>" (!before . comment-start))))
	(iterator . ((indent "<attribute name=\""
			     cursor "\" type=\"\"/>")))) ))

(defvar epo-rlx-relation-alist
  (append
   '((?f (type . file)
	 (idpattern . "[^ \t\n\r]+")
	 (relation
	  (href-file
	   (pattern "\\<include\\s +moduleLocation=\\s *"
		    (after . "\\(\"\\)?\\([^\"]*\\)\\1") (return))
	   (group . 2)))))
  epo-xml-relation-alist))

;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-rlx-command-line "relaxer" "*relaxer command name")
(defvar epo-rlx-process-alist
  '((?j (type . compile) (prompt . t)
	(command "relaxer" epo-rlx-command-line basename))
    (?a (type . version)
	(command "relaxVer" epo-rlx-command-line "-version"))
    (?v (type . version)
	(command "version" "java" "-version")))
  "*dependent process alists")

(provide 'epo-rlx)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 