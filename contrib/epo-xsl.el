;;; -*- Emacs-Lisp -*-
;;; EPO XSLT dependent settings
;;; (c) 2001 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2001 Aug 11
;;; $Lastupdate: Sat Mar 23 08:20:03 2002 $ on inspire.

(require 'epo-xml)
(require 'epo-html)
;;[Commentary]
;;	
;;	EPO -- XSLT
;;      http://www.infoteria.com/jp/contents/xml-data/
;;

;;;
;; Variables for Input Aider(EPOI)
;;;

(defvar epo-xsl-structure-alist
  (append
   '((?b (type . block)
	 (structure indent "<" keyword argument ">\n" indent cursor "\n"
		    "</" keyword ">" indent)
	 (table . epo-xsl-block-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?l (type . typeface)
	 (structure indent "<" keyword argument ">\n" indent cursor "\n"
		    "</" keyword ">" indent)
	 (table . epo-html-typeface-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?s (type . inline)
	 (structure indent "<" keyword argument " />" cursor)
	 (table . epo-xsl-inline-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?n (type . ret)
	 (structure "<br />" indent cursor)
	 (mustmatch . t)))
   (list (car epo-xml-basic-alist))))

(defvar epo-xsl-version-aligns '(("1.0")))
(defvar epo-xsl-xmlns-aligns  '(("http://www.w3.org/1999/XSL/Transform")))
(defvar epo-xsl-level-aligns  '(("single") ("multiple") ("any")))
(defvar epo-xsl-letter-aligns '(("alphabetic") ("traditional")))
(defvar epo-xsl-method-aligns '(("xml") ("html") ("text")))
(defvar epo-xsl-order-aligns  '(("ascending") ("descending")))
(defvar epo-xsl-case-aligns   '(("upper-first") ("lower-first")))

(defvar epo-xsl-block-common-table
  '(("xsl:apply-templates" ("select") ("mode"))
    ("xsl:copy" ("use-attribute-sets"))
    ("xsl:variable" ("name") ("select")))
  "Default common elements")

(defvar epo-xsl-block-base-table
  '(("xsl:stylesheet" ("version" . epo-xsl-version-aligns)
     ("xmlns:xsl" . epo-xsl-xmlns-aligns))
    ("xsl:attribute" ("name") ("namespace"))
    ("xsl:attribute-set" ("name") ("use-attribute-sets"))
    ("xsl:call-template" ("name"))
    ("xsl:choose" . 0)
    ("xsl:comment" . 0)
    ("xsl:element"  ("name") ("namespace") ("use-attribute-sets"))
    ("xsl:fallback" . 0)
    ("xsl:for-each" ("select"))
    ("xsl:if" ("test"))
    ("xsl:message" ("terminate" . epo-xml-yes))
    ("xsl:otherwise" . 0)
    ("xsl:param" ("name") ("select"))
    ("xsl:processing-instruction" ("name"))
    ("xsl:template" ("name") ("match") ("priority") ("mode"))
    ("xsl:text" ("disable-output-escaping" . epo-xml-yes))
    ("xsl:transform" ("id") ("extension-element-prefixes")
     ("exclude-result-prefixes") ("version"))
    ("xsl:when" ("test"))
    ("xsl:with-param" ("name") ("select")))
  "Default block type base elements")

(defvar epo-xsl-block-table
  (append epo-xsl-block-common-table
	  epo-xsl-block-base-table
	  epo-html-block-table)
  "Default block type elements")

(defvar epo-xsl-inline-table
  (append
   epo-xsl-block-common-table
   epo-html-inline-table
   '(("xsl:apply-imports" . 0)
     ("xsl:copy-of" ("select"))
     ("xsl:decimal-format"
      ("name") ("decimal-separator") ("grouping-separator")
      ("infinity") ("minus-sign") ("NaN") ("percent")
      ("per-mille") ("zero-digit") ("digit") ("pattern-separator"))
     ("xsl:import" ("href" . epo-xml-file-name))
     ("xsl:include" ("href" . epo-xml-file-name))
     ("xsl:key" ("name") ("match") ("use"))
     ("xsl:namespace-alias" ("stylesheet-prefix") ("result-prefix"))
     ("xsl:number" ("level" . epo-xsl-level-aligns)
      ("count") ("from") ("value") ("format")
      ("lang") ("letter-value" . epo-xsl-letter-aligns)
      ("grouping-separator") ("grouping-size"))
     ("xsl:output"
      ("method" . epo-xsl-method-aligns) ("version")
      ("encoding" . epo-xml-encoding)
      ("omit-xml-declaration" . epo-xml-yes) ("standalone" . epo-xml-yes)
      ("doctype-public") ("doctype-system") ("cdata-section-elements")
      ("indent" . epo-xml-yes) ("media-type"))
     ("xsl:preserve-space" ("elements"))
     ("xsl:sort" ("select") ("lang") ("data-type")
      ("order" . epo-xsl-order-aligns)
      ("case-order" . epo-xsl-case-aligns))
     ("xsl:strip-space" ("elements"))
     ("xsl:value-of" ("select") ("disable-output-escaping" . epo-xml-yes))))
  "Default inline type elements")

(defvar epo-xsl-relation-alist
  (append
   '((?f (type . file)
	 (idpattern . "[^ \t\n\r]+")
	 (relation
	  (href-file
	   (pattern "\\<in\\(clude\\|port\\)\\s +name=\\s *"
		    (after . "\\(\"\\)?\\([^\"]*\\)\\1") (return))
	   (group . 2)))))
   epo-xml-relation-alist))

;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-xsl-process-alist
  (if (getenv "JAVA_HOME")
      '((?x (type . compile)
	    (command "xalan" "java" "org.apatch.xalan.xslt.Process"
		     "-IN" epo-xml-in-target
		     "-XSL" basename
		     "-OUT" epo-xml-out-target))
	(?j (type . compile)
	    (command "xt" "java" "com.jclark.xsl.Driver"
		     epo-xml-in-target basename epo-xml-out-target))
	(?s (type . compile)
	    (command "saxon" "java" "com.icl.saxon.StyleSheet" "-o"
		     epo-xml-in-target basename epo-xml-out-target))
	(?p (type . compile)
	    (command "saxon use PI" "java" "com.icl.saxon.StyleSheet" "-o"
		     epo-xml-in-target epo-xml-out-target)))
    '((?j (type . compile)
	  (command "xt" "xt" epo-xml-in-target basename epo-xml-out-target))
      (?s (type . compile)
	  (command "saxon" "saxon" "-o"
		   epo-xml-in-target epo-xml-out-target basename))
      (?p (type . compile)
	  (command "saxon use PI" "saxon" "-o"
		   epo-xml-in-target epo-xml-out-target)))))
;;
;;	epop-tagjump-alist
;; 
;(defvar epo-xsl-tagjump-alist
;      '(("ant" (type . inline)
;	 (pattern . "\\s +\\[javac\\]\\s +\\(\\S +\\.java\\):\\(%l\\):")
;	 (matchinfo 1 . 2))))

(provide 'epo-xsl)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
