;;; -*- Emacs-Lisp -*-
;;; EPO jsp tag library dependent settings
;;; (c) 2002 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2002 Mar 24
;;; $Lastupdate: Sat Mar 30 10:07:33 2002 $ on inspire.

(require 'epo-xml)
;;[Commentary]
;;	
;;	EPO -- jsp tag library
;;      http://jakarta.apache.org/struts/
;;
;; <!DOCTYPE taglib PUBLIC
;;     "-//Sun Microsystems, Inc.//DTD JSP Tag Library 1.1//EN"
;;     "http://java.sun.com/j2ee/dtds/web-jsptaglibrary_1_1.dtd">

;;;
;; Variables for Input Aider(EPOI)
;;;

(defvar epo-tld-structure-alist
  (append
   '((?b (type . block)
	 (structure indent "<" keyword ">\n" argument indent cursor "\n"
		    "</" keyword ">" indent)
	 (table . epo-tld-block-table)
	 (argsep "" "" "" "" "" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?s (type . inline)
	 (structure indent "<" keyword ">" argument cursor
		    "</" keyword ">" indent)
	 (table . epo-tld-block-table)
	 (argsep "" "" "" "" "" "")
	 (mustmatch . nil) (arg-reader . nil)))
  epo-xml-basic-alist))

(defun epo-tld-boolean (arg1 arg2 arg3)
    (completing-read "boolean: " epo-xml-boolean))

(defvar epo-tld-block-table
  '(("taglib" . 0)
    ("tlibversion" . 0)
    ("jspversion" . 0)
    ("shortname" . 0)
    ("uri" . 0)
    ("tag" . 0)
    ("name" . 0)
    ("tagclass" . 0)
    ("teiclass" . 0)
    ("bodycontent" . 0)
    ("attribute" . 0)
    ("required" ("" . epo-tld-boolean))
    ("rtexprvalue" ("" . epo-tld-boolean)))
  "Default block type elements")

(defvar epo-tld-iteration-alist
  '((?l (type . taglib)
	(opener . ((pattern "<taglib\\>" (!before . comment-start))))
	(closer . ((pattern "</taglib>" (!before . comment-start))))
	(iterator . ((indent
		      "<tlibversion>" cursor "</tlibversion>\n"
		      "<jspversion>" indent "</jspversion>\n"
		      "<shortname>" indent "</shortname>\n"
		      "<uri>" indent "</uri>\n"
		      "<tag>" indent "\n</tag>" indent))))
    (?t (type . tag)
	(opener . ((pattern "<tag\\>" (!before . comment-start))))
	(closer . ((pattern "</tag>" (!before . comment-start))))
	(iterator . ((indent
		      "<name>" cursor "</name>\n"
		      "<tagclass>" indent "</tagclass>\n"
		      "<teiclass>" indent "</teiclass>\n"
		      "<bodycontent>" indent "</bodycontent>\n"
		      "<attribute>" indent "\n</attribute>" indent))))
    (?a (type . attribute)
	(opener . ((pattern "<attribute\\>" (!before . comment-start))))
	(closer . ((pattern "</attribute>" (!before . comment-start))))
	(iterator . ((indent
		      "<name>" cursor "</name>\n"
		      "<required>" indent "</required>\n"
		      "<rtexprvalue>" indent "</rtexprvalue>\n")))) ))

(defvar epo-tld-relation-alist epo-xml-relation-alist)
;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-tld-process-alist epo-xml-process-alist)
(defvar epo-tld-tagjump-alist epo-xml-tagjump-alist)

(provide 'epo-tld)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
