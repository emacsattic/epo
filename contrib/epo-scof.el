;;; -*- Emacs-Lisp -*-
;;; EPO Struts-config dependent settings
;;; (c) 2002 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2002 Mar 19
;;; $Lastupdate: Tue Apr 02 19:31:46 2002 $ on inspire.

(require 'epo-xml)
;;[Commentary]
;;	
;;	EPO -- Struts-config
;;      http://jakarta.apache.org/struts/

;; <!DOCTYPE struts-config PUBLIC
;;    "-//Apache Software fundation//DTD Struts Configration 1.0//EN"
;;    "http://jakarta.apache.org/struts/dtd/struts-config_1_0.dtd">

;;;
;; Variables for Input Aider(EPOI)
;;;

(defvar epo-scof-structure-alist
  (append
   '((?b (type . block)
	 (structure indent "<" keyword argument ">\n" indent cursor "\n"
		    "</" keyword ">" indent)
	 (table . epo-scof-block-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?s (type . inline)
	 (structure indent "<" keyword argument " />" cursor)
	 (table . epo-scof-inline-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil)))
   epo-xml-basic-alist))

(defvar epo-scof-scope-alist
  '(("session") ("request")))

(defvar epo-scof-block-table
  '(("struts-config" . 0)
    ("global-forwards" ("type"))
    ("from-beans" . 0)
    ("action-mappings" . 0)
    ("action" ("path") ("type")
     ("name" . epo-xml-same-alist)
     ("scope" . epo-scof-scope-alist)
     ("input") ("unknown") ("validate"))
    ("data-sources" . 0) )
  "Default block type elements")

(defvar epo-scof-inline-table
  '(("from-bean" ("name" . epo-xml-same-alist) ("type"))
    ("forward" ("name" . epo-xml-same-alist) ("path") ("redirect"))
    ("data-source" ("autoCommit") ("description")
     ("driverClass") ("maxCount") ("minCount")
     ("password") ("url") ("user")))
  "Default inline type elements")

(defvar epo-scof-iteration-alist
  '((?s (type . config)
	(opener . ((pattern "<struts-config\\>" (!before . comment-start))))
	(closer . ((pattern "</struts-config>" (!before . comment-start))))
	(iterator . ((indent "<from-beans>\n" indent cursor "\n"
			     "</from-beans>" indent "\n"
			     indent "<action-mappings>\n" indent
			     "</action-mappings>" indent "\n"))))
    (?f (type . from)
	(opener . ((pattern "<from-beans\\>" (!before . comment-start))))
	(closer . ((pattern "</from-beans>" (!before . comment-start))))
	(iterator . ((indent "<from-bean " cursor " />\n"))))
    (?m (type . map)
	(opener . ((pattern "<action-mappings\\>" (!before . comment-start))))
	(closer . ((pattern "</action-mappings>" (!before . comment-start))))
	(iterator . (("<action path=\"" indent cursor
		      "\" type=\"\" name=\"\" scope=\"\">\n" indent 
		      "</action>" indent "\n"))))
    (?a (type . action)
	(opener . ((pattern "<action \\>" (!before . comment-start))))
	(closer . ((pattern "</action>" (!before . comment-start))))
	(iterator . ((indent "<forward name=\"" cursor "\" path=\"\" />\n"))))
    (?g (type . global)
	(opener . ((pattern "<global-forwards\\>" (!before . comment-start))))
	(closer . ((pattern "</global-forwards>" (!before . comment-start))))
	(iterator . ((indent "<forward name=\"" cursor "\" path=\"\" />\n"))))
    (?d (type . data)
	(opener . ((pattern "<data-sources\\>" (!before . comment-start))))
	(closer . ((pattern "</data-sources>" (!before . comment-start))))
	(iterator . ((indent "<data-source " cursor " />\n"))))  ))

(defvar epo-scof-relation-alist epo-xml-relation-alist)
;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-scof-process-alist epo-xml-process-alist)
;; 
;;	epop-tagjump-alist
;; 
(defvar epo-scof-tagjump-alist epo-xml-tagjump-alist)

(provide 'epo-scof)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
