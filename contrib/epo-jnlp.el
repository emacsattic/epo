;;; -*- Emacs-Lisp -*-
;;; EPO jnlp dependent settings
;;; (c) 2002 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2002 Mar 19
;;; $Lastupdate: Thu Mar 21 08:46:58 2002 $ on inspire.

(require 'epo-xml)
;;[Commentary]
;;	
;;	EPO -- jnlp
;;      http://java.sun.com/products/javawebstart/
;;

;;;
;; Variables for Input Aider(EPOI)
;;;

(defvar epo-jnlp-structure-alist
  (append
   '((?b (type . block)
	 (structure indent "<" keyword argument ">\n" indent cursor "\n"
		    "</" keyword ">" indent)
	 (table . epo-jnlp-block-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?s (type . inline)
	 (structure indent "<" keyword argument " />" cursor)
	 (table . epo-jnlp-inline-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil)))
   epo-xml-basic-alist))

(defvar epo-jnlp-block-table
  '(("jnlp" ("spec") ("codebase") ("href" . epo-xml-file-name))
    ("information" . 0) ("title" . 0) ("vendor" . 0) ("description" . 0)
    ("icon " ("href" . epo-xml-file-name))
    ("resources" . 0) ("j2se" ("version"))
    ("jar" ("href" . epo-xml-file-name))
    ("jnlp-versions" . 0) ("resource" . 0) ("pattern" . 0) ("file" . 0)
    ("name" . 0) ("version-id" . 0) ("locale" . 0)
    ("platform" . 0) ("product-version-id" . 0))
  "Default block type elements")

(defvar epo-jnlp-inline-table
  '(("offline-allowed" . 0)
    ("security" . 0)
    ("application-desc" ("main-class")))
  "Default inline type elements")

(defvar epo-jnlp-iteration-alist
  '((?j (type . jnlp)
	(opener . ((pattern "<jnlp\\>" (!before . comment-start))))
	(closer . ((pattern "</jnlp>" (!before . comment-start))))
	(iterator . ((indent "<information>\n" indent cursor "\n" 
			     "</information>" indent "\n" indent
			     "<resources>\n" indent "</resources>\n" indent
			     "<security />\n" indent
			     "<application-desc />"))))
    (?i (type . info)
	(opener . ((pattern "<action-mappings\\>" (!before . comment-start))))
	(closer . ((pattern "</action-mappings>" (!before . comment-start))))
	(iterator . ((indent "<action>\n" indent cursor "</action>\n"))))
    (?a (type . action)
	(opener . ((pattern "<information\\>" (!before . comment-start))))
	(closer . ((pattern "</information>" (!before . comment-start))))
	(iterator . ((indent "<title>" cursor "</title>\n" indent
			     "<vendor></vendor>\n" indent
			     "<description></description>\n" indent
			     "<icon href=\"\" />"))))
    (?r (type . res)
	(opener . ((pattern "<resources\\>" (!before . comment-start))))
	(closer . ((pattern "</resources>" (!before . comment-start))))
	(iterator . ((indent "<j2se version=\"" cursor "\" />\n" indent
			     "<jar href=\"\" />"))))
    (?v (type . ver)
	(opener . ((pattern "<jnlp-versions\\>" (!before . comment-start))))
	(closer . ((pattern "</jnlp-versions>" (!before . comment-start))))
	(iterator . ((indent "<resource>\n" indent cursor "\n"
			     "</resource>" indent "\n"))))
    (?s (type . vres)
	(opener . ((pattern "<resource\\>" (!before . comment-start))))
	(closer . ((pattern "</resource>" (!before . comment-start))))
	(iterator . ((indent "<pattern>\n" indent cursor "\n"
			     "</pattern>" indent "\n" indent
			     "<file></file>\n"))))
    (?p (type . pattern)
	(opener . ((pattern "<pattern\\>" (!before . comment-start))))
	(closer . ((pattern "</pattern>" (!before . comment-start))))
	(iterator . ((indent "<name>" cursor "</name>\n" indent
			     "<version-id></version-id>\n")))) ))

(defvar epo-jnlp-relation-alist epo-xml-relation-alist)
;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-jnlp-process-alist epo-xml-process-alist)
;; 
;;	epop-tagjump-alist
;; 
(defvar epo-jnlp-tagjump-alist epo-xml-tagjump-alist)

(provide 'epo-jnlp)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
