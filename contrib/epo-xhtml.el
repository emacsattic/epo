;;; -*- Emacs-Lisp -*-
;;; EPO XHTML dependent settings
;;; (c) 2002 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2002 Mar 16
;;; $Lastupdate: Sat Apr 06 17:57:55 2002 $ on inspire.

(require 'epo-xml)
(require 'epo-html)
;;[Commentary]
;;	
;;	EPO -- XHTML
;;	

;;<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML Basic 1.0//EN"
;;    "http://www.w3.org/TR/xhtml-basic/xhtml-basic10.dtd">
;;<!DOCTYPE html 
;;     PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
;;     "DTD/xhtml1-strict.dtd">
;;<!DOCTYPE html 
;;     PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
;;     "DTD/xhtml1-transitional.dtd">
;;<!DOCTYPE html
;;     PUBLIC "-//W3C//DTD XHTML 1.0 Frameset//EN"
;;     "DTD/xhtml1-frameset.dtd">

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-xhtml-structure-alist
  (append
   '((?b (type . block)
	 (structure indent "<" keyword argument ">\n" indent cursor "\n"
		    "</" keyword ">" indent)
	 (table . epo-xhtml-block-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?l (type . typeface)
	 (structure indent "<" keyword argument ">" cursor "</" keyword ">")
	 (table . epo-html-typeface-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?s (type . inline)
	 (structure indent "<" keyword argument " />" cursor)
	 (table . epo-html-inline-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?n (type . ret)
	(structure "<br />" indent cursor)
	(mustmatch . t)))
   (list (car epo-xml-basic-alist))))

(defvar epo-xhtml-xmlns-aligns
  '(("http://www.w3.org/1999/xhtml")))

(setq epo-xhtml-block-table epo-html-block-table)
(setq epo-xhtml-block-table
      (delete
       (assoc "xhtml" epo-xhtml-block-table) epo-xhtml-block-table))
(setq epo-xhtml-block-table
      (delete
       (assoc "html" epo-xhtml-block-table) epo-xhtml-block-table))
(setq epo-xhtml-block-table
      (delete
       (assoc "style" epo-xhtml-block-table) epo-xhtml-block-table))
(setq epo-xhtml-block-table
      (append
       '(("html" ("xmlns" . epo-xhtml-xmlns-aligns)
	  ("lang" . epo-xml-lang) ("xml:lang" . epo-xml-lang))
	 ("li" . 0))
       epo-xhtml-block-table))

(defvar epo-xhtml-iteration-alist
  '((?o (type . ordered-list)
	(opener . ((pattern "<ol\\>" (!before . comment-start))))
	(closer . ((pattern "</ol>" (!before . comment-start))))
	(iterator . ((indent "<li>" cursor "</li>"))))
    (?u (type . unordered-list)
	(opener . ((pattern "<ul\\>" (!before . comment-start))))
	(closer . ((pattern "</ul>" (!before . comment-start))))
	(iterator . ((indent "<li>" cursor "</li>"))))
    (?d (type . definition-list)
	(opener . ((pattern "<dl\\>" (!before . comment-start))))
	(closer . ((pattern "</dl>" (!before . comment-start))))
	;;(iterator . ("<dt>" "<dd>"))
	(iterator . ((indent "<dt>" cursor "</dt>\n" indent "<dd></dd>"))))
    (?s (type . simple-table)
	(opener . ((pattern "<table\\>" (!before . comment-start))))
	(closer . ((pattern "</table>" (!before . comment-start))))
	(iterator . (("<tr>\n" indent "<td>" cursor 
		      "</td>\n" indent "</tr>" indent))))
    (?t (type . table)
	(opener . ((pattern "<table\\>" (!before . comment-start))))
	(closer . ((pattern "</table>" (!before . comment-start))))
	(iterator . (("<thead>" ("<tr>" ("<th>")))
		     ("<tbody>" ("<tr>" ("<td>" repeat) repeat))
		     ;("<tfoot>" ("<tr>" ("<td>"))) ;unnecessary in most case
		     )))
    (?r (type . table-row)
	(opener . ((pattern "<tr\\>" (!before . comment-start))))
	(closer . ((pattern "</tr>\\|</tbody>\\|<tr>\\|</table>"
		   (!before . comment-start))))
	(iterator . ((indent "<td>" cursor "</td>"))))  ))

(defvar epo-xhtml-relation-alist epo-xml-relation-alist)

;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-xhtml-process-alist
  (append
   '((?r (type . run) (prompt . t)
	 (command "preview" epo-xml-open-remote filename)))
   epo-xml-process-alist))
(defvar epo-xhtml-tagjump-alist epo-xml-tagjump-alist)

(provide 'epo-xhtml)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
