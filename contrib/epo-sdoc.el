;;; -*- Emacs-Lisp -*-
;;; EPO SmartDoc dependent settings
;;; (c)2002 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2001 Apr.6
;;; $Lastupdate: Wed Jun 05 23:21:15 2002 $ on inspire.

(require 'epo-xml)
(require 'epo-html)
;;[Commentary]
;;	
;;	EPO -- SmartDoc
;;      http://www.asahi-net.or.jp/~DP8T-ASM/java/tools/SmartDoc/index_ja.html
;;

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-sdoc-structure-alist
  (append
   '((?b (type . block)
	 (structure indent "<" keyword argument ">" indent cursor
		    "</" keyword ">" indent)
	 (table . epo-sdoc-block-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?l (type . typeface)
	 (structure indent "<" keyword argument ">" cursor "</" keyword ">")
	 (table . epo-sdoc-typeface-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?s (type . inline)
	 (structure indent "<" keyword argument " />" cursor)
	 (table . epo-sdoc-inline-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?n (type . ret)
	(structure "<p />" indent cursor)
	(mustmatch . t)))
   (list (car epo-xml-basic-alist))))

(defun epo-xml-idref-alist (arg1 arg2 arg3)
  (epo-xml-same-alist arg1 arg2 "id"))

(defvar epo-sdoc-align-alist '(("left") ("right") ("center")))
(defvar epo-sdoc-format-alist
  '(("html4") ("html3") ("latex2e") ("latex2e") ("javahelp")
    ("puresmartdoc")))
(defvar epo-sdoc-block-table
  '( ("doc" ("xml:lang" . epo-xml-lang))
     ("head" . 0)
     ("title" . 0)
     ("attribute" ("locale") ("name"))
     ("subtitle" . 0) ("author" ("org") ("email") ("hp"))
     ("org" . 0) ("date" . 0) ("hp" . 0) ("email" . 0) ("abstract" . 0)
     ("header" . 0) ("footer" . 0) ("prologue" . 0)
     ("body" . 0) ("p" . 0)
     ("part"  ("id" . epo-xml-same-alist) ("title")) ;;; h1
     ("chapter" ("id" . epo-xml-same-alist) ("title")) ;;; h2
     ("section" ("id" . epo-xml-same-alist) ("title")) ;;; h3
     ("subsection" ("id" . epo-xml-same-alist) ("title")) ;;; h4
     ("subsubsection" ("id" . epo-xml-same-alist) ("title")) ;;; h5
     ("appendix" ("id" . epo-xml-same-alist)
      ("title"))
     ("ul" . 0) ("ol" . 0) ("dl" . 0) ("li" . 0) ("dt" . 0) ("dd" . 0)
     ("table" ("id" . epo-xml-same-alist)
      ("title") ("style" . (("width:95%")))
      ("adapter") ("aparam"))
     ("thead" . 0) ("tfoot" . 0) ("tbody" ("src" . epo-xml-file-name))
     ("tr" . 0)
     ("td" ("colspan") ("rowspan") ("align" . epo-sdoc-align-alist))
     ("th" ("colspan") ("rowspan") ("align" . epo-sdoc-align-alist))
     ("colgroup" ("span"))
     ("col" ("align" . epo-sdoc-align-alist))
     ("tnote" . 0)
     ("a" ("href" . epo-xml-file-name))
     ("cite" ("href" . epo-xml-file-name))
     ("program" ;;("src" . epo-xml-file-name)
      ("title" . epo-xml-file-name)
      ("normalizer" . epo-sdoc-normalizer-alist)
      ("javasrcKeyword")
      ("javasrcCount")
      ("javasrcSyntaxHilight" . epo-xml-boolean))
     ("console" ("title")
      ("normalizer" . epo-sdoc-normalizer-alist))
     ("native" ("format" . epo-sdoc-format-alist))
     ("or" . 0)
     ("bibliography" . 0) ("bibliopole" . 0) ("book" . 0)
     ("article" ("id" . epo-xml-same-alist))
     ("author" . 0) ("editor" . 0) ("title" . 0)
     ("subtitle" . 0) ("edition" . 0) ("publisher" . 0)
     ("year" . 0) ("month" . 0) ("volume" . 0) ("number" . 0)
     ("journal" ("id" . epo-xml-same-alist)) )
  "Default block type elements")

(defvar epo-sdoc-normalizer-alist
  '(("none") ("natural") ("naturallabel") ("csv") ("image") ("href")
    ("program") ("console") ("line") ("regex")
    ("javasrc") ("xmlsrc")
    ("tex")))
(defun epo-sdoc-image-style (arg1 arg2 arg3)
  (if (and epo-html-work (car epo-html-work))
      (concat "width:" (int-to-string (car epo-html-work))
	      ";height:" (int-to-string (car (cdr epo-html-work))))
    (read-string (concat arg3 " : "))))
(defvar epo-sdoc-inline-table
  '(("a" ("href" . epo-xml-file-name))
    ("cite" ("href" . epo-xml-file-name))
    ("figure" ("src" . epo-html-image-file-name)
     ("align" . epo-sdoc-align-alist)
     ("title" . epo-html-image-alt)
     ("style" . epo-sdoc-image-style))
    ("img" ("src" . epo-html-image-file-name)
     ("align" . epo-sdoc-align-alist)
     ("title" . epo-html-image-alt)
     ("style" . epo-sdoc-image-style))
    ("program" ("src" . epo-xml-file-name)
     ("title" . epo-xml-file-name)
     ("normalizer" . epo-sdoc-normalizer-alist)
     ("javasrcKeyword")
     ("javasrcCount")
     ("javasrcSyntaxHilight" . epo-xml-boolean))
    ("link" ;; <native format="html4"><link ... /></native>
     ("rel" . (("stylesheet")))
     ("type" . (("text/css")))
     ("href" . epo-html-file-name))
    ("journal" ("idref" . epo-xml-idref-alist)) )
  "Default inline type elements")

(defvar epo-sdoc-typeface-table
  '(("fyi" . 0) ("note" . 0)
    ("div" . 0) ("p" . 0) ("pre" . 0)
    ("blockquote" . 0) ("q" . 0) ("equation" . 0)
    ("span" . 0) ("em" . 0) ("strong" . 0)
    ("dfn" . 0) ("code" . 0) ("b" . 0) ("i" . 0) ("tt" . 0))
  "Default teypeface type elements")

(defvar epo-sdoc-iteration-alist
  '((?a (type . doc)
	(opener . ((pattern "<doc\\>" (!before . comment-start))))
	(closer . ((pattern "</doc>" (!before . comment-start))))
	(iterator . ((indent "<head>\n" indent "</head>" indent
			     "\n<body>" indent "\n" indent cursor
			     "\n</body>" indent))))
    (?b (type . head)
	(opener . ((pattern "<head\\>" (!before . comment-start))))
	(closer . ((pattern "</head>" (!before . comment-start))))
	(iterator . ((indent "<title>" cursor "</title>\n" indent
			     "<author></author>\n" indent
			     "<date></date>"))))
    (?c (type . body)
	(opener . ((pattern "<body\\>" (!before . comment-start))))
	(closer . ((pattern "</body>" (!before . comment-start))))
	(iterator . ((indent "<part title=\"" cursor
			     "\">\n\n</part>" indent))))
    (?d (type . part)
	(opener . ((pattern "<part\\>" (!before . comment-start))))
	(closer . ((pattern "</part>" (!before . comment-start))))
	(iterator . ((indent "<chapter title=\"" cursor
			     "\">\n\n</chapter>" indent))))
    (?e (type . chapter)
	(opener . ((pattern "<chapter\\>" (!before . comment-start))))
	(closer . ((pattern "</chapter>" (!before . comment-start))))
	(iterator . ((indent "<section title=\"" cursor
			     "\">\n\n</section>" indent))))
    (?f (type . section)
	(opener . ((pattern "<section\\>" (!before . comment-start))))
	(closer . ((pattern "</section>" (!before . comment-start))))
	(iterator . ((indent "<subsection title=\"" cursor
			     "\">\n\n</subsection>" indent))))
    (?g (type . subsection)
	(opener . ((pattern "<subsection\\>" (!before . comment-start))))
	(closer . ((pattern "</subsection>" (!before . comment-start))))
	(iterator . ((indent "<subsubsection title=\"" cursor
			     "\">\n\n</subsubsection>" indent))))
    (?h (type . subsubsection)
	(opener . ((pattern "<subsubsection\\>" (!before . comment-start))))
	(closer . ((pattern "</subsubsection>" (!before . comment-start))))
	(iterator . ((indent "<p>\n" indent cursor "\n</p>" indent))))
    (?i (type . unordered-list)
	(opener . ((pattern "<ol\\>" (!before . comment-start))))
	(closer . ((pattern "</ol>" (!before . comment-start))))
	(iterator . (indent "<li>" cursor "</li>")))
    (?j (type . unordered-list)
	(opener . ((pattern "<ul\\>" (!before . comment-start))))
	(closer . ((pattern "</ul>" (!before . comment-start))))
	(iterator . (indent "<li>" cursor "</li>")))
    (?k (type . definition-list)
	(opener . ((pattern "<dl\\>" (!before . comment-start))))
	(closer . ((pattern "</dl>" (!before . comment-start))))
	(iterator . ((indent "<dt>" cursor "</dt>\n" indent "<dd></dd>"))))
    (?l (type . table)
	(opener . ((pattern "<table\\>" (!before . comment-start))))
	(closer . ((pattern "</table>" (!before . comment-start))))
	(iterator . ((indent "<thead>\n" indent cursor "\n</thead>" indent
			     "\n<tbody>" indent "\n</tbody>" indent
			     "\n<tfoot>" indent "\n</tfoot>" indent))))
    (?m (type . tbody)
	(opener . ((pattern "<tbody\\>" (!before . comment-start))))
	(closer . ((pattern "</tbody>" (!before . comment-start))))
	(iterator . ((indent "<tr><td>" cursor "</td></tr>"))))
    (?n (type . thead)
	(opener . ((pattern "<thead\\>" (!before . comment-start))))
	(closer . ((pattern "</thead>" (!before . comment-start))))
	(iterator . ((indent "<tr><th>" cursor "</th></tr>"))))
    (?o (type . tfoot)
	(opener . ((pattern "<tfoot\\>" (!before . comment-start))))
	(closer . ((pattern "</tfoot>" (!before . comment-start))))
	(iterator . ((indent "<tr><th>" cursor "</th></tr>"))))
    (?p (type . colgroup)
	(opener . ((pattern "<colgroup\\>" (!before . comment-start))))
	(closer . ((pattern "</colgroup>" (!before . comment-start))))
	(iterator . ((indent "<col align=\"" cursor
			     "\"></col>" indent))))
    (?q (type . or)
	(opener . ((pattern "<or\\>" (!before . comment-start))))
	(closer . ((pattern "</or>" (!before . comment-start))))
	(iterator . ((indent "<native format=\"" cursor
			     "\"></native>" indent)))) ))
(defvar epo-sdoc-relation-alist epo-xml-relation-alist)

;;;
;; Variables for Process Handler(EPOP)
;;;
(defun epo-sdoc-posthook ()
  (if (fboundp 'epocclib-update-file)
      (epocclib-update-file) (message "sdoc posthook done.") ))
(defvar epo-sdoc-command-line "sdoc" "*SmartDoc command name")
(defun epo-sdoc-file-name ()
  (concat "-html4.css.url:"
	  (read-file-name "css: " "" "" nil "")))
(defun epo-sdoc-include ()
  (concat "-html4.css.include:"
	  (completing-read "include: " '(("embed") ("link")))))
(defvar epo-sdoc-process-alist
  '((?j (type . compile) (prompt . t)
	(command "sDoc" epo-sdoc-command-line basename)
	);(posthook 'epo-sdoc-posthook))
    (?p (type . compile) (prompt . t)
	(command "sDoc2" epo-sdoc-command-line
		 epo-sdoc-file-name epo-sdoc-include basename)
	);(posthook 'epo-sdoc-posthook))
    (?r (type . run) (prompt . t)
	(command "preview" epo-xml-open-remote
		 (basename "\\(.*\\)\\.sdoc$" "\\1.html")))
    (?a (type . version)
	(command "sDocVer" epo-sdoc-command-line "-version"))
    (?v (type . version)
	(command "version" "java" "-version")))
  "*dependent process alists")
(defvar epo-sdoc-tagjump-alist
      '(("sDoc" (type . inline)
	 (pattern . "^\\(\\S +\\.sdoc\\):\\(%l\\):")
	 (matchinfo 1 . 2))))

(provide 'epo-sdoc)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 