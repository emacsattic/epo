;;; -*- Emacs-Lisp -*-
;;; EPO HTML dependent settings
;;; (c)1999-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Tue May 14 00:57:48 2002 on balius

(condition-case nil (require 'yahtml) (error nil))
(require 'epo)
;;[Commentary]
;;	
;;	EPOのHTML依存設定
;;	

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-html-structure-alist
  '((?b (type . block)
	(structure "<" keyword argument ">\n" indent cursor "\n"
		   "</" keyword ">" indent)
	(table . epo-html-block-table)
	(argsep . sgml) (mustmatch . t))
    (?l (type . typeface)
	(structure "<" keyword argument ">\n" indent cursor "\n"
		   "</" keyword ">" indent)
	(table . epo-html-typeface-table)
	(argsep . sgml)	(mustmatch . t))
    (?s (type . inline)
	(structure "<" keyword argument ">" cursor
		   "</" keyword ">" indent)
	(table . epo-html-inline-table)
	(argsep . sgml)	(arg-reader . nil))
    (?n (type . ret)
	(structure "<br>" indent cursor)
	(mustmatch . t))))

(defvar epo-html-colors
  (cond
   ((and (boundp 'x-colors) (consp x-colors))
    (mapcar 'list x-colors))
   ((let (colors dir
	  (dirs (list data-directory "/usr/X11R6/lib/X11" "/usr/lib/X11"
		      "/usr/X11/lib/X11" "/usr/local/X11R6/lib/X11")))
      (if (catch 'found
	    (while dirs
	      (setq dir (car dirs))
	      (if (file-exists-p (expand-file-name "rgb.txt" dir))
		  (throw 'found t))
	      (setq dirs (cdr dirs))))
	  (save-excursion
	    (set-buffer (get-buffer-create " *rgb2txt*"))
	    (erase-buffer)
	    (insert-file-contents (expand-file-name "rgb.txt" dir))
	    (goto-char (point-min))
	    (message "Collecting color names...")
	    (while (re-search-forward "^[0-9]+" nil t)
	      (skip-chars-forward "^A-Za-z")
	      (if (looking-at "[A-Za-z]")
		  (setq colors (cons
				(list (buffer-substring
				       (point)
				       (progn (end-of-line)
					      (skip-chars-backward " \t")
					      (point))))
				colors))))
	    (message "Collecting color names...done")
	    (kill-buffer nil)
	    colors))))
   (t
    '(("black")("red")("green")("yellow")
      ("blue")("magenta")("cyan")("white"))))
  "Color names alist")

(defvar epo-html-codetype-aligns
  '(("text/css") ("text/html") ("text/plain") ("text/richtext")
    ("text/sgml") ("text/xml")
    ("application/octet-stream") ("application/postscript")
    ("application/x-java-jnlp-file") ("application/x-java-archive")
    ("application/pdf") ("application/java")
    ("image/jpeg") ("image/gif") ("image/tiff") ("image/png") ("image/bmp")
    ("audio/basic") ("video/mpeg") ("sound/wav") ("sound/midi") ))
(defvar epo-html-align-alist
  '(("top") ("middle") ("bottom") ("left") ("right") ("center")))
(defvar epo-html-caption-align-alist '(("top") ("bottom")))
(defvar epo-html-method-alist '(("POST") ("GET")))
(defvar epo-html-meta-types
  '(("keywords") ("author") ("copyright") ("date") ("generator")))
(defvar epo-html-ol-type-alist '(("1") ("a") ("A") ("i") ("I")))
(defvar epo-html-input-types
  '(("text") ("password") ("checkbox") ("radio") ("submit")
    ("reset") ("image") ("hidden") ("file")))
(defvar epo-html-link-types
  '(("alternate") ("stylesheet") ("start") ("next") ("prev")
    ("contents") ("index") ("glossary") ("chapter") ("section")
    ("subsection") ("appendix") ("help") ("bookmark")))
(defvar epo-html-charset
  '(("text/html; charset=us-ascii") ("text/html; charset=en-us")
    ("text/html; charset=ASCII") ("text/html; charset=EUCJIS")
    ("text/html; charset=Shift_JIS") ("text/html; charset=ISO2022JP")
    ("text/html; charset=JIS0201") ("text/html; charset=JIS0208")
    ("text/html; charset=JIS0212") ("text/html; charset=MS932")
    ("text/html; Windows-31J")
    ("text/html; charset=UTF-8") ("text/html; charset=UTF-16")
    ("text/html; charset=UnicodeLittle") ("text/html; charset=UTF-16BE")
    ("text/html; charset=UTF-16LE")))
;;
(defvar epo-html-work nil "work area")

;;; completing function.
(defun epo-html-complete-class (element n &optional attr)
  "Complete class name according to ELEMENT.
Second argument N (argument position) is not used in this function."
  (if (featurep 'yahtml)
      (let ((a (yahtml-css-get-element-completion-alist element)))
	(if (or (equal last-command-char ?\C-j) (null a))
	    ""
	  (completing-read "class: " a)))
    "")) ;; yahtml がない状態で聞いたら全部でてきちゃうからダメ
(defun epo-html-file-name (arg1 arg2 arg3)
  (read-file-name (concat arg3 " : ") "" "" nil ""))
(defun epo-html-set-meta-types (arg1 arg2 arg3)
  (setq epo-html-work
	(completing-read (concat arg3 " : ") epo-html-meta-types)))
(defun epo-html-set-http-equiv (arg1 arg2 arg3)
  (if (string= "" epo-html-work)
      (completing-read (concat arg3 " : ") '(("refresh") ("Content-Type")))
    ""))
(defun epo-html-set-content (arg1 arg2 arg3)
  (cond
   ((string-match "date" epo-html-work)
    (read-string (concat arg3 "/" epo-html-work " : ") (current-time-string)))
   ((string-match "author" epo-html-work)
    (read-string
     (concat arg3 "/" epo-html-work " : ")
     (if (and (user-full-name) (string< "" (user-full-name)))
	 (user-full-name)
       (user-login-name))))
   ((string-match "generator" epo-html-work)
    "epo / http://www.yatex.org/epo/")
   ((string-match "Content-Type" epo-html-work)
    (completing-read (concat arg3 " : ") epo-html-charset))
   ((string-match "refresh" epo-html-work) ";URL=")
   (t (read-string (concat arg3 " : ")))))
(defun epo-html-set-rev-link (arg1 arg2 arg3)
  (setq epo-html-work
	(completing-read (concat arg3 " : ") epo-html-link-types)))
(defun epo-html-set-rel-link (arg1 arg2 arg3)
  (if (string= "" epo-html-work)
      (setq epo-html-work
	    (completing-read (concat arg3 " : ") epo-html-link-types))
    ""))
(defun epo-html-set-css-style (arg1 arg2 arg3)
  (if (string= "stylesheet" epo-html-work)
      "text/css" ""))
(defun epo-html-image-file-name (arg1 arg2 arg3)
  (setq epo-html-work
	(read-file-name (concat arg3 " : ") "" "" nil "")))
(defun epo-html-image-alt (arg1 arg2 arg3)
  (let ((file epo-html-work))
    (if (and (featurep 'yahtml) (stringp epo-html-work)
	     (string< "" epo-html-work) (file-exists-p epo-html-work)
	     (setq epo-html-work (yahtml-get-image-info epo-html-work))
	     (car epo-html-work))
	(read-string
	 (concat arg3 " : ")
	 (YaTeX-replace-formats
	  yahtml:img-default-alt-format
	  (list (cons "x" (int-to-string (car epo-html-work)))
		(cons "y" (int-to-string (car (cdr epo-html-work))))
		(cons "s" (car (cdr (cdr epo-html-work))))
		(cons "f" (file-name-nondirectory file))
		(cons "c" (nth 4 epo-html-work)))))
      (setq epo-html-work nil)
      (read-string (concat arg3 " : ")))))
(defun epo-html-image-width (arg1 arg2 arg3)
  (if (and epo-html-work (car epo-html-work))
      (int-to-string (car epo-html-work))
    (read-string (concat arg3 " : "))))
(defun epo-html-image-height (arg1 arg2 arg3)
  (if (and epo-html-work (car epo-html-work))
      (int-to-string (car (cdr epo-html-work)))
    (read-string (concat arg3 " : "))))

(defvar epo-html-block-table
  '(("html" ("class" . epo-html-complete-class))
    ("head" ("class" . epo-html-complete-class))
    ("title" ("class" . epo-html-complete-class))
    ("body"
     ("bgcolor" . epo-html-colors)
     ("text" . epo-html-colors)
     ("link" . epo-html-colors)
     ("vlink" . epo-html-colors)
     ("class" . epo-html-complete-class))
    ("ul" ("class" . epo-html-complete-class))
    ("ol" ("start") ("type" . epo-html-ol-type-alist)
     ("class" . epo-html-complete-class))
    ("pre" ("class" . epo-html-complete-class))
    ("a" ("href" . epo-html-file-name)
     ("class" . epo-html-complete-class)) ;; name/id??
    ("form" ("method" . epo-html-method-alist)
     ("action")
     ("class" . epo-html-complete-class))
    ("select" ("name")
     ("class" . epo-html-complete-class))
    ("center" ("class" . epo-html-complete-class))
    ("textarea" ("name") ("cols") ("rows")
     ("class" . epo-html-complete-class))
    ("blockquote" ("class" . epo-html-complete-class))
    ("dl" ("class" . epo-html-complete-class))
    ("dt" ("class" . epo-html-complete-class))
    ("dh" ("class" . epo-html-complete-class))
    ("table" ("border") ("align" . epo-html-align-alist) ("width")
     ("class" . epo-html-complete-class))
    ("thead" ("class" . epo-html-complete-class))
    ("tbody" ("class" . epo-html-complete-class))
    ("tfoot" ("class" . epo-html-complete-class))
    ("caption" ("align" . epo-html-caption-align-alist)
     ("class" . epo-html-complete-class))
    ("tr" ("colspan") ("class" . epo-html-complete-class))
    ("th" ("rowpan") ("class" . epo-html-complete-class))
    ("td" ("rowpan") ("class" . epo-html-complete-class))
    ("address" ("class" . epo-html-complete-class)) 
    ("h1" ("class" . epo-html-complete-class))
    ("h2" ("class" . epo-html-complete-class))
    ("h3" ("class" . epo-html-complete-class))
    ("h4" ("class" . epo-html-complete-class))
    ("h5" ("class" . epo-html-complete-class))
    ("h6" ("class" . epo-html-complete-class))
    ("p" ("align" . epo-html-align-alist)
     ("class" . epo-html-complete-class))
    ("style" ("type" . (("text/css")))
     ("class" . epo-html-complete-class))
    ("div" ("align" . epo-html-align-alist)
     ("class" . epo-html-complete-class))
    ("object" ("codetype" . epo-html-codetype-aligns)
     ("classid") ("width") ("height")
     ("align" . epo-html-align-alist)))
  "Default block type elements")

(defvar epo-html-inline-table
  '(("img" ("src" . epo-html-image-file-name)
     ("align" . epo-html-align-alist)
     ("border")
     ("alt" . epo-html-image-alt)
     ("width" . epo-html-image-width)
     ("height" . epo-html-image-height))
    ("input" ("name") ("type" . epo-html-input-types)
     ("value") ("size") ("maxlength"))
    ("link" ("rev" . epo-html-set-rev-link)
     ("rel" . epo-html-set-rel-link)
     ("type" . epo-html-set-css-style)
     ("href" . epo-html-file-name))
    ("meta" ("name" . epo-html-set-meta-types)
     ("http-equiv" . epo-html-set-http-equiv)
     ("content" . epo-html-set-content))
    ("hr" . 0)
    ("br" . 0))
  "*Default inline elements")

(defvar epo-html-typeface-table
  '(("dfn"    ("class" . epo-html-complete-class))
    ("em"     ("class" . epo-html-complete-class))
    ("cite"   ("class" . epo-html-complete-class))
    ("pre"    ("class" . epo-html-complete-class))
    ("code"   ("class" . epo-html-complete-class))
    ("kbd"    ("class" . epo-html-complete-class))
    ("samp"   ("class" . epo-html-complete-class))
    ("strike" ("class" . epo-html-complete-class))
    ("s"      ("class" . epo-html-complete-class))
    ("strong" ("class" . epo-html-complete-class))
    ("var"    ("class" . epo-html-complete-class))
    ("b"      ("class" . epo-html-complete-class))
    ("i"      ("class" . epo-html-complete-class))
    ("tt"     ("class" . epo-html-complete-class))
    ("u"      ("class" . epo-html-complete-class))
    ("big"    ("class" . epo-html-complete-class))
    ("small"  ("class" . epo-html-complete-class))
    ("sup"    ("class" . epo-html-complete-class))
    ("sub"    ("class" . epo-html-complete-class))
    ("span"   ("class" . epo-html-complete-class))
    ("font" ("color" . epo-html-colors) ("size")))
  "*Default typeface elements")

(defvar epo-html-iteration-alist
  '((?o (type . unordered-list)
	(opener . ((pattern "<ol\\>" (!before . comment-start))))
	(closer . ((pattern "</ol>" (!before . comment-start))))
	(iterator . ("<li>")))
    (?u (type . unordered-list)
	(opener . ((pattern "<ul\\>" (!before . comment-start))))
	(closer . ((pattern "</ul>" (!before . comment-start))))
	(iterator . ("<li>")))
    (?d (type . definition-list)
	(opener . ((pattern "<dl\\>" (!before . comment-start))))
	(closer . ((pattern "</dl>" (!before . comment-start))))
	(iterator . ((indent "<dt>" cursor "\n" indent "<dd>"))))
    (?s (type . simple-table)
	(opener . ((pattern "<table\\>" (!before . comment-start))))
	(closer . ((pattern "</table>" (!before . comment-start))))
	(iterator . (("<tr>\n" indent "<td>" cursor 
		      "</td>\n" "</tr>" indent))))
    (?t (type . table)
	(opener . ((pattern "<table\\>" (!before . comment-start))))
	(closer . ((pattern "</table>" (!before . comment-start))))
	(iterator . (("<thead>" ("<tr>" ("<th>")))
		     ("<tbody>" ("<tr>" ("<td>" repeat) repeat)) )))
    (?r (type . table-row)
	(opener . ((pattern "<tr\\>" (!before . comment-start))))
	(closer . ((pattern "</tr>\\|</tbody>\\|<tr>\\|</table>"
		   (!before . comment-start))))
	(iterator . ((indent "<td>" cursor "</td>")))) ))

(defvar epo-html-relation-alist
  '((?h (type . external)
	(idpattern . "[^ \t\n\r]+")
	(relation
	 (href-ext
	  (pattern "\\<href=\\s *" (after . "\\(\"\\)?\\(http://.*\\)\\1")
		   (return))
	  (group . 2))))
    (?f (type . file)
	(idpattern . "[^ \t\n\r]+")
	(relation
	 (href-file
	  (pattern "\\<href=\\s *" (after . "\\(\"\\)?\\([^\"]*\\)\\1")
		   (return))
	  (group . 2))))))

;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-html-process-alist
  '((?j (type . compile) (command "lint" "jweblint" basename)
	(builtin . "#!"))
    (?r (type . run) (prompt . t)
	(command "preview" "netscape -remote" filename)))
  "*HTML dependent process alists")

(defvar epo-html-tagjump-alist
  '(("lint" (type . inline) (pattern . "^\\(\\S +\\)(\\(%l\\))")
     (matchinfo 1 2))))


(provide 'epo-html)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 

