;;; -*- Emacs-Lisp -*-
;;; EPO XML dependent settings
;;; (c) 2001-2002 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2001 Aug 11
;;; $Lastupdate: Tue May 14 00:22:53 2002 $ on inspire.

;;[Commentary]
;;	
;;	EPO -- XML dependent settings
;;

;; vs. (add-hook 'html-mode-hook 'epo-xml-enable)
(require 'epo)

;; if you'd used epo-xml for yahtm-mode,sgml-mode or psgml-mode.
;; then your ~/.emacs there could be:
;;
;; (autoload 'epo-xml-enable "epo-xml" "Editing Process Organizer for XML" t)
;; (add-hook 'yahtml-mode-hook 'epo-xml-enable) ;;; YaHTML example
;; (add-hook 'sgml-mode-hook   'epo-xml-enable) ;;; SGML/PSGML example

;;;
;; Select for epo languge.
;;;
(defvar epo-xml-select-file-regexp
  '(("build\\.xml$" ant)
    ("web\\.xml$" wapp)
    ("server\\.xml$" sev)
    ("struts-config\\.xml" scof)
    ("\\.xi$" xi)
    ("\\.xsl$" xsl)
    ("\\.tld$" tld)
    ("\\.jsp$" jsp)
    ("\\.[x]?html$" xhtml)
    ("\\.r\\(lx\\|xm\\|xg\\)$" rlx)
    ("\\.sdoc$" sdoc)
    ("\\.g[se]ml$" gsml)
    ("\\.xml$" nil) )
  "*selet epo-mode file regrep and major-mode name")
(defvar epo-xml-select-regexp
  '(("http://www\\.w3\\.org/1999/XSL/Transform" xsl)
    ("http://www\\.xml\\.gr\\.jp/xmlns/relaxCore" rlx)
    ("http://www\\.baykit\\.org/Xi/" xi)
    ("^<project" ant)
    ("^<html" xhtml)
    ("^<web-app" wapp)
    ("^<struts-config" scof)
    ("^<jnlp" jnlp)
    ("<!DOCTYPE\\s +scenario\\s +SYSTEM\\s +\"geml.dtd\"\\s *>" gsml)
    (nil xml))
  "*select epo-mode regexp and major-mode name")
(defun epo-xml-select-file ()
  "seach and select epo-mode file"
  (let ((ans (epo-xml-select-file-second)))
    (if (and (boundp 'epo-Z-default-host)
	     buffer-file-name
	     (string-match (concat "@" epo-Z-default-host ":/")
			   (buffer-file-name))
	     (or (string= "xhtml" (symbol-name ans))
		 (string= "xml" (symbol-name ans))))
	'Z
      ans )))
(defun epo-xml-select-file-second ()
  "seach and select epo-mode file"
  (let ((data epo-xml-select-file-regexp) (element))
    (if (not (buffer-file-name))
	(epo-xml-select)
      (or
       (catch 'epo-xml-select-file-tag
	   (while (setq element (car data))
	     (setq data (cdr data))
	     (if (string-match (car element) (buffer-file-name))
		 (throw 'epo-xml-select-file-tag (car (cdr element))))))
       (epo-xml-select) ))))
(defun epo-xml-select ()
  "seach and select epo-mode"
  (let ((data epo-xml-select-regexp) (element))
    (save-excursion
      (catch 'epo-xml-select-tag
	(while (setq element (car data))
	  (setq data (cdr data))
	  (if (not (car element))
	      (throw 'epo-xml-select-tag (car (cdr element))))
	  (goto-char (point-min))
	  (if (re-search-forward (car element) nil t)
	      (throw 'epo-xml-select-tag (car (cdr element)))))))))
(defun epo-xml-enable ()
  "epo-enable for XML language"
  (interactive)
  (setq epo-language-alist
	(delete (assoc (symbol-name major-mode)
		       epo-language-alist) epo-language-alist))
  (setq epo-language-alist
	(cons (cons (symbol-name major-mode)
		    (symbol-name (epo-xml-select-file)))
	      epo-language-alist))
  (epo-enable))

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-xml-basic-alist
  '((?f (type . xml)
	(structure indent "<?" keyword argument "?>\n" cursor)
	(table . epo-xml-table)
	(argsep " " "\"" "=" " " "\"" "")
	(mustmatch . nil) (arg-reader . nil))
    (?l (type . dtd)
	(structure indent "<!" keyword " " cursor ">\n")
	(table . epo-xml-block-table)
	(argsep " " "\"" "=" " " "\"" "")
	(mustmatch . nil) (arg-reader . nil))
    (?! (type . inline) (structure "<!-- " indent cursor " -->" indent)))
  "*XML dependent structure alists")

(defvar epo-xml-structure-alist
  (append 
   '((?b (type . block)
	 (structure indent "<" keyword ">\n" indent cursor "\n"
		    "</" keyword ">" indent)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?s (type . inline)
	 (structure indent "<" keyword " />" cursor)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil)))
   epo-xml-basic-alist)
  "*XML dependent structure alists")

(defvar epo-xml-relation-alist 
  '((?x (type . file)
	(idpattern . "[a-zA-Z0-9./_\-]+")
	(relation
	 (doctype-file
	  (pattern "<!DOCTYPE\\s +\\S +\\s +\\S +\\s +\"\\(%i\\)\"\\s *>")
	  (group . 1)))) ; <!DOCTYPE module SYSTEM "aaa.dtd">
    (?h (type . external)
	(idpattern . "[^ \t\n\r]+")
	(relation
	 (href-ext
	  (pattern "\\(xmlns\\|href\\|file\\|src\\)=\\s *"
		   (after . "\\(\"\\)?\\(http://[^\"]*\\|\\(file://\\)?\\([^\"]*\\)\\.\\(gif\\|jpeg\\|jpg\\|bmp\\|png\\|wav\\|psd\\|obj\\|e?ps\\)\\)\\1") (return))
	  (group . 2))))
    (?f (type . file)
	(idpattern . "[^ \t\n\r]+")
	(relation
	 (href-file
	  (pattern "\\(xmlns\\|href\\|file\\|src\\|uri\\)=\\s *"
		   (after . "\\(\"\\)?\\(file://\\)?\\([^\"]*\\)\\1") (return))
	  (group . 3)))))
  "*XML dependent relation alists")

;;; completing function.
(defun epo-xml-file-name (arg1 arg2 arg3)
  (read-file-name (concat arg3 " : ") "" "" nil ""))

(defun epo-xml-same-alist (arg1 arg2 arg3)
  (let ((word) (word-alist nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      (concat arg3 "\\s *=\\s *\"\\(\\w+\\)\"") nil t)
	(setq word (buffer-substring (match-beginning 1) (match-end 1)))
	(if (not (assoc word word-alist))
	    (setq word-alist (cons (list word) word-alist)))))
    (completing-read (concat arg3 " : ") word-alist)))

;;; completing variable.
(defvar epo-xml-dos (memq system-type '(ms-dos windows-nt OS/2)))
(defvar epo-xml-encoding
  '(("us-ascii") ("en-us") ("ASCII")
    ("EUCJIS") ("SJIS") ("Shift_JIS") ("ISO-8859-1")
    ("ISO2022JP") ("JIS0201") ("JIS0208") ("JIS0212") ("MS932")
    ("Windows-31J") ("UTF-8") ("UTF-16")
    ("UnicodeLittle") ("UTF-16BE") ("UTF-16LE") ("Big5") ("GB2312")))
(defvar epo-xml-lang '(("en") ("ja") ("fr") ("de") ("zh_TW") ("zh_CN")))
(defvar epo-xml-yes '(("yes") ("no")))
(defvar epo-xml-boolean '(("true") ("false")))

;;; completiong table.
(defvar epo-xml-block-table 
  '(("DOCTYPE") ("ATTLIST") ("ELEMENT") ("ENTITY") ("[]")))
(defvar epo-xml-table
  '(("xml" ("version" . (("1.0"))) ("encoding" . epo-xml-encoding))
    ("xml-stylesheet" ("href" . epo-xml-file-name) ("type") ("media"))
    ("cocoon-process" ("type"))))

;;;
;; Variables for Process Handler(EPOP)
;;;
(defun epo-xml-in-target ()  (read-file-name "in file: "))
(defun epo-xml-xsl-target ()  (read-file-name "xsl file: "))
(defun epo-xml-out-target () (read-file-name "out file: "))
(defun epo-xml-open-remote () (if epo-xml-dos "start" "netscape -remote"))
(defvar epo-xml-process-alist
  (if (getenv "JAVA_HOME")
      '(;;(?r (type . run) (prompt . t)
	;;(command "preview" epo-xml-open-remote filename))
	(?x (type . compile)
	    (command "xalan" "java" "org.apatch.xalan.xslt.Process"
		     "-IN" basename
		     "-XSL" epo-xml-xsl-target
		     "-OUT" epo-xml-out-target))
	(?j (type . compile)
	    (command "xt" "java" "com.jclark.xsl.Driver"
		     basename epo-xml-xsl-target epo-xml-out-target))
	(?s (type . compile)
	    (command "saxon" "java" "com.icl.saxon.StyleSheet" "-o"
		     basename epo-xml-xsl-target epo-xml-out-target))
	(?p (type . compile)
	    (command "saxon use PI" "java" "com.icl.saxon.StyleSheet" "-o"
		     basename epo-xml-out-target)))
    '(;;(?r (type . run) (prompt . t)
	;;(command "preview" epo-xml-open-remote filename))
      (?j (type . compile)
	  (command "xt" "xt" basename epo-xml-xsl-target epo-xml-out-target))
      (?s (type . compile)
	  (command "saxon" "saxon" "-o"
		   basename epo-xml-out-target epo-xml-xsl-target))
      (?p (type . compile)
	  (command "saxon use PI" "saxon" "-o"
		   basename epo-xml-out-target)))))

(defvar epo-xml-tagjump-alist nil "*XML dependent process tagjump")

(provide 'epo-xml)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End:
