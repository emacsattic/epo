;;; -*- Emacs-Lisp -*-
;;; EPO Xi dependent settings
;;; (c) 2001 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2001 Aug 11
;;; $Lastupdate: Sun Feb 02 03:51:43 2003 $ on inspire.

(require 'epo-ant)
(require 'epo-xhtml)
;;[Commentary]
;;	
;;	EPO -- Xi (Extend it! Xi for XML)
;;      http://www.baykit.org/xi/
;;

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-xi-structure-alist
  (append 
   '((?b (type . block)
	 (structure "<" keyword argument ">" indent cursor
		    "\n</" keyword ">" indent)
	 (table . epo-xi-block-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?l (type . typeface)
	 (structure indent "<" keyword argument ">" cursor "</" keyword ">")
	 (table . epo-xi-typeface-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?s (type . inline)
	 (structure indent "<" keyword argument " />" cursor)
	 (table . epo-xi-inline-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?n (type . ret)
	 (structure "<br />" indent cursor)
	 (mustmatch . t))
     (?! (type . inline)
	 (structure "<!-- " indent cursor " -->" indent)))
  (list (car epo-xml-basic-alist))))

(defvar epo-xi-version-aligns '(("1.1") ("1.0")))

(defvar epo-xi-xmlns-aligns
  '(("http://www.baykit.org/Xi/1.1")
    ("http://www.baykit.org/Xi/1.0") ))

(defvar epo-xi-xmlns-processor-aligns
  '(("http://www.baykit.org/Xi/processor") ))
(defvar epo-xi-xmlns-sql-aligns '(("http://www.baykit.org/Xi/sql") ))
(defvar epo-xi-xmlns-e-aligns '(("http://www.baykit.org/Xi/exception") ))
(defvar epo-xi-xmlns-mail-aligns '(("http://www.baykit.org/Xi/mail") ))
(defvar epo-xi-xmlns-db-aligns '(("http://www.baykit.org/Xi/db") ))
(defvar epo-xi-xmlns-format-aligns '(("http://www.baykit.org/Xi/format") ))
(defvar epo-xi-xmlns-sp-aligns '(("http://www.baykit.org/Xi/spawn") ))
(defvar epo-xi-xmlns-tr-aligns '(("http://www.baykit.org/Xi/transform") ))

(defvar epo-xi-data-type '(("text") ("number")))
(defvar epo-xi-order '(("acsending") ("descending")))
(defvar epo-xi-method '(("html" "xml")))
(defvar epo-xi-include-base '(("document" "runtime" "system")))
(defvar epo-xi-case-method '(("lower" "upper" "preserve")))
(defvar epo-xi-rowset '(("rowset" "sql:none")))
(defvar epo-xi-row '(("row" "sql:none")))
(defvar epo-xi-error-type '(("element" "exception" "ignore")))
(defvar epo-xi-data-source '(("bxs")))

(defvar epo-xi-ninja
  '(("$Runtime") ("$Java.") ("$Math.") ("$Util.") ("$Text.")
    ("$Node") ("$Calendar.") ("$Web.") ("$FS.") ("$IO.")
     ("$COM.") ("$Wsdl.") ("$Format.")))
(defvar epo-xi-ninja-dot
  '(("$Runtime." .
     (("environments") ("parameters") ("arguments")
      ("documentBase") ("currentDirectory") ("documentName")))
    ("$Java." .
     (("string.length()") ("string.charAt()") ("string.getChars()")
      ("string.getBytes()") ("string.equals()") ("string.contentEquals()")
      ("string.equalsIgnoreCase()") ("string.compareTo()")
      ("string.compare()") ("string.compareToIgnoreCase()")
      ("string.regionMatches()") ("string.startsWith()")
      ("string.endsWith()") ("string.hashCode()") ("string.indexOf()")
      ("string.lastIndexOf()") ("string.substring()")
      ("string.subSequence()") ("string.concat()") ("string.replace()")
      ("string.matches()") ("string.replaceFirst()") ("string.replaceAll()")
      ("string.split()") ("string.toLowerCase()") ("string.toUpperCase()")
      ("string.trim()") ("string.toString()") ("string.toCharArray()")
      ("string.valueOf()") ("string.copyValueOf()") ("string.intern()")
      ("null") ("new()") ("method()") ("class()")))
    ("$Math." .
     (("abs()") ("max()") ("min()") ("round()") ("random()") ("pow()")))
    ("$Util." . (("collection()") ("counter()")))
    ("$Text." .
     (("length()") ("char()") ("substring()")
      ("left()") ("right()") ("concat()") ("search()")))
    ("$Node." .
     (("extract()") ("append-child()") ("insert-before()")
      ("replace-child()") ("remove-child()")))
    ("$Math." . (("abs()") ("max()") ("min()") ("round()")
		 ("random()") ("pow()")))
    ("$Calendar" . (("new()")))
    ("$Web." .
     (("redirect()") ("parameters.")
      ("parameters.get()") ("parameters.id") ("parameters.id.count")
      ("headers.") ("headers.get()")
      ("cgi.get()")
      ("cookies.") ("cookies.get()") ("cookies.put()") ("cookies.out()")
      ("sessionController.")
      ("session.get()") ("session.put()")
      ("request.") ("servlet.") ("servletRequest.")
      ("servletResponse.") ("servletContext")))
    ("$FS." .
     (("currentDirectory") ("exists()") ("isDirectory()")
      ("createDirectory()") ("move()") ("copy()") ("remove()")
      ("createFile()") ("createTempFile()")
      ("get.NativePath()") ("getDirectory()") ("getFileName()")
      ("getFileList()") ("getFileLength()") ("getPathName()")))
    ("$IO." .
     (("loadXml()") ("loadXmlFromStream()") ("loadXmlFromString()")
      ("loadHtml()") ("loadHtmlFromStream()") ("loadHtmlFromString()")
      ("loadProperties()") ("loadString()") ("loadBytes()")
      ("saveXml()") ("saveXmlToString()") ("saveHtml()")
      ("saveHtmlToString()") ("saveBytes()") ("saveString()")))
    ("$COM." . (("new()")))
    ("$Wsdl." . (("describe()") ("new()") ("holder()")))
    ("$Format." . (("number()") ("date()")))))

(defun epo-xi-minibuffer-begin ()
 (if (fboundp 'field-beginning) (field-beginning (point-max)) (point-min)))
(defun epo-xi-minibuffer-end ()
 (if (fboundp 'field-end) (field-end (point-max)) (point-max)))
(defun epo-xi-minibuffer-string ()
  (buffer-substring (epo-xi-minibuffer-begin) (epo-xi-minibuffer-end)))
(defun epo-xi-complete-xpath ()
  "Complete external XPath from history or local file name."
  (interactive)
  (let ((initial (epo-xi-minibuffer-string))
	(cmpl) (st) (ed) (ans) (mid) (key)
	(beg (if (fboundp 'field-beginning)
		 (field-beginning) (point-min))))
    (cond
     ((string-match "\\(.*[/@]\\)\\([^/$@!=<>^+-*]*\\)$" initial)
      (setq st (substring initial (match-beginning 1) (match-end 1)))
      (setq ed (substring initial (match-beginning 2) (match-end 2)))
      (setq ans (try-completion ed epo-xi-xpath-alist))
      (cond
       ((eq ans t) nil)
       ((eq ans nil) nil)
       (t (delete-region (point) beg)
	   (insert (concat st ans))
	   (with-output-to-temp-buffer "*Completions*"
	     (display-completion-list
	      (all-completions ans epo-xi-xpath-alist))))))
     ((string-match "\\(.*\\)\\(\\$[^$.]+\\.\\)\\([^$!=<>^+-*]*\\)$" initial)
      (setq st  (substring initial (match-beginning 1) (match-end 1)))
      (setq mid (substring initial (match-beginning 2) (match-end 2)))
      (setq ed  (substring initial (match-beginning 3) (match-end 3)))
      (setq key (assoc mid epo-xi-ninja-dot))
      (if (not key) nil
	(setq ans (try-completion ed (cdr key)))
	(cond
	 ((eq ans t) nil)
	 ((eq ans nil) nil)
	 (t (delete-region (point) beg)
	    (insert (concat st mid ans))
	    (with-output-to-temp-buffer "*Completions*"
	      (display-completion-list
	       (all-completions ans (cdr key))))))))
     (t
      (if (string-match "\\(.*\\)\\(\\$[^$!=<>^+-*]*\\)$" initial)
	  (progn
	    (setq st (substring initial (match-beginning 1) (match-end 1)))
	    (setq ed (substring initial (match-beginning 2) (match-end 2)))
	    (setq ans (try-completion ed epo-xi-select-alist)))
	(if (string-match "\\(.*[!=<>^+-*]\\)\\([!=<>^+-*]*\\)$" initial)
	    (progn
	      (setq st (substring initial (match-beginning 1) (match-end 1)))
	      (setq ed (substring initial (match-beginning 2) (match-end 2)))
	      (setq ans (try-completion ed epo-xi-select-alist)))
	  (setq ans (try-completion initial epo-xi-select-alist))))
      (cond
       ((eq ans t) nil)
       ((eq ans nil) nil)
       (t (delete-region (point) beg)
	  (insert (concat st ans))
	  (with-output-to-temp-buffer "*Completions*"
	    (display-completion-list
	     (all-completions ans epo-xi-select-alist)))))))))
(defun epo-xi-select-defun ()
  (let ((word) (word-alist epo-xi-ninja))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      "\\(name\\|item\\|select\\)\\s *=\\s *\"\\$?\\([A-Za-z0-9_]+\\)\"" nil t)
	(setq word (buffer-substring (match-beginning 2) (match-end 2)))
	(setq word (concat "$" word))
	(if (not (assoc word word-alist))
	    (setq word-alist (cons (list word) word-alist)))))
    word-alist))
(defun epo-xi-xpath-defun ()
  (let ((word) (word-alist nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<\\([A-Za-z0-9_]+\\)[\\s >/]" nil t)
	(setq word (buffer-substring (match-beginning 1) (match-end 1)))
	(if (not (assoc word word-alist))
	    (setq word-alist (cons (list word) word-alist)))))
    word-alist))
(defvar epo-xi-select-alist nil)
(defvar epo-xi-xpath-alist nil)
(defun epo-xi-xpath (arg1 arg2 arg3)
  (setq epo-xi-select-alist (epo-xi-select-defun))
  (setq epo-xi-xpath-alist (epo-xi-xpath-defun))
  (read-from-minibuffer (concat arg3 ": ") "" epo-xi-xpath-completion-map))

(defvar epo-xi-xpath-completion-map nil
  "Key map used in XPath completion buffer")
(if epo-xi-xpath-completion-map nil
  (setq epo-xi-xpath-completion-map
	(copy-keymap minibuffer-local-completion-map))
  (define-key epo-xi-xpath-completion-map "\t" 'epo-xi-complete-xpath)
  (define-key epo-xi-xpath-completion-map " "  'epo-xi-complete-xpath) )

(defvar epo-xi-basic-table
  '( ("xi:program"
      ("version" . epo-xi-version-aligns)
      ("xmlns:xi" . epo-xi-xmlns-aligns)
      ("xmlns:pr" . epo-xi-xmlns-processor-aligns)
      ("xmlns:e" . epo-xi-xmlns-e-aligns)
      ("xmlns:sql" . epo-xi-xmlns-sql-aligns)
      ("xmlns:mail" . epo-xi-xmlns-mail-aligns)
      ("xmlns:db" . epo-xi-xmlns-db-aligns)
      ("xmlns:format" . epo-xi-xmlns-format-aligns)
      ("xmlns:sp" . epo-xi-xmlns-sp-aligns)
      ("xmlns:tr" . epo-xi-xmlns-tr-aligns))
     ("xi:var" ("name" . epo-xml-same-alist)
      ("select" . epo-xi-xpath))
     ;;;("xi:variable"
     ;;; ("name" . epo-xml-same-alist)
     ;;; ("select" . epo-xi-xpath))
     ("xi:value-of"
      ("select" . epo-xi-xpath)
      ("disable-output-escaping" . epo-xml-yes))
     ("xi:eval" ("select" . epo-xi-xpath))
     ("xi:exec" ("select" . epo-xi-xpath))
     ("xi:set" ("out" . epo-xi-xpath) ("select" . epo-xi-xpath))
     ("xi:copy-of" ("select" . epo-xi-xpath))
     ("xi:text"
      ("disable-output-escaping" . epo-xml-yes)
      ("cdata-section" . epo-xml-yes))
     ("xi:element" ("name" . epo-xml-same-alist))
     ("xi:attribute" ("name" . epo-xml-same-alist))
     ("xi:comment" . 0)
     ("xi:processing-instruction" ("name" . epo-xml-same-alist))
     ("xi:block" . 0)
     ("xi:if" ("test" . epo-xi-xpath))
     ("xi:choose" . 0)
     ("xi:when" ("test" . epo-xi-xpath))
     ("xi:otherwise" . 0)
     ("xi:for-each" ("item" . epo-xml-same-alist)
      ("select" . epo-xi-xpath))
     ("xi:sort"
      ("select" . epo-xi-xpath)
      ("data-type" . epo-xi-data-type)
      ("order" . epo-xi-order))
     ("xi:while" ("test" . epo-xi-xpath))
     ("pr:output" ("method" . epo-xi-method)
      ("encoding" . epo-xml-encoding))
     ("pr:message" ("select" . epo-xi-xpath)
      ("terminate" . epo-xml-yes))
     ("pr:include" ("href" . epo-xml-file-name)
      ("base" . epo-xi-include-base))
     ("pr:resource" ("name" . epo-xml-same-alist))
     ("resource-name" . 0)
     ;;;("pr:template" ("name" . epo-xml-same-alist) ("return"))
     ;;;("pr:param" ("name" . epo-xml-same-alist)
     ;;; ("select" . epo-xi-xpath))
     ;;;("pr:call-param" ("name" . epo-xml-same-alist))
     ;;;("pr:with-param" ("name" . epo-xml-same-alist)
     ;;; ("select" . epo-xi-xpath))
     ("e:raise" ("select" . epo-xi-xpath))
     ("e:try" . 0)
     ("e:catch" ("name" . epo-xml-same-alist))
     ("e:finally" . 0)
     ("sql:connect" ("data-source" . epo-xi-data-source)
      ("name" . epo-xml-same-alist)
      ("autocommit" . epo-xml-yes))
     ("sql:execute"
      ("rowset-element" . epo-xi-rowset) ("row-element" . epo-xi-row)
      ("error-type" . epo-xi-error-type)
      ("col-case" . epo-xi-case-method)
      ("range") ("connection" . epo-xi-xpath))
     ("sql:insert"
      ("rowset-element" . epo-xi-rowset) ("row-element" . epo-xi-row)
      ("error-type" . epo-xi-error-type) ("table" . epo-xml-same-alist)
      ("ignore-case" . epo-xml-yes) ("connection" . epo-xi-xpath))
     ("sql:update" ("key-list" . epo-xml-same-alist)
      ("rowset-element" . epo-xi-rowset) ("row-element" . epo-xi-row)
      ("error-type" . epo-xi-error-type) ("table" . epo-xml-same-alist)
      ("ignore-case" . epo-xml-yes) ("connection" . epo-xi-xpath))
     ("sql:delete" ("key-list" . epo-xml-same-alist)
      ("rowset-element" . epo-xi-rowset) ("row-element" . epo-xi-row)
      ("error-type" . epo-xi-error-type) ("table" . epo-xml-same-alist)
      ("ignore-case" . epo-xml-yes) ("connection" . epo-xi-xpath))
     ("sql:bind" ("select" . epo-xi-xpath))
     ("sql:commit" ("connection" . epo-xi-xpath))
     ("sql:rollback" ("connection" . epo-xi-xpath))
     ("sql:call" ("connection" . epo-xi-xpath))
     ("mail:resource" ("name" . epo-xml-same-alist))
     ("type" . 0) ("smtp-host" . 0) ("lookup" . 0)
     ("mail:send" ("name" . epo-xml-same-alist))
     ("db:collection" ("resource" . epo-xml-same-alist)
      ("name" . epo-xml-same-alist))
     ("db:save" ("name" . epo-xml-file-name)
      ("collection " . epo-xml-same-alist)
      ("select" . epo-xi-xpath))
     ("db:load" ("name" . epo-xml-file-name)
      ("collection " . epo-xml-same-alist))
     ("db:update" ("name" . epo-xml-file-name)
      ("collection " . epo-xml-same-alist)
      ("select" . epo-xi-xpath))
     ("db:query" ("name" . epo-xml-file-name)
      ("collection " . epo-xml-same-alist)
      ("xpath" . epo-xi-xpath))
     ("db:remove" ("name" . epo-xml-file-name)
      ("collection " . epo-xml-same-alist))
     ("db:list" ("collection " . epo-xml-same-alist))
     ("format:number" ("select" . epo-xi-xpath) ("pattern"))
     ("format:date" ("select " . epo-xi-xpath) ("pattern"))
     ("sp:shell" . 0)
     ("sp:set-env" ("name" . epo-xml-same-alist))
     ("sp:get-env" ("name" . epo-xml-same-alist))
     ("sp:spawn" ("out-element" . epo-xml-same-alist)
      ("err-element" . epo-xml-same-alist)
      ("exit-element" . epo-xml-same-alist)
      ("encoding" . epo-xml-encoding))
     ("tr:transform" ("stylesheet" . epo-xi-xpath)
      ("stylesheetHref" . epo-xml-file-name)))
  "Default block type elements")

(defvar epo-xi-block-table (append epo-xi-basic-table epo-xhtml-block-table))
(defvar epo-xi-inline-table (append epo-xi-basic-table epo-html-inline-table))

(defvar epo-xi-typeface-table
  (append
   '(("type" . 0) ("driver" . 0) ("url" . 0) ("user" . 0) ("password" . 0))
   epo-html-typeface-table))

(defvar epo-xi-iteration-alist
  (append
   '((?k (type . ordered-list)
	 (opener . ((pattern "<xi:program\\>" (!before . comment-start))))
	 (closer . ((pattern "</xi:program>" (!before . comment-start))))
	 (iterator . (("<xi:variable name=\"\">" indent cursor
		       "\n</xi:variable>" indent))))
     (?l (type . ordered-list)
	 (opener . ((pattern "<xi:try\\>" (!before . comment-start))))
	 (closer . ((pattern "</xi:try>" (!before . comment-start))))
	 (iterator . (("<xi:exception name=\"\">" indent cursor
		       "\n</xi:exception>" indent))))
     (?m (type . ordered-list)
	 (opener . ((pattern "<xi:choose\\>" (!before . comment-start))))
	 (closer . ((pattern "</xi:choose>" (!before . comment-start))))
	 (iterator . (("<xi:when test=\"\">" indent cursor
		       "\n</xi:when>" indent))))
     (?n (type . ordered-list)
	 (opener . ((pattern "<xi:element\\>" (!before . comment-start))))
	 (closer . ((pattern "</xi:element>" (!before . comment-start))))
	 (iterator . (("<xi:attribute test=\"\">" indent cursor
		       "\n</xi:attribute>" indent))))
     (?o (type . ordered-list)
	 (opener . ((pattern "<xi:when\\>" (!before . comment-start))))
	 (closer . ((pattern "</xi:when>" (!before . comment-start))))
	 (iterator . (("<xi:variable name=\"\">" indent cursor
		       "\n</xi:variable>" indent))))
     (?p (type . ordered-list)
	 (opener . ((pattern "<sql:data-source\\>"
			     (!before . comment-start))))
	 (closer . ((pattern "</sql:data-source>"
			     (!before . comment-start))))
	 (iterator . (("<type>" indent cursor "</type>"
		       "\n<driver></driver>" indent
		       "\n<url></url>" indent "\n<user></user>" indent
		       "\n<password></password>" indent))))
     (?q (type . ordered-list)
	 (opener . ((pattern "<mail:data-source\\>"
			     (!before . comment-start))))
	 (closer . ((pattern "</mail:data-source>"
			     (!before . comment-start))))
	 (iterator . (("<type>" indent cursor "</type>"
		       "\n<smtp-host></smtp-host>" indent
		       "\n<lookup></lookup>" indent)))) )
   epo-xhtml-iteration-alist))

(defvar epo-xi-relation-alist epo-xml-relation-alist)

;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-xi-command-line "xi" "*xi command name")
(defvar epo-xi-process-alist
  '((?r (type . compile)
	 (command "ant" epo-ant-process "-emacs" "-f"
		  epo-ant-other-makefile epo-ant-other-target))
    (?j (type . compile) (prompt . t)
	(command "xi" epo-xi-command-line basename)))
  "*dependent process alists")

(provide 'epo-xi)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 