;;; -*- Emacs-Lisp -*-
;;; EPO Ant dependent settings
;;; (c) 2001-2002 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2001 Aug 11
;;; $Lastupdate: Sat Dec 21 23:30:48 2002 $ on inspire.

(require 'epo-xml)
;;[Commentary]
;;	
;;	EPO -- Ant
;;      http://jakarta.apache.org/ant/index.html
;;

;;;
;; Variables for Input Aider(EPOI)
;;;

;;;
;;;
;;;
(defvar epo-ant-other-makefile-value
  (if (getenv "ANT_HOME") "build.xml" "Makefile")
  "*jakarta-ant default xml filename")

(defun epo-ant-other-target ()
  "ant target project complet"
  (save-excursion
    (set-buffer (find-file-noselect epo-ant-other-makefile-value))
    (epo-ant-target)))

(defun epo-ant-other-makefile ()
  "find file for make file"
  (let ((pos 0) (file (file-name-nondirectory epo-ant-other-makefile-value))
	(dir (file-name-directory epo-ant-other-makefile-value)) (name))
    (catch 'epo-ant-other-makefile-tag
      (while (< pos 20) ;; 20 is ok??
	(setq name (expand-file-name (concat dir file)))
	(if (file-exists-p name)
	    (progn
	      (setq epo-ant-other-makefile-value name)
	      (throw 'epo-ant-other-makefile-tag t)))
	(setq dir (concat dir "../"))
	(setq pos (+ 1 pos)))))
  (setq
   epo-ant-other-makefile-value
   (expand-file-name
    (read-file-name
     (concat "file("
	     (file-name-nondirectory epo-ant-other-makefile-value) "): ")
     (file-name-directory epo-ant-other-makefile-value)
     epo-ant-other-makefile-value t
     (file-name-nondirectory epo-ant-other-makefile-value)))))

;;;
;;;
;;;
(defvar epo-ant-process
  (concat (getenv "ANT_HOME") (if epo-xml-dos "\\bin\\ant.bat" "/bin/ant"))
  "*jakarta-ant execute file")

(defun epo-ant-target (&optional arg1 arg2 dummy)
  (let ((word) (word-alist)
	(project (if (equal arg2 2) "depends" "project")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      "<target\\s +name\\s *=\\s *\"\\([-_.A-Za-z0-9]+\\)\"" nil t)
	(setq word (buffer-substring (match-beginning 1) (match-end 1)))
	(if (not (assoc word word-alist))
	    (setq word-alist (cons (list word) word-alist)))))
    (completing-read (concat project " : ") word-alist)))

(defvar epo-ant-structure-alist
  (append
   '((?b (type . block)
	 (structure indent "<" keyword argument ">\n" indent cursor "\n"
		    "</" keyword ">" indent)
	 (table . epo-ant-block-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?s (type . inline)
	 (structure indent "<" keyword argument " />" cursor)
	 (table . epo-ant-inline-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil)))
   epo-xml-basic-alist))

(defvar epo-ant-on '(("on") ("off")))
(defvar epo-ant-formatter '(("xml") ("plain") ("brief")))
(defvar epo-ant-format '(("frames") ("noframes")))

(defvar epo-ant-common-table
  '(("java"
     ("classname")
     ("fork" . epo-xml-yes)
     ("classpath")
     ("classpathref")
     ("dir" . epo-xml-file-name))
    ("javadoc"
     ("package")
     ("sourcepath" . epo-xml-file-name)
     ("destdir" . epo-xml-file-name)
     ("locale" . epo-xml-lang)
     ("encoding" . epo-xml-encoding))
    ("javac"
     ("srcdir" . epo-xml-file-name)
     ("destdir" . epo-xml-file-name)
     ("debug" . epo-ant-on)
     ("optimize" . epo-ant-on)
     ("deprecation" . epo-ant-on))
    ("jar"  ("jarfile" . epo-xml-file-name)
     ("manifest" . epo-xml-file-name)
     ("basedir" . epo-xml-file-name)
     ("includes") ("includesfile" . epo-xml-file-name)
     ("excludes") ("excludesfile" . epo-xml-file-name)
     ("defaultexcludes" . epo-xml-yes))
    ("ear" ("earfile" . epo-xml-file-name)
     ("appxml" . epo-xml-file-name)
     ("manifest" . epo-xml-file-name)
     ("basedir" . epo-xml-file-name)
     ("includes") ("includesfile" . epo-xml-file-name)
     ("excludes") ("excludesfile" . epo-xml-file-name)
     ("defaultexcludes" . epo-xml-yes))
    ("war" ("warfile" . epo-xml-file-name)
     ("webxml" . epo-xml-file-name)
     ("manifest" . epo-xml-file-name)
     ("basedir" . epo-xml-file-name)
     ("includes") ("includesfile" . epo-xml-file-name)
     ("excludes") ("excludesfile" . epo-xml-file-name)
     ("defaultexcludes" . epo-xml-yes))
    ("zip"  ("zipfile" . epo-xml-file-name)
     ("basedir" . epo-xml-file-name)
     ("includes") ("includesfile" . epo-xml-file-name)
     ("excludes") ("excludesfile" . epo-xml-file-name)
     ("defaultexcludes" . epo-xml-yes))
    ("tar"  ("tarfile" . epo-xml-file-name)
     ("basedir" . epo-xml-file-name)
     ("includes") ("includesfile" . epo-xml-file-name)
     ("excludes") ("excludesfile" . epo-xml-file-name)
     ("defaultexcludes" . epo-xml-yes))
    ("gzip" ("zipfile" . epo-xml-file-name)
     ("src" . epo-xml-file-name))
    ("fileset"
     ("dir" . epo-xml-file-name)
     ("includes") ("includesfile" . epo-xml-file-name)
     ("excludes") ("excludesfile" . epo-xml-file-name)
     ("defaultexcludes" . epo-xml-yes))
    ("copy" ("file" . epo-xml-file-name)
     ("tofile" . epo-xml-file-name)
     ("todir" . epo-xml-file-name)
     ("overwrite" . epo-xml-yes))
    ("move" ("file" . epo-xml-file-name)
     ("tofile" . epo-xml-file-name)
     ("todir" . epo-xml-file-name)
     ("overwrite" . epo-xml-yes))
    ("chmod" ("file" . epo-xml-file-name)
     ("dir" . epo-xml-file-name)
     ("perm")
     ("includes") ("includesfile" . epo-xml-file-name)
     ("excludes") ("excludesfile" . epo-xml-file-name)
     ("defaultexcludes" . epo-xml-yes))
    ("delete"
     ("file" . epo-xml-file-name)
     ("dir" . epo-xml-file-name))
    ("echo" ("message"))
    ("fail" ("message"))
    ;;
    ("ant" ("dir" . epo-xml-file-name)
     ("target") ("output") ("inheritAll"))
    ("antcall" ("target") ("inheritAll"))
    ("apply" ("executable") ("dest" . epo-xml-file-name))
    ("available" ("property") ("classname")
     ("file" . epo-xml-file-name) ("resource"))
    ("condition" ("property") ("value"))
    ("cvs" . 0)
    ("cvspass" ("cvsroot") ("password") ("passfile". epo-xml-file-name))
    ("record" ("name"))
    ("replace" ("file" . epo-xml-file-name)
     ("dir" . epo-xml-file-name) ("token"))
    ("style" ("basedir" . epo-xml-file-name)
     ("destdir" . epo-xml-file-name)
     ("extension") ("style") ("force") ("processor")
     ("includes") ("includesfile" . epo-xml-file-name)
     ("excludes") ("excludesfile" . epo-xml-file-name)
     ("defaultexcludes" . epo-xml-yes))
    ("touch" ("file" . epo-xml-file-name) ("millis") ("datetime"))
    ("tstamp" ("property") ("pattern"))
    ("test" ("name") ("fork" . epo-xml-yes))
    ("batchtest" ("fork" . epo-xml-yes)
     ("todir" . epo-xml-file-name)) )
  "epo-xml common set")

(defvar epo-ant-block-table
  (append
   '(("project" ("name") ("default")
      ("basedir" . epo-xml-file-name))
     ("target" ("name") ("depends" . epo-ant-target))
     ("classpath" . 0)
     ("patternset" . 0)
     ("junit" ("printsummary" . epo-ant-on)
      ("fork" . epo-xml-yes))
     ("junitreport" ("tofile" . epo-xml-file-name)
      ("todir" . epo-xml-file-name))
     ("dependset" . 0)
     ("parallel" . 0)
     ("pathconvert" ("targetos") ("property"))
     ("sequential" . 0)
     ("sql" ("driver") ("url") ("userid") ("password")
      ("src" . epo-xml-file-name))
     ("uptodate" ("property") ("value") ("targetfile")))
     epo-ant-common-table)
  "Default block type elements")

(defvar epo-ant-inline-table 
  (append
   '(("tstamp" . 0)
     ("property" ("name") ("value"))
     ("mkdir" ("dir" . epo-xml-file-name))
     ("pathelement" ("location") ("path" . epo-xml-file-name))
     ("include" ("name" . epo-xml-file-name))
     ("includesfile" ("name" . epo-xml-file-name))
     ("exclude" ("name" . epo-xml-file-name))
     ("arg" ("value") ("line")
      ("file" . epo-xml-file-name) ("path" . epo-xml-file-name))
     ("jvmarg" ("value") ("line")
      ("file" . epo-xml-file-name) ("path" . epo-xml-file-name))
     ("env" ("key") ("value")
      ("file" . epo-xml-file-name) ("path" . epo-xml-file-name))
     ("sysproperty" ("key") ("value")
      ("file" . epo-xml-file-name) ("path" . epo-xml-file-name))
     ("taskdef" ("name") ("classname"))
     ("formatter" ("type" . epo-ant-formatter)
      ("classname") ("extension") ("usefile"))
     ("report" ("format" . epo-ant-format)
      ("todir" . epo-xml-file-name)
      ("styledir" . epo-xml-file-name))
     ;;
     ("antstructure" ("output"))
     ("filter" ("token") ("value") ("filtersfile"))
     ("fixcrlf"
      ("srcdir" . epo-xml-file-name)
      ("destdir" . epo-xml-file-name)
      ("includes") ("includesfile" . epo-xml-file-name)
      ("excludes") ("excludesfile" . epo-xml-file-name)
      ("defaultexcludes" . epo-xml-yes))
     ("getkey" ("alias") ("storepass"))
     ("get" ("src" . epo-xml-file-name)
      ("dest" . epo-xml-file-name))
     ("mail" ("from") ("tolist") ("message")
      ("files") ("mailhost") ("subject"))
     ("patch" ("patchfile" . epo-xml-file-name))
     ("rmic" ("base")
      ("includes") ("includesfile" . epo-xml-file-name)
      ("excludes") ("excludesfile" . epo-xml-file-name)
      ("defaultexcludes" . epo-xml-yes))
     ("signjar" ("jarfile" . epo-xml-file-name)
      ("alias") ("storepass"))
     ("sleep" ("hours") ("minutes") ("seconds")
      ("milliseconds") ("failonerror"))
     ("typedef" ("name") ("classname")
      ("file" . epo-xml-file-name)
      ("resource") ("classpath")))
   epo-ant-common-table)
  "Default inline type elements")

(defvar epo-ant-iteration-alist
  '((?p (type . project)
	(opener . ((pattern "<project\\>" (!before . comment-start))))
	(closer . ((pattern "</project>" (!before . comment-start))))
	(iterator . ((indent "<target name=\"" cursor "\"/>\n"
			     "</target>" indent "\n")))) ))

(defvar epo-ant-relation-alist
  (append
   '((?f (type . file)
	 (idpattern . "[^ \t\n\r]+")
	 (relation
	  (href-file
	   (pattern "\\(\\<include\\s +name\\|file\\|manifest\\)=\\s *"
		    (after . "\\(\"\\)?\\([^\"]*\\)\\1") (return))
	   (group . 2)))))
   epo-xml-relation-alist))

;;;
;; Variables for Process Handler(EPOP)
;;;

(defvar epo-ant-process-alist
  '((?j (type . compile)
	(command "ant" epo-ant-process "-f" basename epo-ant-target))
    (?p (type . compile)
	(command "ant2" epo-ant-process
		 "-emacs" "-f" basename epo-ant-target))
    (?a (type . version)
	(command "antVer" epo-ant-process "-version"))
    (?v (type . version)
	(command "ver" "java" "-version")))
  "*dependent process alists")

;; 
;;	epop-tagjump-alist
;; 
(defvar epo-ant-tagjump-alist
      '(("ant" (type . inline)
	 (pattern . "\\s +\\[javac\\]\\s +\\(\\S +\\.java\\):\\(%l\\):")
	 (matchinfo 1 . 2))))

(provide 'epo-ant)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
