;;; -*- Emacs-Lisp -*-
;;; EPO Relax dependent settings
;;; (c)2000, 2001 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2001 Feb.25
;;; $Lastupdate: Mon Mar 18 23:13:50 2002 $ on inspire.

(require 'epo-xml)
;;[Commentary]
;;	
;;	EPO -- geml (Gassan Environment Markup Language)
;;	    -- gsml (Gassan Scenario Markup Language)
;;      http://www.wffm.org/download/gassan/
;;

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-gsml-structure-alist
  (append
   '((?b (type . block)
	 (structure indent "<" keyword argument ">\n" indent cursor "\n"
		    "</" keyword ">" indent)
	 (table . epo-gsml-block-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?s (type . inline)
	 (structure indent "<" keyword argument " />" cursor)
	 (table . epo-gsml-block-table)
;;;	(table . epo-gsml-inline-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil)))
   epo-xml-basic-alist))

(defvar epo-gsml-version-aligns '(("1.1")) )
(defvar epo-gsml-set-aligns '((" := true") (" := false")))
(defvar epo-gsml-compair-aligns '((" = true") (" = false")))
(defvar epo-gsml-effect-aligns
  '(("OverLay ") ("Ellipse") ("Perapera_L") ("Perapera_R")
    ("Patapata_Height") ("Patapata_Height_R") ("Patapata_Width")
    ("Kuropata_Height") ("Kuropata_Width")))
(defvar epo-gsml-align-aligns
  '(("right") ("center") ("left")))
(defvar epo-gsml-on-aligns '(("on") ("off")))
(defvar epo-gsml-layer-aligns '(("bgm") ("se") ("voice")))
(defvar epo-gsml-command-aligns '(("play") ("loop") ("stop")))

(defvar epo-gsml-block-table
  '( ("gsml" ("version" . epo-gsml-version-aligns))
     ("data" ("operation" . epo-gsml-set-aligns))
     ("scene" ("name"))
     ("cut" . 0)
     ("imagelist" . 0)
     ("effect" ("name" . epo-gsml-effect-aligns))
     ("image"
      ("file" . epo-xml-file-name)
      ("position" . epo-gsml-align-aligns) ("x") ("y") ("loader"))
     ("filter"
      ("name") ("position" . epo-gsml-align-aligns) ("x") ("y") ("loader"))
     ("composer"
      ("name") ("position" . epo-gsml-align-aligns) ("x") ("y") ("loader"))
     ("text" ("prompt" . epo-gsml-on-aligns) ("scroll" . epo-gsml-on-aligns))
     ("audio"
      ("layer" . epo-gsml-layer-aligns)
      ("command" . epo-gsml-command-aligns)
      ("file" . epo-xml-file-name))
     ("plugin" ("name"))
     ("param" ("name") ("value"))
     ("selection" . 0)
     ("alternative" . epo-gsml-set-aligns)
     ("timer" ("value"))
     ("if" ("condition" . epo-gsml-compair-aligns) 
      ("gsmlFile" . epo-xml-file-name))
     ("loop" ("condition" . epo-gsml-compair-aligns)
      ("gsmlFile" . epo-xml-file-name))
     ("end" . 0)
     ("scenario" ("name") ("gsmlFile" . epo-xml-file-name))
     ("title" . 0)
     ("creator" ("name") ("URL") ("E-mail") ("date"))
     ("view"
      ("type") ("width") ("height")
      ("iconFile" . epo-xml-file-name)
      ("imageFile" . epo-xml-file-name)
      ("audioFile" . epo-xml-file-name))
     ("opening" ("name" . epo-xml-file-name))
     ("ending" ("name" . epo-xml-file-name)))
  "Default block type elements")

(defvar epo-gsml-relation-alist
  (append
   '((?g (type . file)
	 (idpattern . "[^ \t\n\r]+")
	 (relation
	  (href-file
	   (pattern "gsmlFile=\\s *"
		    (after . "\\(\"\\)?\\([^\"]*\\)\\1") (return))
	   (group . 2)))))
   epo-xml-relation-alist))

;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-gsml-process-alist
  '((?j (type . compile)
	(command "gassan" "java" "-jar" "gassan.jar" basename))
    (?g (type . gVersion)
	(command "gVersion" "java" "-jar" "gassan.jar" "-version"))
    (?v (type . version)
	(command "version" "java" "-version")))
  "*gsml dependent process alists")

(provide 'epo-gsml)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 