;;; -*- Emacs-Lisp -*-
;;; EPO Tomcat/server.xml dependent settings
;;; (c)2002 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2002 Mar 30
;;; $Lastupdate: Sat Mar 30 12:01:21 2002 $ on inspire.

(require 'epo-xml)
;;[Commentary]
;;	
;;	EPO -- Tomcat/server.xml
;;

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-sev-structure-alist
  (append
   '((?b (type . block)
	 (structure indent "<" keyword argument ">\n" indent cursor "\n"
		    "</" keyword ">" indent)
	 (table . epo-sev-block-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?s (type . inline)
	 (structure indent "<" keyword argument " />" cursor)
	 (table . epo-sev-block-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil)))
   epo-xml-basic-alist))

(defvar epo-sev-port
  '(("80") ("8005") ("8008") ("8009") ("8080") ("8081") ("8082") ("8443")))

(defvar epo-sev-block-table
  '( ("Server" ("port" . epo-sev-port) ("shutdown" . (("SHUTDOWN")))
      ("debug"))
     ("Service" ("name"))
     ("Connector" ("className")
      ("port" . epo-sev-port) ("minProcessors") ("maxProcessors")
      ("enableLookups" . epo-xml-boolean) ("redirectPort")
      ("appBase")("acceptCount") ("debug") ("scheme" . (("https")))
      ("connectionTimeout") ("secure" . epo-xml-boolean))
     ("Engine" ("className") ("name") ("defaultHost") ("debug"))
     ("Valve" ("className") ("debug")
      ("directory") ("prefix") ("suffix") ("pattern"))
     ("Logger" ("className") ("directory") ("prefix") ("suffix")
      ("timestamp" . epo-xml-boolean))
     ("Realm" ("className") ("debug") ("driverName")
      ("connectionURL") ("userTable") ("userNameCol")
      ("userCredCol") ("userRoleTable") ("roleNameCol"))
     ("Host" ("name") ("debug") ("appBase") ("unpackWARs" . epo-xml-boolean))
     ("Context" ("path") ("docBase" . epo-xml-file-name)
      ("debug") ("privileged" . epo-xml-boolean) ("reloadable")
      ("crossContext"))
     ("Ejb" ("name") ("type") ("home") ("remote"))
     ("Manager" ("className") ("debug") ("saveOnRestart" . epo-xml-boolean)
      ("maxActiveSessions") ("minIdleSwap") ("maxIdleSwap")
      ("maxIdleBackup"))
     ("Store" ("className"))
     ("Environment" ("name") ("type") ("value"))
     ("Parameter" ("name") ("value") ("override" . epo-xml-boolean))
     ("ResourceParams" ("name") ("auth") ("type"))
     ("parameter" . 0)
     ("name" . 0)    
     ("value" . 0) )
  "Default block type elements")

(defvar epo-sev-iteration-alist nil)

(defvar epo-sev-relation-alist epo-xml-relation-alist)

;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-sev-process-alist epo-xml-process-alist)

(provide 'epo-sev)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 