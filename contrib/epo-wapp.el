;;; -*- Emacs-Lisp -*-
;;; EPO web-app dependent settings
;;; (c) 2002 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2002 Mar 19
;;; $Lastupdate: Fri Mar 29 23:38:40 2002 $ on inspire.

(require 'epo-xml)
;;[Commentary]
;;	
;;	EPO -- web-app
;;      http://java.sun.com/j2ee/
;;

;;;
;; Variables for Input Aider(EPOI)
;;;

(defvar epo-wapp-structure-alist
  (append
   '((?b (type . block)
	 (structure indent "<" keyword ">\n" argument indent cursor "\n"
		    "</" keyword ">" indent)
	 (table . epo-wapp-block-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil))
     (?s (type . inline)
	 (structure indent "<" keyword ">" argument cursor
		    "</" keyword ">" indent)
	 (table . epo-wapp-block-table)
	 (argsep " " "\"" "=" " " "\"" "")
	 (mustmatch . nil) (arg-reader . nil)))
   epo-xml-basic-alist))

(defvar epo-wapp-block-table
  '(("web-app" . 0)
    ("icon" . 0) ("small-icon" . 0) ("large-icon" . 0)
    ("display-name" . 0)
    ("description" . 0)
    ("distoributable" . 0)
    ("context-param" . 0) ("param-name" . 0) ("param-value" . 0)
    ("filter" . 0) ("filter-name" . 0) ("filter-class" . 0)
    ("init-param" . 0)
    ("filter-mapping" . 0) ("url-pattern" . 0) ("servlet-name" . 0)
    ("listener" . 0) ("listner-class" . 0)
    ("servlet" . 0) ("servlet-class" . 0) ("jsp-file" . 0)
    ("load-on-startup" . 0)
    ("run-as" . 0) ("role-name" . 0) ("role-link" . 0)
    ("servlet-mapping" . 0)
    ("session-coding" . 0) ("session-timeout" . 0)
    ("mime-mapping" . 0) ("extension" . 0) ("mime-type" . 0)
    ("welcome-file-list" . 0) ("welcome-file" . 0)
    ("error-page" . 0) ("error-code" . 0) ("exception-type" . 0)
    ("location" . 0)
    ("tablib" . 0) ("tablib-url" . 0) ("tablib-location" . 0)
    ("resource-env-ref" . 0) ("resource-env-ref-name" . 0)
    ("resource-env-ref-type" . 0) 
    ("resource-ref" . 0) ("resource-ref-name" . 0)
    ("res-type" . 0) ("res-auth" . 0)
    ("security-constraint" . 0) ("web-resource-collection" . 0) 
    ("web-resource-name" . 0) ("http-method" . 0) ("auth-constraint" . 0)
    ("user-data-constraint" . 0) ("transport-guarantee" . 0)
    ("login-config" . 0) ("auth-method" . 0) ("relam-name" . 0)
    ("from-login-name" . 0)
    ("from-login-config" . 0) ("from-login-page" . 0) ("from-error-page" . 0)
    ("security-role" . 0) ("security-role-ref" . 0)
    ("env-entry" . 0) ("env-entry-name" . 0)
    ("env-entry-value" . 0) ("env-entry-type" . 0)
    ("ejb-ref" . 0) ("ejb-ref-name" . 0) ("ejb-ref-type" . 0)
    ("home" . 0) ("remote" . 0)
    ("ejb-link" . 0)
    ("ejb-local-ref" . 0) ("local-home" . 0) ("local" . 0))
  "Default block type elements")

(defvar epo-wapp-iteration-alist
  '((?i (type . icon)
	(opener . ((pattern "<icon\\>" (!before . comment-start))))
	(closer . ((pattern "</icon>" (!before . comment-start))))
	(iterator . ((indent "<small-icon>" cursor "</small-icon>\n"
			     indent "<large-icon><large-icon>\n"))))
    (?l (type . listener)
	(opener . ((pattern "<listener\\>" (!before . comment-start))))
	(closer . ((pattern "</listener>" (!before . comment-start))))
	(iterator . ((indent "<listner-class>" cursor "</listner-class>\n"))))
    (?s (type . sconfig)
	(opener . ((pattern "<session-coding\\>" (!before . comment-start))))
	(closer . ((pattern "</session-coding>" (!before . comment-start))))
	(iterator . ((indent "<session-timeout>" cursor
			     "</session-timeout>\n"))))
    (?w (type . welcome)
	(opener . ((pattern "<welcome-file-list\\>"
			    (!before . comment-start))))
	(closer . ((pattern "</welcome-file-list>"
			    (!before . comment-start))))
	(iterator . ((indent "<welcome-file>" cursor "</welcome-file>\n"))))
    (?m (type . mime)
	(opener . ((pattern "<mime-mapping\\>" (!before . comment-start))))
	(closer . ((pattern "</mime-mapping>" (!before . comment-start))))
	(iterator . ((indent "<extension>" cursor "</extension>\n"
			     indent "<mime-type><mime-type>\n"))))
    (?t (type . tablib)
	(opener . ((pattern "<tablib\\>" (!before . comment-start))))
	(closer . ((pattern "</tablib>" (!before . comment-start))))
	(iterator . ((indent "<tablib-url>" cursor "</tablib-url>\n"
			     indent "<tablib-location><tablib-location>\n"))))
    (?f (type . login)
	(opener . ((pattern "<from-login-config\\>"
			    (!before . comment-start))))
	(closer . ((pattern "</from-login-config>"
			    (!before . comment-start))))
	(iterator . ((indent
		      "<from-login-page>" cursor "</from-login-page>\n"
		      indent "<from-error-page><from-error-page>\n"))))
    (?p (type . param)
	(opener . ((pattern "<init-param\\>"
			    (!before . comment-start))))
	(closer . ((pattern "</init-param>"
			    (!before . comment-start))))
	(iterator . ((indent
		      "<servlet-name>" cursor "</servlet-name>\n"
		      indent "<url-pattern><url-pattern>\n"))))
    (?e (type . map)
	(opener . ((pattern "<servlet-mapping\\>"
			    (!before . comment-start))))
	(closer . ((pattern "</servlet-mapping>"
			    (!before . comment-start))))
	(iterator . ((indent
		      "<param-name>" cursor "</param-name>\n"
		      indent "<param-value><param-value>\n")))) ))

(defvar epo-wapp-relation-alist epo-xml-relation-alist)
;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-wapp-process-alist epo-xml-process-alist)
;; 
;;	epop-tagjump-alist
;; 
(defvar epo-wapp-tagjump-alist epo-xml-tagjump-alist)

(provide 'epo-wapp)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
