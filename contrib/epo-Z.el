;;; epojava.el -- Editing Process Organizer / Zope startup for emacs.
;;<plaintext>

;; Author: Toshikazu Ando <ando@park.ruru.ne.jp>
;; Maintainer: Toshikazu Ando <ando@park.ruru.ne.jp>
;; Created: 2002 May.01
;; Version: $Lastupdate: Thu May 02 12:49:41 2002 $ on inspire.
;; URL : http://park.ruru.ne.jp/ando/work/who/epojava/
;; Keywords: epo, Zope;; user setting.

(require 'epo-html)
;;

;; user setting.
(defvar epo-Z-default-account "foo" "*zope defalut ftp account")
(defvar epo-Z-default-host "localhost" "*zope defalut ftp host")
(defvar epo-Z-default-port "21" "*zope defalut ftp port")
(defvar epo-Z-http-port "80" "*zope defalut http port")

;;
(defvar epo-Z-dos (memq system-type '(ms-dos windows-nt OS/2)))
(defvar epo-Z-default-filename nil "default-filename")

;;;
;; Zope start
;;;
(defun epo-Z-start ()
  "if you used by Meadow then your wold like to writte for start.bat(Zope).
  \"-f21\" option"
  (interactive)
  (setq epo-Z-default-account
   (read-string (concat "account(" epo-Z-default-account ") ")
		epo-Z-default-account))
  (setq epo-Z-default-host
   (read-string (concat "host(" epo-Z-default-host ") ")
		epo-Z-default-host))
  (setq epo-Z-default-port
   (read-string (concat "port(" epo-Z-default-port ") ")
		epo-Z-default-port))
  (setq epo-Z-default-filename
	(if (string= "21" epo-Z-default-port)
	    (concat "/" epo-Z-default-account "@" epo-Z-default-host ":/")
	  (concat "/" epo-Z-default-account "@" epo-Z-default-host ":"
		epo-Z-default-port "/")))
  (setq auto-mode-alist
	(delete (assoc (concat "^" epo-Z-default-filename)
		       auto-mode-alist) auto-mode-alist))
  (setq auto-mode-alist
	(reverse
	 (cons (cons (concat "^" epo-Z-default-filename) 'epo-Z-mode)
	       (reverse auto-mode-alist))))
  (find-file epo-Z-default-filename))

(defun epo-Z-mode ()
  "serach html-mode."
  (interactive)
  (let ((list auto-mode-alist) (buff) (mode 'html-mode))
    (while list
      (setq buf (car list))
      (if (string-match (car buf) "index.html")
	  (progn (setq mode (cdr buf))
		 (setq list nil))
	(setq list (cdr list))))
    (if (string= (symbol-name 'epo-Z-mode) (symbol-name mode))
	(funcall 'html-mode)
      (funcall mode))))

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-Z-structure-alist
  '((?b (type . block)
	(structure "<" keyword argument ">\n" indent cursor "\n"
		   "</" keyword ">" indent)
	(table . epo-Z-block-table)
	(argsep . sgml)	(mustmatch . t))
    (?l (type . typeface)
	(structure "<" keyword argument ">\n" indent cursor "\n"
		   "</" keyword ">" indent)
	(table . epo-html-typeface-table)
	(argsep . sgml)	(mustmatch . t))
    (?s (type . inline)
	(structure indent "<" keyword argument ">" cursor)
	(table . epo-Z-inline-table)
	(argsep . sgml) (arg-reader . nil))
    (?! (type . !)
	(structure "<!-- " cursor " -->" indent)
	(mustmatch . t))
    (?n (type . ret)
	(structure "<br>" indent cursor)
	(mustmatch . t))))

(defvar epo-Z-boolean-aligns '(("0") ("1")))
(defvar epo-Z-disposition-aligns '(("attachment")))
(defvar epo-Z-mime-encode-aligns '(("base64")))
(defvar epo-Z-encode-aligns '(("base64")))
(defvar epo-Z-block-dtml-table
  '(("dtml-if" ("expr"))
    ("dtml-in" ("prefix") ("expr") ("size") ("start"))
    ("dtml-with" . 0)
    ("dtml-let" ("person") ("relation") ("target"))
    ("dtml-call" ("expr"))
    ("dtml-comment" . 0)
    ("dtml-tree" ("branches") ("leaves") ("nowrap" . epo-Z-boolean-aligns)
     ("sort") ("assume_children" . epo-Z-boolean-aligns)
     ("single" . epo-Z-boolean-aligns)
     ("skip_unauthorized" . epo-Z-boolean-aligns))
    ("dtml-return" ("expr"))
    ("dtml-sendmail" ("smtphost"))
    ("dtml-mime" ("type" . epo-html-codetype-aligns)
     ("encode" . epo-Z-mime-encode-aligns))
    ("dtml-boundary" ("type" . epo-html-codetype-aligns)
     ("disposition" . epo-Z-disposition-aligns)
     ("encode" . epo-Z-encode-aligns))
    ("dtml-unless" ("expr"))
    ("dtml-raise" ("type"))
    ("dtml-try" . 0))
  "Dtml-if elements")

(defvar epo-Z-block-table
  (append epo-Z-block-dtml-table epo-html-block-table))

(defvar epo-Z-inline-table
  (append
   '(("dtml-var" ("expr") ("fmt"))
     ("dtml-if" ("expr"))
     ("dtml-elif" ("expr"))
     ("dtml-else" . 0)
     ("dtml-except" . 0)
     ("dtml-finally" . 0))
   epo-html-typeface-table)
  "Dtml-var elements")

(defvar epo-Z-iteration-alist epo-html-iteration-alist)
(defvar epo-Z-relation-alist epo-html-relation-alist)
(defun  epo-Z-process ()
  (if epo-Z-dos "start" "netindentscape -remote"))
(defun  epo-Z-url ()
  (let ((host) (file))
    (if (string-match "^/\\S +@\\(\\S +\\):\\s0*/\\(\\S +\\)$"
		      (buffer-file-name))
	(progn
	  (setq host (substring (buffer-file-name)
				(match-beginning 1) (match-end 1))
		file (substring (buffer-file-name)
				(match-beginning 2) (match-end 2)))
	  (concat "http://" host ":" epo-Z-http-port "/" file))
      (concat "http://" epo-Z-default-host ":" epo-Z-http-port "/"))))
(defun  epo-Z-url-manager ()
  (concat "http://" epo-Z-default-host "/manage"))
(defvar epo-Z-process-alist
  '((?r (type . run) (prompt . t)
	(command "preview" epo-Z-process epo-Z-url))
    (?j (type . compile) (prompt . t)
	(command "manage" epo-Z-process epo-Z-url-manager)))
  "*Zope dependent process alists")
(defvar epo-Z-tagjump-alist epo-html-tagjump-alist)

(provide 'epo-Z)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
