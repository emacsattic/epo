;;; epo-zope.el -- Editing Process Organizer / Zope startup for emacs.
;;<plaintext>

;; Author: Toshikazu Ando <ando@park.ruru.ne.jp>
;; Maintainer: Toshikazu Ando <ando@park.ruru.ne.jp>
;; Created: 2002 May.01
;; Version: $Lastupdate: Wed Jan 15 01:12:00 2003 $ on inspire.
;; URL : http://park.ruru.ne.jp/ando/work/who/epojava/
;; Keywords: epo, Zope
;; user setting.

(require 'epo-html)
;;

;; user setting.
(defvar epo-zope-default-account "foo" "*zope defalut ftp account")
(defvar epo-zope-default-host "localhost" "*zope defalut ftp host")
(defvar epo-zope-default-port "21" "*zope defalut ftp port")
(defvar epo-zope-http-port "80" "*zope defalut http port")

;;
(defvar epo-zope-dos (memq system-type '(ms-dos windows-nt OS/2)))
(defvar epo-zope-default-filename nil "default-filename")

;;;
;; Zope start
;;;
(defun epo-zope-start ()
  "if you used by Meadow then your wold like to writte for start.bat(Zope).
  \"-f21\" option"
  (interactive)
  (setq epo-zope-default-account
   (read-string (concat "account(" epo-zope-default-account ") ")
		epo-zope-default-account))
  (setq epo-zope-default-host
   (read-string (concat "host(" epo-zope-default-host ") ")
		epo-zope-default-host))
  (setq epo-zope-default-port
   (read-string (concat "port(" epo-zope-default-port ") ")
		epo-zope-default-port))
  (setq epo-zope-default-filename
	(if (string= "21" epo-zope-default-port)
	    (concat "/" epo-zope-default-account "@" epo-zope-default-host ":/")
	  (concat "/" epo-zope-default-account "@" epo-zope-default-host " "
		epo-zope-default-port ":/")))
  (setq auto-mode-alist
	(delete (assoc (concat "^" epo-zope-default-filename)
		       auto-mode-alist) auto-mode-alist))
  (setq auto-mode-alist
	(reverse
	 (cons (cons (concat "^" epo-zope-default-filename) 'epo-zope-mode)
	       (reverse auto-mode-alist))))
  (find-file epo-zope-default-filename))

(defun epo-zope-mode ()
  "serach html-mode."
  (interactive)
  (let ((list auto-mode-alist) (buff) (mode 'html-mode))
    (while list
      (setq buff (car list))
      (if (string-match (car buff) "index.html")
	  (progn (setq mode (cdr buff))
		 (setq list nil))
	(setq list (cdr list))))
    (if (string= (symbol-name 'epo-zope-mode) (symbol-name mode))
	(funcall 'html-mode)
      (funcall mode))))

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-zope-structure-alist
  '((?b (type . block)
	(structure "<" keyword argument ">\n" indent cursor "\n"
		   "</" keyword ">" indent)
	(table . epo-zope-block-table)
	(argsep . sgml)	(mustmatch . t))
    (?l (type . typeface)
	(structure "<" keyword argument ">\n" indent cursor "\n"
		   "</" keyword ">" indent)
	(table . epo-html-typeface-table)
	(argsep . sgml)	(mustmatch . t))
    (?s (type . inline)
	(structure indent "<" keyword argument ">" cursor)
	(table . epo-zope-inline-table)
	(argsep . sgml) (arg-reader . nil))
    (?! (type . !)
	(structure "<!-- " cursor " -->" indent)
	(mustmatch . t))
    (?n (type . ret)
	(structure "<br>" indent cursor)
	(mustmatch . t))))

(defvar epo-zope-boolean-aligns '(("0") ("1")))
(defvar epo-zope-disposition-aligns '(("attachment")))
(defvar epo-zope-mime-encode-aligns '(("base64")))
(defvar epo-zope-encode-aligns '(("base64")))
(defvar epo-zope-block-dtml-table
  '(("dtml-if" ("expr"))
    ("dtml-in" ("prefix") ("expr") ("size") ("start"))
    ("dtml-with" . 0)
    ("dtml-let" ("person") ("relation") ("target"))
    ("dtml-call" ("expr"))
    ("dtml-comment" . 0)
    ("dtml-tree" ("branches") ("leaves") ("nowrap" . epo-zope-boolean-aligns)
     ("sort") ("assume_children" . epo-zope-boolean-aligns)
     ("single" . epo-zope-boolean-aligns)
     ("skip_unauthorized" . epo-zope-boolean-aligns))
    ("dtml-return" ("expr"))
    ("dtml-sendmail" ("smtphost"))
    ("dtml-mime" ("type" . epo-html-codetype-aligns)
     ("encode" . epo-zope-mime-encode-aligns))
    ("dtml-boundary" ("type" . epo-html-codetype-aligns)
     ("disposition" . epo-zope-disposition-aligns)
     ("encode" . epo-zope-encode-aligns))
    ("dtml-unless" ("expr"))
    ("dtml-raise" ("type"))
    ("dtml-try" . 0))
  "Dtml-if elements")

(defvar epo-zope-block-table
  (append epo-zope-block-dtml-table epo-html-block-table))

(defvar epo-zope-inline-table
  (append
   '(("dtml-var" ("expr") ("fmt"))
     ("dtml-if" ("expr"))
     ("dtml-elif" ("expr"))
     ("dtml-else" . 0)
     ("dtml-except" . 0)
     ("dtml-finally" . 0))
   epo-html-typeface-table)
  "Dtml-var elements")

(defvar epo-zope-iteration-alist epo-html-iteration-alist)
(defvar epo-zope-relation-alist epo-html-relation-alist)
(defun  epo-zope-process ()
  (if epo-zope-dos "start" "netindentscape -remote"))
(defun  epo-zope-url ()
  (let ((host) (file))
    (if (string-match "^/\\S +@\\(\\S +\\):\\s0*/\\(\\S +\\)$"
		      (buffer-file-name))
	(progn
	  (setq host (substring (buffer-file-name)
				(match-beginning 1) (match-end 1))
		file (substring (buffer-file-name)
				(match-beginning 2) (match-end 2)))
	  (concat "http://" host ":" epo-zope-http-port "/" file))
      (concat "http://" epo-zope-default-host ":" epo-zope-http-port "/"))))
(defun  epo-zope-url-manager ()
  (concat "http://" epo-zope-default-host "/manage"))
(defvar epo-zope-process-alist
  '((?r (type . run) (prompt . t)
	(command "preview" epo-zope-process epo-zope-url))
    (?j (type . compile) (prompt . t)
	(command "manage" epo-zope-process epo-zope-url-manager)))
  "*Zope dependent process alists")
(defvar epo-zope-tagjump-alist epo-html-tagjump-alist)

(provide 'epo-zope)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
