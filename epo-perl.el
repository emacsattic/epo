;;; -*- Emacs-Lisp -*-
;;; EPO Perl dependent settings
;;; (c)2000-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Mon Feb  4 13:12:30 2002 on firestorm

;;[Commentary]
;;	
;;	EPOのPerl依存設定
;;	

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-perl-structure-alist
  '((?i (type . if)
	(structure "if (" cursor ") {\n} else {" indent "\n}" indent))
    (?w (type . while)
	(structure "while (" cursor ") {\n" indent "\n}" indent))
    (?f (type . for)
	(structure "for (" cursor ") {\n" indent "\n}" indent))
    (?u (type . function)
	(structure keyword)
	(table . epo-perl-functions))
    ))

(defvar epo-perl-iteration-alist
  '((?i (type . if)
	(opener . ((pattern "if\\s *(.*)\\s *{" (!before . comment-start))))
	(closer . ((pattern "}\\s *else\\s *{" (!before . comment-start))))
	(iterator . ("} elsif (" cursor ") {")))
    )
  "Iteration alist for perl, suggested by ando")

;;;
;; Variables for Reference Handler(EPOR)
;;;
(defvar epo-perl-relation-alist
  '((?f (type . reference)
	(idpattern . "[A-Za-z_][0-9A-Za-z_]*")
	(relation
	 (deffunction
	  (pattern "^[\t ]*sub\\s +\\(%i\\)") ;; (after . "{"))
	  (group . 1))
	 (reference
	  (pattern "&\\(%i\\)\\>") (group . 1)))
	(table . nil))
    (?v (type . reference)
	(idpattern . "[A-Za-z_][0-9A-Za-z_]*")
	(relation
	 (defvariable
	   (pattern "\\(\\$\\|@\\)\\<\\(%i\\)[ \t]*[;,=]")
	   (group . 2))
	 (reference
	  (pattern "\\(\\$\\|@\\)\\(%i\\)\\>") (group . 2))))
    (?s (type . file)		;system function
	(idpattern . "[A-Za-z_][0-9A-Za-z_]*")
	(relation
	 (deffunction
	  (pattern "DummyPatternsForSystemFunction")
	  (group . 1))
	 (reference
	  (pattern "\\(%i\\)\\>") (group . 1)))
	(table . epo-perl-functions))
    ))


;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-perl-process-alist
  '((?r (type . run) (prompt . t) (command "run" magic)))
  "*Perl dependent process alists")

(defvar epo-perl-tagjump-alist
  '(("run" (type . inline)
     (pattern . " \\(in file\\|at\\) \\(\\S +\\) \\(at \\)?line \\(%l\\),")
     (matchinfo 2 . 4))))

(defvar epo-perl-file-alist
  '((extension . ".pl")
    (search . epo-perl-load-path)
    (recursive-search . nil)))

(defvar epo-perl-load-path
  (epo*process-output-list
   "perl -e 'print join(\"\\n\", @INC)'" " *EPO perl tmp*")
  "*List of each @INC of perl")

(defvar epo-perl-functions		;from perlplus
  '(("accept") ("alarm") ("atan2") ("bind") ("binmode") ("caller") ("chdir")
    ("chmod") ("chop") ("chown") ("chroot") ("close") ("closedir") ("cmp")
    ("connect") ("cos") ("crypt") ("dbmclose") ("dbmopen") ("defined")
    ("delete") ("die") ("do") ("dump") ("each") ("endgrent") ("endhostent")
    ("endnetent") ("endprotoent") ("endpwent") ("endservent") ("eof") ("eq")
    ("eval") ("exec") ("exit") ("exp") ("fcntl") ("fileno") ("flock")
    ("for") ("foreach") ("fork") ("ge") ("getc") ("getgrent") ("getgrgid")
    ("getgrnam") ("gethostbyaddr") ("gethostbyname") ("gethostent")
    ("getlogin") ("getnetbyaddr") ("getnetbyname") ("getnetent")
    ("getpeername") ("getpgrp") ("getppid") ("getpriority")
    ("getprotobyname") ("getprotobynumber") ("getprotoent") ("getpwent")
    ("getpwnam") ("getpwuid") ("getservbyname") ("getservbyport")
    ("getservent") ("getsockname") ("getsockopt") ("gmtime") ("goto")
    ("grep") ("gt") ("hex") ("if") ("index") ("int") ("ioctl") ("join")
    ("keys") ("kill") ("last") ("le") ("length") ("link") ("listen")
    ("local") ("localtime") ("log") ("lstat") ("lt") ("mkdir") ("msgctl")
    ("msgget") ("msgrcv") ("msgsnd") ("ne") ("next") ("oct") ("open")
    ("opendir") ("ord") ("pack") ("package") ("pipe") ("pop") ("print")
    ("printf") ("push") ("rand") ("read") ("readdir") ("readlink") ("recv")
    ("redo") ("rename") ("require") ("reset") ("return") ("reverse")
    ("rewinddir") ("rindex") ("rmdir") ("scalar") ("seek") ("seekdir")
    ("select") ("semctl") ("semget") ("semop") ("send") ("setgrent")
    ("sethostent") ("setnetent") ("setpgrp") ("setpriority") ("setprotoent")
    ("setpwent") ("setservent") ("setsockopt") ("shift") ("shmctl")
    ("shmget") ("shmread") ("shmwrite") ("shutdown") ("sin") ("sleep")
    ("socket") ("socketpair") ("sort") ("splice") ("split") ("sprintf")
    ("sqrt") ("srand") ("stat") ("study") ("substr") ("symlink") ("syscall")
    ("sysread") ("system") ("syswrite") ("tell") ("telldir") ("time")
    ("times") ("truncate") ("umask") ("undef") ("unless") ("unlink")
    ("unpack") ("unshift") ("until") ("utime") ("values") ("vec") ("wait")
    ("waitpid") ("wantarray") ("warn") ("while") ("write"))
  "Alist of Perl's built-in functions")

(provide 'epo-perl)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
