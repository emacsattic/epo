;;; -*- Emacs-Lisp -*-
;;; EPO Process Handler
;;; (c)1999-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; $Id$
;;; Last modified Mon Jun  3 13:35:00 2002 on balius

;;[Commentary]
;;	
;;	EPO$B$N%W%m%;%9%O%s%I%i(B
;;	
;;[Abstract]
;;	
;;	epop$B$OJT=8%U%!%$%k(B s $B$K4XO"$7$F30It%W%m%;%9$r8F$V2aDx$r0J2<$N;0(B
;;	$B$D$KJ,N`$7$=$l$>$l$KBP$7$FE,@Z$J=hM}$r9T$J$&!#(B
;;	
;;	* $B%3%s%Q%$%k2aDx(B
;;		$B$"$k%"%W%j%1!<%7%g%s$K(B s $B$rEO$7=hM}$5$;$k2aDx(B
;;	* $B;nAv2aDx(B
;;		$B$"$k%"%W%j%1!<%7%g%s$K(B s $B$+$i$N@8@.J*$G$"$k(B o $B$rEO$7=hM}(B
;;		$B$5$;$k2aDx(B
;;	* $BHo;2>H2aDx(B(?)
;;		<<html -> appletviewer$B$_$?$$$J$d$D(B>>
;;	
;;[Info-tree]
;;	
;;	$B%W%m%;%95/F0$K$O0J2<$N>pJs%D%j!<$rM?$($k!#(B
;;	
;;	
;;[epo-$B8@8lL>(B-process-alist$B$N9=B$(B]
;;	
;;	$B5-=R$9$k8@8lL>$K$h$C$F7hDj$9$k(B epo-$B8@8lL>(B-process-alist $B$H$$$&(B
;;	Lisp$BJQ?t$K$h$C$F(B s $B$K4XO"$9$k%W%m%;%9$r8F$V>l9g$NF0:n$r7hDj$9$k!#(B
;;	$B0J8e$3$NJQ?t$r(B epo-lang-process-alist $B$GBeI=$9$k!#$3$NJQ?t$N9=(B
;;	$BB$$O0J2<$NDL$j!#(B
;;	
;;	epo-lang-process-alist := (<identifier>
;;				    <command-info$B$N%j%9%H(B...>) $B$N%j%9%H(B
;;	<identifier> := command-info$B$r0l0U$KDj$a$k0lJ8;z!#F~NO%-!<$H$7$F$b(B
;;			$B;H$o$l$k!#(B
;;	<command-info> := ((type . <type-value>)
;;	                   (command . <command-value>)
;;			   [ (prompt . <prompt-value>) ]
;;			   [ (builtin . <builtin-prefix>) ]
;;			   [ (makefile . <makefile-pattern>) ]
;;			   )
;;	<type-value> := compile | run | make
;;	  'compile $B"*(B $B%=!<%9$HF1$8%G%#%l%/%H%j$G(B <command-value> $B$r5/F0(B
;;	  'run     $B"*(B $B%=!<%9$HF1$8%G%#%l%/%H%j$G(B <command-value> $B$r5/F0(B
;;	  'make    $B"*(B $B%=!<%9%G%#%l%/%H%j$+$i=g$K>e0L%G%#%l%/%H%j$rC)$j(B
;;		      <makefile> $B$N%Q%?!<%s$K%^%C%A$9$k%U%!%$%k$,B8:_$9$k(B
;;		      $B:G>e0L%G%#%l%/%H%j$G(B <command-value> $B$r5/F0(B
;;	<command-value> := ("$B=hM}L>(B" <argument-making-method>$B$N%j%9%H(B)
;;	"$B=hM}L>(B" := $B8eB3$9$k%3%^%s%I$,9T$J$&=hM}$N@bL@J8;zNs(B($BC;$/(B)
;;	<argument-making-method> := <name-keyword> | "string"
;;	                         | (<name-keyword> <regexp> <replexp>)
;;	<name-keyword> := 'filename | 'basename | 'dirname | 'text
;;	"string" := $BJ8;zNs(B "string" $B<+?H(B
;;	'filename := $B%U%k%Q%9$N%U%!%$%kL>$KCV49$5$l$k(B
;;	'basename := $B%G%#%l%/%H%jL>$r=|$$$?%U%!%$%kL>$KCV49$5$l$k(B
;;	'rootname := basename$B$+$i3HD%;R$r<h$j=|$$$?L>A0$KCV49$5$l$k(B
;;	'dirname := $B%U%!%$%kL>$r=|$$$?%G%#%l%/%H%jL>$KCV49$5$l$k(B
;;	'text := s$B$N%F%-%9%HA4BN$KCV49$5$l$k(B
;;	'magic := $B%3%^%s%INs$r%U%!%$%k@hF,(B #! $B$K=>$C$F@8@.$9$k(B
;;	<regexp> := $B%^%C%A$5$;$k@55,I=8=(B
;;	<replexp> := <regexp> $B$G%^%C%A$7$?$b$N$rCV49$9$k$H$-$KMxMQ$9$kI=8=(B
;;	<prompt-value> := t | nil  ($B%3%^%s%I%i%$%s$N3NG'$rMW$9$k$+(B)
;;	<builtin-prefix> := $B%=!<%9Kd$a9~$_%3%^%s%I$N;XDjMQ(Bprefix
;;	<makefile-pattern> := make$BE*%W%m%8%'%/%H%a!<%+%W%m%0%i%`$N%8%g%VDj5A(B
;;			      $B%U%!%$%k$N%Q%?!<%s(B
;;
;;	
;;	



(defvar epop:process-alists nil
  "Process handler list")
(defun epop*get-cur-lang-alist (type)
  (or nil ;;(assoc epo:current-language epop:process-alists)
      (let*((lang epo:current-language)
	    (sym (intern-soft (concat "epo-" lang "-"
				      (symbol-name type) "-alist")))
	    (val (and sym (symbol-value sym))))
	val)))

(defun epop*processor-info-get (entity alist)
  (cdr (assq entity alist)))


(defvar epop:menu-string-alist nil)
(defun epop*language-menu ()
  (or (assoc epo:current-language epop:menu-string-alist)
      (let ((list (epop*get-cur-lang-alist 'process)) (menu "process:")
	    elm bind command guide)
	(while list
	  (setq elm (car list)
		bind (car elm)
		command (epop*processor-info-get 'command (cdr elm)))
	  ;;$B$3$3$G$O(B (command "commandname" .....)
	  (or bind command
	      (error "%s$B8@8lMQ$N(B process-alist $B$,JQ$G$9(B" epo:current-language))
	  (setq command (car command))
	  (setq menu
		(concat menu
			(format " (%c)%s"
				bind
				command)))
	  (setq list (cdr list))
	  )
	(concat menu ": "))))

;; (defun epop*create-menu

(defun epop-send-process-command-line ()
  "$B%W%m%;%9%P%C%U%!$NF~NO9T$r%W%m%;%9$KAw$k(B"
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (cmd (buffer-substring
	       (progn (goto-char (process-mark proc))
		      (skip-chars-forward " \t")
		      (point))
	       (progn (end-of-line) (point)))))
    (insert "\n ")
    (set-marker (process-mark proc) (1- (point)))
    (process-send-string proc (concat cmd "\n"))))

(defvar epo-process-mode-map (make-keymap) "$B%W%m%;%9%P%C%U%!%-!<%^%C%W(B")
(let ((ch ? ))
  (while (< ch 128)
    (define-key epo-process-mode-map
      (make-string 1 ch) 'epop-process-buffer-insert)
    (setq ch (1+ ch))))

(defun epop-process-buffer-insert (x)
  "$B%W%m%;%9%P%C%U%!$G$N%-!<F~NO(B"
  (interactive "p")
  (if (> (point) (process-mark (get-buffer-process (current-buffer))))
      (call-interactively
       (lookup-key global-map (this-command-keys)))))


;(suppress-keymap javaplus:process-buffer-map)
(define-key epo-process-mode-map
  "\C-m" 'epop-send-process-command-line)


(defun epop*process-sentinel (proc mess)
  "Display the end of process buffer."
  (cond
   ((memq (process-status proc) '(signal exit))
    (save-excursion
      (let ((sw (selected-window)) w)
	(set-buffer (process-buffer proc))
	(goto-char (point-max))
	(insert
	 (format "\nProcess %s finished at %s" proc (current-time-string)))
	(cond
	 ((and epo:frame-feature-p
	       (setq w (get-buffer-window (current-buffer) t)))
	  (select-frame (window-frame w))
	  (select-window w)
	  (goto-char (point-max))
	  (recenter -1))
	 ((setq w (get-buffer-window (current-buffer)))
	  (select-window w)
	  (goto-char (point-max))
	  (recenter -1)))
	(select-window sw))
      (message "done")))))

(defun epop*command-process-buffer (commandname)
  "Return the buffer for process of COMMANDNAME."
  (get-buffer-create (concat " *" commandname "*")))

(defun epop*get-interpreter ()
  "Get current language interpreter."
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (if (looking-at "#!")
	  (epo*buffer-substring
	   (progn
	     (goto-char (match-end 0))
	     (skip-chars-forward " \t")
	     (point))
	   (progn
	     (end-of-line)
	     (point)))
	epo:current-language))))

(defvar epop:process-command-name nil)
(defvar epop:current-process-buffer nil)

(defun epop*start-process-other-window (name commandline &optional dir)
  "Start command line (via shell) in the next window."
  (let ((b (epop*command-process-buffer name))
	(sw (selected-window)) p
	(dir (or dir default-directory)))
    (set (make-local-variable 'epop:current-process-buffer) b)
    (epo*showup-buffer b t) ;popup buffer and select it
    (kill-all-local-variables)
    (erase-buffer)
    (cd dir)				;for 19
    (setq default-directory dir)
    (or (string-match "/$" default-directory)
	(setq default-directory (concat dir "/")))
    (insert commandline "\n ")
    (set (make-local-variable 'epop:process-command-name) name)
    (set-process-sentinel
     (setq p (start-process name b shell-file-name epo:shell-c commandline))
     'epop*process-sentinel)
    (set-marker (process-mark p) (1- (point)))
    (setq major-mode 'epop-process-mode)
    (use-local-map epo-process-mode-map)
    (select-window sw)))

(defun epop*get-builtin (keyword)
  "Get built-in string specified by KEYWORD in current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (search-forward
	   (concat
	    comment-start	   ;buffer local variable in any buffer
	    keyword) nil t)
	  (let ((peol (progn (end-of-line) (point))))
	    (epo*buffer-substring
	     (progn
	       (goto-char (match-end 0))
	       (skip-chars-forward " \t")
	       (point))
	     (if (and comment-end
		      (stringp comment-end)
		      (string< "" comment-end)
		      (re-search-forward
		       (concat (regexp-quote comment-end)
			       "\\|$")
		       peol 1))
		 (match-beginning 0)
	       peol)))))))

(defun epop*update-builtin (keyword newdef)
  "Update built-in KEYWORD to NEWDEF"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (search-forward (concat comment-start keyword) nil t)
	  (let ((peol (progn (end-of-line) (point))))
	    (goto-char (match-end 0))
	    (skip-chars-forward " \t")
	    (delete-region
	     (point)
	     (if (and comment-end (stringp comment-end)
		      (string< "" comment-end)
		      (search-forward comment-end peol t))
		 (progn (goto-char (match-beginning 0)) (point))
	       peol))
	    (insert newdef))
	(while (and (progn (skip-chars-forward " \t")
			   (looking-at (regexp-quote comment-start)))
		    (not (eobp)))
	  (forward-line 1))
	(open-line 1)
	(insert comment-start keyword newdef comment-end)))))

(defun epo*get-toplevel-dir (makefile &optional start-dir)
  "Return the directory which contains toplevel file matches with MAKEFILE."
  (let ((lastdir (or start-dir default-directory))
	(case-fold-search nil)
	dir ino lastino files founddir)
    (if (directory-files lastdir nil makefile)
	(setq dir lastdir
	      foundir lastdir)
      (setq dir (expand-file-name ".." lastdir)))
    (epo*abbrev-file
     (catch 'stop
       (while (not (equal (setq ino (nthcdr 10 (file-attributes dir)))
			  lastino))
	 (if (directory-files dir nil makefile)
	     (setq founddir dir)
	   (if founddir (throw 'stop founddir)))
	 (setq lastdir dir
	       lastino ino
	       dir (expand-file-name ".." dir)))
       (throw 'stop "/")))))

(defun epop*do-eporc ()
)
(defun epop*read-eporc ()
)


(defun epop*start-processor (processor-alist &optional ask)
  "Start text processor on current file.
PROCESSOR-ALIST is the form of;
  ((type . TYPE) (command \"ProcessorCommand\" ARGMAKEMETHOD))
  ARGMAKEMETHOD is the form of;
One of symbol; filename, dirname, basename, text.
If ARGMAKEMETHOD is a list whose car is one of above symbol,
then next two list element is assumed as SearchRegexp and ReplaceRegexp."
  (let*((clist (epop*processor-info-get 'command processor-alist))
	(type  (epop*processor-info-get 'type processor-alist))
	(prompt (epop*processor-info-get 'prompt processor-alist))
	(builtin (epop*processor-info-get 'builtin processor-alist))
	(makefile (epop*processor-info-get 'makefile processor-alist))
	(prehook  (epop*processor-info-get 'prehook processor-alist))
	(posthook  (epop*processor-info-get 'posthook processor-alist))
	(command (car clist))
	(argsym (car (cdr clist)))
	arg pat repl string newarg dir
	(getarg (function
		 (lambda (sym)
		   (cond
		    ((eq sym 'filename) buffer-file-name)
		    ((eq sym 'dirname) default-directory)
		    ((eq sym 'basename)
		     (file-name-nondirectory buffer-file-name))
		    ((eq sym 'rootname)
		     (epo*file-name-root ;May I strip dirname??
		      (file-name-nondirectory buffer-file-name)))
		    ((eq sym 'text) (buffer-string))
		    (t (error "process-alist format is wrong"))))))
	(stypes '(filename dirname basename rootname text)))
    (setq arg
	  (or
	   ;; if built-in processor specified, use it
	   (and builtin (epop*get-builtin builtin))
	   ;; else, construct command line by alist
	   (mapconcat
	    (function
	     (lambda (argsym)
	       (cond
		((eq argsym 'magic)
		 (concat
		  (epop*get-interpreter)
		  " "
		  (funcall getarg 'basename)))
		((memq argsym stypes)
		 ;; if args is a reserved symbol, one of filename, dirname,
		 ;; basename, rootname, text
		 (funcall getarg argsym))
		((and (symbolp argsym) (boundp argsym)
		      (stringp (symbol-value argsym)))
		 (symbol-value argsym))
		((stringp argsym)
		 argsym)
		((and (symbolp argsym) (fboundp argsym))
		 (funcall argsym))
		;; else, argsym should be (type regexp replaceexp)
		(t
		 (setq pat (car (cdr argsym))
		       repl (car (cdr (cdr argsym))))
		 (setq string
		       (if (memq (car argsym)
				 '(filename basename dirname rootname))
			   (funcall getarg (car argsym))
			 (save-excursion
			   (goto-char (point-min))
			   (re-search-forward pat)
			   (epo*buffer-substring
			    (match-beginning 0) (match-end 0)))))
		 (string-match pat string)
		 (replace-match repl nil nil string)))))
	    (cdr clist)
	    " ")))
    (basic-save-buffer)
    ;(setq arg (concat command " " arg))
    (if (and (eq type 'make)
	     (null epo-project-root-dir))
	(let ((topdir (epo*get-toplevel-dir makefile)))
	  (or (string-match "/$" topdir)
	      (setq topdir (concat topdir "/")))
	  (catch 'ok
	    (while t
	      (setq topdir (read-file-name "Project root dir: " topdir topdir))
	      (if (file-directory-p topdir)
		(throw 'ok t))
	      (ding)
	      (message "Must be directory!")
	      (sit-for 3)))
	  (or (string-match "/$" topdir)
	      (setq topdir (concat topdir "/")))
	  (setq epo-project-root-dir topdir)))
    (run-hooks prehook)
    (epop*start-process-other-window
     command
     (cond
      (prompt
       (read-string "Execute: " arg))
      (ask
       (setq newarg (read-string "eeeExecute: " arg))
       (if (and builtin
		(not (string= newarg arg))
		(y-or-n-p "Use this command also in the future? "))
	   (epop*update-builtin builtin newarg))
       newarg)
      (t arg))
     epo-project-root-dir)
    (run-hooks posthook)))

(defun epop*kill-processor (processor-alist)
  (let*((cmd (car (epop*processor-info-get 'command processor-alist)))
	(b (epop*command-process-buffer cmd))
	p)
    (setq p (get-buffer-process (set-buffer b)))
    (if (and p (memq process-status '(run)))
	(progn
	  (interrupt-process p)
	  ;; Should I watch the process status?
	  )
      (message "Process %s already exited." cmd))))

;;;
;; Process Starting Menu
;;;
(defun epop-start-menu (&optional ask)
  "Process starting menu"
  (interactive "P")
  (let ((mess (epop*language-menu)) c l)
    (message "Start %s" mess)
    (setq c (read-char))
    (if (setq l (assq c (epop*get-cur-lang-alist 'process)))
	(epop*start-processor (cdr l) ask)
      (error "No such choice: %c" c))))

(defun epop-kill-menu ()
  "Process killing menu"
  (interactive)
  (let ((mess (epop*language-menu)) c l)
    (message "Kill %s" mess)
    (setq c (read-char))
    (if (setq l (assq c (epop*get-cur-lang-alist 'process)))
	(epop*kill-processor (cdr l))
      (error "No such choice: %c" c))))



;;;
;; Parsing error/warning messages and jump to error line
;;;

;;[epo-$B8@8lL>(B-tagjump-alist$B$N9=B$(B]
;;
;;	$B%=!<%9%U%!%$%k$r$=$lMQ$N=hM}7O$K$+$1$?$H$-$K!"=hM}7O$,%=!<%9%U%!(B
;;	$B%$%k$K$"$C$?%(%i!<$d7Y9p3:Ev2U=j$r<($9$H$-$N%Q%?!<%s$r@55,I=8=$r(B
;;	$BMxMQ$7$F(B epo-$B8@8lL>(B-tagjump-alist $B$K;XDj$9$k!#$3$NJQ?t$N9=B$$O(B
;;	$B0J2<$NDL$j!#(B
;;	
;;	epo-$B8@8lL>(B-tagjump-alist
;;		:= '($B=hM}L>%Q%?!<%s(B
;;			<tagjump-info> $B$N%j%9%H(B) $B$N(B $B%j%9%H(B
;;	$B=hM}L>%Q%?!<%s(B := epo-$B8@8lL>(B-process-alist$B$G$N(B $B!V=hM}L>!W(B
;;			      $B$N@55,I=8=!#<B:]$K$O=hM}7O$rF0$+$7$?%P%C%U%!(B
;;			      $BL>$N%Q%?!<%s!#(B
;;	<tagjump-info> := ((type . <type-value>)
;;			 (pattern . <line-regexp>)
;;			 (matchinfo . <match-cons>))
;;	<type-value> := 'inline | 'paren | 'current
;;	 'paren := $B=hM}Cf$N%U%!%$%kL>$r(B () $B$G3g$C$FI=<($9$k(B(TeX$B$HF1$8(B)
;;	 'inline := $B%(%i!<H/@8%U%!%$%k$r%(%i!<%a%C%;!<%8$HF1$89T$KI=<((B
;;	 'current := $B=hM}BP>]%U%!%$%k$OC10l!#%U%!%$%kL>I=<(L5$7!#(B
;;	<line-regexp> := $B9THV9fItJ,$N@55,I=8=(B
;;	<match-cons> := (<group-F> . <group-L>)
;;	(<group-F> $B$O%(%i!<H/@8%U%!%$%kL>(B, <group-L> $B$OH/@89THV9f$H7k9g(B)
;;	<group-F> := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | <func-sym>
;;	<group-L> := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | <func-sym>
;;	0$B!A(B9 := <line-regexp> $B$G%^%C%A$7$?(B \( \) $B$N%0%k!<%WHV9f(B
;;	<func-sym> := <line-regexp> $B$G%^%C%A$7$?2U=j$+$i$3$N4X?t$r8F$s$G%(%i!<(B
;;		    $BH/@8%U%!%$%k(B($B$^$?$O9T(B)$B$rF@$k(B
;;	
;;	<line-regexp> $B$N@55,I=8=Cf$G$O0J2<$NCV$-49$($,MxMQ$G$-$k(B
;;	
;;	  %F	$B%=!<%9%U%!%$%k$N%U%k%Q%9L>(B
;;	  %f	$B%=!<%9%U%!%$%k$N%Y!<%9L>(B
;;	  %l	$B9THV9f$N@55,I=8=(B ([0-9]+)
;;

;; Hope to be same as epop*processor-info-get forever.
(defun epop*tagjump-info-get (entity alist)
  (cdr (assq entity alist)))

(defun epop*get-processor-tagjump-alist (command alist)
  "Get tagjump information for COMMAND refering ALIST."
  (catch 'found
    (while alist
      (if (string-match (car (car alist)) command)
	  (throw 'found (cdr (car alist))))
      (setq alist (cdr alist)))))

(defvar epop:file-name-regexp "\\([a-z]:\\)?[/a-z0-9_.,@%+]+"
  "Regexp of file name")
;; This variable should depend on language??

(defun epop*get-file-name-inline ()
  "Get file name from current line."
  (let ((lim (point)) f (case-fold-search t))
    (beginning-of-line)
    (catch 'found
      (while (re-search-forward epop:file-name-regexp lim t)
	(setq f (epo*match-string 0))
	(if (file-exists-p f) (throw 'found f))))))

(defun epop*get-file-name-paren ()
  "Get parenthesized file name from current buffer."
  (save-excursion
    (let(s)
      (condition-case () (up-list -1)
	(error
	 (let ((list 0) found)
	   (while
	       (and (<= list 0) (not found)
		    (re-search-backward "\\((\\)\\|\\()\\)" nil t))
	     (if (equal (match-beginning 0) (match-beginning 2)) ;close paren.
		 (setq list (1- list)) ;open paren
	       (setq list (1+ list))
	       (if (= list 1)
		   (if (looking-at "\\([^,{}%]+\.\\)tex\\|sty")
		       (setq found t)
		     (setq list (1- list)))))))))
      (setq s
	    (buffer-substring
	     (progn (forward-char 1) (point))
	     (progn (skip-chars-forward
		     "^ \n"
		     (save-excursion (end-of-line) (point)))
		    (point))))
      (if (string= "" s) nil s))))

(defun epop-jump-to-error ()
  "Jump to error line according to the next window."
  (interactive)
  (if (and epop:current-process-buffer
	   (bufferp epop:current-process-buffer)
	   (get-buffer epop:current-process-buffer))
      (let*((pb (get-buffer epop:current-process-buffer))
	    (sw (selected-window))
	    (cb (current-buffer))
	    (filename (buffer-file-name))
	    (basename (file-name-nondirectory filename))
	    command talist attr type pattern matchinfo fmatch lmatch)
	(or (setq talist (epop*get-cur-lang-alist 'tagjump))
	    (error "There's no tagjump definition for this language"))
	(epo*showup-buffer pb t)
	(setq command epop:process-command-name)
	(or (setq attr (epop*get-processor-tagjump-alist command talist))
	    (error "There's no tagjump information for command: %s" command))
	(setq type	(epop*tagjump-info-get 'type attr)
	      pattern	(epop*tagjump-info-get 'pattern attr)
	      matchinfo	(epop*tagjump-info-get 'matchinfo attr)
	      fmatch	(car matchinfo)
	      lmatch	(cdr matchinfo))
	(setq pattern (epo*replace-format
		       (epo*replace-format
			(epo*replace-format
			 pattern "l" "[0-9]+")
			"F" (regexp-quote filename))
		       "f" (regexp-quote basename)))
	(if (re-search-backward pattern nil t)
	    (let ((b0 (match-beginning 0)) lnum source sbuf filecand)
	      (if (numberp fmatch)
		  (setq filecand (epo*match-string fmatch)))
	      (if (numberp lmatch)
		  (setq lnum (epo*match-string lmatch))
		(goto-char (match-end 0))
		(skip-chars-backward "^0-9" b0)
		(setq lnum (epo*buffer-substring
			    (point)
			    (progn (skip-chars-backward "0-9" b0) (point)))))
	      (setq source
		    (expand-file-name
		     (cond
		      ((stringp filecand) filecand)
		      ((eq type 'inline)
		       (epop*get-file-name-inline))
		      ((eq type 'paren)
		       (epop*get-file-name-paren))
		      ((eq type 'current)
		       filename))))
	      (select-window sw)
	      (setq sbuf (or (get-file-buffer source)
			     (find-file-noselect source)))
	      (or (eq sbuf (current-buffer))
		  (switch-to-buffer sbuf))
	      ;;(epo*showup-buffer sbuf t)
	      (switch-to-buffer (current-buffer))
	      (goto-line (string-to-int lnum))
	      (message "error at %s line %s" source lnum)
	      (if (not (eq (current-buffer) cb))
		  (progn
		    (set (make-local-variable 'epop:current-process-buffer) pb)
		    (epo-enable))))
	  (message "No more error/warning")
	  (select-window sw)))
    (error "No processor associated with this source was invoked.")))

;;;
;; Processor Invocation
;;;
(defvar epo-file-processor-alist-default
  (if (memq system-type '(ms-dos windows-nt OS/2))
      '(("\\.obj$" . "tgif %s")
	("\\.e?ps$" . "ghostview %s")
	("\\.\\(gif\\|jpeg\\|jpg\\|bmp\\|png\\|wav\\|psd\\)$" . "start %s")
	("^http://" . "start %s")
	(t . ""))
    '(("\\.obj$" . "tgif %s")
      ("\\.e?ps" . "ghostview %s")
      ("\\.\\(gif\\|jpeg\\|jpg\\|bmp\\|png\\)" . "xv %s")
      ("^http://" . "netscape -remote \"openURL(%s,new-window)\"")
      (t . "")))
  "Alist of filename pattern vs. its suitable processor.
Those files that are typically included into source files are registered here.
Each alist element consist of '(PATTERN . COMMAND).
In COMMAND string, `%s' is replaced to file name.")

(defvar epo-file-processor-alist nil
  "*User option.  See also epo-file-processor-alist.")

(defun epop*invoke-processor (file)
  "Invoke processor specific to FILE."
  (let ((a1 epo-file-processor-alist)
	(a2 epo-file-processor-alist-default)
	elm
	cmd)
    (setq cmd (catch 'found
		(while (setq elm (car a1)) ;2while-s to avoid append
		  (if (string-match (car elm) file) (throw 'found (cdr elm)))
		  (setq a1 (cdr a1)))
		(while (setq elm (car a2))
		  (if (string-match (car elm) file) (throw 'found (cdr elm)))
		  (setq a2 (cdr a2)))))
    (if cmd
	(epop*start-process-other-window
	 " *EPO processor*"
	 (epo*replace-format cmd "s" file)))))



(provide 'epop)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
