;;; -*- Emacs-Lisp -*-
;;; EPO Input Aider
;;; (c)1999-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; $Id$
;;; Last modified Mon Feb  4 16:49:02 2002 on duke

;;	2001/1/6 $B%7%s%\%kJd40:n@.3+;O(B
;;	2001/1/8 $B%7%s%\%kJd4040@.(B($B3+$$$F$$$k%P%C%U%!72$N$_(B)
;;	
;;[Commentary]
;;	
;;	EPO$B$N?75,F~NO;Y1g%O%s%I%i(B
;;	
;;[Abstract]
;;	
;;	epoi$B$OJT=8%U%!%$%k(B s $B$K?75,$K9=B$$rF~NO$9$k2aDx$r;Y1g$9$k!#(B
;;	$BF~NO2DG=$J9=B$$r0J2<$N$h$&$KJ,N`$9$k!#(B
;;	
;;	* $B7?A^F~(B
;;	  $B8@8lFCM-$N%V%m%C%/9=B$$rJd405!G=IU$-$G0l3gF~NO$9$k(B
;;		
;;	* $B7+$jJV$7;XDj;RA^F~(B
;;	  $B6I=jH"Fb$G!"$"$k0lDj$N%-!<%o!<%I$r7+$jJV$7$F5-=R$9$k4D6-(B($BJq3g(B
;;	  $B;R(B)$B$,$"$k>l9g!"$=$NFbIt$GH/@8$9$Y$-%-!<%o!<%I$r<+F0G'<1$7$F%]%$(B
;;	  $B%s%H0LCV$K=PNO$9$k!#(B
;;		
;;	
;;[epo-$B8@8lL>(B-structure-alist$B$N9=B$(B]
;;	
;;	epo-$B8@8lL>(B-structure-alist := '(<identifier>
;;				    <structure-info>) $B$N%j%9%H(B
;;	<identifier> := <structure-info> $B$r0l0U$KDj$a$k0lJ8;z!#F~NO%-!<$H$7$F$b(B
;;			$B;H$o$l$k(B
;;	<structure-info> := ((type . <type-value>)
;;			     (structure . <structure-list>)
;;			     (table . <completion-table>)
;;			     (argsep . <argument-seplist>)
;;			     [ (mustmatch . <mustmatch-value>) ]
;;			     [ (arg-reader . <argument-read-function>) ]
;;			   )
;;	<type-value> := $B9=B$$N7A<0$r<($9G$0U$N%7%s%\%k(B
;;	<structure-list> := <structure-list> | "$BJ8;zNs(B" | 'keyword | 'argument
;;			   | 'indent | 'cursor
;;		$B9=B$$r%F%-%9%HCf$KF~NO$9$k>l9g$NN.$l$rNs5s$7$?$j%9%H$H(B
;;		$B$J$k(B
;;		"$BJ8;zNs(B"	$B$=$N$^$^%F%-%9%H$KA^F~$5$l$k(B
;;		'keyword	$B$=$3$K9=B$$N(BID$B$H$J$k%-!<%o!<%I$,A^F~$5$l$k(B
;;		'argument	$B$=$3$K9=B$$N0z?t$,A^F~$5$l$k(B
;;		'indent		$B$=$3$^$GA^F~$,:Q$s$@;~E@$G%$%s%G%s%H$r9T$&(B
;;		'cursor		$B$=$3$^$GA^F~$,:Q$s$@0LCV$K!"A^F~40N;8e$N(B
;;				$B%+!<%=%k$,@_Dj$5$l$k!#$?$@$7!"(Bargument
;;				$B$NF~NO$,>JN,$5$l$?>l9g$K$O%+!<%=%k0LCV$O(B
;;				$B0z?t0LCV$K@_Dj$5$l$k!#(B
;;	<completion-table> := <alist> | <alist-symbol>
;;	<alist> := $B$3$N9=B$$N%-!<%o!<%I0lMw$r;}$DJd40%j%9%H(B
;;		 $B3F8uJd$r4^$`%j%9%H$N(Bcdr$BIt$O(B($B$b$7$"$l$P(B)$B$=$N%j%9%H$N3FMWAG$,(B
;;		 $B0z?t$rFI$_9~$`$?$a$KMxMQ$5$l$k!#J8;zNs$J$i%W%m%s%W%H$H(B
;;		 $B$7$F!"%7%s%\%k$J$i$=$N%7%s%\%k$K%P%$%s%I$5$l$?4X?t$,8F(B
;;		 $B$P$l$k(B($B%-!<%o!<%IL>$H0z?t$N0LCV(B[$B@0?t(B]$B$,EO$5$l$k(B)$B!#(B
;;		 $B$b$7MWAG$,99$K%j%9%H$K$J$C$F$$$?>l9g$O$=$N(Bcar$BIt$,%W%m%s(B
;;		 $B%W%H$H$7$F!"(Bcdr$BIt$,8uJd$r7hDj$9$k(Balist$B$^$?$O(Balist$B$rJ];}(B
;;		 $B$9$kJQ?t$H$7$FMxMQ$5$l$k!#(B
;;	<alist-symbol> := $B$3$N9=B$$N%-!<%o!<%I0lMw$r;}$DJd40%j%9%H$rJ];}$9(B
;;			$B$k(B Lisp $B%7%s%\%k!#$=$NCM$K4X$7$F!">e5-(Balist$B$HF1(B
;;			$B0l$NI>2A$,$J$5$l$k!#(B
;;	<argument-seplist> := (<delim-open> <delim-left> <delim-let>
;;			     <delim-mid> <delim-right> <delim-close>)
;;			($BE57?E*$J(Bseplist$B$r(Balias$B$K$7$F$*$/$H$($($+$b(B)
;;	<delim-open>  := $B0z?tNsA4BN$N:83g8L$KAjEv$9$kJ8;zNs(B
;;	<delim-left>  := $B0z?t0l8D$r3g$k:83g8L$KAjEv$9$kJ8;zNs(B
;;	<delim-let>   := $B0z?t$NM?$(J}$,(B $BB0@-(B=$BCM(B(Hash$BJ}<0(B) $B$G$"$k$H$-$KBeF~(B
;;		       $B1i;;;R$H$J$kJ8;zNs!#(Bnil $B$J$i0z?t$O0LCV$G0UL#$,7h(B
;;		       $BDj$7$FCM$N$_5-=R$9$kJ}<0$H$J$k!#(B
;;		       ($B;EMMJQ99$N2DG=@-$"$j(B)
;;	<delim-mid>   := $B0z?t$I$&$7$r6h@Z$k%G%j%_%?J8;zNs(B
;;	<delim-right> := $B0z?t0l8D$r3g$k1&3g8L$KAjEv$9$kJ8;zNs(B
;;	<delim-close> := $B0z?tNsA4BN$N1&3g8L$KAjEv$9$kJ8;zNs(B
;;	<delim-func>  := $B0z?t$rI,MW8D?tFI$_9~$s$G%G%j%_%?J8;zNs$H6&$KJV$9(B
;;		         $B4X?t$N(B Lisp $B%7%s%\%k(B
;;	<mustmatch-value> := t | nil
;;			($BJd40%-!<%o!<%I$K0lCW$9$k$3$H$r6/@)$9$k$+H]$+(B)
;;	<argument-read-function> := $B0z?t$rI,MW8D?tFI$_9~$`$?$a$N%G%U%)%k%H4X?t(B
;;
;;[epo-$B8@8lL>(B-structure-alist $B$N@_DjNc(B]
;;
;;	(defvar epo-tex-structure-alist
;;	  '((?b (type . begin)
;;		(structure "\\begin{" keyword "}" argument "\n" indent
;;			   cursor "\n" "\\end{" keyword "}" indent)
;;		(table . env-table)
;;		(argsep "{" "" "}")
;;		(mustmatch . nil) (arg-reader . epo-tex-arg-reader-env))
;;	    (?s (type . section)
;;		(structure "\\" keyword "{" argument "}")
;;		(table . section-table)
;;		(arg-reader . epo-tex-arg-reader-section))))

(require 'epo)

(defun epoi*language-menu ()
  (let ((list (epo*get-cur-lang-alist 'structure))
	(menu ":")
	elm bind type guide)
    (while list
      (setq elm (car list)
	    bind (car elm)
	    type (epo*structure-info-get 'type (cdr elm)))
      ;;$B$3$3$G$O(B (command "commandname" .....)
      (or bind type
	  (error "%s$B8@8lMQ$N(B structure-alist $B$,JQ$G$9(B" epo:current-language))
      (setq type (symbol-name type))
      (setq menu
	    (concat menu
		    (format " (%c)%s"
			    bind
			    type)))
      (setq list (cdr list)))
    (concat menu ": ")))

(defun epoi*make-arguments (keyword argread delim-type)
  "Make argument list including delimiter and parentheses.
CAUTION: This function returns argument composition list in reverse order."
  (if (and (symbolp delim-type) (fboundp delim-type))
      (funcall delim-type argc)
    (let ((open (nth 0 delim-type))
	  (left (or (nth 1 delim-type) ""))
	  (letin (nth 2 delim-type))
	  (delim (nth 3 delim-type))
	  (right (or (nth 4 delim-type) ""))
	  (close (nth 5 delim-type))
	  args ans (i 0) stop x head attr flag)
      (cond
       ((numberp argread)
	(while (<= (setq i (1+ i)) argread)
	  (if stop (setq args (cons "" args))
	    (setq ans (read-string (format "#%d arg: " i)))
	    (if (equal "" ans) (setq stop t)
	      (setq args (cons ans args))))))
       ((listp argread)
	(while argread
	  (setq attr "" i (1+ i))
	  (if stop (setq args (cons "" args))
	    (setq ans
		  (cond
		   ((listp (setq head (car argread)))
		    (setq attr (car head))
		    (if (and (symbolp (cdr head)) (fboundp (cdr head)))
			(funcall (cdr head) keyword i attr)
		      (completing-read
		       (concat (car head) ": ")
		       (epo*get-value (cdr head)))))
		   ((and (symbolp head) (fboundp head))
		    (funcall head keyword i attr))
		   (t
		    (read-string (concat (setq attr (car argread)) ": ")))))
	    (if letin			;hash format
		(if (equal "" ans)
		    nil			;omit both variable and value
		  (setq ans (concat attr letin left ans right)))
	      ;; In case, arguments' meanings are defined by order(Normal case)
	      (if (equal "" ans)
		  (setq stop t ans 'cursor)
		(setq ans (concat left ans right))))
	    (if ans
		(if (and (stringp ans) (string< "" ans))
		    (setq flag t
			  args (cons delim (cons ans args))))
		  (setq args cons ans args)))
	  (setq argread (cdr argread))))
       (t (error "argtype must be list of strings or integer")))
      (if (eq delim (car args)) (setq args (cdr args))) ;delete trailing " "
      (if flag				;some string inserted
	  (setq args (cons close args)
		args (nconc args (list open)))
	args))))

(defun epoi*insert-structure-list (outlist)
  "Insert output structure OUTLIST into current buffer."
  (let ((insert-list-f
	 (function
	  (lambda (outlist)
	    (while outlist
	      (setq elm (car outlist))
	      (cond
	       ((eq elm 'cursor)
		(or cursor (setq cursor (point))))
	       ((eq elm 'indent)
		(and indent-line-function
		     (fboundp indent-line-function)
		     (funcall indent-line-function)))
	       ((listp elm)
		(funcall insert-list-f elm))
	       (t (insert elm)))
	      (setq outlist (cdr outlist))))))
	elm cursor)
    (funcall insert-list-f outlist)
    (if cursor (goto-char cursor))))

(defun epoi*structure-input (structure-alist &optional kwd)
  "Insert language dependent structures in STRUCTURE-ALIST with completion.
Optional second argument KWD is used to replace 'keyword symbol
instead of reading it from minibuffer."
  (let* ((list structure-alist)
	 (type (symbol-name (epo*structure-info-get 'type list)))
	 (struc (epo*structure-info-get 'structure list))
	 (table (epo*structure-info-get 'table list))
	 (keymap (epo*structure-info-get 'keymap list))
	 (argsep (epo*structure-info-get 'argsep list))
	 (mustmatch (epo*structure-info-get 'mustmatch list))
	 (arg-reader (epo*structure-info-get 'arg-reader list))
	 x newlist arg outlist elm cursor keyword (argp 0))
    (and (symbolp table) (listp (symbol-value table))
	 (setq table (symbol-value table)))
    (or keymap (setq keymap (if mustmatch minibuffer-local-must-match-map
			      minibuffer-local-completion-map)))
    (if (symbolp argsep)
	(if (setq x
		  (cdr (assq argsep
			     '((c-function "" "" nil ", " "" "")
			       (latex "" "{" nil "" "}" "")
			       (sgml " " "\"" "=" " " "\"" "")
			       (lisp "" "" nil " " "" "")
			       (perl-hash "{" "" "=>" "," "" "}")))))
	    (setq argsep x)
	  (setq argsep (epo*get-value argsep))))
    (while struc
      (setq elm (car struc))
      (cond
       ((eq elm 'keyword)
	(if (null keyword)
	    (if kwd (setq keyword kwd)
	      (let ((minibuffer-completion-table table))
		(setq keyword (read-from-minibuffer
			       (format "%s: " (capitalize type))
			       nil
			       keymap)))))
	(setq outlist (cons keyword outlist)))
       (t
	(setq outlist (cons elm outlist))))
      (setq struc (cdr struc)))
    (setq outlist (nreverse outlist))
    ;;--------------
    (while outlist
      (setq elm (car outlist))
      (cond
       ((eq elm 'argument)
	(setq argp (1+ argp))
	(setq x (cdr-safe (assoc keyword table)))
	(setq arg (cond
		   (x (epoi*make-arguments keyword x argsep))
		   ((and arg-reader (fboundp arg-reader))
		    (funcall arg-reader keyword argp))
		   (t
		    (read-string (format "arg#%d for %s: " argp keyword)))))
	(cond
	 ((null arg) nil)
	 ((listp arg)
	  (setq newlist (append arg newlist)))
	 ((equal "" arg) (setq newlist (cons 'cursor newlist)))
	 (t (setq newlist (cons arg newlist)))))
       (t (setq newlist (cons elm newlist))))
      (setq outlist (cdr outlist)))
    ;;--- insert now
    (epoi*insert-structure-list (nreverse newlist))))

(defun epoi-insert-structure ()
  "Completing input menu"
  (interactive)
  (epo*check-mode)
  (let ((mess (epoi*language-menu)) c l)
    (message "Completion %s" mess)
    (setq c (read-char))
    (if (setq l (assq c (epo*get-cur-lang-alist 'structure)))
	(epoi*structure-input l)
      (error "No such choice: %c" c))))

;;$B!Z7+$jJV$7;XDj;RA^F~![(B
;;
;;	$B!V7+$jJV$7!W$H$O%=!<%9$N0lIt$KB8:_$9$k6I=jH"$NCfFCM-$N!"F1MM$N9=(B
;;	$BB$$,7+$jJV$78=$l$k@-<A$r$$$&!#%W%m%0%i%_%s%08@8l$G$O(Bcase$BJ,4t!"%^!<(B
;;	$B%/%"%C%W8@8l$G$O%"%$%F%`Ns5s$d!"I=$J$I$,$3$l$KEv$?$k!#%=!<%9%F%-(B
;;	$B%9%H$NFCDj$N>l=j$GH/@8$9$Y$-7+$jJV$7$rG'<1$9$k$?$a$N%Q%?!<%s$r(B
;;	epoi-$B8@8lL>(B-iteration-alist $B$G;XDj$9$k!#(B
;;	
;;	$BFbIt$KFCM-$N7+$jJV$7$r;}$D6I=jH"$N<gBN$rJq3g;R$H8F$V$3$H$K$9$k!#(B
;;
;;[epo-$B8@8lL>(B-iteration-alist$B$N9=B$(B]
;;	
;;	epo-$B8@8lL>(B-iteration-alist := '(identifier
;;				    iteration-info) $B$N%j%9%H(B
;;	identifier := structure-info $B$r0l0U$KDj$a$k0lJ8;z!#(B
;;		      $BL@<(E*$K8=:_$NJq3g;R$r;X<($9$k$?$a$N(B
;;		      $BF~NO%-!<$H$7$F$b;H$o$l$k!#(B
;;	iteration-info := ((type . type-value)
;;			   (opener . opening-pattern)
;;			   (closer . closing-pattern)
;;			   (iterator . iterator-list)
;;			   )
;;	opening-pattern := opening-regexp | opening-rule
;;	opening-regexp := $BJq3g;R$N3+;O0LCV$r<($9@55,I=8=(B($BJ8;zNs(B)
;;	opening-rule := (opening-rule-unit *)
;;	opening-rule-unit := (opening-check-method . opening-check-rule)
;;	opening-check-method := 'pattern | 'before | '!before
;;				| 'after | '!after
;;	opening-check-rule := opening-check-function | regexp
;;	opening-check-function := 
;;	iterator-list := (structure-list *)
;;	

(defvar epoi*iteration-opener-ccre nil)
(defvar epoi*iteration-closer-ccre nil)
(defun epoi*make-iterator-ccre (alist)
  "Make iterator opener and close context-confirming-regexp(ccre) from language iterator alist.
Set ccre of opener to the property '<LANGUAGE> of
epoi*iteration-opener-ccre, and closer to the same property of
epoi*iteration-closer-ccre.
  If the box closure is based on parentheses, closer pattern will be returned
in the form '(paren . closing-parenthesis)"
  (let ((lang epo:current-language)
	elm o opener c closer p paren)
    (while alist
      (setq elm (cdr (car alist))	;strip identifier
	    o (cdr (assq 'pattern (cdr (assq 'opener elm))))
	    c (cdr (assq 'pattern (cdr (assq 'closer elm))))
	    p (cdr (assq 'paren   (cdr (assq 'closer elm)))))
      (if o (setq opener (cons o opener)))
      (if c (setq closer (cons c closer)))
      (if p (setq paren p))
      (setq alist (cdr alist)))
    (put 'epoi*iteration-opener-ccre (intern lang) opener)
    (if paren
	(put 'epoi*iteration-closer-ccre (intern lang) (cons 'paren paren))
      (put 'epoi*iteration-closer-ccre (intern lang) closer))))

(defun epoi*iteration-alist ()
  (let ((sym (intern-soft
	      (concat "epo-" epo:current-language "-iteration-alist"))))
    (and sym (boundp sym) (symbol-value sym))))

(defun epoi*get-box-alist (matched-string)
  "Return box structure alist from MATCHED-STRING."
  (let ((list (epoi*iteration-alist)) pat)
    (catch 'alist
      (while list
	(setq pat (car (cdr (assq 'pattern (cdr (assq 'opener (cdr (car list))))))))
	(if (and (stringp pat) (string-match pat matched-string))
	    (throw 'alist (car list)))
	(setq list (cdr list))))))

(defun epoi*iteration-cache-clear ()
  (put 'epoi*iteration-opener-ccre (intern epo:current-language) nil))

(defun epoi*inner-box (&optional p)
  "Get inner-most box structure on the point."
  (or (get 'epoi*iteration-opener-ccre (intern epo:current-language))
      (epoi*make-iterator-ccre (epoi*iteration-alist)))
  (epo*current-box
   (get 'epoi*iteration-opener-ccre (intern epo:current-language))
   (get 'epoi*iteration-closer-ccre (intern epo:current-language))
   p 'epo*check-not-in-comment))

(defun epoi*insert-iterator (&optional indent)
  "Insert iterators according to current box.
If optional second argument INDENT is non-nil, indent according to the
column of box opener."
  (or epo-mode (error "Not in epo-mode"))
  (let ((box (epoi*inner-box)) alist itr output)
    (if (null box)
	(message "No iterator is applicable here.")
      (setq alist (epoi*get-box-alist (car box))
	    itr (cdr (assq 'iterator alist)))
      (if indent (indent-to-column
		  (+ (car (cdr (cdr box))) epo-box-indent-depth)))
      (if (listp itr)
	  (cond
	   ((= 1 (length itr))
	    (setq output (car itr)))
	   ((listp (car itr))
	    ;;search iterators in current box, and guess next one
	    ;;
	    ;; -- should be inplemented here -- (or unnecessary?)
	    ;;
	    (error
	     "Multiple kind of iterators should be supported. But not yet.."))
	   (t (setq output itr)))
	(setq output itr))
      (cond
       ((stringp output)(insert output))
       ((symbolp output)
	(if (fboundp output)
	    (funcall output (car box))
	  (epoi*insert-structure-list (symbol-value output))))
       (t
	(epoi*insert-structure-list output))))))

(defun epoi-intelligent-newline (arg)
  "Insert newline and next iterators guessed according to current box."
  (interactive "P")
  (epo*check-mode)
  (end-of-line)
  (newline)
  (epoi*insert-iterator t))

(defun epoi-insert-iterator (arg)
  "Insert iterators iterators guessed according to current box."
  (interactive "P")
  (epoi*insert-iterator nil))

;;;
;; Completion
;;;
(defun epoi*collect-symbols-internal (ccre n)
  "Collect symbols from matched  CCRE group N, in current buffer."
  (save-excursion
    (let (alist ms)
    (goto-char (point-min))
    (while (epo*coco-re-search-forward ccre nil t)
      (or (assoc (setq ms (epo*match-string n)) alist)
	  (setq alist (cons (cons ms nil) alist))))
    alist)))

(defun epoi*symbols-internal (refid ccre n)
  "Return a list of symbols searched by epoi*collect-symbols-internal."
  (save-excursion
    (if (null epoi:symbol-cache)
	(set (make-local-variable 'epoi:symbol-cache)
	     (list (cons refid nil))))
    (if (null (assq refid epoi:symbol-cache))
	(setq epoi:symbol-cache
	      (cons (cons refid nil) epoi:symbol-cache)))
    (let ((cache (assq refid epoi:symbol-cache)))
      (if (or (null (cdr cache))
	      (epo*time< (car (cdr cache))
			 (nth 5 (file-attributes
				 (or (buffer-file-name) ".")))))
	  (setcdr cache
		  (cons (epo*current-time)
			(epoi*collect-symbols-internal ccre n))))
      (cdr (cdr cache)))))

;;(epor

(defun epoi*all-related-buffers ()
  "Return a list of all buffers related to current buffer."
  ;; want to collect all files under project-root directory
  ;; but currently return opened buffers
  (let*((pr (epo*project-root))
	(ptn (concat "^" (regexp-quote pr))))
    (epo*epo-buffer-list
     '(lambda ()
	(and (stringp (buffer-file-name))
	     (string-match ptn (buffer-file-name)))))))

(defun epoi*symbols-external (refid ccre n)
  "Return a list of all symbols of REFID."
  (let ((cb (current-buffer)) answer
	(blist (epoi*all-related-buffers)))
    (unwind-protect
	(while blist
	  (if (eq (car blist) cb)
	      nil
	    (set-buffer (car blist))
	    (setq answer (append answer (epoi*symbols-internal refid ccre n))))
	  (setq blist (cdr blist)))
      (set-buffer cb))
    answer))

(defvar epoi:symbol-cache nil
  "Symbol cache of current buffer")
(setq-default epoi:symbol-cache nil)
(defun epoi*try-completion (word refid ccre n)
  (let*((epo:internal (epoi*symbols-internal refid ccre n))
	(epo:external (epoi*symbols-external refid ccre n))
	(epo:relation (get 'epor*on-relation-p 'matched-type))
	(epo:builtin (assq 'table epo:relation)) epo:x)
    ;; Declare local variables with prefix `epo:' for the same reason	
    ;; as epoi-complete-symbol.
    (put 'epoi*try-completion 'table nil)
    (put 'epoi*try-completion 'predicate nil)
    (or (progn
	  (setq epo:x (append epo:internal epo:external))
	  (put 'epoi*try-completion 'table epo:x)
	  (try-completion word epo:x))
	(let (epo:pred)
	  (setq epo:x (cdr epo:builtin))
	  (if (and (consp epo:x) (not (consp (cdr epo:x))))
	      (setq epo:pred (cdr epo:x) epo:x (car epo:x)))
	  (if (not (fboundp epo:x))
	      (setq epo:x (epo*get-value epo:x)))
	  (put 'epoi*try-completion 'table epo:x)
	  (put 'epoi*try-completion 'predicate epo:pred)
	  ;; AAh, how fantastic, the design of try-completion and
	  ;; all-completions is!!  Because it can take a function as
	  ;; completion table, we can provide fancy completions by setting
	  ;; variables to function name.
	  (try-completion word epo:x epo:pred)))))

(defun epoi*display-candidates (candidates)
  "Display completion candidates."
  (setq candidates (sort candidates 'string<))
  (if (<= (length candidates) (- ?z ?a -1))
      (let ((cand "") w (ww (window-width)) (len 0) (tag (1- ?a)) aalist)
	(while candidates
	  (setq w (car candidates)
		aalist (cons (cons (setq tag (1+ tag)) w) aalist))
	  (if (> (+ len (length w) 5) ww)
	      (setq w (format "\n(%c)%s  " tag w) len (+ (length w) 5))
	    (setq w (format "(%c)%s  " tag w) len (+ len (+ (length w) 5))))
	  (setq cand (concat cand w)
		candidates (cdr candidates)))
	(if (get-buffer "*EPO Completions*")
	    (let ((wc (get 'epoi*display-candidates 'winconf)))
	      (kill-buffer "*EPO Completions*")
	      (and wc (eq (current-buffer) (car wc))
		   (eq (selected-window) (car (cdr wc)))
		   (set-window-configuration (car (cdr (cdr wc)))))
	      (put 'epoi*display-candidates 'winconf nil)))
	(epo*flash-string
	 (format "== Matches with %s\n%s\n%s"
		 (make-string (- ww 17) ?=) cand
		 (make-string (1- ww) ?=)))
	(if (input-pending-p)
	    (let ((c (+ 0 (read-char)))) ;+0 for XEmacs
	      (if (assq c aalist)
		  (cdr (assq c aalist))
		(if (and (featurep 'xemacs)
			 (fboundp 'character-to-event)) ;XEmacs
		    (setq unread-command-event (character-to-event c))
		  (setq unread-command-char c))
		nil))))
    ;; in case more than 26 candidates...
    (let ((sw (selected-window)) (b (get-buffer-create "*EPO Completions*")))
      (or (get 'epoi*display-candidates 'winconf)
	  (put 'epoi*display-candidates 'winconf
	       (list (current-buffer)
		     (selected-window)
		     (current-window-configuration))))
      (epo*showup-buffer b)
      (with-output-to-temp-buffer (buffer-name b)
	(display-completion-list candidates)))
    nil))

(defun epoi-complete-symbol (&optional arg)
  "Complete symbol instantly in the buffer."
  (interactive "P")
  (epo*check-mode)
  (let ((epo:r (epor*on-relation-p)))
    ;; We declare local variable with prefix because all local
    ;; variables here will appear in completion in epo-elisp.el
    (cond
     ((null epo:r)
      (message "Nothing to complete here."))
     (t
      (let*((epo:type (car epo:r))
	    ;;(epo:b (match-beginning 0))
	    ;;(epo:e (match-end 0))
	    (epo:relation (get 'epor*on-relation-p 'matched-type))
	    (epo:refid (car epo:relation))
	    (epo:partner (cdr (car (cdr epo:r))))
	    (epo:mygrp (cdr (nth 4 epo:r)))
	    (epo:wb (match-beginning epo:mygrp))
	    ;(epo:we (match-end epo:mygrp))
	    (epo:we (point))
	    (epo:grp (cdr (nth 3 epo:r)))
	    (epo:idpat (epo*get-value (cdr (assq 'idpattern epo:relation))))
	    (epo:pt
	     (epo*get-value
	      (cdr (assq 'pattern
			 (epo*get-value
			  (cdr (assq epo:partner
				     (assq 'relation epo:relation))))))))
	    (epo:ccre (cons (epo*replace-format (car epo:pt) "i" epo:idpat)
			    (cdr epo:pt)))
	    ;;(epo:word (epo*match-string epo:mygrp))
	    (epo:word (epo*buffer-substring
		       (match-beginning epo:mygrp) epo:we))
	    epo:result epo:selection)
	;;(if (or (> (point) epo:we) (< (point) epo:wb))
	;;    (error "You can complete only the word in some relation."))
	(setq epo:result (epoi*try-completion
			  epo:word epo:refid epo:ccre epo:grp))
	(cond
	 ((null epo:result)
	  (error "No match for %s" epo:word))
	 ((eq epo:result t)
	  (message "Sole completion"))
	 ((string= epo:result epo:word)
	  (setq epo:selection (epoi*display-candidates
			       (all-completions
				epo:word
				(get 'epoi*try-completion 'table)
				(get 'epoi*try-completion 'predicate))))
	  (if epo:selection
	      (progn
		(delete-region epo:wb epo:we)
		(goto-char epo:wb)
		(insert epo:selection))))
	 ((stringp epo:result)		;must be always t
	  (delete-region epo:wb epo:we)
	  (save-excursion
	    (goto-char epo:wb)
	    (insert-before-markers epo:result)))))))))

;;; C/C++/Java workaround
(defun epoi-complete-function ()
  "Completing input for C/C++/Java function name in the buffer."
  (interactive)
  (let ((p (point)) c)
    (if (eq last-command this-command)
	nil
      (if (looking-at "(")
	  nil
	(insert "()")
	(goto-char p))
      (if (prog2 (forward-char -1) (looking-at "\\w") (goto-char p))
	  nil
	(message "Initial of function")
	(setq c (read-char))
	(insert c)))
    (epoi-complete-symbol)))

(provide 'epoi)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
