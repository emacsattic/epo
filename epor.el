;;; -*- Emacs-Lisp -*-
;;; EPO Relation Resolver
;;; (c)1999-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; $Id$
;;; Last modified Sun Mar 24 17:34:17 2002 on balius

;;	2000/12/28
;;	$B%Q%?!<%s%8%c%s%W$O40@.!#<!$O%U%!%$%k%8%c%s%W$H%W%m%;%9(B?
;;	$B%U%!%$%k%8%c%s%W40@.!#3HD%;R4X78$,:#$R$H$D(B?
;;	2000/12/29
;;	$B%W%m%;%95/F040@.!#(B
;;	
;;[Commentary]
;;	
;;	EPO$B$N;2>H(B/$BHo;2>H%j%>%k%P(B
;;	
;;[Abstract]
;;	
;;	epor$B$OJT=8BP>]%=!<%9%U%!%$%kCf$K=q$+$l$?!V;2>H!W$rDI@W!"$*$h$S?7(B
;;	$B5,:n@.$9$k:n6H$r;Y1g$9$k5!G=$rDs6!$9$k!#(B
;;	
;;	* $BDj5A;2>H(B
;;		$BJL2U=j$GDj5A$5$l$F$$$k4X?t(B($B%^%/%m(B)$B$r$=$N>l$GMxMQ$9$k!#(B
;;		
;;	
;;[epo-$B8@8lL>(B-relation-alist$B$N9=B$(B]
;;	
;;	epo-$B8@8lL>(B-relation-alist := '(identifier
;;				    relation-info) $B$N%j%9%H(B
;;	identifier := relation-info $B$r0l0U$KDj$a$k0lJ8;z!#F~NO%-!<$H$7$F$b(B
;;			$B;H$o$l$k!#(B
;;	relation-info := ((type . <type-value>)
;;			   (idpattern . <id-regexp>)
;;			   (relation . <relation-item-alist>))
;;	
;;	<type-value> := reference | file | hyperlink | box
;;			box$B$N>l9g$O(B identifier $B$,0l0U$G$J$/!"%F%-%9%H$N(B
;;			$B0lIt$KH"$r7A@.$9$k$b$N$G!"3+;O$H=*N;$NA08e4X78$,(B
;;			$B7h$^$C$F$$$F!"$J$*$+$D%M%9%H$9$k2DG=@-$r9MN8$9$k!#(B
;;	<id-regexp> := $B$=$N;2>H$G$N<1JL;R$H$J$jF@$k8lWC$N@55,I=8=(B
;;	<relation-item-alist> := (<AnyRelationSymbol> . <PatternList>)
;;	<AnyRelationSymbol> := $BG$0U$N4X78J]M-9`L\%7%s%\%k(B
;;	<PatternList> := (<CCRE> <NUM> <relation-type-specific>)
;;	<CCRE> := $B4X78J]M-9`L\$N(B $BJ8L.3NG'@55,I=8=!#4X78$N<1JL;R$H$J$kItJ,$N(B
;;		  $B@55,I=8=$OI,$:(B %f $B$GI=8=$7$F$*$/$3$H!#(B
;;	<NUM> := <CCRE>$B8!:w$,%^%C%A$7$?$H$-$K$=$N4X78$r0UL#$E$1$k(B
;;	          $B<1JL;R$,4^$^$l$k@55,I=8=%0%k!<%WHV9f(B
;;	<relation-type-specific> := <text-type-specific>
;;				     | <file-type-specific>
;;				     | <process-type-specific>
;;				     | <function-type-specific>
;;	<text-type-specific> := nil | <partner-symbol>
;;	<partner-symbol> := $B4X78$NAj<j$H$J$k(B <AnyRelationSymbol>
;;			    <AnyRelationSymbol>$B$,Fs8D$N>l9g$O>JN,$G$-$k!#(B
;;	<file-type-specific> := '(file)
;;				$B4X78$NAj<j$,%U%!%$%k$N$H$-$K$3$l$r;XDj!#(B
;;				$B%U%!%$%kL>$O@55,I=8=$N<1JL;RItJ,$H$J$k!#(B
;;	<process-type-specific> := '(process)
;;				$B4X78$NAj<j$,%U%!%$%k$G!"$=$N%U%!%$%k$r30(B
;;				$BIt%W%m%;%9$,=hM}$9$k>l9g$K;XDj!#(B
;;				$B%U%!%$%kL>$O@55,I=8=$N<1JL;RItJ,$H$J$k!#(B
;;	<function-type-specific> := '(process)
;;				$B4X78$NAj<j$,%U%!%$%k$G!"$=$N%U%!%$%k$r4X(B
;;				$B?t$,=hM}$9$k>l9g$K;XDj!#(B
;;				$B%U%!%$%kL>$O@55,I=8=$N<1JL;RItJ,$H$J$k!#(B
;;	
;;[$BJ8L.3NG'@55,I=8=(B]
;;	
;;	$BJ8L.3NG'@55,I=8=$H$O!"@55,I=8=8!:w$GH/8+$7$?%Q%?!<%s$NCV$+$l$F$$(B
;;	$B$k>l=j$K4X$9$k$$$/$D$+$NJ8L.>pJs$rD4$Y!"FCDj$NJ8L.$K0LCV$9$k$b$N(B
;;	$B$N$_K\Ev$KE,9g$9$k$b$N$H$_$J$9@55,I=8=$r4^$a$?%Q%?!<%s$G$"$k!#(B
;;	$B%Q%?!<%s$OG$0UD9$N%j%9%H$GM?$($k!#(B
;;	
;;	$B%Q%?!<%s%j%9%H(B := (<search-regexp> <patlist-elem> *)
;;	<patlist-elem> := <context-cons>
;;	<search-regexp> := $B=i4|8!:wMQ@55,I=8=$H$7$FMxMQ$5$l$k(B
;;	<context-cons> := <box-p> | <line-context> | <neighbor-context>
;;			  | <parenscan-context>
;;			  | <exclude-list> | <require-list>
;;			  | <recursive-exp> | <or-exps>
;;			  | <return-exp>
;;	<box-p> := (<box-or-not> . <box-name>)
;;	<box-or-not> := box | !box
;;		     'box $B$O%]%$%s%H0LCV$,(B <box-name> $B$N(Bbox$BFb$K$"$l$P??(B
;;		     '!box $B$O%]%$%s%H0LCV$,(B <box-name> $B$N(Bbox$BFb$K$J$1$l$P??(B
;;	<box-name> := $BJq3g;R$NL>A0$H$J$kJ8;zNs(B
;;	<line-context> := (<number> . <regexp>)
;;		       <number>$B9T0JFb$K(B <regexp> $B$,8+IU$+$l$P??(B
;;		       <number>$B$,Ii$J$i@hF,J}8~$r8!:w(B
;;	<neighbor-context> :- (<neighborhood> . <regexp>)
;;			<neighborhood>$BJ}8~$K(B <regexp> $B$,8+IU$+$l$P??(B
;;			neighbor $B$H$O6uGrJ8;z$N$_$G6h@Z$i$l$?HO0OFb$r$$$&(B
;;	<neighborhood> := before | !before | after | !after
;;		       'before $B$O@hF,J}8~!"(B'after $B$OKvHxJ}8~(B
;;	<parenscan-context> := (<parenmove> <number> <regexp>))
;;	<parenmove> := paren | !paren | upparen | !upparen
;;		       'paren $B$O3g8L(B <number> $B8DJ,0\F0$7$?@h$,(B<regexp>$B$K%^%C%A(B
;;		       $B$9$l$P??!"(B!paren $B$O%^%C%A$7$J$1$l$P??(B
;;		       'upparen $B$O3g8L(B <number> $B8DJ,>e0L$K0\F0$7$?@h(B
;;		       ($BIi?t$N>l9g$O%P%C%U%!@hF,J}8~$KH4$1$k(B)$B$,(B
;;		       <regexp>$B$K%^%C%A$7$?$i??!"(B!upparen$B$O%^%C%A$7$J$1$l$P??(B
;;	
;;	<exclude-list> := (exclude <number> <regexp>)
;;		      <search-regexp> $B$K$h$C$F%^%C%A$7$?%0%k!<%WHV9f(B 
;;		      <number> $B$NJ8;zNs$,(B <regexp> $B$K%^%C%A$7$J$1$l$P??(B
;;	<require-list> := (require <number> <regexp>)
;;		      <search-regexp> $B$K$h$C$F%^%C%A$7$?%0%k!<%WHV9f(B 
;;		      <number> $B$NJ8;zNs$,(B <regexp> $B$K%^%C%A$9$l$P??(B
;;	<recursive-exp> := (context . $B%Q%?!<%s%j%9%H(B)
;;			$B!V%Q%?!<%s%j%9%H!W$rMQ$$$FF1MM$NJ8L.3NG'@55,I=8=(B
;;			$B8!:w$r:F5"E*$K9T$J$&!#(B
;;	<or-exps> := (or . (<patlist-elem>*))
;;		  <patlist-elem> $B$N3F%j%9%H$N$I$l$+$,(B non-nil $B$rJV$;$P??(B
;;	<return-exp> := (return) | (return . <return-exp>)
;;		  $BA4$F$NJ8L.8!::$,??$N$H$-!":G=*E*$J%^%C%A$N7k2L$r(B
;;		  $B=i4|8!:w$H$O0c$&0LCV$K$9$k>l9g$KMxMQ$9$k!#=i4|8!:w$N@.(B
;;		  $B8y$7$?D>8e$N0LCV$+$iFs<!8!:w$,9T$o$l$k(B($BI,MW$J$i(B)$B!#(B
;;		  'return $B$N$_$N>l9g$O!"$=$l0JA0$N(B <context-cons> $B$G9T$o(B
;;		  $B$l$?8!:w$N%^%C%A>pJs$,A4BN$N%^%C%A>pJs$H$7$F%9%H%"$5$l$k(B
;;	<return-exp> := (<direction> . <Regexp>)
;;	<direction> := forward | backward
;;		    $BFs<!8!:w$r9T$&J}8~$H@55,I=8=(B
;;	
;;[epo-$B8@8lL>(B-relation-alist $B$N@_DjNc(B]
;;

;;;
;; context-regexp-search related functions are defined in epolib.el
;;;

(require 'epo)
(defun epor*on-relation-p (&optional p)
  "If point is not on a pair of some relation return nil.
If on one of pairs, return the list of `relation-type', 'pair-name,
identifier and searching information cons of its partner and me respectively.
For example;
there is a relation `definition and reference'.  When point is on some
identifier's reference, this function returns a list as below.
  '(type (me . partner) identifier (CCRE-of-its-Definition . group#)
         (CCRE-of-myself . group#))
There is a relation `beginning and ending'.  When point is on some
ending, this function returns a list as below.
  '(begin-end (end . begin) text (CCRE-of-its-beginning . Num)
         (CCRE-of-myself . Num))
"
  (let ((list (epo*get-cur-lang-alist 'relation))
	elm type x rels relbak role pattern group partner ptgrp ptpat ptstruc
	ptrole i ccr idpat defpat defgrp refpat refgrp beglim endlim)
    (setplist 'epor*on-relation-p nil)
    (save-excursion
      (setq p (or p (point)))
      (goto-char p)
      (setq beglim (progn (forward-line -1) (point))
	    endlim (progn (forward-line 5) (point)))
      (catch 'on
	(while list
	  (setq elm	(cdr (car list))
		type	(cdr (assq 'type elm))
		idpat	(epo*get-value (cdr (assq 'idpattern elm)))
		rels	(epo*get-value (cdr (assq 'relation elm)))
		relbak	rels
		i	0)
	  (while rels
	    (goto-char beglim)
	    (setq x	(car rels)
		  role	(car x)
		  pattern (epo*get-value (cdr (assq 'pattern x)))
		  group	(or (cdr (assq 'group x)) 0)
		  partner (or (cdr (assq 'partner x))
			      (if (= (length relbak) 2)
				  (if (= i 0)
				      (nth 1 relbak)
				    (car relbak)))))
	    (if (symbolp partner)
		(setq partner (assq partner relbak)))
	    (setq ptpat (epo*get-value (cdr (assq 'pattern partner)))
		  ptgrp (cdr (assq 'group partner))
		  ptstruc (epo*alist-get 'structure partner)
		  ptrole (car partner))
	    (or (and role pattern
		     (or (memq type '(external processor file)) partner))
		(error "epo-%s-relation-alist: invalid format.  See epor.el."
		       epo:current-language))
	    (setq ccr (cons (epo*replace-format (car pattern) "i" idpat)
			    (cdr pattern)))
	    (while (and (> p (point))
			(epo*coco-re-search-forward ccr endlim t))
	      (if (and (>= p (match-beginning 0))
		       (<= p (match-end 0)))
		  (progn
		    (put 'epor*on-relation-p 'matched-relation x)
		    (put 'epor*on-relation-p 'matched-type (car list))
		    (throw 'on
			   (list type
				 (cons role ptrole)
				 (setq x (epo*match-string group))
				 (cond
				  ((not (memq type '(reference box)))
				   nil)
				  (t
				   (cons
				    (cons
				     (epo*replace-format
				      (car ptpat) "i"
				      (regexp-quote x))
				     (cdr ptpat))
				    ptgrp)))
				 (cons
				  (cons
				   (epo*replace-format
				    (car pattern) "i"
				    (regexp-quote x))
				   (cdr pattern))
				  group)
				 ptstruc)))))
	    (setq rels (cdr rels)
		  i (1+ i)))
	  (setq list (cdr list)))))))

(defun epor*goto-opposite-box-edge (edge-info-list)
  "Jump to opposite box edge.
EDGE-INFO-LIST consists of list.
 '((me . partner) identifier 
     (opposite-ccre . matchgroup) (current-pos-ccre . match-group))"
  (let*((edge (car (car edge-info-list)))
	(identifier (nth 1 edge-info-list))
	(ccre-opp (car (nth 2 edge-info-list)))
	(ccre-me (car (nth 3 edge-info-list)))
	(ccrelist (list ccre-opp ccre-me))
	(direction (if (eq edge 'begin) 'f 'b))
	(nest 0))
    (goto-char (if (eq direction 'f) (match-end 0) (match-beginning 0)))
    (while (and (>= nest 0)
		(epo*coco-re-search-list direction ccrelist nil t))
      (if (eq (get 'epo*coco-re-search-list 'matched-ccre) ccre-me)
	  (setq nest (1+ nest))
	(setq nest (1- nest))))
    (if (< nest 0)			;partner found 
	(goto-char (match-beginning 0))
      (message "Opposite box edge of %s not found" identifier))))

(defun epor*search-relation-current-buffer (ccre)
  (let ((p (point)))
    (if (or (epo*coco-re-search-backward ccre nil t)
	    (epo*coco-re-search-forward ccre nil t))
	(progn (epo*store-position-in-register p)
	       (goto-char (match-beginning 0))
	       (if (pos-visible-in-window-p (match-beginning 0))
		   nil
		 (epo*set-window-pretty-position (point) (- (point) p)))
	       t))))

(defun epor*search-relation-buffers (ccre)
  (let ((cb (current-buffer)) p
	(lang epo:current-language) (bl (epo*epo-buffer-list)))
    (unwind-protect
	(catch 'found
	  (while bl
	    (set-buffer (car bl))
	    (setq p (point))
	    (goto-char (point-min))
	    (if (epo*coco-re-search-forward ccre nil t)
		(progn
		  (set-buffer cb)
		  (epo*store-position-in-register)
		  (epo*goto-buffer (car bl))
		  (goto-char (match-beginning 0))
		  (if (pos-visible-in-window-p (point)) nil
		    (epo*set-window-pretty-position))
		  (throw 'found t)))
	    (goto-char p)
	    (setq bl (cdr bl))))
      (set-buffer cb))))

(defun epor*create-partner (epoi-structure keyword)
  "Memorize EPOI-STRUCTURE and KEYWORD for the next structure insertion."
  (set (make-local-variable 'epo-structure-yank-register)
       (list (list (cons 'structure epoi-structure)) keyword)))

(defvar epor-resolve-relation-cache nil
  "Internal cache for epor-resolve-relation")
(defun epor-resolve-relation (arg)
  "Resolve the current position's relation.
If on some relation, jump to its partner.  If universal-argument is given,
search all possible directories.  Default action is to search only for
project root and current directory."
  (interactive "P")
  (epo*check-mode)
  (let*((relation (epor*on-relation-p)) (p (point)) target
	(cb (current-buffer)) type keyword method structure m
	(alist (epo*get-cur-lang-alist 'file))
	(ext (epo*structure-info-get 'extension alist))
	(path (epo*get-value (epo*structure-info-get 'search alist)))
	(recursive (epo*structure-info-get 'recursive-search alist)))
    (if (null relation)
	(message "Not on relation pair.")
      (setq type (car relation)
	    keyword (nth 2 relation)
	    target (nth 3 relation)
	    structure (nth 5 relation))
      (cond
       ;;compare the car part of `target'.  cdr part may be used in the future.
       ((eq type 'box)
	(epor*goto-opposite-box-edge (cdr relation)))
       ((eq type 'file)
	(epo*find-file keyword (cons (epo*project-root) path) ext recursive)
	(if (not (eq cb (current-buffer)))
	    (set (make-local-variable 'epo:referer) (buffer-file-name cb))))
       ((eq type 'external)
	(epop*invoke-processor keyword))
       ((eq type 'function)
	(funcall function keyword))
       ;; Jump to...
       ;;(1)current buffer
       ((epor*search-relation-current-buffer (car target)))
       ;;(2)opened buffers, whose major-mode equals to current one
       ((epor*search-relation-buffers (car target)))
       ;;(3)all files in load-path directories
       ((epo*coco-re-search-all-files
	  keyword (car target)
	  (if arg (cons (epo*project-root) path)
	    (list "." (epo*project-root)))
	  ext (not recursive)))
       ;;Not found...
       ((and structure
	     (y-or-n-p
	      (format
	       "No partner found. Create %s now?"
	       (if (setq m (cdr (car (cdr relation))))
		   m ;	 (epo*hoge
		 ""))))
	(epor*create-partner structure keyword)
	(message "Type %s to insert %s template of %s"
		 (epo*function-key-description 'epo-yank-structure)
		 m keyword))
       (t
	(set-buffer cb)
	(message "Partner for %s: %s not found in any file."
		 (car relation)
		 keyword))))))

(provide 'epor)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
