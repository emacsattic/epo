;;; -*- Emacs-Lisp -*-
;;; EPO Sample Definition File for Language Dependent Information 
;;; (c)2001-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Mon Feb  4 13:12:14 2002 on firestorm

;;[Commentary]
;;	
;;	EPO$B$N8@8l0MB8@_Dj$N%5%s%W%k!#(B
;;	$B$3$l$r?7$7$$8@8l$N@_Dj8+K\$H$7$F;H$C$F2<$5$$!#(B
;;	$B$3$NNc$G$O?7$7$$8@8l$,(B "example" $B$H$$$&L>A0$G$"$k$H2>Dj$7$^$9!#(B
;;	
;; 
;; $B!Z%U%!%$%k$N:n@.![(B
;; 
;; $B?7$7$$8@8lMQ$N@_Dj$O!"(Bepo- $B$N8e$m$K8@8lL>$rIU$1$?L>A0$N%U%!%$%k$K3JG<(B
;; $B$7$^$9!#$D$^$j!"8@8l$,(B "example" $B$J$i%U%!%$%kL>$r(B epo-example.el $B$H(B
;; $B$7$^$9!#$h$j@53N$K$$$&$H!"$=$N8@8l$rJT=8$9$k%a%8%c!<%b!<%I$NL>A0$K0M(B
;; $BB8$7$^$9!#(B
;; 
;;;
;; ========== Variables for Input Aider(EPOI) ==========
;;;
;; $B!Z9=B$F~NO$NJd40%F%s%W%l!<%HMQ(Balist$B![(B
;; 
;; $B$"$k8@8lFCM-$N9=B$$rJd40F~NO$9$k>pJs$O(B Emacs-Lisp $BJQ?t(B
;; 
;;	epo-$B8@8lL>(B-structure-alist
;; 
;; $B$K@_Dj$7$^$9!#(B
;; alist$B$O(B ($BG$0U$N(B1$BJ8;z(B $BJd40%j%9%H(B) $B$,B3$/AH9g$;$G!"!VG$0U$N(B1$BJ8;z!W$O(B
;; $B$=$N9=B$$rJd40$9$k$H$-$NF~NO%-!<$H$7$FMxMQ$5$l$^$9!#I,$:0l0U$K$7$^$9!#(B
;; $B!VJd40%j%9%H!W$OMWAG$H$7$F(B 'type, 'structure, 'argsep, 'table,
;; 'mustmatch, 'arg-reader $B$r4^$`(Balist$B$G$9!#(B'argsep $B0J9_$O>JN,2DG=$G$9!#(B
;;
;; * type	$B9=B$$N<oJL$r<($9%7%s%\%k!#%a%K%e!<$N0lMw$NJ8;z$K;H$o$l$^$9!#(B
;;		$B0l0U$KDj$^$kG$0U$NL>A0$r%7%s%\%k$G;XDj$7$F2<$5$$!#(B
;; * structure	$B9=B$$rF~NO$9$k$H$-$NN.$l$r;XDj$9$k%j%9%H$r;XDj$7$^$9!#8G(B
;;		$BDjE*$JJ8;zNs$r%P%C%U%!$KF~NO$9$k$H$-$OJ8;zNs$r$=$N$^$^=q(B
;;		$B$-$^$9!#J8;zNs0J30$K0J2<$N%7%s%\%k$,;H$($^$9!#(B
;;		- 'keyword	$B9=J8$N80$H$J$kC18l$,Mh$k0LCV$r;XDj$7$^$9!#(B
;;		  $B$3$NItJ,$NF~NO$KBP$7$F$OJd40F~NO$,E,MQ$5$l$^$9!#(B
;;		- 'argument 	$B9=J8$KM?$($k0z?t$,Mh$k0LCV$r;XDj$7$^$9!#(B
;;		  $B$3$NItJ,$NF~NO$KBP$7$F$ODL>o$NJ8;zNsF~NO$,E,MQ$5$l$^$9!#(B
;;		  $B9=B$Jd40F~NO;~$K2?$bF~NO$7$J$+$C$?>l9g$O$3$N0LCV$K%+!<(B
;;		  $B%=%k$,Mh$^$9!#(B
;;		- 'cursor	$B9=B$F~NO$,=*$o$C$?$H$-$K%+!<%=%k$rCV$/0LCV(B
;;		  $B$r;XDj$7$^$9!#(B'argument$B$K2?$bF~$l$J$+$C$?>l9g$O$=$N0L(B
;;		  $BCV$K$J$k>l9g$,$"$j$^$9!#(B
;;		- 'indent	$B$=$3$^$G$NF~NO$,=*$o$C$?;~E@$G$=$N9T$N%$(B
;;		  $B%s%G%s%H$r;\$7$^$9!#$?$@$7!"%a%8%c!<%b!<%I$,%$%s%G%s%H(B
;;		  $B5!G=$rDs6!$7$F$$$J$$>l9g$O4|BT$7$?DL$j$KF0$-$^$;$s!#(B
;; * argsep	$B$A$g$C$HFq$7$$$G$9$h(B:-)
;;		$B$=$N9=B$$KM?$($k0z?t$r6h@Z$kJ8;zNs$r!"D9$5(B6$B$N%j%9%H$G;X(B
;;		$BDj$7$^$9!#(B
;;		1. $BJB$S3+;O5-9f(B
;;		   $B0z?tJB$S$,B8:_$9$k$H$-JB$SA4BN$N@hF,(B($B:8B&(B)$B$KMh$kJ8;zNs(B
;;		   $B0z?t$,$J$$$H$-$K$O>JN,$5$l$k$h$&$J3+;O5-9f$J$N$G!"(BC$B8@(B
;;		   $B8l$N4X?t0z?t$r$/$/$k3g8L$O$3$l$K$OAjEv$7$^$;$s!#(B
;;		2. $BMWAG3+;O5-9f(B
;;		   $B0l$D$N0z?t$NCM$N:8B&$KMh$kJ8;zNs(B
;;		3. $BMWAGBeF~5-9f(B
;;		   $B0z?tEO$7$,CMBeF~J}<0(B($BO"A[G[NsJ}<0(B)$B$G$"$k>l9g$K!"BeF~(B
;;		   $B$r0UL#$9$kJ8;zNs!#(BHTML$B$N3F%(%l%a%s%H$NB0@-$N>l9g$O(B
;;		   "=" $B$G$"$j!"(BPerl$B$NO"A[G[Ns$N>l9g$O(B "=>" $B$H$J$j$^$9!#(B
;;		   $BCMBeF~J}<0$G$O$J$/!"0lHLE*$J=g=x0MB8EO$7$G$"$k>l9g(B
;;		   $B$O!"I,$:(B nil $B$r;XDj$7$^$9(B(""$B$G$O$@$a(B)$B!#(B
;;		4. $BMWAG6h@Z$j5-9f(B
;;		   $B0z?t$,J#?t$"$k$H$-$K!"$=$l$i$r6h@Z$kJ8;zNs$r;XDj$7$^(B
;;		   $B$9!#(BC$B8@8l7O$N>l9g$O(B "," $B$G$9!#(BLisp$B7O$J$i(B " " $B$G$7$g$&!#(B
;;		5. $BMWAG=*C<5-9f(B
;;		   $B0l$D$N0z?t$NCM$N1&B&$KMh$kJ8;zNs(B
;;		6. $BJB$S=*C<5-9f(B
;;		   $B0z?tJB$S$,B8:_$9$k$H$-JB$SA4BN$NKvHx(B($B1&B&(B)$B$KMh$kJ8;zNs(B
;;	
;;		$BBeI=E*$J8@8l$G$O0J2<$N$h$&$K$J$j$^$9!#(B
;;	
;;		- C$B$N4X?t;2>H(B		"" "" nil ", " "" ""
;;		- HTML$B$NB0@-;XDj(B	" " "\"" "=" " " "\"" ""
;;		- Perl$B$NO"A[EO$7(B	"{" "" "=>" "," "" "}"
;;	
;;		$B$?$@!"$d$O$j(B argsep $B$r5-=R$9$k$N$OFq$7$$$N$G!"4JC1$J%7%s(B
;;		$B%\%k$GE57?E*$J8@8l$N0z?t6h@Z$j>pJs$rI=$;$k$h$&$K$7$^$7$?!#(B
;;	
;;		(argsep . c-function)	;;C$B$N4X?t8F$S=P$7$N3g8LFbJ}<0(B
;;		(argsep . latex)	;;LaTeX$B$N(B\begin$BD>8eM?$($k0z?tJ}<0(B
;;		(argsep . c-function)	;;C$B$N4X?t8F$S=P$7$N3g8LFbJ}<0(B
;;		(argsep . sgml)		;;SGML(HTML$B$J$I(B)$B$NB0@-EO$75-K!(B
;;		(argsep . lisp)		;;Lisp$B$N4X?t8F$S=P$7$NJ}<0(B
;;		(argsep . perl-hash)	;;Perl5$B$NO"A[EO$77?<0(B
;;	
;; * table	$B9=B$$N(B 'keyword $B$N8uJd$H$J$kC18l$r(B alist $B7A<0$G;XDj$7$^$9!#(B
;;		alist$B$N(Bcdr$BIt$KJ8;zNs$r;XDj$7$?>l9g$O!"0z?t$NF~NO;~$K$=$N(B
;;		$BJ8;zNs$,%W%m%s%W%H$H$7$FMxMQ$5$l$^$9!#(B
;;		alist$B$=$N$b$N$G$J$/!"%7%s%\%k$rM?$($k$H$=$N%7%s%\%k$,;}(B
;;		$B$DCM$rMxMQ$7$^$9!#8uJd$,B??t$K$J$k>l9g$K$OJL$NJQ?t$KDj5A(B
;;		$B$9$kJ}$,NI$$$G$7$g$&!#(B
;;	
;; * mustmatch	$B9=B$$N(B 'keyword $B$N8uJd$H$J$kC18l$rJd40F~NO$9$k>l9g$K!"8u(B
;;		$BJd$KB8:_$9$k$b$N$7$+F~NO$r<u$1IU$1$J$$>l9g$K(B t $B$K%;%C%H(B
;;		$B$7$^$9!#(B
;; * arg-reader	$B0z?t$rF~NO$9$k$H$-$KFCJL$KMxMQ$7$?$$4X?t$,$"$l$P$=$l$r;X(B
;;		$BDj$7$^$9!#$=$N4X?t$O0z?t$rFs$D<h$k$3$H$,MW5a$5$l$^$9!#Bh(B
;;		1$B0z?t$O$=$N$H$-$KF~NO$5$l$?%-!<%o!<%I!"Bh(B2$B0z?t$O2?HVL\$N(B
;;		$B0z?tF~NO$+$r<($9@0?t$,EO$5$l$^$9!#(B
;;		epo-tex.html $B$K$O!"(BEPO$B$N9=B$F~NO$GLnD;$N%"%I%$%s4X?t$r8F(B
;;		$B$V$?$a$N@_DjNc$,$"$j$^$9!#(B
;;		($B$3$@$o$k?M8~$1(B)
;; 
;; $B0J2<$,<B:]$NDj5ANc$G$9!#(B

(defvar epo-example-structure-alist
  '((?b (type . block)
	(structure keyword " (" argument ") {\n" indent cursor "\n" "}" indent)
	(table ("if" "condition") ("for") ("while") ("switch"))
	;;(arg-reader . epo-example-block)
	(mustmatch . t))
    (?f (type . function)
	(structure keyword "(" argument ");")
	(argsep "" "" nil ", " "" ""))))

;; ?b $B$H(B ?f $B$J$N$G!"9=B$F~NOJd40%-!<(B([prefix] C-s)$B$r2!$9$H8+$K%P%C%U%!$K(B
;; $B0J2<$N$h$&$K8=$l$^$9!#(B
;; 
;;	Competion : (b)block (f)function
;; 
;; $B$3$3$G(Bb$B$r2!$9$H(Bblock$B9=B$$,!"(Bf$B$r2!$9$H(Bfunction$B9=B$$,A*Br$5$l$^$9!#(B
;; block$B9=B$$N(B 'structure $B$O%P%C%U%!$KA^F~$5$l$?%$%a!<%8$KD>$9$H0J2<$N$h(B
;; $B$&$K$J$j$^$9!#(B
;; 
;;	keyword (argument) {
;;	__$B""(B
;;	} ($B",$3$3$K%+!<%=%k(B)
;; 
;; $B$^$?!"(B'arg-reader $B$H$7$F(B 'epo-example-block $B4X?t$,;XDj$7$?>l9g$O(B
;; $B0z?t$NF~NO;~$K$O0J2<$N$h$&$K:n@.$5$l$?4X?t$,8F$P$l$k$3$H$K$J$j$^$9!#(B
;;
;; (defun epo-example-block (keyword argp)
;;   (cond
;;    ((= argp 1) (read-string (format "%s, condition: " keyword)))))
;; 
;; $BFs$D$N0z?t$NMxMQJ}K!$KCm0U$7$F2<$5$$!#(B
;; 

;; $B!ZH?I|;R<+F0H=JLF~NO$N%F%s%W%l!<%H(Balist$B![(B
;; 
;; $B$"$k8@8lFCM-$NH?I|;R(B($B7+$jJV$7MWAG(B)$B$rJd40F~NO$9$k>pJs$O(B Emacs-Lisp $BJQ?t(B
;; 
;;	epo-$B8@8lL>(B-iteration-alist
;; 
;; $B$K@_Dj$7$^$9!#(B
;; alist$B$O(B ($BG$0U$N(B1$BJ8;z(B $BJd40%j%9%H(B) $B$,B3$/AH9g$;$G!"@hF,$NJ8;z$O(B
;; $BI,$:0l0U$K$7$^$9!#(B
;; 
;; $B!VJd40%j%9%H!W$OMWAG$H$7$F(B 'type, 'opener, 'closer, 'iterator$B$r4^$`(B
;; alist$B$G$9!#$3$l$O(BLaTeX$B$H(BHTML$B0J30$G$O$"$^$jI,MW@-$,L5$$$N$G!"$3$3$G$O(B
;; LaTeX$BMQ$NNc$N$_Ds<($7!"4JC1$J@bL@$r2C$($k$K$H$I$a$^$9!#(B

(defvar epo-example-iteration-alist
  '((?i (type . itemize)
	(opener . ((pattern "\\\\begin{itemize}" (!before . comment-start))))
	(closer . ((pattern "\\\\end{itemize}" (!before . comment-start))))
	(iterator . ("\\item ")))
    (?e (type . enumerate)
	(opener . ((pattern "\\\\begin{enumerate}"
			    (!before . comment-start))))
	(closer . ((pattern "\\\\end{enumerate}" (!before . comment-start))))
	(iterator . ("\\item ")))
    (?d (type . description)
	(opener . ((pattern "\\\\begin{description}"
			    (!before . comment-start))))
	(closer . ((pattern "\\\\end{description}"
		   (!before . comment-start))))
	(iterator . (("\\item[" cursor "]"))))
    (?t (type . tabular)
	(opener . ((pattern "\\\\begin{tabular}" (!before . comment-start))))
	(closer . ((pattern "\\\\end{tabular}" (!before . comment-start))))
	(iterator . epoi-tex-enviroment-iterators))))

;; $B3FJd40%j%9%H$NMWAG$K$O0J2<$NFbMF$rDj5A$7$^$9!#(B
;; 
;; * type	$B$=$N7+$jJV$7$,5/$-$k4D6-(B($BJq3g;R$H8F$V$3$H$K$7$^$9(B)$B$N<oJL(B
;;		$B$rI=$9%7%s%\%k!#(B
;; * opener, closer	$BJq3g;R$N3+;O(B(opener)$B$^$?$O=*N;(B(closer)$B5-9f$H$J$k(B
;;   		$B9=B$$K4X$9$k>pJs$rM?$($^$9!#M?$($k>pJs$b$d$O$j(Balist$B7A<0(B
;;		$B$H$J$j!"FbIt$K4^$a$kMWAG$K$O0J2<$N$b$N$,$"$j$^$9!#(B
;; 
;;		- pattern	$B%Q%?!<%s$N!VJ8L.3NG'@55,I=8=!W$r;XDj$7$^(B
;;		  $B$9!#J8L.3NG'@55,I=8=$H$O!"@55,I=8=$G%^%C%A$7$?$b$N$,0L(B
;;		  $BCV$9$kJ8L.$r$5$i$KD4$Y$k$3$H$G!"$h$j@53N$J9=B$$r8!:w$G(B
;;		  $B$-$k$h$&$K3HD%$7$?$b$N$G$9!#$?$H$($P!"IaDL$N@55,I=8=$G(B
;;		  $B$O3g8L$NBP1~$r<h$k$N$O:$Fq$G$9$,!"J8L.3NG'@55,I=8=$G$O(B
;;		  $B$=$l$r5-=R$9$k$3$H$rMF0W$K$7$^$9!#(B
;;		- $B$=$NB>(B
;;		  $B:#$N$H$3$m(B opener, closer $B$KM?$($kMWAG$O(B pattern $B$@$1(B
;;		  $B$G$9(B($B3HD%M=Dj(B)$B!#(B
;; * iterator	$B$=$NJq3g;R$K0O$^$l$?H"$NFbIt$G!"7+$jJV$9$Y$-F~NO9=B$$rM?(B
;;		$B$($^$9!#$3$N7A<0$O!"@h=R$N(B structure-alist $B$N(B 'structure
;;		$BMWAG$HF1$8$b$N$G$9!#$^$?!"MWAG$H$7$F4X?tL>$r;XDj$9$k$HH?(B
;;		$BI|;R$NF~NO$K$O$=$N4X?t$r8F$V$h$&$K$J$j$^$9!#4X?t$O0l$D$N(B
;;		$B0z?t$r$H$kI,MW$,$"$j!"Jq3g;R$N3+;O$r0UL#$9$kJ8;zNs$,EO$5(B
;;		$B$l$^$9!#$?$H$($P!"(BLaTeX$B$N(Bitemze$B4D6-$G$3$NH?I|;RA^F~$,H/(B
;;		$BF0$7$?>l9g$O!"(B"\begin{itemize}" $B$H$$$&J8;zNs$,EO$j$^$9!#(B
;; 
;; $B$3$NJQ?t$r@5$7$/@_Dj$9$k$H(B [prefix] C-i $B$K$h$j!"$=$N>l$K$U$5$o$7$$H?(B
;; $BI|;R$,A^F~$5$l$^$9!#(B[prefix] RET $B$H$7$?>l9g$O!"2~9T$7$F$+$iH?I|;R$rA^(B
;; $BF~$7$^$9!#(B


;;;
;; ========== Variables for Relation Resolver(EPOR) ==========
;;;

;;;
;; $B!Z4X78DI@W$N>pJsDI@W(Balist$B![(B
;; 
;; $B$"$k8@8lFCM-$N4X78(B($B<1JL;R$N;2>H$HDj5A!"%U%!%$%k%$%s%/%k!<%I(B)$B$r;J$k>p(B
;; $BJs$O(B Emacs-Lisp $BJQ?t(B
;; 
;;	epo-$B8@8lL>(B-relation-alist
;; 
;; $B$K@_Dj$7$^$9!#(B
;; alist$B$O(B ($BG$0U$N(B1$BJ8;z(B $B4X78>pJs%j%9%H(B) $B$,B3$/AH9g$;$G!"@hF,$NJ8;z$O(B
;; $BI,$:0l0U$K$7$^$9!#(B
;; 
;; $B!V4X78>pJs%j%9%H!W$OMWAG$H$7$F(B 'type, 'idpattern, 'relation, table $B$r(B
;; $B4^$`(Balist$B$G$9!#$3$l$O$"$i$f$k8@8l$GHs>o$KM-8z$K5!G=$9$k$N$G$-$A$s$H@_(B
;; $BDj$7$^$7$g$&!#(B
;; $B<BNc$r@h$K<($7$^$9$N$G!"$3$l$H>H$i$79g$o$;$J$,$i3FMWAG$N0UL#$rM}2r$7(B
;; $B$F2<$5$$!#(B

(defvar epo-example-relation-alist
  '((?f (type . reference)
	(idpattern . "[A-Za-z_][0-9A-Za-z_]*")
	(relation
	 (definition
	   (pattern . ("\\<%i\\>" (after . "(") (paren 1 "{")))
	   (group . 0))
	 (reference
	  (pattern . ("\\<%i\\>" (after . "(") (!paren 1 "{")))
	  (group . 0)))
	(table . epo-example-libc-functions))
    ))
;; 
;; * type	$B!V4X78!W$N<oJL$r;XDj$7$^$9!#<oJL$O0J2<$N$&$A$N$I$l$+$G$9!#(B
;; 
;;		- reference	$B0l$D$N<1JL;R$N;2>H!&Ho;2>H4X78$G$9!#$?$H(B
;;		  $B$($P!"%W%m%0%i%_%s%08@8l$K$*$1$k!"JQ?t!&4X?t$NDj5A$H;2(B
;;		  $B>H$,$b$D$*8_$$$N4X78$O(B reference $B$G$9!#2?$+0l$D$N0l0U(B
;;		  $B$KDj$^$kC18l$rCg2p$H$7$F!"%=!<%9$N!"$I$3$+$N>l=j$HJL$N(B
;;		  $B>l=j$,7k$S$D$1$i$l$k$H$-!"$=$l$O(B reference $B$G$9!#(BLaTeX
;;		  $B$G$$$($P(B \label{foo} $B$G$"$k>l=j$K(B foo $B$H$$$&L>A0$rIU$1!"(B
;;		  $BJL$N>l=j$G(B \ref{foo} $B$K$h$C$F$=$N>l=j$r<($9$3$H$,=PMh(B
;;		  $B$^$9$,!"$3$N4X78$b(B reference $B$G$9!#(B
;; 
;;		- box		$B%=!<%9$N$"$kFs$D$N2U=j$G64$^$l$?NN0h$K(B
;;		  $B!VH"!W$r7A@.$9$kFs$D$N%Z%"$O!"(Bbox $B$H$$$&4X78$G$9!#(B
;;		  LaTeX$B$G$$$($P(B \begin{} $B$H(B \end{} $B$,$*8_$$$K;}$D4X78$,(B
;;		  box$B$G$9!#(B
;; 
;;		- file		$B%=!<%9Cf$GJL$N%U%!%$%k$r;X$7<($95-K!$,$"(B
;;		  $B$k>l9g!"$=$N2U=j$H!";X$7<($5$l$?%U%!%$%k$N4X78$O(B file
;;		  $B$G$9!#(B
;;		
;;		- external	$B%=!<%9$N30It$KB8:_$9$k$b$N(B($B%W%m%;%9$d(BURL
;;		  $B$J$I(B)$B$r;X$7<($94X78$,(B external $B$G$9!#%=!<%9$KB8:_$7$J(B
;;		  $B$$$b$N$G!"$J$*$+$DD>@\JT=8$G$-$J$$$b$N$,<g$KEv$F$O$^$j(B
;;		  $B$^$9!#$3$l$r;XDj$9$k>l9g$O(B Emacs-Lisp $BJQ?t(B
;;		  epo-file-processor-alist $B$KBP>]Aj<j(B($B%U%!%$%kL>$J$I(B)$B$N(B
;;		  $B%Q%?!<%s$H!"$=$l$r=hM}$9$k$?$a$K5/F0$9$k%3%^%s%I$N(B
;;		  alist $B$r;XDj$7$F$*$-$^$9!#$3$NJQ?t$N;XDj$K$D$$$F$O!"(B
;;		  epop.el $B$r;2>H$7$F2<$5$$!#(B
;; 
;; * idpattern	$B4X78$,(B reference $B$G$"$k$H$-!"$=$l$i$r7k$S$D$1$k<1JL;R$K(B
;;		$B5v$5$l$?%Q%?!<%s$r(B($BIaDL$N(B)$B@55,I=8=$G;XDj$7$^$9!#(B
;;		$B>e5-$NNc$N(B
;;			(idpattern . "[A-Za-z_][0-9A-Za-z_]*")
;;		$B$O!"!V%"%k%U%!%Y%C%H$+(B _ $B$G$O$8$^$j!"FsJ8;zL\0J9_$O?t;z(B
;;		$B$G$bNI$$(B($B0lJ8;z$G$b2D(B)$B!W$H$$$&$N$,<1JL;RL?L>5,B'$K$"$k>l(B
;;		$B9g$NDj5ANc$G$9!#(B
;; 
;; * relation	$B!V4X78!W$K$h$C$F7k$S$D$1$i$l$k$b$N$O!"Fs$D$N$b$N$G$9!#$=(B
;;		$B$NFs$D$K4X$9$k>pJs$r@_Dj$7$^$9!#:GDc$G$bFs$D$NMWAG$r(B
;;		alist$B$G;XDj$7$^$9!#$3$NMWAG$O(B
;; 
;;		  ($B4X78$NLrL>%7%s%\%k(B $B4X78>pJs(Balist)
;; 
;;		$B$H$$$&7A<0$G!"!V4X78$NLrL>%7%s%\%k!W$OG$0U$N%7%s%\%k$G$9!#(B
;;		$BB>$@$7!"$=$N4X78$K!VDj5A!W$H!V;2>H!W$H$$$&0UL#Aj2l8_$$(B
;;		$B$KB8:_$9$k>l9g$O!"Dj5A$9$kLrL>$K(B def$B!";2>H$9$kLrL>$K(B ref
;;		$B$r4^$a$F$*$$$F2<$5$$!#!V4X78>pJs(Balist$B!W$K$O0J2<$NMWAG$r(B
;;		$B;XDj$7$^$9!#(B
;; 
;;		- pattern	$B$=$NLr3d$r$3$J$99=J8$NJ8L.3NG'@55,(B
;;		$BI=8=(B($B0J8e(BCCRE$B$HI=5-(B)$B$r;XDj$7$^$9!#(BCCRE$B$N@hF,$N@55,I=8=J8(B
;;		$B;zNsCf$G$O(B %i $B$H$$$&I=5-$,;H$($^$9!#(B%i $B$O(B idpattern $B$KCV(B
;;		$B49$5$l$^$9!#$^$?!"(B%i $B$O$+$J$i$:(B \\( \\) $B$G%0%k!<%T%s%0$7(B
;;		$B$F2<$5$$(B($B8!:w8e$K<1JL;R$r<h$j=P$9$?$a$KI,MW$G$9(B)$B!#(B
;; 
;;		- group		%i $B$r%0%k!<%T%s%0$7$?(B \\(\\) $B$NHV9f$r@0(B
;;		$B?t$G;XDj$7$^$9!#(B
;; 
;;		- structure	$B$=$NLr3d$r$3$J$99=J8$,H/8+$G$-$J$+$C$?>l(B
;;		$B9g$K!"$=$l$r<+F0E*$K@8@.$9$k$?$a$N9=B$$rM?$($^$9!#$3$N9=(B
;;		$BB$$O!"(Bstructure-alist $B$N(B structure $B$HF1$87A<0$G$9!#$3$N(B
;;		$B$H$-!"(Bkeyword $B$NItJ,$K$O!"(Breference $B$N85$H$J$C$?<1JL;R$,(B
;;		$BBeF~$5$l$^$9!#$?$H$($P!"(BEmacs-Lisp$B$G2?$+$N4X?t$r5-=R$7$F(B
;;		$B$$$F!"$=$NESCf!V$3$3$O4X?t$K$7$h$&!W$H;W$&2U=j$,$"$C$?$i!"(B
;;		$B$=$N>l$K4X?t$N;2>H$r=q$$$F$7$^$$$^$7$g$&!#$=$3$G(BEPO$B$N4X(B
;;		$B782r7h5!G=$r8F$S=P$9$H!"4X?tDj5A$N%F%s%W%l!<%H$r:n@.$7$^(B
;;		$B$9!#$3$NNc$O!"(Bepo-elisp.el $B$N(B epo-elisp-relation-alist
;;		$B$K$"$k$N$G;29M$K$7$F2<$5$$!#(B
;; 
;; * table	$B4X78$,(B reference $B$N>l9g!"<1JL;R$OF~NOESCf$G(BEPO$B$,Jd40$7$^(B
;;		$B$9!#$=$N>l9g$NJd408uJd%F!<%V%k$rFCJL$K;}$A$?$$$H$-$K;XDj(B
;;		$B$7$^$9!#%G%U%)%k%H$G$O!":n@.$7$F$$$k8@8l$NJ#?t$N%=!<%9%U%!(B
;;		$B%$%k$+$i<1JL;R$NDj5AItJ,$rA4$FC5$7$F!"$=$l$r8uJd$K$7$^$9!#(B
;;		$B$3$N>l9g%f!<%6$,Dj5A$7$?<1JL;R(B($B4X?t(B)$B$7$+Jd40$G$-$J$$$3$H(B
;;		$B$K$J$j$^$9$,!"$3$l0J30$K$=$N8@8l=hM}7O$,%G%U%)%k%H$G;}$D(B
;;		$B4X?t$J$I$rJd408uJd$K4^$a$?$$$H$-$K$3$l$r;XDj$7$^$9!#(B
;;		epo-c.el $B$K$O!"(Bman$B%Z!<%8$N(B3$B>O$K$"$kL>A0$NA4$F$r(BC$B8@8l$N(B
;;		$B<1JL;R$N%G%U%)%k%HJd408uJd$K4^$a$kNc$,$"$k$N$G;29M$K$7$F(B
;;		$B2<$5$$!#(B
;; 
;; $B$3$N@_Dj$K$h$j(B [prefix] C-r $B$r2!$9$H!"$=$N8@8l8GM-$N!V4X78!W$r(BEPO$B$,DI(B
;; $B@W$7$^$9!#4X78$,(B reference $B$N>l9g$OAj<j$N>l=j$K%]%$%s%H$r0\F0$7!"(Bfile
;; $B$N>l9g$O$=$N%U%!%$%k$K0\F0$7!"(Bexternal $B$N>l9g$OBP1~%W%m%;%9$r5/F0$7$^(B
;; $B$9!#(B
;; 
;; $B$^$?!"J8=qF~NOCf$K$I$3$+JL2U=j$GDj5A$5$l$F$$$k<1JL;R$rF~NO$9$k$H$-$K!"(B
;; $B$=$N<1JL;R(B($B4X?t$dJQ?tL>!"%i%Y%kL>$J$I(B)$B$N%$%K%7%c%k$@$1$rF~NO$7$?>uBV(B
;; $B$G(B [prefix] C-@ $B$r2!$9$H!"$=$N4X78$NDj5A$rB?$/$N%U%!%$%k$+$iC5$7=P$7(B
;; $B$F!"$=$N<1JL;RL>$rJd40$7$^$9!#(B
;; 
;; $B0l$DCm0U$,I,MW$J$N$O!"(B[prefix] C-@ $B$N%+!<%=%k0LCVJd40$G$O!"$3$N5!G=$r(B
;; $B8F$S=P$9>uBV$G!"$=$N0LCV$,!V4X78!W$NJRJ}$G$"$k$3$H$,7hDj$9$k7A<0$K$J$C(B
;; $B$F$$$J$1$l$P$J$j$^$;$s!#$?$H$($P!"(BC$B8@8l$NJQ?t$N;2>H$O(B
;; 
;;	$B<1JL;R$N%Q%?!<%s(B
;; 
;; $B$,%P%C%U%!Cf$KB8:_$9$k$3$H$GH=Dj$G$-$^$9$,!"4X?t$N>l9g$O(B
;; 
;;	$B<1JL;R$N%Q%?!<%s(B($B!D(B)
;; 
;; $B$N$h$&$KD>8e$K4]3g8L$NAH$,$"$k$3$H$r4|BT$7$F$$$^$9!#$7$+$7IaDL4X?tL>(B
;; $B$rF~NO$9$kCJ3,$G$O$^$@4]3g8L$rF~NO$9$kA0$J$N$G!"(B[prefix] C-@ $B$rESCf$G(B
;; $B2!$7$F$b!"$=$l$OJQ?t;2>H$N0LCV$@$H;W$o$l$F$7$^$$$^$9!#$3$l$O(BC$B7ONs$N8@(B
;; $B8lFCM-$NLdBj$G$"$k$?$a!"FCJL$K(B [prefix] C-f $B$rMQ0U$7$F$$$^$9!#$3$A$i(B
;; $B$N%-!<$O!"<+F0E*$KD>8e$K(B () $B$rJd$C$F$+$i(B [prefix] C-@ $B$r8F$S=P$7$^$9!#(B
;; $B$D$^$j!"(BC$B7ONs8@8l$N4X?t8F$S=P$7$rJd40$9$k$?$a$NC1$J$kJXMx%-!<$G$9!#(B
;; 
;; $B$^$?!"!V4X78!W$H$7$F(B file, box $B$N$$$:$l$+$K%+!<%=%k$r9g$o$;$F(B
;; [prefix] C-c $B$r2!$9$H!"$=$l$i$NBP1~$r<h$C$F$$$kL>A0$rJQ99$7$^$9!#$?$H(B
;; $B$($P!"(B\include{hoge} $B$G(B hoge $B%U%!%$%k$r4X78$E$1$F$$$k>l9g!"(Bhoge$B$N0LCV(B
;; $B$G(B [prefix] C-c $B$r2!$9$H$=$N0LCV$N(B hoge $B$H$$$&I=5-$H!"$=$l$K$h$C$F;X(B
;; $B$7<($5$l$k%U%!%$%k(B hoge $B$NL>A0$rF1;~$KJQ99$7$^$9!#(B

;;;
;; ========== Variables for Process Handler(EPOP) ==========
;;;
;;;
;; $B!Z8@8l=hM}7O$N5/F0>pJs$N(Balist$B![(B
;; 
;; $B$"$k8@8l$G=q$$$?%=!<%9$r<B:]$KL\E*J*$KJQ49$7$?$j!"6cL#$7$?$j$9$k$?$a(B
;; $B$K5/F0$9$k30It%W%m%;%9$N>pJs$O(B Emacs-Lisp $BJQ?t(B
;; 
;;	epo-$B8@8lL>(B-process-alist
;; 
;; $B$K@_Dj$7$^$9!#(B
;; alist$B$O(B ($BG$0U$N(B1$BJ8;z(B $B%W%m%;%9>pJs%j%9%H(B) $B$,B3$/AH9g$;$G!"@hF,$NJ8;z$O(B
;; $B%W%m%;%95/F0%a%K%e!<$G$I$l$r5/F0$9$k$+$NF~NO%-!<$H$7$FMxMQ$5$l$^$9!#(B
;; 
;; $B!V%W%m%;%9>pJs%j%9%H!W$OMWAG$H$7$F(B 'type, 'command, 'prompt, 'builtin 
;; $B$r4^$`(Balist$B$G$9!#(Bprompt, builtin $B$O>JN,2DG=$G$9!#(B
;; 
;; * type	$B$=$N30It%W%m%;%9$,9T$&=hM}$N<oJL$r<($9%7%s%\%k$r;XDj$7$^(B
;;		$B$9!#:#$N$H$3$mG$0U$N%7%s%\%k$,;XDj$G$-$^$9$,!">-Mh$3$l$K(B
;;		$B0UL#$r;}$?$;$k$+$b$7$l$^$;$s!#$H$j$"$($:$O(B compile, run,
;;		preview $B$J$I$K$7$F$*$$$F2<$5$$!#(B
;; * command	$B$=$N30It%W%m%;%9$r5/F0$9$k%3%^%s%I%i%$%s>pJs$r;XDj$7$^$9!#(B
;;		$B%3%^%s%I%i%$%s>pJs$O%j%9%H$G9=@.$7$^$9!#@hF,MWAG$O(B
;;		
;; 
;;		1.	$B%3%^%s%I$NF/$-$r<($9@bL@J8;zNs$r(B($B$J$k$Y$/C;$/(B)$B;XDj(B
;;		2.	$B$=$N=hM}7O$N%3%^%s%IL>$r;XDj$7$^$9(B
;;		3$B0J9_(B	$B%3%^%s%I$KM?$($k0z?t$r;XDj$7$^$9!#(B
;;			$B0z?t$K$O<!$N%7%s%\%k$,;H$($^$9!#(B
;;			- filename	$B%U%!%$%kL>$KCV49$5$l$^$9!#(B
;;			- basename	$B%U%!%$%kL>$N$&$A%G%#%l%/%H%jL>$r(B
;;					$B<h$j=|$$$?L>A0$KCV49$5$l$^$9!#(B
;;			- rootname	basename$B$+$i$5$i$K3HD%;R$r<h$j=|(B
;;					$B$$$?L>A0$KCV49$5$l$^$9!#(B
;;			- dirname	$B%U%!%$%k$,$"$k%G%#%l%/%H%jL>(B
;;			- text		$B%F%-%9%H$KCV49$5$l$^$9(B
;;			- magic		$B%U%!%$%k@hF,$K$"$k(B #! $B9T$+$iI,MW(B
;;					$B$J%3%^%s%I%i%$%s$r:n$j$=$l$KCV$-(B
;;					$B49$($^$9!#(B
;; 
;;			$B$3$l$i$N%7%s%\%k$,0UL#$9$kJ8;zNs$N$&$A!"$5$i$K$=(B
;;			$B$N0lIt$@$1$r<h$j=P$7$?$$$H$-$O@55,I=8=$K$h$kCV$-(B
;;			$B49$(5,B'$,5-=R$G$-$^$9!#$3$l$r=q$-$?$$>l9g$O!"(B
;;			
;;				($B>e5-$N%7%s%\%k(B "$B@55,I=8=(B" "$BCV$-49$(I=8=(B")
;;			
;;			$B$N$h$&$K5-=R$7$^$9!#0J2<$NNc$O!"8=:_$N%U%!%$%kL>(B
;;			$B$N3HD%;R$r(B .tex $B$+$i(B .dvi $B$KJQ$($F(B dvi2ps $B$r5/F0(B
;;			$B$9$k$?$a$N(B command $BMWAGDj5A$G$9!#(B
;;			
;;				(command "printing" "dvips"
;;				   (basename "\\(.*\\)\\.tex$" "\\1.dvi"))
;; 
;;			\\N (N$B$O@0?t(B)$B$O85$N@55,I=8=$N%0%k!<%WHV9f(BN$B$K%^%C(B
;;			$B%A$7$?$b$N$KCV$-49$($i$l$^$9!#(B
;; 
;; * prompt	$B$=$N%W%m%;%9$r5/F0$9$k$H$-$K!"(BEPO$B$,H=CG$7$F@8@.$7$?%3%^(B
;;		$B%s%I%i%$%s$r%_%K%P%C%U%!$KI=<($7$F0l;~Dd;_$9$k>l9g$K$O$3(B
;;		$B$l$r(B Non-nil $B$K@_Dj$7$^$9!#(B
;; 
;; * builtin	EPO$B$,(B process-alist $B$r85$K<+F0E*$K@8@.$9$k%3%^%s%I%i%$%s(B
;;		$B$G$O$J$/!"FCDj$N%=!<%9FH<+$N=hM}%3%^%s%I$r!"%=!<%9<+?H$K(B
;;		$BKd$a9~$`$3$H$,=PMh$^$9!#$?$H$($P%=!<%9$rJ,3d$7$F$$$k$H$-(B
;;		$B$K!"$=$N%=!<%9$G$O$J$/(Bmain$B$H$J$kJ}$NJL%=!<%9$r%3%s%Q%$%i(B
;;		$B$KEO$9$h$&$J>l9g$,5s$2$i$l$^$9!#$3$N>l9g!"%3%^%s%I%i%$%s(B
;;		$B$rKd$a9~$`$H$-$N%-!<%o!<%I(B(prefix)$B$r(B builtin $BMWAG$K;XDj(B
;;		$B$7$^$9!#(B
;;			(builtin . "#!")
;;		
;;		$B$H;XDj$9$k$H!"$=$N8@8l$N%3%a%s%H3+;OJ8;zNs$ND>8e$K(B "#!" 
;;		$B$,$"$C$?$H$-$K!"$=$l$h$j8e$m$N9TKv$^$G(B($B$^$?$O%3%a%s%H=*(B
;;		$BC<J8;zNs$^$G(B)$B$r$=$N=hM}$r9T$&%3%^%s%I%i%$%s$H$7$FMxMQ$7(B
;;		$B$^$9!#(BEmacs$B$G$O!"DL>o$=$N8@8l$N%3%a%s%H3+;OJ8;zNs$H=*C<(B
;;		$BJ8;zNs$O$=$l$>$l!"(BEmacs-Lisp $BJQ?t(B comment-start $B$*$h$S(B
;;		comment-end $B$KF~$C$F$$$^$9!#$?$H$($P(BC$B8@8l$N(B 
;;		comment-start $B$O(B "/* "$B!"(Bcomment-end $B$O(B " */" $B$J$N$G!"(BC$B$N(B
;;		$B%=!<%9Cf$K(B
;;		
;;			/* #! cc -o hoge main.c hoge.c */
;;		
;;		$B$H=q$$$F$*$1$P$=$N%U%!%$%k$N%3%s%Q%$%k$9$k$H$-$@$1$O(B
;;		"cc -o hoge main.c hoge.c"
;;		$B$r5/F0$9$k$h$&$K$J$j$^$9!#(B
;; 
;; $B$3$l$i$r@_Dj$7$?$b$N$,0J2<$NNc$H$J$j$^$9!#(B

(defvar epo-example-process-alist
  '((?j (type . compile) (command "Compile" "make" rootname)
	(builtin . "#!"))
    (?r (type . run) (prompt . t)
	(command "run-it" (basename "\\(.*\\)\\.c" "./\\1"))))
  "*Some language dependent process alists")

;; $B$3$N@_Dj$K$h$j(B [prefix] C-t $B$r2!$9$H!"=hM}7O%3%^%s%I5/F0%a%K%e!<$H$7(B
;; $B$F(B
;;	Start process: (j)Compile (r)run-it:
;; 
;; $B$HI=<($5$l!"(Bj$B$^$?$O(Br$B$G3F%W%m%;%9$,$H$J$j$N%&%#%s%I%&$G5/F0$7$^$9!#(B

;; $B!Z8@8l=hM}7O$N%(%i!<=PNO>pJs$N(Balist$B![(B
;; 
;; $B$"$k8@8l$G=q$$$?%=!<%9$r=hM}7O$rMQ$$$FL\E*J*$KJQ49$7$?$j!"J8K!;n83$7(B
;; $B$?$j$9$k$H$-$K!"$=$N=hM}7O$,I8=`=PNO$KI=<($9$k%(%i!<>pJs$O(B Emacs-Lisp 
;; $BJQ?t(B
;; 
;;	epo-$B8@8lL>(B-tagjump-alist
;; 
;; $B$K@_Dj$7$^$9!#(B
;; alist$B$O(B ($B=hM}L>(B $B%(%i!<%a%C%;!<%8>pJs%j%9%H(B) $B$,B3$/AH9g$;$G!"!V=hM}L>!W(B
;; $B$O(B process-alist $B$N(B command $BMWAG$N@hF,$K;XDj$7$?!"%3%^%s%I$N5!G=$N@b(B
;; $BL@J8;zNs$r;XDj$7$^$9!#$=$l$KBP1~$9$k8@8l=hM}7O$,EG$-=P$9%(%i!<%a%C%;!<(B
;; $B%8$K4X$9$k>pJs$r!V%(%i!<%a%C%;!<%8>pJs%j%9%H!W$K;XDj$7$^$9!#(B
;; 
;; $B!V%(%i!<%a%C%;!<%8>pJs%j%9%H!W$O!"(B'type, 'pattern, 'matchinfo $B$r4^$`(B
;; alist$B$G;XDj$7$^$9!#$=$l$>$l$NMWAG$N0UL#$O0J2<$NDL$j$G$9!#(B
;; 
;; * type	$B=hM}7O$,=P$9%(%i!<%a%C%;!<%8Cf!"2r@OCf$N%U%!%$%kL>$rI=<((B
;;		$B$9$kJ}<0$r;XDj$7$^$9!#J}<0$K$O0J2<$N<oJL$,$"$j$^$9!#(B
;;		
;;		- inline	C$B%3%s%Q%$%i$N$h$&$K!"%a%C%;!<%8$N0l9T$K(B
;;				$B%U%!%$%kL>$H%(%i!<$NN>J}$rI=<($9$k%?%$%W!#(B
;;		- paren		TeX$B$N%?%$%W%;%C%?$N$h$&$K!"%U%!%$%k$N=h(B
;;				$BM}$,;O$^$k$HF1;~$K3+$-3g8L(B ( $B$rI=<($7!"(B
;;				$B$=$N%U%!%$%k$N2r@O$,=*$o$k$HJD$83g8L(B )
;;				$B$rI=<($9$k%?%$%W!#(B
;;		- current	$BC10l%U%!%$%k$r2r@O$9$k$3$H$,A0Ds$H$J$C$F(B
;;				$B$$$k=hM}7O$G!"=hM}Cf$N%U%!%$%kL>$r0l@Z=P(B
;;				$BNO$7$J$$%?%$%W!#(B
;;
;; * pattern	$B%(%i!<H/@89T$r<($9@55,I=8=$r;XDj$9$k!#MxJX$N$?$a!"$3$N@5(B
;;		$B5,I=8=Cf$G$O9THV9f$rI=$9@55,I=8=$H$7$F(B %l $B$rMxMQ$G$-$^$9!#(B
;;		type $B$,(B inline $B$N>l9g$K$O!"%U%!%$%kL>$*$h$S9THV9f$r<($9(B
;;		$B%Q%?!<%s$r(B \\( \\) $B$K$h$j%0%k!<%W2=$9$kI,MW$,$"$j$^$9!#(B
;;
;; * matchinfo	$B>e5-$N(B pattern $BMWAG$G;XDj$7$?@55,I=8=Cf$+$i!"%U%!%$%kL>(B
;;		$B$H9THV9fItJ,$KAjEv$9$k%0%k!<%WHV9f$r%3%s%9%;%k$G;XDj$7$^(B
;;		$B$9!#(B
;; 
;; $B$?$H$($P!"0J2<$NNc(B

(defvar epo-example-tagjump-alist
  '(("Compile" (type . inline) (pattern . "\\(\\S +\\.c\\):\\(%l\\):")
     (matchinfo 1 . 2))))

;; $B$O!"=hM}7O$N%(%i!<=PNO$,(B inline $B%?%$%W$G$"$j!"%(%i!<9T$O(B
;; 
;;	$BHs6uGrJ8;z(B.c:$B9THV9f(B:
;; 
;; $B$H$$$&%Q%?!<%s$GI=<($5$l!"$=$N$&$A@55,I=8=%0%k!<%W$N(B 1$BHV$H(B2$BHV(B $B$,$=$l(B
;; $B$>$l%U%!%$%kL>$H9THV9f$KAjEv$9$k$3$H$r0UL#$7$^$9!#(B 
;;
;; $B$3$N@_Dj$K$h$j(B [prefix] ' $B$r2!$9$H!"=hM}7O$,=PNO$7$?%(%i!<%a%C%;!<%8(B
;; $B$r85$K!"%(%i!<H/@89T$K0\F0$7$^$9!#(B
;; 

;; $B!Z8@8l%=!<%9%U%!%$%k$N%U%!%$%k>pJs$N(Balist$B![(B
;; 
;; $B$=$N8@8l$N%=!<%9%U%!%$%k$N3HD%;R$d!"CV$->l=j(B($B8!:w>l=j(B)$B$K4X$9$k>pJs$O!"(B
;; 
;;	epo-$B8@8lL>(B-file-alist
;; 
;; $B$H$$$&L>A0$N(BEmacs-Lisp$BJQ?t$KJ]B8$7$^$9!#$3$N(Balist$B$K@_Dj$G$-$kMWAG$O8=(B
;; $B:_$N$H$3$m(B3$B$D$G$9!#(B
;; 
;; * extension		$B%G%U%)%k%H$N3HD%;R$r;XDj$7$^$9!#(B
;; * search		$B%=!<%9%U%!%$%k$r8!:w$9$k%G%#%l%/%H%j$N%j%9%H$r;X(B
;;			$BDj$7$^$9!#JQ?tL>$r;XDj$9$k$H!"$=$NJQ?t$NCM$rMxMQ(B
;;			$B$7$^$9!#$3$N%j%9%HCf$KB8:_$7$J$/$F$b!"%+%l%s%H%G%#(B
;;			$B%l%/%H%j$O>o$K8!:wBP>]$K4^$^$l$^$9!#(B
;; * recursive-search	$B>e5-$N%j%9%H$r5/E@$K!"3F%G%#%l%/%H%j$r:F5"E*$K2<(B
;;			$B9_$7$F8!:w$9$k$+$I$&$+$r;XDj$7$^$9!#:F5"8!:w$9$k(B
;;			$B>l9g$O(B non-nil $B$r;XDj$7$^$9!#(B
;; 
(defvar epo-example-file-alist
  '((extension . ".el")
    (search . load-path)
    (recursive-search . nil)
    ))
;; 
;; 
;;	-	-	$B0J>e$GI,MW$J>pJs$OA4$F$G$9!#(B	-	-
;; 
;; 
;; $BA4$F$N(Balist$BJQ?tDj5A$,=*$o$C$?$i:G8e$K(B provide $B$7$F2<$5$$!#(B
;; provide$BL>$O%U%!%$%kL>$HF1$8$K$7$^$9!#(B

(provide 'epo-example)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
