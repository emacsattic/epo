;;; -*- Emacs-Lisp -*-
;;; EPO Java dependent settings
;;; (c)1999-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Mon Feb  4 13:12:24 2002 on firestorm

;;[Commentary]
;;	
;;	EPO$B$N(BJava$B0MB8@_Dj(B
;;	
;;[Abstract]
;;	

;;;
;; Variables for Input Aider(EPOI)
;;;
(defvar epo-java-structure-alist
  '((?b (type . block)
	(structure keyword " (" argument ") {\n" indent cursor "\n" "}" indent)
	(table ("if" "condition") ("for") ("while") ("switch"))
	(mustmatch . t) (arg-reader . epoi-java-block))
    (?t (type . try)
	(structure "try {\n" indent cursor
		   "\n" "} catch() {" indent
		   "\n" "} finally {" indent "\n" "}" indent)
	(mustmatch . t) (arg-reader . epo-java-block))
    (?s (type . identifer)
	(structure keyword)
	(table . epo-java-freq-identifer))
    (?f (type . function)
	(structure keyword "(" argument ");")
	(argsep "" "" nil ", " "" "")
	(table . epo-java-freq-funcs)) ))

(defun epo-java-block (keyword argp)
  (cond
   ((= argp 1) (read-string (format "%s, condition: " keyword)))))

(defvar epo-java-freq-identifer ;;; for java-font-lock-keywords-3
  '(("@author")     ;;; $B%I%-%e%a%s%H!?:n<TL>(B
    ("@exception")  ;;; $B%I%-%e%a%s%H!?Ej=P2DG=@-(B Exception class
    ("@return")     ;;; $B%I%-%e%a%s%H!?La$jCM(B
    ("@param")      ;;; $B%I%-%e%a%s%H!?0z?t(B
    ("@see")        ;;; $B%I%-%e%a%s%H!?;2>H$7$?J}$,NI$$(B class.$B%a%=%C%I(B#$BDLHV(B
    ("@version")    ;;; $B%I%-%e%a%s%H!?HG?t(B
    ("import")      ;;; $B@hF,!?;HMQ(B package $B;XDj(B
    ("package")     ;;; $B@hF,!?<+(B package $B;XDj(B
    ("class")       ;;; class$B;XDj!?@hF,(B
    ("extend")      ;;; class $B;XDj!?7Q>5(B
    ("implements")  ;;; class$B;XDj!?<BAu(B
    ("interface")   ;;; class$B;XDj!?<BAu;XDj(B
    ("trows")       ;;; class$B;XDj!?<u<j$,(Bcatch$B$7$J$1$l$P$J$i$J$$(BException
    ("abstract")    ;;; $B=$>~;R!?%9!<%Q(Bclass$B$^$?$O%a%=%C%I;XDj(B
    ("final")       ;;; $B=$>~;R!?7Q>5!?%*!<%P%i%$%I6X;_;XDj(B
    ("native")      ;;; $B=$>~;R!?(BVM / .dll ($B%M%$%F%#%V%a%=%C%I(B)$B8F$S=P$7;XDj(B
    ("private")     ;;; $B=$>~;R!?<+(Bclass$B$N$_;2>H2DG=;XDj(B
    ("protected")   ;;; $B=$>~;R!??FB2(Bclass$B$N$_;2>H2DG=;XDj(B
    ("public")      ;;; $B=$>~;R!?;2>H2DG=;XDj(B
    ("static")      ;;; $B=$>~;R!?@EE*2=;XDj(B
    ("synchronized");;; $B=$>~;R!?%9%l%C%IGSB>@)8f;XDj(B
    ("catch")       ;;; $BK\J8!?(Btry-catch
    ("do")          ;;; $BK\J8!?(Bdo-while$B!?$"$C$?$C$1!)(B
    ("default")     ;;; $BK\J8!?(Bswich-case-default
    ("else")        ;;; $BK\J8!?(Bif-else
    ("finally")     ;;; $BK\J8!?(Btry-catch-finaly
    ("for")         ;;; $BK\J8!?(Bfor $B!?(B $B7+$jJV$7(B
    ("if")          ;;; $BK\J8!?(Bif $B!?(B $BA*Br(B
    ("instanceof")  ;;; $BK\J8!?(Bclass $BL>%A%'%C%/(B
    ("new")         ;;; $BK\J8!?%$%s%9%?%s%92=;X<((B
    ("return")      ;;; $BK\J8!?%a%=%C%I=*C<(B
    ("super")       ;;; $BK\J8!??F%/%i%9$N%3%s%9%H%i%/%?8F$S=P$7(B
    ("switch")      ;;; $BK\J8!?(Bswitch $B!?A*Br(B
    ("while")       ;;; $BK\J8!?(Bwhile $B!?7+$jJV$7(B
    ("this")        ;;; $BK\J8!?<+%]%$%s%?(B($B;2>H(B)$B;XDj(B
    ("trow")        ;;; $BK\J8!?(Btry-catch
    ("try")         ;;; $BK\J8!?(Btry-catch
    ("break")       ;;; $BK\J8!?7+$jJV$7CfCG(B
    ("case")        ;;; $BK\J8!?(Bswich-case-default
    ("continue")    ;;; $BK\J8!?7+$jJV$7$d$jD>$7(B
    ("false")       ;;; $BK\J8!?(Bboolean $B$N56CM(B
    ("true")        ;;; $BK\J8!?(Bboolean $B$N??CM(B
    ("null")        ;;; $BK\J8!?CM$N$J$$%]%$%s%?(B($B;2>H(B)
    ("boolean")     ;;; $BK\J8!?(B1bit $B%G!<%?(B
    ("byte")        ;;; $BK\J8!?(B1byte $B%G!<%?!?(Bsigned char
    ("char")        ;;; $BK\J8!?(B2byte $B%G!<%?!?(Bsigned char[2]
    ("double")      ;;; $BK\J8!?<B?t(B
    ("float")       ;;; $BK\J8!?<B?t$=$N#2(B
    ("int")         ;;; $BK\J8!?@0?t(B
    ("long")        ;;; $BK\J8!?@0?t$=$N#2(B
    ("short")       ;;; $BK\J8!?@0?t$=$N#3(B
    ("void") ))     ;;; $BK\J8!?La$jCM$J$7(B

(defvar epo-java-freq-funcs
  '(("System.out.print")
    ("System.out.println")
    ("System.exit" "exit code")))

(defvar epo-java-iteration-alist
  '((?s (type . switch-case)
	(opener . ((pattern "\\<switch\\s *(.*)\\s *{"
			    (!before . comment-start))))
	(closer . ((pattern "}")))
	(iterator . ("case " cursor ":")))
    ))

;;;
;; Variables for Reference Handler(EPOR)
;;;
(defvar epo-java-relation-alist
  '((?f (type . reference)
	(idpattern . "[A-Za-z$_][0-9A-Za-z$_]*")
	(relation
	 (definition (pattern . epo-java-crexp-deffunc) (group . 0))
	 (reference (pattern . epo-java-crexp-reffunc) (group . 0))))
    ))

(defvar epo-java-crexp-deffunc
  '("\\<%i\\>" (after . "(") (paren 1 "{")))
(defvar epo-java-crexp-reffunc
  '("\\<%i\\>" (after . "(") (!paren 1 "{")))

;;(defvar epo-java-crexp-function
;;  '("^\\s *\\(public\\|protected\\|private\\|static\\|"))

;;;
;; Variables for Process Handler(EPOP)
;;;

(defvar epo-java-process-alist
  '((?j (type . compile) (command "Compile" "javac" basename))
    (?r (type . run) (prompt . t)
	(command "Interpreter" "java"
		 (text "^\\s *\\(public\\s *\\)class\\s *\\([A-z_][0-9A-z_]*\\)" "\\2")))
    (?a (type . run)
	(command "Applet"
		 "appletviewer" (basename "\\(.*\\)\\.java$" "\\1.html"))))
  "*Java dependent process alists")

(defvar epo-java-tagjump-alist
  '(("Compile" (type . inline) (pattern . "\\.java:%l:"))))


(provide 'epo-java)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
