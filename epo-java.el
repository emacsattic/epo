;;; -*- Emacs-Lisp -*-
;;; EPO Java dependent settings
;;; (c)1999-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Mon Feb  4 13:12:24 2002 on firestorm

;;[Commentary]
;;	
;;	EPOのJava依存設定
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
  '(("@author")     ;;; ドキュメント／作者名
    ("@exception")  ;;; ドキュメント／投出可能性 Exception class
    ("@return")     ;;; ドキュメント／戻り値
    ("@param")      ;;; ドキュメント／引数
    ("@see")        ;;; ドキュメント／参照した方が良い class.メソッド#通番
    ("@version")    ;;; ドキュメント／版数
    ("import")      ;;; 先頭／使用 package 指定
    ("package")     ;;; 先頭／自 package 指定
    ("class")       ;;; class指定／先頭
    ("extend")      ;;; class 指定／継承
    ("implements")  ;;; class指定／実装
    ("interface")   ;;; class指定／実装指定
    ("trows")       ;;; class指定／受手がcatchしなければならないException
    ("abstract")    ;;; 修飾子／スーパclassまたはメソッド指定
    ("final")       ;;; 修飾子／継承／オーバライド禁止指定
    ("native")      ;;; 修飾子／VM / .dll (ネイティブメソッド)呼び出し指定
    ("private")     ;;; 修飾子／自classのみ参照可能指定
    ("protected")   ;;; 修飾子／親族classのみ参照可能指定
    ("public")      ;;; 修飾子／参照可能指定
    ("static")      ;;; 修飾子／静的化指定
    ("synchronized");;; 修飾子／スレッド排他制御指定
    ("catch")       ;;; 本文／try-catch
    ("do")          ;;; 本文／do-while／あったっけ？
    ("default")     ;;; 本文／swich-case-default
    ("else")        ;;; 本文／if-else
    ("finally")     ;;; 本文／try-catch-finaly
    ("for")         ;;; 本文／for ／ 繰り返し
    ("if")          ;;; 本文／if ／ 選択
    ("instanceof")  ;;; 本文／class 名チェック
    ("new")         ;;; 本文／インスタンス化指示
    ("return")      ;;; 本文／メソッド終端
    ("super")       ;;; 本文／親クラスのコンストラクタ呼び出し
    ("switch")      ;;; 本文／switch ／選択
    ("while")       ;;; 本文／while ／繰り返し
    ("this")        ;;; 本文／自ポインタ(参照)指定
    ("trow")        ;;; 本文／try-catch
    ("try")         ;;; 本文／try-catch
    ("break")       ;;; 本文／繰り返し中断
    ("case")        ;;; 本文／swich-case-default
    ("continue")    ;;; 本文／繰り返しやり直し
    ("false")       ;;; 本文／boolean の偽値
    ("true")        ;;; 本文／boolean の真値
    ("null")        ;;; 本文／値のないポインタ(参照)
    ("boolean")     ;;; 本文／1bit データ
    ("byte")        ;;; 本文／1byte データ／signed char
    ("char")        ;;; 本文／2byte データ／signed char[2]
    ("double")      ;;; 本文／実数
    ("float")       ;;; 本文／実数その２
    ("int")         ;;; 本文／整数
    ("long")        ;;; 本文／整数その２
    ("short")       ;;; 本文／整数その３
    ("void") ))     ;;; 本文／戻り値なし

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
