;;; -*- Emacs-Lisp -*-
;;; EPO Input Aider
;;; (c)1999-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; $Id$
;;; Last modified Mon Feb  4 16:49:02 2002 on duke

;;	2001/1/6 シンボル補完作成開始
;;	2001/1/8 シンボル補完完成(開いているバッファ群のみ)
;;	
;;[Commentary]
;;	
;;	EPOの新規入力支援ハンドラ
;;	
;;[Abstract]
;;	
;;	epoiは編集ファイル s に新規に構造を入力する過程を支援する。
;;	入力可能な構造を以下のように分類する。
;;	
;;	* 型挿入
;;	  言語特有のブロック構造を補完機能付きで一括入力する
;;		
;;	* 繰り返し指定子挿入
;;	  局所箱内で、ある一定のキーワードを繰り返して記述する環境(包括
;;	  子)がある場合、その内部で発生すべきキーワードを自動認識してポイ
;;	  ント位置に出力する。
;;		
;;	
;;[epo-言語名-structure-alistの構造]
;;	
;;	epo-言語名-structure-alist := '(<identifier>
;;				    <structure-info>) のリスト
;;	<identifier> := <structure-info> を一意に定める一文字。入力キーとしても
;;			使われる
;;	<structure-info> := ((type . <type-value>)
;;			     (structure . <structure-list>)
;;			     (table . <completion-table>)
;;			     (argsep . <argument-seplist>)
;;			     [ (mustmatch . <mustmatch-value>) ]
;;			     [ (arg-reader . <argument-read-function>) ]
;;			   )
;;	<type-value> := 構造の形式を示す任意のシンボル
;;	<structure-list> := <structure-list> | "文字列" | 'keyword | 'argument
;;			   | 'indent | 'cursor
;;		構造をテキスト中に入力する場合の流れを列挙したりストと
;;		なる
;;		"文字列"	そのままテキストに挿入される
;;		'keyword	そこに構造のIDとなるキーワードが挿入される
;;		'argument	そこに構造の引数が挿入される
;;		'indent		そこまで挿入が済んだ時点でインデントを行う
;;		'cursor		そこまで挿入が済んだ位置に、挿入完了後の
;;				カーソルが設定される。ただし、argument
;;				の入力が省略された場合にはカーソル位置は
;;				引数位置に設定される。
;;	<completion-table> := <alist> | <alist-symbol>
;;	<alist> := この構造のキーワード一覧を持つ補完リスト
;;		 各候補を含むリストのcdr部は(もしあれば)そのリストの各要素が
;;		 引数を読み込むために利用される。文字列ならプロンプトと
;;		 して、シンボルならそのシンボルにバインドされた関数が呼
;;		 ばれる(キーワード名と引数の位置[整数]が渡される)。
;;		 もし要素が更にリストになっていた場合はそのcar部がプロン
;;		 プトとして、cdr部が候補を決定するalistまたはalistを保持
;;		 する変数として利用される。
;;	<alist-symbol> := この構造のキーワード一覧を持つ補完リストを保持す
;;			る Lisp シンボル。その値に関して、上記alistと同
;;			一の評価がなされる。
;;	<argument-seplist> := (<delim-open> <delim-left> <delim-let>
;;			     <delim-mid> <delim-right> <delim-close>)
;;			(典型的なseplistをaliasにしておくとええかも)
;;	<delim-open>  := 引数列全体の左括弧に相当する文字列
;;	<delim-left>  := 引数一個を括る左括弧に相当する文字列
;;	<delim-let>   := 引数の与え方が 属性=値(Hash方式) であるときに代入
;;		       演算子となる文字列。nil なら引数は位置で意味が決
;;		       定して値のみ記述する方式となる。
;;		       (仕様変更の可能性あり)
;;	<delim-mid>   := 引数どうしを区切るデリミタ文字列
;;	<delim-right> := 引数一個を括る右括弧に相当する文字列
;;	<delim-close> := 引数列全体の右括弧に相当する文字列
;;	<delim-func>  := 引数を必要個数読み込んでデリミタ文字列と共に返す
;;		         関数の Lisp シンボル
;;	<mustmatch-value> := t | nil
;;			(補完キーワードに一致することを強制するか否か)
;;	<argument-read-function> := 引数を必要個数読み込むためのデフォルト関数
;;
;;[epo-言語名-structure-alist の設定例]
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
      ;;ここでは (command "commandname" .....)
      (or bind type
	  (error "%s言語用の structure-alist が変です" epo:current-language))
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

;;【繰り返し指定子挿入】
;;
;;	「繰り返し」とはソースの一部に存在する局所箱の中特有の、同様の構
;;	造が繰り返し現れる性質をいう。プログラミング言語ではcase分岐、マー
;;	クアップ言語ではアイテム列挙や、表などがこれに当たる。ソーステキ
;;	ストの特定の場所で発生すべき繰り返しを認識するためのパターンを
;;	epoi-言語名-iteration-alist で指定する。
;;	
;;	内部に特有の繰り返しを持つ局所箱の主体を包括子と呼ぶことにする。
;;
;;[epo-言語名-iteration-alistの構造]
;;	
;;	epo-言語名-iteration-alist := '(identifier
;;				    iteration-info) のリスト
;;	identifier := structure-info を一意に定める一文字。
;;		      明示的に現在の包括子を指示するための
;;		      入力キーとしても使われる。
;;	iteration-info := ((type . type-value)
;;			   (opener . opening-pattern)
;;			   (closer . closing-pattern)
;;			   (iterator . iterator-list)
;;			   )
;;	opening-pattern := opening-regexp | opening-rule
;;	opening-regexp := 包括子の開始位置を示す正規表現(文字列)
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
