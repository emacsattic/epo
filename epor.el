;;; -*- Emacs-Lisp -*-
;;; EPO Relation Resolver
;;; (c)1999-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; $Id$
;;; Last modified Sun Mar 24 17:34:17 2002 on balius

;;	2000/12/28
;;	パターンジャンプは完成。次はファイルジャンプとプロセス?
;;	ファイルジャンプ完成。拡張子関係が今ひとつ?
;;	2000/12/29
;;	プロセス起動完成。
;;	
;;[Commentary]
;;	
;;	EPOの参照/被参照リゾルバ
;;	
;;[Abstract]
;;	
;;	eporは編集対象ソースファイル中に書かれた「参照」を追跡、および新
;;	規作成する作業を支援する機能を提供する。
;;	
;;	* 定義参照
;;		別箇所で定義されている関数(マクロ)をその場で利用する。
;;		
;;	
;;[epo-言語名-relation-alistの構造]
;;	
;;	epo-言語名-relation-alist := '(identifier
;;				    relation-info) のリスト
;;	identifier := relation-info を一意に定める一文字。入力キーとしても
;;			使われる。
;;	relation-info := ((type . <type-value>)
;;			   (idpattern . <id-regexp>)
;;			   (relation . <relation-item-alist>))
;;	
;;	<type-value> := reference | file | hyperlink | box
;;			boxの場合は identifier が一意でなく、テキストの
;;			一部に箱を形成するもので、開始と終了の前後関係が
;;			決まっていて、なおかつネストする可能性を考慮する。
;;	<id-regexp> := その参照での識別子となり得る語彙の正規表現
;;	<relation-item-alist> := (<AnyRelationSymbol> . <PatternList>)
;;	<AnyRelationSymbol> := 任意の関係保有項目シンボル
;;	<PatternList> := (<CCRE> <NUM> <relation-type-specific>)
;;	<CCRE> := 関係保有項目の 文脈確認正規表現。関係の識別子となる部分の
;;		  正規表現は必ず %f で表現しておくこと。
;;	<NUM> := <CCRE>検索がマッチしたときにその関係を意味づける
;;	          識別子が含まれる正規表現グループ番号
;;	<relation-type-specific> := <text-type-specific>
;;				     | <file-type-specific>
;;				     | <process-type-specific>
;;				     | <function-type-specific>
;;	<text-type-specific> := nil | <partner-symbol>
;;	<partner-symbol> := 関係の相手となる <AnyRelationSymbol>
;;			    <AnyRelationSymbol>が二個の場合は省略できる。
;;	<file-type-specific> := '(file)
;;				関係の相手がファイルのときにこれを指定。
;;				ファイル名は正規表現の識別子部分となる。
;;	<process-type-specific> := '(process)
;;				関係の相手がファイルで、そのファイルを外
;;				部プロセスが処理する場合に指定。
;;				ファイル名は正規表現の識別子部分となる。
;;	<function-type-specific> := '(process)
;;				関係の相手がファイルで、そのファイルを関
;;				数が処理する場合に指定。
;;				ファイル名は正規表現の識別子部分となる。
;;	
;;[文脈確認正規表現]
;;	
;;	文脈確認正規表現とは、正規表現検索で発見したパターンの置かれてい
;;	る場所に関するいくつかの文脈情報を調べ、特定の文脈に位置するもの
;;	のみ本当に適合するものとみなす正規表現を含めたパターンである。
;;	パターンは任意長のリストで与える。
;;	
;;	パターンリスト := (<search-regexp> <patlist-elem> *)
;;	<patlist-elem> := <context-cons>
;;	<search-regexp> := 初期検索用正規表現として利用される
;;	<context-cons> := <box-p> | <line-context> | <neighbor-context>
;;			  | <parenscan-context>
;;			  | <exclude-list> | <require-list>
;;			  | <recursive-exp> | <or-exps>
;;			  | <return-exp>
;;	<box-p> := (<box-or-not> . <box-name>)
;;	<box-or-not> := box | !box
;;		     'box はポイント位置が <box-name> のbox内にあれば真
;;		     '!box はポイント位置が <box-name> のbox内になければ真
;;	<box-name> := 包括子の名前となる文字列
;;	<line-context> := (<number> . <regexp>)
;;		       <number>行以内に <regexp> が見付かれば真
;;		       <number>が負なら先頭方向を検索
;;	<neighbor-context> :- (<neighborhood> . <regexp>)
;;			<neighborhood>方向に <regexp> が見付かれば真
;;			neighbor とは空白文字のみで区切られた範囲内をいう
;;	<neighborhood> := before | !before | after | !after
;;		       'before は先頭方向、'after は末尾方向
;;	<parenscan-context> := (<parenmove> <number> <regexp>))
;;	<parenmove> := paren | !paren | upparen | !upparen
;;		       'paren は括弧 <number> 個分移動した先が<regexp>にマッチ
;;		       すれば真、!paren はマッチしなければ真
;;		       'upparen は括弧 <number> 個分上位に移動した先
;;		       (負数の場合はバッファ先頭方向に抜ける)が
;;		       <regexp>にマッチしたら真、!upparenはマッチしなければ真
;;	
;;	<exclude-list> := (exclude <number> <regexp>)
;;		      <search-regexp> によってマッチしたグループ番号 
;;		      <number> の文字列が <regexp> にマッチしなければ真
;;	<require-list> := (require <number> <regexp>)
;;		      <search-regexp> によってマッチしたグループ番号 
;;		      <number> の文字列が <regexp> にマッチすれば真
;;	<recursive-exp> := (context . パターンリスト)
;;			「パターンリスト」を用いて同様の文脈確認正規表現
;;			検索を再帰的に行なう。
;;	<or-exps> := (or . (<patlist-elem>*))
;;		  <patlist-elem> の各リストのどれかが non-nil を返せば真
;;	<return-exp> := (return) | (return . <return-exp>)
;;		  全ての文脈検査が真のとき、最終的なマッチの結果を
;;		  初期検索とは違う位置にする場合に利用する。初期検索の成
;;		  功した直後の位置から二次検索が行われる(必要なら)。
;;		  'return のみの場合は、それ以前の <context-cons> で行わ
;;		  れた検索のマッチ情報が全体のマッチ情報としてストアされる
;;	<return-exp> := (<direction> . <Regexp>)
;;	<direction> := forward | backward
;;		    二次検索を行う方向と正規表現
;;	
;;[epo-言語名-relation-alist の設定例]
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
