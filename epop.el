;;; -*- Emacs-Lisp -*-
;;; EPO Process Handler
;;; (c)1999-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; $Id$
;;; Last modified Mon Jun  3 13:35:00 2002 on balius

;;[Commentary]
;;	
;;	EPOのプロセスハンドラ
;;	
;;[Abstract]
;;	
;;	epopは編集ファイル s に関連して外部プロセスを呼ぶ過程を以下の三
;;	つに分類しそれぞれに対して適切な処理を行なう。
;;	
;;	* コンパイル過程
;;		あるアプリケーションに s を渡し処理させる過程
;;	* 試走過程
;;		あるアプリケーションに s からの生成物である o を渡し処理
;;		させる過程
;;	* 被参照過程(?)
;;		<<html -> appletviewerみたいなやつ>>
;;	
;;[Info-tree]
;;	
;;	プロセス起動には以下の情報ツリーを与える。
;;	
;;	
;;[epo-言語名-process-alistの構造]
;;	
;;	記述する言語名によって決定する epo-言語名-process-alist という
;;	Lisp変数によって s に関連するプロセスを呼ぶ場合の動作を決定する。
;;	以後この変数を epo-lang-process-alist で代表する。この変数の構
;;	造は以下の通り。
;;	
;;	epo-lang-process-alist := (<identifier>
;;				    <command-infoのリスト...>) のリスト
;;	<identifier> := command-infoを一意に定める一文字。入力キーとしても
;;			使われる。
;;	<command-info> := ((type . <type-value>)
;;	                   (command . <command-value>)
;;			   [ (prompt . <prompt-value>) ]
;;			   [ (builtin . <builtin-prefix>) ]
;;			   [ (makefile . <makefile-pattern>) ]
;;			   )
;;	<type-value> := compile | run | make
;;	  'compile → ソースと同じディレクトリで <command-value> を起動
;;	  'run     → ソースと同じディレクトリで <command-value> を起動
;;	  'make    → ソースディレクトリから順に上位ディレクトリを辿り
;;		      <makefile> のパターンにマッチするファイルが存在する
;;		      最上位ディレクトリで <command-value> を起動
;;	<command-value> := ("処理名" <argument-making-method>のリスト)
;;	"処理名" := 後続するコマンドが行なう処理の説明文字列(短く)
;;	<argument-making-method> := <name-keyword> | "string"
;;	                         | (<name-keyword> <regexp> <replexp>)
;;	<name-keyword> := 'filename | 'basename | 'dirname | 'text
;;	"string" := 文字列 "string" 自身
;;	'filename := フルパスのファイル名に置換される
;;	'basename := ディレクトリ名を除いたファイル名に置換される
;;	'rootname := basenameから拡張子を取り除いた名前に置換される
;;	'dirname := ファイル名を除いたディレクトリ名に置換される
;;	'text := sのテキスト全体に置換される
;;	'magic := コマンド列をファイル先頭 #! に従って生成する
;;	<regexp> := マッチさせる正規表現
;;	<replexp> := <regexp> でマッチしたものを置換するときに利用する表現
;;	<prompt-value> := t | nil  (コマンドラインの確認を要するか)
;;	<builtin-prefix> := ソース埋め込みコマンドの指定用prefix
;;	<makefile-pattern> := make的プロジェクトメーカプログラムのジョブ定義
;;			      ファイルのパターン
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
	  ;;ここでは (command "commandname" .....)
	  (or bind command
	      (error "%s言語用の process-alist が変です" epo:current-language))
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
  "プロセスバッファの入力行をプロセスに送る"
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

(defvar epo-process-mode-map (make-keymap) "プロセスバッファキーマップ")
(let ((ch ? ))
  (while (< ch 128)
    (define-key epo-process-mode-map
      (make-string 1 ch) 'epop-process-buffer-insert)
    (setq ch (1+ ch))))

(defun epop-process-buffer-insert (x)
  "プロセスバッファでのキー入力"
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

;;[epo-言語名-tagjump-alistの構造]
;;
;;	ソースファイルをそれ用の処理系にかけたときに、処理系がソースファ
;;	イルにあったエラーや警告該当箇所を示すときのパターンを正規表現を
;;	利用して epo-言語名-tagjump-alist に指定する。この変数の構造は
;;	以下の通り。
;;	
;;	epo-言語名-tagjump-alist
;;		:= '(処理名パターン
;;			<tagjump-info> のリスト) の リスト
;;	処理名パターン := epo-言語名-process-alistでの 「処理名」
;;			      の正規表現。実際には処理系を動かしたバッファ
;;			      名のパターン。
;;	<tagjump-info> := ((type . <type-value>)
;;			 (pattern . <line-regexp>)
;;			 (matchinfo . <match-cons>))
;;	<type-value> := 'inline | 'paren | 'current
;;	 'paren := 処理中のファイル名を () で括って表示する(TeXと同じ)
;;	 'inline := エラー発生ファイルをエラーメッセージと同じ行に表示
;;	 'current := 処理対象ファイルは単一。ファイル名表示無し。
;;	<line-regexp> := 行番号部分の正規表現
;;	<match-cons> := (<group-F> . <group-L>)
;;	(<group-F> はエラー発生ファイル名, <group-L> は発生行番号と結合)
;;	<group-F> := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | <func-sym>
;;	<group-L> := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | <func-sym>
;;	0〜9 := <line-regexp> でマッチした \( \) のグループ番号
;;	<func-sym> := <line-regexp> でマッチした箇所からこの関数を呼んでエラー
;;		    発生ファイル(または行)を得る
;;	
;;	<line-regexp> の正規表現中では以下の置き換えが利用できる
;;	
;;	  %F	ソースファイルのフルパス名
;;	  %f	ソースファイルのベース名
;;	  %l	行番号の正規表現 ([0-9]+)
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
