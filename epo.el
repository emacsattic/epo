;;; -*- Emacs-Lisp -*-
;;; Editing Process Organizer
;;; epo.el rev. 1.4h
;;; (c)1999-2003 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Sun Mar 30 20:56:34 2003 on firestorm

(defconst epo-revision-number "1.4h")
;;	
;;	What's new
;;	  -- [prefix] C-r での位置記憶レジスタを 1〜9 の9個にした。
;;	     何回も [prefix] C-r したときは1→9にローテート
;;
;;	(個人的メモ)
;;	  -- project-root 以下の全ファイルを開く → シンボル収集のため
;;	  -- project-root 以下の関連ファイルを探して開く → 〃
;;	  -- inclusion hierarchy tree を作る?
;;	
;;
;;[Comentary]
;;	
;;	プログラミング言語、マークアップ言語、あらゆる言語の入力編集工程
;;	を統合的に支援する。
;;	
;;[Abstract]
;;	
;;	Editing Process Organizer(EPO)では、我々がエディタを用いて入力す
;;	る作業を以下の四基本工程に分類し、それぞれに対して適切な支援操作
;;	を行なう。
;;		1. 新規入力操作
;;		2. 既入力テキスト変更操作
;;		3. 連関(参照)の結合・修正・分離・移動操作(含む補完)
;;		4. オブジェクトへの翻訳操作・吟味操作
;;	
;;	それぞれの操作は以下のモジュールで行なう。
;;	
;;	1. epoi		epo input aider
;;	2. epoc		epo change operation 
;;	3. epor		epo relation resolver
;;	4. epop		epo process interface
;;	
;;[Attributes]
;;	
;;	ソースに現れる補完可能な属性は以下の通り
;;	
;;	* 環境の開始/終了
;;	* 繰り返し箱(包括子)
;;	* 繰り返し指定子(イテレータ)
;;	* 識別子の参照 / 識別子の定義
;;	+ 注釈記号
;;	
;;[Pairs]
;;	
;;	文書には以下の連関が存在する。
;;	
;;	* 定義 / 参照
;;	* 開始 / 終了
;;	* 他文書参照
;;	* 非文書データ参照(プロセス起動等)
;;	
;;[./.eporc の書式]	
;;
;;	作業ディレクトリの .eporc ファイルに、その場所でのEPO関連機能起
;;	動に必要な情報を与えることができる。書式は以下のとおり。
;;	
;;	モジュール-タグ:
;;	
;;	
;;[Symbol Naming Rules]
;;	
;;	EPOモジュール群で定義するシンボルは以下のように定義する。
;;	
;;	* 全てのEPO従属シンボルの prefix は必ず "epo" で始まる。
;;	* そのうち、epoi, epoc, epor, epop 固有の機能や値にバインドされた
;;	  シンボルは、それぞれのモジュール名と同じ prefix となる。
;;	* さらに delimiter を加えて、シンボルの prefix を完成する。
;;	  o インタラクティブ関数(コマンド)と、ユーザオプション変数は
;;	    delimiter を "-" とする  → prefix = "epo{,i,c,r,p}-"
;;	  o その他の関数は delimiter を "*" とする
;;	     → prefix = "epo{,i,c,r,p}*"
;;	  o その他の変数は delimiter を ":" とする
;;	     → prefix = "epo{,i,c,r,p}:"
(require 'epolib)

(defvar epo:current-language nil
  "Current processing language")

(defvar epo-box-indent-depth 1
  "*Indentation depth relative to iterator-box opener.")

(defvar epo-last-seen-position-registers "123456789"
  "*String of register characters to save last seen point.
Most recently seen position is saved in the 0-th of this string.
When the seen position is saved in to 0-th of the string,
previous content in 0-th entry is moved to 1st, 1st to 2nd, and so on.")

(defvar epo-select-frame (fboundp 'select-frame)
  "Non-nil selects frame if target buffer is in other frame.")

;;;
;; Keymap
;;;
(defvar epo-prefix-key "\C-c"
  "*Prefix key stroke of invoking epo facilities")
(defvar epo-yank-key "\C-y"
  "*Key stroke after epo-prefix-key to yank generated structure.")

(defvar epo-mode-map (make-keymap) "Keymap used in epo")
(defmacro epo-define-key (map bind target &optional autoload)
  (list
   'progn
   (list 'define-key map (list 'concat 'epo-prefix-key bind) target)
   (if autoload (list 'autoload target autoload
		      (symbol-name (car (cdr target))) t))))


; genelic
(define-key epo-mode-map "\M-n" 'epo-coco-re-search-again-forward)
(define-key epo-mode-map "\M-p" 'epo-coco-re-search-again-backward)
(autoload 'epo-coco-re-search-again-forward "epolib" "ccre search forward" t)
(autoload 'epo-coco-re-search-again-backward "epolib" "ccre search forward" t)
(epo-define-key epo-mode-map epo-yank-key 'epo-yank-structure)
(epo-define-key epo-mode-map "^" 'epo-back-to-referer)
(epo-define-key epo-mode-map "\C-q" 'epo-quit)
(epo-define-key epo-mode-map "\C-v" 'epo-version)

; for epoi
(epo-define-key epo-mode-map "\C-s" 'epoi-insert-structure "epoi")
(epo-define-key epo-mode-map "\C-i" 'epoi-insert-iterator "epoi")
(epo-define-key epo-mode-map "\C-m" 'epoi-intelligent-newline "epoi")
(epo-define-key epo-mode-map "\C-@" 'epoi-complete-symbol "epoi")
(cond
 ((featurep 'xemacs)
  (define-key epo-mode-map
    [epo-prefix-key (control space)] 'epoi-complete-symbol))
 (window-system
  (define-key epo-mode-map
    (vector (string-to-char epo-prefix-key) ?\C-  ) 'epoi-complete-symbol)))
;;	must consider epo-prefix-key is more than 1 char...
; epoi special for C/C++/Java families
(epo-define-key epo-mode-map "\C-f" 'epoi-complete-function "epoi")
(autoload 'epoi*structure-input "epoi" "Insert language's structure" t)

; for epoc
(epo-define-key epo-mode-map "\C-c" 'epoc-change-relation-elements "epoc")
(epo-define-key epo-mode-map "\C-k" 'epoc-kill-relations "epoc")

; for epor
(epo-define-key epo-mode-map "\C-r" ' epor-resolve-relation "epor")
(autoload 'epor*on-relation-p "epor")

; for epop
(epo-define-key epo-mode-map "\C-t" 'epop-start-menu "epop")
(epo-define-key epo-mode-map "'" 'epop-jump-to-error "epop")
(autoload 'epop*invoke-processor "epop" "EPO Process starter" t)

(defvar epo-mode nil "Minor mode indicator.")
(make-variable-buffer-local 'epo-mode)
(or (boundp 'minor-mode-map-alist)
    (error "Sorry, EPO can run on Emacs-19 or later..."))

(setq minor-mode-map-alist
      (delq (assq 'epo-mode minor-mode-map-alist) minor-mode-map-alist)
      minor-mode-map-alist
      (append minor-mode-map-alist ;;to make me most modest one
	      (list (cons 'epo-mode epo-mode-map))))
(or (assq 'epo-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(epo-mode epo-mode) minor-mode-alist)))

;;;
;; autoloads
;;;
(autoload 'epop-start-menu "epop" "Start Language Processor" t)

;;;
;; Convert major-mode
;;;
(defvar epo-major-mode-lang-alist
  '((java-mode . "java")))

(defvar epo-project-root-dir nil
  "Current file's project root directory")
(setq-default epo-project-root-dir nil)

(defvar epo-language-alist nil
  "Alist of major-mode(or mode-name) patterns vs. corresponding language names.
Each element looks like (REGEXP . LANGNAME).")

(defun epo-get-lang ()
  "Return the current language."
  (let ((mode (concat (symbol-name major-mode) ":" mode-name))
	(alist epo-language-alist))
    (or (catch 'lang
	  (while alist
	    (if (string-match (car (car alist)) mode)
		(throw 'lang (cdr (car alist))))
	    (setq alist (cdr alist))))
	(cond
	 ((string-match "java" mode)				"java")
	 ((string-match "\\<\\(cc\\|c\\+\\+\\)-mode" mode)	"c++")
	 ((string-match "\\<c-mode" mode)			"c")
	 ((string-match "ruby\\>" mode)				"ruby")
	 ((string-match "perl\\>" mode)				"perl")
	 ((string-match "\\(la\\)?tex\\>" mode)			"tex")
	 ((string-match "html\\>" mode)				"html")
	 ((string-match "\\<emacs-lisp" mode)			"elisp")
	 ((string-match "\\<lisp-interaction" mode)		"elisp")
	 (t							"example")))))

(defun epo-setup-language (lang)
  "Load current language settings, if necessary."
  (let ((module (intern (concat "epo-" lang))))
    (or (featurep module)
	(progn
	  (condition-case err
	      (require module)
	    (error
	     (error "%s language set is not provided in this system" lang)))
	  ; Need something?
	  ))))

(defun epo*get-cur-lang-alist (type)
  (let*((lang epo:current-language)
	(sym (intern-soft (concat "epo-" lang "-"
				  (symbol-name type) "-alist")))
	(val (and sym (symbol-value sym))))
    val))

(defun epo*structure-info-get (entity alist)
  (cdr (assq entity alist)))

(defvar epo:major-mode nil
  "Memorize major-mode at startup of epo-mode.")

(defun epo-enable (&optional language)
  "Enable EPO in this buffer."
  (interactive)
  (setq epo-mode " EPO")
  (make-variable-buffer-local 'epo:current-language)
  (epo-setup-language (setq epo:current-language (or language (epo-get-lang))))
  (set (make-local-variable 'epo:major-mode) major-mode)
  (set (make-local-variable 'epo-project-root-dir) nil)
  (run-hooks 'epo-mode-hook))

(fset 'epo 'epo-enable)			;alias

(defun epo-disable ()
  (setq epo-mode nil)
  (force-mode-line-update))

(defun epo-quit ()
  "Interactive function to quit from EPO"
  (interactive)
  (epo-disable)
  (message "Quit from EPO"))

(defun epo-mode (arg)
  "Toggle EPO"
  (interactive "P")
  (if (or (not epo-mode)
	  (and arg (> (prefix-numeric-value arg) 0)))
      (epo-enable)
    (epo-disable))
  (force-mode-line-update))

(defun epo*check-mode (&rest args)
  "Check major-mode is the same as startup time."
  (cond
   ((not epo-mode)
    (error "Not in epo-mode!"))
   ((eq major-mode epo:major-mode)
      nil)			;OK
    ;; Turn off epo-mode automatically
   (t
    (epo-disable)
    (let ((func (or (lookup-key (current-local-map) (this-command-keys))
		    (lookup-key (current-global-map) (this-command-keys)))))
      (cond
       ((fboundp func)
	(apply func args))
       (t nil)))
    (error "Quit from epo-mode!"))))

(defun epo*store-position-in-register (&optional point buffer)
  "Store current point-marker in register (default ?1)."
  (if epo-last-seen-position-registers
      (let*((regs epo-last-seen-position-registers)
	    (n (1- (length regs)))
	    alive m mp mb
	    (top (aref regs 0)))
	(while (> n 0)			;shift regster positions
	  (if (and (setq m (get-register (aref regs (1- n))))
		   (setq mp (marker-position m))
		   (setq mb (marker-buffer m))
		   (epo*buffer-live-p mb))
	      (progn
		(set-marker (get-register (aref regs n)) mp mb)
		(setq alive (cons (aref regs n) alive)))
	    (or (markerp (get-register (aref regs n)))
		(set-register (aref regs n) (make-marker)))
	    (set-marker (get-register (aref regs n)) nil))
	  (setq n (1- n)))
	(or (markerp (get-register top))
	    (set-register top (make-marker)))
	(set-marker (get-register top)
		    (or point (point)) buffer)
	(message "Type `%s %c' to return to last position(marks in [%c%s])"
		 (epo*function-key-description
		  (cond
		   ((fboundp 'jump-to-register-compatibility-binding)
		    'jump-to-register-compatibility-binding)
		   ((fboundp 'jump-to-register) 'jump-to-register)
		   (t 'register-to-point)))
		 top
		 (aref epo-last-seen-position-registers 0)
		 (mapconcat 'char-to-string alive "")))))

(defun epo*reset-position-registers ()
  "Reset all registers in epo*store-position-in-register."
  (interactive)				; for debug use
  (let ((n (length epo-last-seen-position-registers))
	(regs epo-last-seen-position-registers))
    (while (>= (setq n (1- n)) 0)
      (if (markerp (aref regs n))
	  (set-marker aref regs n) nil)
      (set-register (aref regs n) (make-marker)))))
(epo*reset-position-registers)

(defvar epo-structure-yank-register nil
  "To save structure template.
This variable holds a cons of '(string . pos); where
string is string to be yanked, pos is a point after yank relative to the
beginning of string.")

(defun epo-yank-structure ()
  "Yank saved structure at point."
  (interactive)
  (if epo-structure-yank-register
      (apply 'epoi*structure-input epo-structure-yank-register)))

(defun epo-query-replace-all (pattern repl)
  (interactive
   (let*((old (read-string "Query replace all files: "))
	 (new (read-string (format "Query replace all files %s with: " old))))
     (list old new)))
  (let ((cb (current-buffer)) (p (point)) (rootdir (epo*project-root)))
    (unwind-protect
	(epo*foreach-files
	 (list rootdir)
	 pattern
	 (list
	  'lambda ()
	  (list 'save-excursion
		'(goto-char (point-min))
		(list 'if (list 'search-forward pattern nil t)
		      (list 'progn
			    '(switch-to-buffer (current-buffer))
			    '(goto-char (point-min))
			    (list 'query-replace pattern repl))))))
      (switch-to-buffer cb)
      (goto-char p))))

(defun epo-back-to-referer ()
  "Return to the file which refers current file."
  (interactive)
  (and epo:referer
       (file-exists-p epo:referer)
       (if (get-file-buffer epo:referer)
	   (epo*goto-buffer (get-file-buffer epo:referer))
	 (find-file epo:referer)
	 (epo-mode 1))))

(run-hooks 'epo-load-hook)

;; Small commands
(defun epo-version ()
  "Show epo's version information"
  (interactive)
  (message "EPO %s" epo-revision-number))

(provide 'epo)

(defconst epo-revision
  "$Id$"
  "Revision number of EPO")

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
