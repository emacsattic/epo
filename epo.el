;;; -*- Emacs-Lisp -*-
;;; Editing Process Organizer
;;; epo.el rev. 1.4h
;;; (c)1999-2003 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; Last modified Sun Mar 30 20:56:34 2003 on firestorm

(defconst epo-revision-number "1.4h")
;;	
;;	What's new
;;	  -- [prefix] C-r $B$G$N0LCV5-21%l%8%9%?$r(B 1$B!A(B9 $B$N(B9$B8D$K$7$?!#(B
;;	     $B2?2s$b(B [prefix] C-r $B$7$?$H$-$O(B1$B"*(B9$B$K%m!<%F!<%H(B
;;
;;	($B8D?ME*%a%b(B)
;;	  -- project-root $B0J2<$NA4%U%!%$%k$r3+$/(B $B"*(B $B%7%s%\%k<}=8$N$?$a(B
;;	  -- project-root $B0J2<$N4XO"%U%!%$%k$rC5$7$F3+$/(B $B"*(B $B!7(B
;;	  -- inclusion hierarchy tree $B$r:n$k(B?
;;	
;;
;;[Comentary]
;;	
;;	$B%W%m%0%i%_%s%08@8l!"%^!<%/%"%C%W8@8l!"$"$i$f$k8@8l$NF~NOJT=89)Dx(B
;;	$B$rE}9gE*$K;Y1g$9$k!#(B
;;	
;;[Abstract]
;;	
;;	Editing Process Organizer(EPO)$B$G$O!"2f!9$,%(%G%#%?$rMQ$$$FF~NO$9(B
;;	$B$k:n6H$r0J2<$N;M4pK\9)Dx$KJ,N`$7!"$=$l$>$l$KBP$7$FE,@Z$J;Y1gA`:n(B
;;	$B$r9T$J$&!#(B
;;		1. $B?75,F~NOA`:n(B
;;		2. $B4{F~NO%F%-%9%HJQ99A`:n(B
;;		3. $BO"4X(B($B;2>H(B)$B$N7k9g!&=$@5!&J,N%!&0\F0A`:n(B($B4^$`Jd40(B)
;;		4. $B%*%V%8%'%/%H$X$NK]LuA`:n!&6cL#A`:n(B
;;	
;;	$B$=$l$>$l$NA`:n$O0J2<$N%b%8%e!<%k$G9T$J$&!#(B
;;	
;;	1. epoi		epo input aider
;;	2. epoc		epo change operation 
;;	3. epor		epo relation resolver
;;	4. epop		epo process interface
;;	
;;[Attributes]
;;	
;;	$B%=!<%9$K8=$l$kJd402DG=$JB0@-$O0J2<$NDL$j(B
;;	
;;	* $B4D6-$N3+;O(B/$B=*N;(B
;;	* $B7+$jJV$7H"(B($BJq3g;R(B)
;;	* $B7+$jJV$7;XDj;R(B($B%$%F%l!<%?(B)
;;	* $B<1JL;R$N;2>H(B / $B<1JL;R$NDj5A(B
;;	+ $BCm<a5-9f(B
;;	
;;[Pairs]
;;	
;;	$BJ8=q$K$O0J2<$NO"4X$,B8:_$9$k!#(B
;;	
;;	* $BDj5A(B / $B;2>H(B
;;	* $B3+;O(B / $B=*N;(B
;;	* $BB>J8=q;2>H(B
;;	* $BHsJ8=q%G!<%?;2>H(B($B%W%m%;%95/F0Ey(B)
;;	
;;[./.eporc $B$N=q<0(B]	
;;
;;	$B:n6H%G%#%l%/%H%j$N(B .eporc $B%U%!%$%k$K!"$=$N>l=j$G$N(BEPO$B4XO"5!G=5/(B
;;	$BF0$KI,MW$J>pJs$rM?$($k$3$H$,$G$-$k!#=q<0$O0J2<$N$H$*$j!#(B
;;	
;;	$B%b%8%e!<%k(B-$B%?%0(B:
;;	
;;	
;;[Symbol Naming Rules]
;;	
;;	EPO$B%b%8%e!<%k72$GDj5A$9$k%7%s%\%k$O0J2<$N$h$&$KDj5A$9$k!#(B
;;	
;;	* $BA4$F$N(BEPO$B=>B0%7%s%\%k$N(B prefix $B$OI,$:(B "epo" $B$G;O$^$k!#(B
;;	* $B$=$N$&$A!"(Bepoi, epoc, epor, epop $B8GM-$N5!G=$dCM$K%P%$%s%I$5$l$?(B
;;	  $B%7%s%\%k$O!"$=$l$>$l$N%b%8%e!<%kL>$HF1$8(B prefix $B$H$J$k!#(B
;;	* $B$5$i$K(B delimiter $B$r2C$($F!"%7%s%\%k$N(B prefix $B$r40@.$9$k!#(B
;;	  o $B%$%s%?%i%/%F%#%V4X?t(B($B%3%^%s%I(B)$B$H!"%f!<%6%*%W%7%g%sJQ?t$O(B
;;	    delimiter $B$r(B "-" $B$H$9$k(B  $B"*(B prefix = "epo{,i,c,r,p}-"
;;	  o $B$=$NB>$N4X?t$O(B delimiter $B$r(B "*" $B$H$9$k(B
;;	     $B"*(B prefix = "epo{,i,c,r,p}*"
;;	  o $B$=$NB>$NJQ?t$O(B delimiter $B$r(B ":" $B$H$9$k(B
;;	     $B"*(B prefix = "epo{,i,c,r,p}:"
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
