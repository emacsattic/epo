;;; -*- Emacs-Lisp -*-
;;; General Library Functions for EPO
;;; (c)1999-2002 HIROSE, Yuuji [yuuji@ae.keio.ac.jp]
;;; $Id$
;;; Last modified Sat Oct 19 12:41:13 2002 on firestorm

(defvar epo*debug (string= (user-login-name) "yuuji"))
(defvar epo-quiet nil)

(defconst epo:frame-feature-p
  (and (fboundp 'make-frame) window-system))

(defvar epo:shell-c
  (or (and (boundp 'shell-command-switch) shell-command-switch)
      (and (boundp 'shell-command-option) shell-command-option)
      (and (string-match "command.com\\|cmd.exe\\|start.exe" shell-file-name)
	   "/c")
      "-c")
  "*Command option for shell")

;;;
;; List Manipulating functions
;;;
(defun epo*rassoc1 (elt alist)
  (catch 'found
    (while alist
      (if (equal elt (cdr (car alist)))
	   (throw 'found (car alist)))
      (setq alist (cdr alist)))))

(if (and (fboundp 'rassoc) (subrp (symbol-function 'rassoc)))
    (fset 'epo*rassoc (symbol-function 'rassoc))
  (fset 'epo*rassoc 'epo*rassoc1))

(defun epo*member1 (elt list)
  (catch 'found
    (while list
      (if (equal elt (car list))
	  (throw 'found list))
      (setq list (cdr list)))))

(if (and (fboundp 'member) (subrp (symbol-function 'member)))
    (fset 'epo*member (symbol-function 'member))
  (fset 'epo*member (symbol-function 'epo*member1)))

(defun epo*abbrev-file1 (file)
  (setq file (expand-file-name file))
  (if (string-match (concat "^" (regexp-quote (getenv "HOME")))
		    file)
      (concat "~" (substring file (match-end 0)))
    file))

(if (fboundp 'abbreviate-file-name)
    (fset 'epo*abbrev-file (symbol-function 'abbreviate-file-name))
  (fset 'epo*abbrev-file (symbol-function 'epo*abbrev-file1)))

;;;
;; Evaluating
;;;
(defun epo*get-value (exp &rest args-if-function)
  "Return the effective value from EXP.
If the EXP is assumed as function, give ARGS-IF-FUNCTION as argument(s)
to the function."
  (cond
   ((symbolp exp)
    (cond
     ((fboundp exp)		(apply exp args-if-function))
     ((boundp exp)		(symbol-value exp))
     (t exp)))
   ((listp exp)
    (cond
     ((eq (car exp) 'lambda)	(apply exp args-if-function))
     ((eq (car exp) 'eval)	(eval exp))
     (t exp)))
   (t
    exp)))

(defun epo*alist-get (key alist)
  (let (val)
    (setq val (epo*get-value (cdr (assq key alist))))
    (if (and (consp val) (symbolp (car val)) (integerp (cdr val)))
	(setq val (cdr (assq (cdr val) (epo*get-value (car val))))
	      val (cdr (assq key val))
	      val (epo*get-value val)))
    val))


(defun epo*and (&rest x)
  "Eval arguments X until one of them yields nil, then return nil.
The movement is the same as primitive C subroutine 'and'.
Built-in 'and is not applicable to apply/funcall.
So define the function."
  (catch 'and
    (let (y)
      (while x
	(or (car x) (throw 'and nil))
	(setq y x x (cdr x)))
    (throw 'and (car y)))))

;;;
;; Context-confirming Regular expression search
;;;
(defun epo*coco-evaluate-context (contexts)
  "Evaluate each of CONTEXTS until one of them yields nil, then return nil.
If none of them returns nil, return t."
  (let ((p (point))
	e				;each element
	how				;context type(car of e)
	val				;cdr of e
	cb				;cache for current-box
	(b0 (match-beginning 0)) (e0 (match-end 0)) (md (match-data))
	re-s skip op mf edge repptn)
    (unwind-protect
	(catch 'context
	  (while (setq e (car contexts))
	    ;;Care little bit about speed here...
	    (setq how (car e)
		  val (epo*get-value (cdr e)))
	    (goto-char p)
	    (or (cond
		 ((eq how 'box)		;box confirmation requires
		  (require 'epoi)	;language dependent iteration info
		  (setq cb (or cb (epoi*inner-box)))
		  (and cb
		       (setq cb (epo*alist-get
				 'type (epoi*get-box-alist (car cb))))
		       (setq cb (symbol-name cb))
		       (string< "" cb)
		       (string-match val cb)))
		 ((eq how '!box)
		  (require 'epoi)
		  (setq cb (or cb (epoi*inner-box)))
		  (and cb
		       (setq cb (epo*alist-get
				 'type (epoi*get-box-alist (car cb))))
		       (setq cb (symbol-name cb))
		       (string< "" cb)
		       (not (string-match val cb))))
		 ((memq how '(before !before))
		  (goto-char b0)
		  (if (re-search-backward val nil t)
		      (progn
			(goto-char (match-end 0))
			(skip-chars-forward " \t\n")
			(prog1
			    (funcall (if (eq how 'before)
					 '>=
				       '<)
				     (point) b0)
			  (goto-char p)))
		    (eq how '!before)))
		 ((memq how '(after !after))
		  (goto-char e0)
		  (if (re-search-forward val nil t)
		      (progn
			(goto-char (match-beginning 0))
			(skip-chars-backward " \t\n")
			(prog1
			    (funcall (if (eq how 'after)
					 '<=
				       '>)
				     (point) e0)
			  (goto-char p)))
		    (eq how '!after)))
		 ((memq how '(paren !paren upparen !upparen))
		  (if (> (car val) 0)
		      (progn
			(setq re-s 're-search-forward
			      mf   'match-beginning
			      skip 'skip-chars-backward
			      op (if (memq how '(paren upparen)) '<= '>))
			(goto-char e0))
		    (setq re-s 're-search-backward
			  mf   'match-end
			  skip 'skip-chars-forward
			  op (if (memq how '(paren upparen)) '>= '<))
		    (goto-char b0))
		  (prog1
		      (catch 'paren
			(condition-case err
			    (funcall (if (memq how '(paren !paren))
					 'forward-list
				       'up-list)
				     (car val))	;number
			  (error (throw 'paren (memq how '(!paren !upparen)))))
			(setq edge (point))
			(if (if (and (memq how '(upparen !upparen))
				     (< (car val) 0))
				(looking-at (epo*get-value (car (cdr val))))
			      (funcall
			       re-s (epo*get-value (car (cdr val))) nil t))
			    (progn
			      (goto-char (funcall mf 0))
			      (funcall skip " \t\n")
			      (funcall op (point) edge))
			  (memq how '(!paren !upparen))))
		    (goto-char p)))
		 ((numberp how)
		  (forward-line (if (> how 0) (1+ how) how))
		  (prog1
		      (funcall
		       (if (> how 0) 're-search-backward 're-search-forward)
		       val
		       (if (> how 0) e0 b0)
		       t)
		    (goto-char p)))
		 ((eq how 'exclude)
		  (store-match-data md)
		  (not (string-match (epo*get-value (car (cdr val)))
				     (epo*match-string (car val)))))
		 ((eq how 'require)
		  (store-match-data md)
		  (string-match (epo*get-value (car (cdr val)))
				(epo*match-string (car val))))
		 ((eq how 'context)
		  (goto-char b0)
		  (prog1
		      (save-restriction
			(narrow-to-region b0 e0)
			(epo*coco-re-search val nil t))
		    (goto-char p)))
		 ((eq how 'repeat-from)
		  (goto-char b0)
		  (setq edge (point)
			repptn (car (cdr val)))
		  (prog1
		      (catch 'ok
			(if (re-search-backward(epo*get-value (car val)) nil t)
			    (progn
			      (goto-char (match-end 0))
			      (skip-chars-forward " \t\n\r\t")
			      (while (< (point) edge)
				(cond
				 ((and (stringp repptn)
				       (looking-at repptn))
				  (goto-char (match-end 0)))	; ok
				 ((and (epo*ccrep repptn)
				       (epo*coco-looking-at repptn))
				  (goto-char (match-end 0)))	; ok
				 (t (throw 'ok nil))) ;ng
				(goto-char (match-end 0))
				(skip-chars-forward " \t\n\r\t"))
			      (throw 'ok t))))
		    (goto-char p)))
		 ((eq how 'return)
		  (cond
		   ((null val)
		    (setq md (match-data) ;update match-data!
			  b0 (match-beginning 0)
			  e0 (match-end 0))
		    (setq p (if (and (boundp 'direction)
				     (eq direction 'f))
				(match-end 0)
			      (match-beginning 0)))
		    (goto-char p)
		    t)
		   (t
		    (prog1
			(funcall
			 (if (eq (car val) 'forward)
			     're-search-forward
			   're-search-backward)
			 (epo*get-value (cdr val))
			 (if (eq (car val) 'forward)
			     (min (+ (point) 1000) (point-max))
			   (max (- (point) 1000) (point-min)))
			 t)
		      (setq md (match-data))))))
		 ((eq how 'or)
		  (prog1
		      (catch 'or
			(while val
			  (goto-char p)
			  (if (epo*coco-evaluate-context (car val))
			      (progn
				(setq md (match-data))
				(throw 'or t)))
			  (setq val (cdr val))))
		    (goto-char p))))
		;; If above condition returns nil, return nil
		(throw 'context nil))
	    (setq contexts (cdr contexts)))
	  ;;All context evaluation product non-nil, then return t
	  p)
      (store-match-data md))))

(defvar epo*coco-re-search-last-expression nil)
(defun epo*coco-re-search (direction expression &optional bound noerror)
  "For DIRECTION ('f or 'b), search EXPRESSION by context-confirming regexp.
Optional 3rd or later arguments BOUND, NOERROR are the same as of
re-search-forward re-search-backward.
The context-confirming regexp search is documented in epor.el.
Here is the brief documentation for context-specifier.
EXPRESSION := (RegexpString . Context-Alist)
Context-Alist := list of Context-Cons in any length
Context-Cons := is the form one of below;
 (box . BoxName)		Current position is in a box <BoxName>
 (!box . BoxName)		Current position is NOT in a box <BoxName>
 (NUM . Regexp)			<Regexp> is found within NUM lines below
 (-NUM . Regexp)		<Regexp> is found within NUM lines above
 (before . Regexp)		<Regexp> is found in left neighbor
 (!before . Regexp)		<Regexp> is NOT found in left neighbor
 (after . Regexp)		<Regexp> is found in left neighbor
 (!after . Regexp)		<Regexp> is NOT found in left neighbor
 (paren . (NUM Regexp))		<Regexp> is found after NUM parens
 (!paren . (NUM Regexp))	<Regexp> is NOT found after NUM parens
 (upparen . (NUM Regexp))	<Regexp> is found at NUM parens up
 (!upparen . (NUM Regexp))	<Regexp> is NOT found at NUM parens up
 (exclude . (NUM Regexp))	Matched group# NUM NOT matches with <Regexp>
 (require . (NUM Regexp))	Matched group# NUM matches with <Regexp>
 (context . (ARGUMENTS...)	Do coco-research again with ARGUMENTS
"
  (let ((f (cdr (assq direction '((f . re-search-forward)
				  (b . re-search-backward)))))
	(c (cdr expression))
	(p (point))
	r)
    (or f (error "epo*coco-re-search: DIRECTION must be 'f or 'b"))
    (setq epo*coco-re-search-last-expression expression)
    (or
     (catch 'found
       (while (setq r (funcall f (car expression) bound noerror))
	 (if (or (null c)
		 (epo*coco-evaluate-context c))
	     (throw 'found (point)))
	 (goto-char r)))		;reset the point to last found position
     (progn
       (goto-char p)
       ;;return nil
       nil))))

(defun epo*coco-re-search-list (direction exp-list &optional bound noerror)
  "Do epo*coco-re-search in DIRECTION for each EXP-LIST.
This is stupidly honest version.  Try search on all expressions, and
choose the list which produced the nearest result.
And matched ccre is stored in tht property 'matched-ccre of
symbol 'epo*coco-re-search-list.
The ideal algorithm is that
search conjunction of each regexp first, and check the list whose regexp
is matched, and evaluate its own context."
  (let ((p (point)) r md matched-ccre
	(nearest (if (eq 'f direction) (point-max) (point-min))))
    (put 'epo*coco-re-search-list 'matched-ccre nil)
    (while exp-list
      (goto-char p)
      (if (setq r (epo*coco-re-search direction (car exp-list) bound noerror))
	  (if (eq 'f direction)
	      (if (< r nearest)
		  (setq nearest r
			md (match-data)
			matched-ccre (car exp-list)))
	    (if (> r nearest)
		(setq nearest r
		      md (match-data)
		      matched-ccre (car exp-list)))))
      (setq exp-list (cdr exp-list)))
    (if md (progn (store-match-data md)
		  (put 'epo*coco-re-search-list 'matched-ccre matched-ccre)
		  (goto-char nearest)))))

(defun epo*coco-looking-at (ccre)
  "Return t if text after point matches with context-confirming regular expression CCRE.
This function modifies match-data."
  (and (looking-at (car ccre))
       (or (null (cdr ccre))
	   (epo*coco-evaluate-context (cdr ccre)))))

(defun epo*ccrep (s)
  "S seems to be context-confirming regexp, then retern t, else nil."
  (and (listp s) (stringp (car s))))

(defun epo*coco-re-search-forward (expression &optional bound noerror)
  (if (listp (car expression))
      (epo*coco-re-search-list 'f expression bound noerror)
    (epo*coco-re-search 'f expression bound noerror)))

(defun epo*coco-re-search-backward (expression &optional bound noerror)
  (if (listp (car expression))
      (epo*coco-re-search-list 'b expression bound noerror)
  (epo*coco-re-search 'b expression bound noerror)))

(defun epo*coco-re-search-forward-list (exp-list &optional bound noerror)
  (epo*coco-re-search-list 'f exp-list bound noerror))

(defun epo*coco-re-search-backward-list (exp-list &optional bound noerror)
  (epo*coco-re-search-list 'b exp-list bound noerror))

(defun epo-coco-re-search-again-forward (&optional backward)
  "Search last performed cc-regexp forward again."
  (interactive "P")
  (if (null epo*coco-re-search-last-expression)
      (message "No ccre search before")
    (let ((sfunc (if backward 'epo*coco-re-search-backward
	       'epo*coco-re-search-forward))
	  (lastccre epo*coco-re-search-last-expression))
    (cond
     ((funcall sfunc lastccre nil t))
     ((not (y-or-n-p
	    "No more match in this buffer.  Search for next buffer?"))
      (message "Stopped"))
     (t
      (let ((cb (current-buffer)) (bl (buffer-list)) later
	    (lang epo:current-language)
	    (p (point)) sp r)
	(while (and bl (not (eq (car bl) cb)))
	  (set-buffer (car bl))
	  (if (string= lang (epo-get-lang))
	      (setq later (cons (car bl) later)))
	  (setq bl (cdr bl)))
	;;Here, bl must be current buffer or nil.  Skip current-buffer.
	(setq bl (cdr bl))
	(if (catch 'found
	      (while bl
		(set-buffer (car bl))
		(if (string= lang (epo-get-lang))
		    (progn
		      (setq sp (point))
		      (goto-char (if backward (point-max) (point-min)))
		      (if (funcall sfunc lastccre nil t)
			  (throw 'found t)
			(goto-char sp))))
		(setq bl (cdr bl)))
	      (while later
		(set-buffer (car latter))
		(if (string= lang (epo-get-lang))
		    (progn
		      (setq sp (point))
		      (goto-char (if backward (point-max) (point-min)))
		      (if (funcall sfunc lastccre nil t)
			  (throw 'found t)
			(goto-char sp))))
		(setq later (cdr later))))
	    (progn
	      (epo*store-position-in-register p cb)
	      (epo*goto-buffer (current-buffer))
	      (epo*set-window-pretty-position))
	  (set-buffer cb)
	  (goto-char p)
	  (message "No more match for %s" (prin1-to-string lastccre)))))))))

(defun epo-coco-re-search-again-backward ()
  (interactive)
  (epo-coco-re-search-again-forward t))

;;;Functions for demonstration... :)
(defun ccre-search-forward (ccre &optional backward)
  (interactive "xCCRE: ")
  (let ((p (point)))
    (condition-case ()
	(funcall (if backward 'epo*coco-re-search-backward
		   'epo*coco-re-search-forward)
		 ccre)
      (error
       (goto-char p)
       (error "Search failed: %s" ccre)))))

(defun ccre-search-backward (ccre)
  (interactive "xCCRE: ")
  (ccre-search-forward ccre t))

;;;
;; Window Managing
;;;

(cond
 ((fboundp 'screen-height)
  (fset 'epo*screen-height 'screen-height)
  (fset 'epo*screen-width 'screen-width))
 ((fboundp 'frame-height)
  (fset 'epo*screen-height 'frame-height)
  (fset 'epo*screen-width 'frame-width))
 (t (error "I don't know how to run EPO on this Emacs...")))

(defun epo*window-list ()
  (let*((curw (selected-window)) (win curw) (wlist (list curw)))
    (while (not (eq curw (setq win (next-window win))))
      (or (eq win (minibuffer-window))
	  (setq wlist (cons win wlist))))
    wlist))

(defvar epo-default-pop-window-height 10
    "Default typesetting buffer height.
If integer, sets the window-height of typesetting buffer.
If string, sets the percentage of it.
If nil, use default pop-to-buffer.")

;;;###autoload
(defun epo*smart-split-window (height)
  "Split current window wight specified HEIGHT.
If HEIGHT is number, make a new window that has HEIGHT lines.
If HEIGHT is string, make a new window that occupies HEIGT % of screen height.
Otherwise split window conventionally."
  (if (one-window-p t)
      (split-window
       (selected-window)
       (max
        (min
         (- (epo*screen-height)
            (if (numberp height)
                (+ height 2)
              (/ (* (epo*screen-height)
                    (string-to-int height))
                 100)))
         (- (epo*screen-height) window-min-height 1))
        window-min-height))))

(defun epo*set-window-pretty-position (&optional pos plusminus)
  "Set POS's window's position easy to view.
If optional second argument PLUSMINUS is minus number,
set the point's window position in the lower part instead of upper."
  (setq pos (or pos (point)))
  (let ((p (point)))
    (goto-char pos)
    (beginning-of-line)
    (set-window-start (selected-window) (point))
    (cond
     ((pos-visible-in-window-p (point-max))
      ;;if End of buffer is visible, locate the EOB at the end of window
      (goto-char (point-max))
      (recenter -2)
      (goto-char p))
     (t (if (and (numberp plusminus)
		 (> plusminus 0))
	    (recenter (max (- (window-height) 6) (* (/ (window-height) 4) 3)))
	  (recenter (min 5 (/ (window-height) 4))))
	(goto-char p)))))

;;;
;; Buffer Operations
;;;
(defun epo*goto-buffer (buffer)
  "Switch to BUFFER.  If already visible, select window or frame of BUFFER."
  (cond
   ((get-buffer-window buffer)
    (select-window (get-buffer-window buffer)))
   ((and epo-select-frame
	 (get-buffer-window buffer t))
    (raise-frame
     (select-frame (window-frame (get-buffer-window buffer t))))
    (select-window (get-buffer-window buffer (selected-frame)))
    (set-mouse-position
     (if (featurep 'xemacs) (selected-window) (selected-frame))
     0 0))
   (t
    (switch-to-buffer buffer))))
    
;;;###autoload
(defun epo*showup-buffer (buffer &optional select)
  "Make BUFFER show up in certain window (except selected window).
Non-nil for optional argument SELECT keeps selection to the target window."
  (let (w)
    (or
     ;;if already visible
     (if epo:frame-feature-p
	 (if (setq w (get-buffer-window buffer t))
	     (if select
		 (progn
		   (raise-frame (select-frame (window-frame w)))
		   (set-mouse-position (if (featurep 'xemacs)
					   (selected-window)
					 (selected-frame))
				       0 -1)
		   (select-window w))
	       w))
       ;;no frames
       (if (setq w (get-buffer-window buffer))
	   (if select (select-window w) w)))
     ;;not visible
     (let ((sw (selected-window))
	   (wlist (epo*window-list)))
       (cond
	((eq (current-buffer) (get-buffer buffer)) nil)
	((one-window-p)
	 (epo*smart-split-window epo-default-pop-window-height)
	 (select-window (next-window nil 1))
	 (switch-to-buffer (get-buffer-create buffer)))
	((= (length wlist) 2)
	 (select-window (get-lru-window))
	 (switch-to-buffer (get-buffer-create buffer)))
	(t   ;more than 2windows
	 (select-window (next-window nil 1))
	 (switch-to-buffer (get-buffer-create buffer))))
       (or select (select-window sw))))))

(defun epo*buffer-live-p-1 (buffer)
  (let ((cb (current-buffer)))
    (if (get-buffer buffer)
	(unwind-protect
	    (condition-case err
		(set-buffer (get-buffer buffer))
	      (error nil))
	  (set-buffer cb)))))

(if (fboundp 'buffer-live-p)
    (fset 'epo*buffer-live-p (symbol-function 'buffer-live-p))
  (fset 'epo*buffer-live-p (symbol-function 'epo*buffer-live-p-1)))

(defvar epo:temporary-buffers nil)
(defun epo*register-temp-buffer (buffer)
  "Register BUFFER as temporal use buffer for current-buffer."
  (if (and (not (memq buffer epo:temporary-buffers))
	   (get-buffer buffer))
      (setq epo:temporary-buffers (cons buffer epo:temporary-buffers))))

(defun epo*kill-temp-buffers ()
  (save-excursion
    (let (buf)
      (while (setq buf (car epo:temporary-buffers))
	(if (get-buffer buf)
	    (kill-buffer buf))
	(setq epo:temporary-buffers (cdr epo:temporary-buffers))))))

(defun epo*epo-buffer-list (&optional predicate)
  "Return a list of all buffers whose language is the same as current-buffer.
Optional argument PREDICATE limits the buffers by that function."
  (let ((cb (current-buffer)) (lang (epo-get-lang)))
    (unwind-protect
	(delq nil
	      (mapcar
	       (function (lambda (s)
			   (set-buffer s)
			   (if (string= lang (epo-get-lang))
			       (if (null predicate)
				   s
				 (if (funcall predicate) s)))))
	       (buffer-list)))
      (set-buffer cb))))

(if (fboundp 'file-name-sans-extension)
    (fset 'epo*file-name-root 'file-name-sans-extension)
  (fset 'epo*file-name-root
	(function
	 (lambda (fn)
	   (let ((md (match-data))
		 (file (file-name-sans-versions (file-name-nondirectory fn)))
		 directory)
	     (unwind-protect
		 (if (string-match "\\.[^.]*\\'" file)
		     (if (setq directory (file-name-directory fn))
			 (expand-file-name
			  (substring file 0 (match-beginning 0))
			  directory)
		       (substring file 0 (match-beginning 0)))
		   fn)
	       (store-match-data md)))))))


;;;
;; String
;;;
(if (fboundp 'buffer-substring-no-properties)
    (fset 'epo*buffer-substring 'buffer-substring-no-properties)
  (fset 'epo*buffer-substring 'buffer-substring))

;;;###autoload
(defun epo*match-string (n &optional m)
  "Return (buffer-substring (match-beginning n) (match-beginning m)).
If N (or M) is given by a list, replace it the integer in a list that
returns non-nil first."
  (if (listp n)
      (let ((n2 n))
	(while n2
	  (if (match-beginning (car n2))
	      (setq n (car n2) n2 nil)
	    (setq n2 (cdr n2))))))
  (if (consp m)
      (let ((m2 m))
	(while m2
	  (if (match-beginning (car m2))
	      (setq m (car m2) m2 nil)
	    (setq m2 (cdr m2))))))
  (if (match-beginning n)
      (epo*buffer-substring (match-beginning n)
			    (match-end (or m n)))))


(defun epo*replace-format-sub (string format repl)
  (let ((beg (or (string-match (concat "^\\(%" format "\\)") string)
		 (string-match (concat "[^%]\\(%" format "\\)") string)))
	(len (length format)))
    (if (null beg) string ;no conversion
      (concat
       (substring string 0 (match-beginning 1)) (or repl "")
       (substring string (match-end 1))))))

;;;###autoload
(defun epo*replace-format (string format repl)
  "In STRING, replace first appearance of FORMAT to REPL;
as if function `format' does.  FORMAT does not contain `%'"
  (let ((md (match-data)) (ans string) (case-fold-search nil))
    (while (not (string=
		 ans (setq string (epo*replace-format-sub ans format repl))))
      (setq ans string))
    (store-match-data md)
    string))

;;;###autoload
(defun epo*current-box (opener closer &optional here predicate)
  "Detect current environment box name, point, column in a list.
Box opening cc-regexp is OPENER, and closing cc-regexp CLOSER.
Optional third argument HERE specifies the point which defaults to (point).
Optional fourth argument PREDICATE is used to check the found opener/closer
is effective or not"
  (if (epo*ccrep (car closer))
      ;; Search box opener by regexp
      (let ((nest 0) (e (or here (point)))
	    (patn (append opener closer))
	    s p c)
	(save-excursion
	  (goto-char e)
	  (while (and (>= nest 0)
		      (epo*coco-re-search-backward-list patn nil t))
	    (if (or (null predicate) (funcall predicate))
		(progn
		  (setq s (epo*match-string 0))
		  (if (memq (get 'epo*coco-re-search-list 'matched-ccre)
			    closer)	;if closer was found
		      (setq nest (1+ nest))
		    ;;if opener was found...
		 ;;Here, it might be desirable to check the found opener
		    ;;is nestable or not.  If not nestable, consecutive
		    ;;opener should be ignored.
		    (setq nest (1- nest)
			  p (point)
			  c (current-column))
		    ))))
	  (if (>= nest 0)
	      nil
	    (list s p c))))
    ;; Search box opener by regexp and paired parentheses
    (let*((e (or here (point)))
	  (close (cdr closer))
	  (open (cdr (assq (string-to-char close)
			   '((?} . "{") (?\] . "[") (?\) . "(") (?> . "<")))))
	  m0 e0 s p c)
      (or open
	  (error "Parenthesis type %s not supported" close))
      (save-excursion
	(goto-char e)
	(catch 'opener
	  (while (epo*coco-re-search-backward-list opener nil t)
	    (setq s (epo*match-string 0)
		  m0 (match-beginning 0)
		  p m0
		  c (current-column)
		  e0 (match-end 0))
	    (or (search-forward open e0 t)
		(error
		 "Iterator box opener pattern should contain parenthesis"))
	    (goto-char (match-beginning 0))
	    (condition-case err
		(forward-sexp 1)
	      (error
	       ;;if unclosed parentheses found, it is the current box opener.
	       (throw 'opener (list s p c))))
	    (if (> (point) e)
		(throw 'opener (list s p c)))
	    (goto-char m0)))))))

(defun epo*check-not-in-comment ()
  "Check if the found point in last search is not after comment starter."
  (if (or (null comment-start) (string= "" comment-start))
      t
    (save-excursion
      (let ((m0 (match-beginning 0)) (md (match-data)))
	(beginning-of-line)
	(prog1
	    (not (re-search-forward comment-start m0 t))
	  (store-match-data md))))))

;;;###autoload
(defun epo*point (type)
  "Return the point of TYPE in current buffer.
TYPE should be one of below;
 'bol	beginning-of-line
 'eol	end-of-line"
  (let ((p (point)))
    (funcall (cdr (assq type '((bol . beginning-of-line)
			       (eol . end-of-line)))))
    (prog1 (point)
      (goto-char p))))

;;;
;; Directory Operation
;;;
(defun epo*project-root ()
  "Return the current buffer's project root directory."
  (let ((cb (current-buffer)) (bl (epo*epo-buffer-list)) dir-flag
	(d default-directory) d2)
    (while bl
      (set-buffer (car bl))
      (or (epo*member default-directory dir-flag)
	  (setq dir-flag (cons default-directory dir-flag)))
      (setq bl (cdr bl)))
    (set-buffer cb)
    (or
     (catch 'root
       (while (string< "/" (setq d2 (file-name-directory (substring d 0 -1))))
	 (or (epo*member d2 dir-flag)
	     (throw 'root (substring d 0 -1)))
	 (setq d d2)))
     ".")))

;;;
;; File operation
;;;
(defun epo*call-command-in-buffer (command buffer)
  "Call COMMAND in BUFFER."
  (set-buffer (get-buffer-create buffer))
  (erase-buffer)
  (call-process shell-file-name nil buffer t epo:shell-c command))

(defvar epo:fgrep-l "fgrep -l"
  "*Command name of fixed grep.")
(defun epo*process-output-list (command buffer &optional dir predicate alistp)
  "Return the process COMMMAND's output as a list using BUFFER.
Optional third argument DIR is used as a execution directory.
Optional fourth argument PREDICATE limits the element.
Non-nil for optional fifth argument ALISTP creates alist instead of list."
  (let ((cb (current-buffer))
	list
	(tmpbuf (get-buffer-create buffer)))
    (set-buffer tmpbuf)
    (if dir (cd (setq default-directory dir)))
    (epo*call-command-in-buffer command tmpbuf)
    (goto-char (point-min))
    (while (not (eobp))
      (setq file (epo*buffer-substring
		  (point) (progn (end-of-line) (point))))
      (if (or (null predicate)
	      (funcall predicate file))
	  (setq list (cons (if alistp (list file) file) list)))
      (forward-line 1))
    (set-buffer cb)
    (nreverse list)))

(defun epo*set-alist (sym key value)
  (set sym (cons (cons key value)
		 (delq (assoc key (symbol-value sym))
		       (symbol-value sym)))))

(defvar epo*process-runner-object '((id . 0)))

(defun epo*process-runner (method command &optional dir predicate)
  "When METHOD eq 'new, run COMMAND in BUFFER and return immediately.
When METHOD eq 'next, return each line of PROCESS's output."
  (let*((procid (format "%s:%s" (setq dir (or dir default-directory)) command))
	(prcalist (cdr (assoc procid epo*process-runner-object)))
	(buf (cdr (assq 'buffer prcalist)))
	(marker (cdr (assq 'marker prcalist)))
	(endmarker (cdr (assq 'endmarker prcalist)))
	(proc (cdr (assq 'process prcalist)))
	(pred (cdr (assq 'predicate prcalist)))
	(cb (current-buffer)))
    (cond
     ((eq method 'next)
      (let (line p)
	(unwind-protect
	    (catch 'next
	      (set-buffer buf)
	      (goto-char marker)
	      (setq p (point-max))
	      (while (or (< (point) endmarker)
			 (if (eq (process-status proc) 'run)
			     (progn
			       (message "Waiting for next output...")
			       (while (and (eq (process-status proc) 'run)
					   (= p (point-max)))
				 (goto-char marker)
				 (sit-for 0.1))
			       (/= p (point-max)))))
		(setq line (epo*buffer-substring
			    (goto-char marker)
			    (prog2
				(end-of-line)
				(point)
			      (forward-line 1)
			      ;(set-marker marker (point))
			      (epo*set-alist 'prcalist 'marker (point))
			      (epo*set-alist 'epo*process-runner-object
					     procid prcalist)
			      )))
		(if (or (null pred)
			(funcall pred line))
		    (throw 'next line))))
	  (set-buffer cb))))
     ((memq method '(new reset rewind))
      (setq dir (or dir default-directory))
      (if (or (eq method 'new) (null prcalist)
	      (not (get-buffer buf)) (not (epo*buffer-live-p buf)))
	  (let ((id (cdr (assq 'id epo*process-runner-object))))
	    (if prcalist
		(progn
		  ;;(set-marker (cdr (assq 'marker prcalist)) nil)
		  (delete-process (cdr (assq 'process prcalist)))
		  (setq prcalist nil)))
	    (setq buf (get-buffer-create
		       (format " *EPO-proc-%d*" id))
		  prcalist (cons (cons 'buffer buf) prcalist)
		  prcalist (cons (cons 'predicate predicate) prcalist))
	    (epo*register-temp-buffer buf)
	    (save-excursion
	      (set-buffer buf)
	      (cd (setq default-directory dir))
	      (erase-buffer)
	      ;(setq marker (point-marker))
	      ;(insert "\n")
	      (setq proc (start-process
			  (format "EPOprocess:%d" id)
			  buf
			  shell-file-name
			  epo:shell-c
			  command)
		    id (1+ id))
	      (set-marker (process-mark proc) (point-max))
	      (setq endmarker (point-marker))
	      (while (= (point-min) (process-mark proc))
		(sit-for 0.1))
	      (epo*set-alist 'epo*process-runner-object 'id id)
	      (epo*set-alist 'prcalist 'buffer buf)
	      ;;(epo*set-alist 'prcalist 'marker marker)
	      (epo*set-alist 'prcalist 'marker (point-min))
	      (epo*set-alist 'prcalist 'endmarker endmarker)
	      (epo*set-alist 'prcalist 'process proc)
	      (epo*set-alist 'prcalist 'predicate predicate)
	      (epo*set-alist 'epo*process-runner-object
			     procid prcalist))))
      (setq buf (cdr (assq 'buffer prcalist))
	    marker (cdr (assq 'marker prcalist))
	    pred (cdr (assq 'predicate prcalist)))
      (set-buffer buf)
      ;;(set-marker marker (point-min))
      (epo*set-alist 'prcalist 'marker (point-min))
      (epo*set-alist 'epo*process-runner-object procid prcalist)
      (set-buffer cb)
      ;;Finally return object
      (list
       'lambda '(method)
       (list
	'epo*process-runner 'method command dir)))
   ;;Object deconstruction
     ((memq method '(delete destroy))
      (if (processp proc) (delete-process proc))
      (if (get-buffer buf)
	  (progn
	    (set-marker marker nil)
	    (kill-buffer buf)))
      (setq epo*process-runner-object
	    (delq prcalist epo*process-runner-object))))))

(defun epo*fgrep-l (pattern dir &optional ext)
  "Return the file list which has the PATTERN lines in DIR."
  (let*((d (expand-file-name dir))
	(inode (nth 10 (file-attributes d)))
	(cb (current-buffer)) p  result (bl (epo*epo-buffer-list))
	(tmpbuf (get-buffer-create " *fgrep-l*")))
    (if (not (file-directory-p dir))
	(progn (message "Directory: %s not found." dir)
	       nil)
      (message "Search pattern `%s' in %s..." pattern dir)
      (setq result
	    (epo*process-output-list
	     (format "%s '%s' %s/*%s" epo:fgrep-l pattern d (if ext ext ""))
	     tmpbuf
	     nil
	     '(lambda (s)
		(and (file-exists-p s)
		     (not (file-directory-p s))
		     (not (backup-file-name-p s))
		     (not (string-match "#.*#$" s))))))
      ;;Append buffers which has PATTERN lines but not in `from-fgrep'
      (while bl
	(set-buffer (car bl))
	(if (= inode (nth 10 (file-attributes default-directory)))
	    (progn
	      (setq p (point))
	      (goto-char (point-min))
	      (if (and (search-forward pattern nil t)
		       (not (epo*member (buffer-file-name) result)))
		  (setq result (cons (buffer-file-name) result)))
	      (goto-char p)))
	(setq bl (cdr bl)))
      (message "Search pattern `%s' in %s...Done" pattern dir)
      (set-buffer cb)
      (delq nil result))))

(defvar epo*all-dirs-use-find (eq system-type 'berkeley-unix))
(defun epo*all-dirs (topdir)
  "Return all directories under TOPDIR."
  (or (file-directory-p topdir)
      (error "Directory %s does not exist." topdir))
  (cond
   (epo*all-dirs-use-find
    (epo*process-output-list
     (format "find %s -type d -print" topdir) " *find-type-d*"))
   (t
    (let ((files (directory-files topdir)) f visited dirs)
      (while (setq f (car files))
	(cond
	 ((not (file-directory-p (expand-file-name f topdir))) nil)
	 ((string-match "^\\.\\.?" f) nil)
	 (t
	 (setq dirs (append
		     dirs
		     (epo*all-dirs (format "%s/%s" topdir f))))))
	(setq files (cdr files)))
      (cons topdir dirs)))))

(defvar epo*lister-object nil)
(defun epo*lister (method id &optional initlist predicate)
  (let*((lalist (cdr (assoc id epo*lister-object)))
	(list (cdr (assq 'list lalist)))
	(pred (cdr (assq 'predicate lalist)))
	(n (cdr (assq 'n lalist)))
	i)
    (cond
     ((memq method '(new rewind))
      (epo*set-alist 'epo*lister-object
		     id
		     (list
		      (cons 'list initlist)
		      (cons 'predicate predicate)
		      (cons 'n 0)))
      (list
       'lambda '(method) (list 'epo*lister 'method id)))
     ((eq method 'next)
      (catch 'next
	(while (setq i (nth n list))
	  (if (or (null pred)
		  (funcall pred i))
	      (progn
		(epo*set-alist 'lalist 'n (1+ n))
		(epo*set-alist 'epo*lister-object id lalist)
		(throw 'next i)))
	  (setq n (1+ n)))))
     ((eq method 'delete)
      (setq epo*lister-object (delq lalist epo*lister-object))))))

(defun epo*dir-lister (dir &optional no-recursive)
  (if epo*all-dirs-use-find
      (epo*process-runner
       'rewind
       (if t
	   (format "find %s -type d %s-print"
		   (or dir ".")
		   (if no-recursive "-maxdepth 0 " ""))
	 (format "./slowlister %s" (or dir ".")))
       )
    (if no-recursive
	(epo*lister 'new (concat "NR:" (expand-file-name dir))
		    (directory-files dir)
		    '(lambda (s) (file-directory-p s)))
      (epo*lister 'new (expand-file-name dir)
		  (epo*all-dirs dir)
		  '(lambda (s) (file-directory-p s))))))

(defun epo*file-lister (dir)
  (if epo*all-dirs-use-find
      (epo*process-runner
       'rewind (format "ls -a %s" (or dir ".")))
    (epo*lister 'new (expand-file-name dir)
		(directory-files dir)
		'(lambda (s) (file-exists-p s)))))

(defun epo*function-recurse-dirs (function dirlist &optional norecursive)
  "Evaluate FUNCTION at all directories under all directories in DIRLIST."
  (let ((g1 "Searching directories under %s...%s")
	dir-lister d visited)
    (catch 'enough
      (while dirlist
	(setq dir-lister (epo*dir-lister (car dirlist) norecursive))
	(while (setq d (funcall dir-lister 'next))
	  (if (epo*member d visited)
	      nil
	    (or epo-quiet (message d))
	    (setq visited (cons d visited))
	    (funcall function d)))
	(setq dirlist (cdr dirlist))))))

(defun epo*match-files (pattern dirlist)
  "Search files which has line(s) matches with PATTERN for DIRLIST."
  (epo*function-recurse-dirs
   (function
    (lambda (d)
      ;;??????
      ()))))

(defun epo*coco-re-search-all-files (keyword ccr dirlist &optional ext norec)
  (let ((cb (current-buffer)) (p (point)) d r visited)
    (if norec
	(setq r (catch 'enough		;Search not recursively
		  (while (setq d (car dirlist))
		    (if (epo*member d visited)
			nil
		      (setq visited (cons d visited))
		      (epo*fgrep-and-coco-re-search d keyword ccr ext))
		    (setq dirlist (cdr dirlist)))))
      (setq r (epo*function-recurse-dirs
	       (list
		'lambda '(d)		;one argument, directory
		(list
		 'epo*fgrep-and-coco-re-search
		 'd keyword (list 'quote ccr) (list 'quote ext)))
	       dirlist norec)))
    (if r
	(progn
	  (set-buffer cb)
	  (epo*store-position-in-register)
	  (epo*goto-buffer (car r))
	  (goto-char (match-beginning 0))
	  (if (epo-get-lang) (epo-mode 1))
	  (if (pos-visible-in-window-p) nil
	    (epo*set-window-pretty-position))))))

(defvar epo*fgrep-and-coco-re-search-skip-opened t
  "If non-nil, skip already opened files.")
(defun epo*fgrep-and-coco-re-search (dir keyword ccr &optional ext)
  (let ((files (epo*fgrep-l keyword dir ext))
	(cb (current-buffer))
	(hilit-auto-highlight nil)
	f open p)
    (unwind-protect
	  (if epo*fgrep-and-coco-re-search-skip-opened
	      (while (setq f (car files))
		(if (get-file-buffer f)
		    ()
		  (set-buffer (find-file-noselect f))
		  (goto-char (point-min))
		  (if (epo*coco-re-search-forward ccr nil t)
		      (throw 'enough (cons (current-buffer)
					   (goto-char (match-beginning 0)))))
		  (kill-buffer (current-buffer)))
		(setq files (cdr files)))
	    (while (setq f (car files))
	      (if (setq open (get-file-buffer f))
		  (set-buffer open)
		(set-buffer (find-file-noselect f)))
	      (setq p (point))
	      (goto-char (point-min))
	      (if (epo*coco-re-search-forward ccr nil t)
		  (throw 'enough (cons (current-buffer)
				       (goto-char (match-beginning 0)))))
	      (if open
		  (goto-char p)
		(kill-buffer (current-buffer)))
	      (setq files (cdr files))))
	(set-buffer cb))))

(defvar epo:referer nil
  "File name from which current file was visited")
(defun epo*find-file (file dirlist &optional ext recursive)
  "Find FILE all directories in DIRLIST recursively.
Optional third argument EXT given, search file name with that extension."
  (let ((cb (current-buffer)))
    (cond
     ;;(1)search strictly match files
     ((get-file-buffer file)
      (switch-to-buffer (get-file-buffer file)))
     ((and ext (get-file-buffer (concat file ext)))
      (switch-to-buffer (get-file-buffer (concat file ext))))
     ;;(2)search only in filename part
     ((get-buffer file)
      (switch-to-buffer (get-buffer file)))
     ((and ext (get-buffer (concat file ext)))
      (switch-to-buffer (get-buffer (concat file ext))))
     ;;Search all element of load-path recursively.
     (recursive
      (let ((r (catch 'found
		 (unwind-protect
		     (epo*function-recurse-dirs
		      (if (null ext)
			  (function
			   (lambda (d)
			     (if (file-exists-p (expand-file-name file d))
				 (throw 'found d))))
			(function
			 (lambda (d)
			   (if (or (file-exists-p
				    (expand-file-name file d))
				   (file-exists-p
				    (expand-file-name (concat file ext) d)))
			       (throw 'found d)))))
		      dirlist)))))
	(if r
	    (if (file-exists-p (expand-file-name file r))
		(find-file (expand-file-name file r))
	      (find-file (expand-file-name (concat file ext) r)))
	  (message "File: %s not found in any possible directories." file))))
     ;;Search only for each element of load-path itself.
     (t
      (let (f)
	(catch 'done
	  (while dirlist
	    (if (or (file-exists-p
		     (setq f (expand-file-name file (car dirlist))))
		    (and ext
			 (file-exists-p
			  (setq f (expand-file-name
				   (concat file ext) (car dirlist))))))
		(throw 'done
		       (find-file f)))
	    (setq dirlist (cdr dirlist)))
	  (message "File: %s not found in any possible directories." file)))))
    (if (and (not (eq cb (current-buffer))) (epo-get-lang))
	(epo-mode 1))))

(defun epo*foreach-files (dirlist pattern function)
  (let ((cb (current-buffer)) (hilit-auto-highlight nil))
    (unwind-protect
	(epo*function-recurse-dirs
	 (list
	  'lambda '(dir)
	  (list
	   'let (list (list 'files (list 'epo*fgrep-l pattern 'dir)))
	      (list 'while 'files
		    '(set-buffer (find-file-noselect
				  (expand-file-name (car files) dir)))
		    (list 'funcall function)
		    '(setq files (cdr files)))))
	 dirlist)
      (set-buffer cb))))

;;;
;; Keymap
;;;
(defun epo*function-key-description (function)
  (key-description
   (or (car (where-is-internal function))
       (format "M-x %s RET" function))))

;;;
;; Time
;;;
(defun epo*current-time1 ()
  "Return the current time, as the number of seconds since 1970-01-01 00:00:00.
The time is returned as a list of two integers.  The first has the
most significant 16 bits of the seconds, while the second has the
least significant 16 bits."
  (let ((tmpf (make-temp-name "/tmp/epo-")))
    (write-region (point-min) (point-min) tmpf nil 'no-msg)
    (prog1 (nth 5 (file-attributes tmpf))
      (delete-file tmpf))))

(defun epo*current-time2 ()
  "Return the current time, as the number of seconds since 1970-01-01 00:00:00.
The time is returned as a list of two integers.  The first has the
most significant 16 bits of the seconds, while the second has the
least significant 16 bits."
  (let ((ct (current-time)))
    (setcdr (cdr ct) nil)
    ct))

(if (fboundp 'current-time)
    (fset 'epo*current-time (symbol-function 'epo*current-time2))
  (fset 'epo*current-time (symbol-function 'epo*current-time1)))

(defun epo*time< (t1 t2)
  "Return t if the time T1 is older than time T2.
Time is in the form returned by epo*current-time."
  (or (and (equal (car t1) (car t2)) (< (nth 1 t1) (nth 1 t2)))
      (< (car t1) (car t2))))

;;;
;; flash-string from rp-describe-function.el
;;;
(defun epo*flash-string (STRING)
  "Momentarily display STRING in the buffer; erase it on the next keystroke.
The string is displayed starting on the line after point.  The window is
recentered if necessary to make the whole string visible.  If the window isn't
large enough, at least you get to read the beginning."
  (let ((buffer-read-only nil)
        (modified (buffer-modified-p))
        (name buffer-file-name)
        insert-start
        insert-end)
    (unwind-protect
        (progn
          (save-excursion
            ;; defeat file locking... don't try this at home, kids!
            (setq buffer-file-name nil)
            ;;(forward-line 1)
            (setq insert-start (point))
            (insert "\n" STRING)
            (setq insert-end (point)))
          ; make sure the whole string is visible
          (if (not (pos-visible-in-window-p insert-end))
            (recenter (max 0
                           (- (window-height)
                              (count-lines insert-start insert-end)
                              2))))
          (message "Type %s to continue editing..."
                   (single-key-description ?\ ))
          (let ((char (read-char)))
            (or (eq char ?\ )
		(if (and (fboundp 'character-to-event)
			 (featurep 'xemacs))
		    (setq unread-command-event (character-to-event char))
		  (setq unread-command-char char))
                (setq unread-command-char char))))
      (if insert-end
          (save-excursion
            (delete-region insert-start insert-end)))
      (setq buffer-file-name name)
      (set-buffer-modified-p modified))))

(provide 'epolib)
