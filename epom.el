;;; -*- Emacs-Lisp -*-
;;; EPO Menu Definition
;;; (c)2001-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; $Id$
;;; Last modified Mon Feb  4 13:13:19 2002 on firestorm

;;	2001/2/21 作成開始
;;	
;;[Commentary]
;;	
;;	EPOのメニュー機能定義部
;;	

(defvar epo-mode-menu-map (make-sparse-keymap "EPO"))

;; Utility funcs
(defun epo*define-menu (keymap bindlist)
  "Define KEYMAP(symbol)'s menu-bindings according to BINDLIST.
KEYMAP should be a quoted symbol of newly allocated keymap.
BINDLIST consists of binding list.  Each element is as follows.

 '(menusymbol DOC_String . contents)

CONTENTS is one of lambda-form, interactive function, or other keymap.
See yatex19.el for example."
  (cond
   ((featurep 'xemacs)
    (let (name)
      (if (keymapp (symbol-value keymap))
	  (progn
	    (setq name (keymap-name (symbol-value keymap)))
	    (set keymap nil))
	(setq name (car (symbol-value keymap)))
	(set keymap (cdr (symbol-value keymap))))
      (mapcar
       (function
	(lambda (bind)
	  (setq bind (cdr bind))
	   (if (eq (car-safe (cdr bind)) 'lambda)
	       (setcar (cdr bind) 'progn))
	   (if (stringp (car-safe (cdr bind)))
	       (set keymap
		    (cons (cdr bind) (symbol-value keymap)))
	     (set keymap
		  (cons (vector (car bind) (cdr bind) t)
			(symbol-value keymap))))))
       bindlist)
      (set keymap (cons name (symbol-value keymap)))))
   (t
    (mapcar
     (function
      (lambda (bind)
	(define-key (symbol-value keymap) (vector (car bind)) (cdr bind))))
     bindlist))))


;;Definitions
(define-key epo-mode-map [menu-bar epo]
  (cons "EPO" epo-mode-menu-map))
(defvar epo-mode-menu-map-process (make-sparse-keymap "Process"))

(defun epom*create-menu-process ()
  (let ((process-list (epop*get-cur-lang-alist 'process))
	element key label menu)
    (while process-list
      (setq element (car process-list)
	    key (car element)
	    label (car (cdr (assq 'command element))))
      (setq menu
	    (cons
	     (list (intern label)
		   label
		   'lambda
		   nil
		   '(interactive)
		   (list 'epop*start-processor
			 (list 'quote (cdr element))
			 nil))
	     menu))
      (setq process-list (cdr process-list)))
    (epo*define-menu
     (make-local-variable 'epo-mode-menu-map-process)
     (nreverse menu))))

(defvar epo-mode-menu-map-structure (make-sparse-keymap "Structure"))
(defun epom*create-menu-structure ()
  (let ((structure-list (epop*get-cur-lang-alist 'structure))
	element key label menu table tsymbol)
    (while structure-list
      (setq element (car structure-list)
	    key (car element)
	    label (cdr (assq 'type element))
	    table (epo*alist-get 'table element))
      (if table
	  (progn
	    (setq tsymbol (make-local-variable
			   (intern (format "epo:menu-structure-%s"
					   (symbol-name label)))))
	    (set tsymbol (
      (setq menu
	    (cons
	     (list label
		   (symbol-name label)
		   'lambda
		   nil
		   '(interactive)
		   (if table
		       (list tsymbol
			     (symbol-name label)
			     
		   (list 'epoi*structure-input
			 (list 'quote element)
			 nil)
		   )
	     menu))
      (setq structure-list (cdr structure-list)))
    (epo*define-menu
     (make-local-variable 'epo-mode-menu-map-structure)
     (nreverse menu))))

(defun epom*setup-menu ()
  (epom*create-menu-process)
  (epom*create-menu-structure)
  (epo*define-menu
   'epo-mode-menu-map
   (nreverse
    (list
     (cons (list 'process "Process")
	   (cons "Process" epo-mode-menu-map-process))
     (cons (list 'structure "Structure")
	   (cons "Structure" epo-mode-menu-map-structure))))))
   
  
