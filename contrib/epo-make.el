;;; -*- Emacs-Lisp -*-
;;; EPO Make dependent settings
;;; (c) 2002 by Toshikazu Ando <ando@park.ruru.ne.jp>
;;; Created: 2002 Apr 20
;;; $Lastupdate: Wed May 01 10:13:36 2002 $ on inspire.

;;[Commentary]
;;	
;;	EPO -- Makefile dependent settings
;;

;;;
;; Variables for Input Aider(EPOI)
;;;
(defun epo-make-target (&optional arg1 arg2 dummy)
  (let ((word) (word-alist))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\([-\\/${}._A-Za-z0-9]+\\):" nil t)
	(setq word (buffer-substring (match-beginning 1) (match-end 1)))
	(if (not (assoc word word-alist))
	    (setq word-alist (cons (list word) word-alist)))))
    (completing-read (concat "project : ") word-alist)))

;;;
;; Variables for Process Handler(EPOP)
;;;
(defvar epo-make-command-line "make" "*make command name") ;;; gmake?
(defvar epo-make-process-alist
  '((?j (type . compile)
	(prompt . nil) ;; use C-u
	(command "Make"
		 epo-make-command-line "-f" basename epo-make-target)))
  "*dependent process alists")

;; 
;;	epop-tagjump-alist
;; 
(defvar epo-make-tagjump-alist
  '(("Make"    (type . inline) (pattern . "^\\(\\S +\\.c\\):\\(%l\\):")
     (matchinfo 1 . 2)) ))

(provide 'epo-make)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
