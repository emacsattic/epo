;;; -*- Emacs-Lisp -*-
;;; EPO Change Operation Supporter
;;; (c)2000-2002 by HIROSE Yuuji [yuuji@ae.keio.ac.jp]
;;; $Id$
;;; Last modified Mon Feb  4 13:12:59 2002 on firestorm

;;	2000/12/29 作成開始
;;	Relation type は、いくつかに限定した方が良さそうだ。
;;	たとえば、reference, file, hyperlink, box.
;;	- reference に対する変更 -> ?? 識別子全部の変更? 参照のみの変更?
;;	- file に対する変更 -> 参照名とファイル名の変更
;;	- hyperlink に対する変更 -> ???
;;	- box に対する変更 -> box名(識別子)の変更
;;	
;;[Commentary]
;;	
;;	EPOの修正操作支援器
;;	
;;
(require 'epo)

(defun epoc*change-box-edges (edge-info-list)
  "Change both edge of boxes identifier.
EDGE-INFO-LIST consists of list.
 '((me . partner) identifier 
     (opposite-ccre . matchgroup) (current-pos-ccre . match-group))
'me and 'partner are one of 'begin xor 'end"
  (let*((p (point))
	(which (car (car edge-info-list)))	;'begin or 'end
	(keyword (nth 1 edge-info-list))
	(grp1 (cdr (nth 2 edge-info-list)))
	(grp2 (cdr (nth 3 edge-info-list)))
	(alist (get 'epor*on-relation-p 'matched-type))
	(prompt (format "Change '%s' to: " keyword))
	p2 b1 e1 b2 e2 newkeyword table)
    (setq table (epo*alist-get 'table alist))
    (setq newkeyword
	  (if table (completing-read prompt table)
	    (read-string prompt)))
    (if (or (string= "" newkeyword) (string= keyword newkeyword))
 	(message "Nothing to be done.")
      (if (eq which 'begin)
	  (setq b1 (match-beginning grp2) ;match group number
		e1 (match-end grp2))
	(setq b2 (match-beginning grp2)
	      e2 (match-end grp2)))
      (epor*goto-opposite-box-edge (cdr r))
      (if (eq which 'end)
	  (setq b1 (match-beginning grp1) ;match group number
		e1 (match-end grp1))
	(setq b2 (match-beginning grp1)
	      e2 (match-end grp1)))
      (goto-char b2)	     ;move to beginning of end-edge's identifier
      (delete-region b2 e2)
      (insert newkeyword)
      (goto-char b1)	;move to beginning of begein-edge's identifier
      (delete-region b1 e1)
      (insert newkeyword)
      (goto-char p))))

(defun epoc*change-file-linkage (info-list)
  "Change file link name and file-name itself simultaneously."
  (let*((p (point))
	(oldfilename (nth 1 info-list))
	(grp (cdr (nth 3 info-list)))
	(mb (match-beginning grp))
	(me (match-end grp))
	newfilename
	(thislinkage (epo*match-string 0)))
    (setq newfilename (read-file-name
		       (format "Change `%s' to: " oldfilename)
		       ""))
    (if (or (string= "" newfilename) (string= oldfilename newfilename))
	(message "No change was made.")
      (goto-char mb)
      (delete-region mb me)
      (insert newfilename)
      (if (and (file-exists-p oldfilename)
	       (not (file-exists-p newfilename))
	       (y-or-n-p
		(format "File `%s' not found, rename it now?" newfilename)))
	  (rename-file oldfilename newfilename))
      ;;Here you might want to change all reference of `oldfilename'
      ;;in all buffers throughout.
      (if (y-or-n-p (format "Replace all occurence of `%s' to `%s'?"
			    oldfilename newfilename))
	  (progn
	    (epo-query-replace-all oldfilename newfilename)
	    (message "Don't forget to save modified buffers.")))
      )))

;;;###autoload
(defun epoc-change-relation-elements (&optional arg)
  "Change elements of some relation specific to the source text language."
  (interactive "P")
  (epo*check-mode)
  (let ((r (epor*on-relation-p)))
    (cond
     (r
      (cond
       ((eq (car r) 'box)
	(epoc*change-box-edges (cdr r)))
       ((eq (car r) 'file)
	(epoc*change-file-linkage (cdr r)))))
     (t (message "No relation to change, here"))
     )))

(defun epoc*kill-file-linkage (info-list)
  "Kill file linkage and pointed file itself."
  (let*((p (point))
	(filename (nth 1 info-list))
	(grp (cdr (nth 3 info-list)))
	(mbeg (match-beginning grp))
	(mend (match-end grp))
	(mb0 (match-beginning 0))
	(me0 (match-end 0))
	(thislinkage (epo*match-string 0))
	(extension (epo*structure-info-get
		    'extension
		    (epo*get-cur-lang-alist 'file))))
    (if (and (stringp filename)
	     (not (file-exists-p filename))
	     (stringp extension))
	(setq filename (concat filename extension)))
    (if (and (stringp filename)
	     (file-exists-p filename)
	     (y-or-n-p (format "Also delete file: %s" filename)))
	(delete-file filename))
    ;;(set-mark mb0)
    ;;(goto-char me0)
    (kill-region mb0 me0)		;save to kill-ring
    (setq epo*coco-re-search-last-expression (car (nth 3 info-list)))
    (message "(mark set) Type %s to find the next same pattern."
	     (epo*function-key-description
	      'epo-coco-re-search-again-forward))))

(defun epoc-kill-relations (&optional arg)
  "Kill elements of some relation specific to the source text language."
  (interactive "P")
  (let ((r (epor*on-relation-p)))
    (cond
     (r
      (cond
       ((eq (car r) 'box)
	(epoc*kill-box-edges (cdr r)))
       ((eq (car r) 'file)
	(epoc*kill-file-linkage (cdr r)))))
     (t (message "No relation to kill, here"))
     ))
  )
	
(provide 'epoc)

; Local variables: 
; fill-prefix: ";;	" 
; paragraph-start: "^$\\|\\|;;$" 
; paragraph-separate: "^$\\|\\|;;$" 
; End: 
