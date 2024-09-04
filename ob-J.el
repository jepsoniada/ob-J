(defcustom org-babel-J-command "jconsole"
  "command that evaluates J taking script file to stdin")

(defun org-babel-expand-body:J (body params)
  ""
  (format "res =. {{\n%s\n}} 0
res ; (#&$ res) ; (>(#&$)&.> res) ; (32 = (3!:0) res) ; (> (3!:0)&.> res)"
	  body))

(defalias 'org-babel-execute:j 'org-babel-execute:J)
(defun org-babel-execute:J (body params)
  ""
  (let* ((full-body (org-babel-expand-body:J body params))
	 (tmp-file (org-babel-temp-file "J-"))
	 (result (progn
		   (with-temp-file tmp-file
		     (insert full-body))
		   (org-babel-eval (format "%s < %s" org-babel-J-command tmp-file) ""))))
    (let-alist params
      (when (and .:session (not (equal .:session "none")))
	(error "ob-J does not support sessions"))
      (cond ((member "verbatim" .:result-params) (caar (org-babel-J-get-box-cells result)))
	    (t (org-babel-J-raw-J-to-elisp result))))))

(defun org-babel-J-raw-J-to-elisp (j-value)
  "transforms result of evabluated body to emacs lisp expresion
J-VALUE is string representation of 3 J boxed values:
  - result of body execution
  - rank of result
  - ranks for every part of result if it is an array
  - boolean number checking when result is enboxed
  - underlying for each element in result"
  (let* ((raw-table (org-babel-J-get-box-cells j-value))
	 (value (nth 0 (car raw-table)))
	 (rank (string-to-number (string-trim (nth 1 (car raw-table)))))
	 (ranks (mapcar #'string-to-number
			(string-split (string-trim (nth 2
							(car raw-table))))))
	 (is-box (if (= 1 (string-to-number (string-trim (nth 3 (car raw-table)))))
		     t
		   nil))
	 (types (mapcar (lambda (a) (org-babel-J-typeof (string-to-number a)))
			(string-split (string-trim (nth 4
							(car raw-table)))))))
    (when (> rank 2)
      (error "rank of value is above 2"))
    (when (or (member 'box types)
	      (member 'sparse_boxed types))
      (error "nested boxes are not allowed"))
    (when (let ((invalid-structurep (seq-mapn (lambda (rank type)
						(cond ((and (equal 'character type)
							    (>= 1 rank))
						       nil)
						      ((and (not (equal 'character type))
							    (= 0 rank))
						       nil)
						      (t t)))
					      ranks
					      types)))
	    (seq-reduce (lambda (a b) (or a b))
			(cdr invalid-structurep)
			(car invalid-structurep)))
      (error "nested arrays are not allowed"))
    (if is-box
	(org-babel-J-get-box-cells value)
      (cond ((and (eq (car types) 'character) (= rank 1))
	     (string-trim value))
	    ((and (eq (car types) 'character) (= rank 2))
	     (mapcar #'list
		     (string-split (string-trim value) "\n")))
	    (t (mapcar (lambda (a) (string-split a))
		       (string-split value "\n")))))))

(defun org-babel-J-get-box-cells (table-as-string)
  (let* ((lines (string-split (string-trim table-as-string) "\n"))
	 (border-col (car lines))
	 (border-row (cl-loop for a in lines
			      concat (char-to-string (elt a 0))))
	 (row-heights (mapcar #'length
			      (string-split (string-trim border-row "\+" "\+") "\+")))
	 (col-widths (mapcar #'length
			     (string-split (string-trim border-col "\+" "\+") "\+")))
	 (filter-out-row-separators (seq-keep (lambda (a) (when (= ?| (elt a 0)) a))
					      (seq-drop (seq-take lines
								  (- (length lines) 1))
							1)))
	 (drop-outer-col-frame (mapcar (lambda (a) (seq-drop (seq-take a (- (length a) 1)) 1))
				       filter-out-row-separators))
	 (group-cols (cl-loop for line in drop-outer-col-frame
			      collect (cl-loop with prev = 0
					       for n in (number-sequence 0 (length col-widths))
					       for a in col-widths
					       collect (seq-subseq line (+ n prev) (+ n (setf prev (+ a prev)))))))
	 (group-rows (cl-loop with prev = 0
			      for a in row-heights
			      collect (seq-subseq group-cols prev (setf prev (+ a prev)))))
	 (result (cl-loop for a in group-rows
			  collect (apply #'seq-mapn
					 (lambda (&rest strings)
					   (string-join strings "\n"))
					 a))))
    result))

(defun org-babel-J-typeof (precision)
  "detuces type based on PRECISION which should be result of '3!:0' monad
values tested are sourced form https://code.jsoftware.com/wiki/Vocabulary/Nouns#Type"
  (cond ((member precision '(1 4 64 128 8 6 7 11 16)) 'number)
	((member precision '(2 131072 262144)) 'character)
	((member precision '(32)) 'box)
	((member precision '(65536)) 'symbol)
	((member precision '(1024 2048 4096 8192 16384)) 'sparse_numeric)
	((member precision '(32768)) 'sparse_boxed)))

(provide 'ob-J)
