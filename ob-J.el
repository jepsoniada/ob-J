(defcustom org-babel-J-command "jconsole"
  "Command that evaluates J taking script file to stdin.")

(defun org-babel-expand-body:J (body params)
  ""
  (let ((params (org-babel-J-process-params params)))
    (let-alist params
      ;; `9!:37' is for output control
      ;; it may bring some issues or fixes in case of weird formating
      (format "(9!:37) 0 _ 0 _\n%s\n%s"
	      (apply 'concat (mapcar (lambda (x)
				       (format "%s =: %s\n"
					       (car x)
					       (org-babel-J-var-to-J (cdr x))))
				     .:vars))
	      (format "res =. {{\n%s\n}} 0
res ; (#&$ res) ; (>(#&$)&.> res) ; (32 = (3!:0) res) ; (> (3!:0)&.> res)"
		      body)))))

(defalias 'org-babel-execute:j 'org-babel-execute:J)
(defun org-babel-execute:J (body params)
  "Execute a block of J code BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'."
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
	    ((member "debug" .:result-params) full-body)
	    (t (org-babel-J-raw-J-to-elisp result))))))

(defun org-babel-J-raw-J-to-elisp (j-value)
  "Transforms result of evaluated body to emacs lisp expresion.
J-VALUE is string representation of 5 J boxed values:
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
	(cl-loop with box-cells = (org-babel-J-get-box-cells value)
		 for row being the elements of-ref box-cells
		 do (cl-loop for col being the elements of-ref row
			     for type = (pop types) then (pop types)
			     when (not (equal 'character type))
			     do (setf col (cond ((equal 'number type) (string-to-number col)))))
		 finally return box-cells)
      (cond ((and (eq (car types) 'character) (= rank 1))
	     (string-trim value))
	    ((and (eq (car types) 'character) (= rank 2))
	     (mapcar (lambda (a) (list (string-trim a)))
		     (string-split (string-trim value) "\n")))
	    (t (mapcar (lambda (a) (mapcar #'string-to-number
					 (string-split a)))
		       (string-split value "\n")))))))

(defun org-babel-J-get-box-cells (table-as-string)
  "Converts a boxed array from text form to elisp list of unparsed string values."
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
  "Detuces type based on PRECISION which should be result of J's '3!:0' monad.
Values tested are sourced form [[https://code.jsoftware.com/wiki/Vocabulary/Nouns#Type]]."
  (cond ((member precision '(1 4 64 128 8 6 7 11 16)) 'number)
	((member precision '(2 131072 262144)) 'character)
	((member precision '(32)) 'box)
	((member precision '(65536)) 'symbol)
	((member precision '(1024 2048 4096 8192 16384)) 'sparse_numeric)
	((member precision '(32768)) 'sparse_boxed)))

(defun org-babel-J-process-params (params)
  "Convenience wrapper around 'org-babel-process-params' aggregating variables into `:vars' entry."
  (cl-loop for a in (org-babel-process-params params)
	   if (eq :var (car a))
	   collect (cdr a) into vars
	   else
	   collect a into rest
	   finally return (cons `(:vars . ,vars) rest)))

(defun org-babel-J-var-to-J (data)
  "Converts elisp value into J expression."
  (cond ((listp data)
	 (let ((flatten-list (flatten-list data)))
	   (format "%s $ %s"
		   (mapconcat #'number-to-string
			      (org-babel-J-aproximate-list-shape data)
			      " ")
		   (mapconcat (lambda (a) (format "(%s)"
						  (org-babel-J-var-to-J a)))
			      flatten-list
			      " ; "))))
	((stringp data)
	 (format "'%s'"
		 (string-replace "'" "''" data)))
	((numberp data)
	 (number-to-string data))
	(t (error "data type not implemented"))))

(defun org-babel-J-aproximate-list-shape (list)
  "Returns dimensions of LIST in J's style of monadic '$'."
  (when (listp list)
    (nconc (list (length list))
	   (if (listp (car list))
	       (org-babel-J-aproximate-list-shape (car list))
	     nil))))

(provide 'ob-J)
