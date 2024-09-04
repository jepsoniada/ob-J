(defcustom org-babel-J-command "jconsole"
  "command that evaluates J taking script file to stdin")

(defun org-babel-expand-body:J (body params)
  ""
  (format "res =. {{\n%s\n}} 0
res"
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
      (cond ((member "verbatim" .:result-params) (prin1-to-string result))
	    (t result)))))

(provide 'ob-J)
