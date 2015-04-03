(defpackage :com.orbbec.sdk
  (:use :common-lisp))

(defstruct param type name)
(defstruct code funcname params returntype)

(defun partial (func &rest args1)
   (lambda (&rest args2) (apply func (append args1 args2)))
)

(defun newline ()
	(write-string (format nil "~%"))
)

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))
			
(defun formatmethod2 (lineformat funcdata) 
	(format nil lineformat 
		(code-returntype funcdata) 
		(code-funcname funcdata) 
		(code-params funcdata)
	)
)

(defun format-paramitem-type (p)
	(format nil "~A" (param-type p))
)

(defun format-paramlist-type (params)
	(format nil "~{~A~^, ~}" (mapcar #'format-paramitem-type params))
)

(defun format-paramitem-name (p)
	(format nil "~A" (param-name p))
)

(defun format-paramlist-name (params)
	(format nil "~{~A~^, ~}" (mapcar #'format-paramitem-name params))
)

(defun format-paramitem-full (p)
	(format nil "~A ~A" (param-type p) (param-name p))
)

(defun format-paramlist-full (params)
	(format nil "~{~A~^, ~}" (mapcar #'format-paramitem-full params))
)

(defun formatmethod (lineformat funcdata) 
	(replace-all 
		(replace-all 
			(replace-all 
				(replace-all 
					(replace-all lineformat 
						"^RETURN^" (code-returntype funcdata)
					)
					"^FUNC^" (code-funcname funcdata)
				)
				"^PARAMS^" (format-paramlist-full (code-params funcdata))
			)
			"^PARAM-NAMES^" (format-paramlist-name (code-params funcdata))
		)
		"^PARAM-TYPES^" (format-paramlist-type (code-params funcdata))
	)
)

(defun formatmethods (lineformat funclist) 
	(map 'list 
		(partial #'formatmethod lineformat) funclist
	)
)

(setq c1 (make-code :returntype "sk_status"
					:funcname "open_frame" 
					:params (list (make-param :type "sk_stream*" :name "stream")
								  (make-param :type "sk_frame**" :name "frame")
							)
		)
)

(setq c2 (make-code :returntype "sk_status"
					:funcname "close_frame" 
					:params (list (make-param :type "sk_frame**" :name "frame" ))
		)
)

(setq f1 "^RETURN^ ^FUNC^(^PARAMS^);
")
(setq f2 "^RETURN^ sensekit_^FUNC^(^PARAMS^) {
	return g_Context->^FUNC^(^PARAM-NAMES^);
}
")
(setq f3 "^RETURN^ (*^FUNC^)(void*, ^PARAM-TYPES^);
")

(setq funcs (list c1 c2))

(defun outputfuncs (f) 
	(map nil #'write-string f)
)

(defun _ () (load "lpp.cl" :verbose nil))
	
(defun t ()
	;(write-string (formatmethod f1 c1))
	;(write-string (formatmethod f2 c1))
	(outputfuncs (formatmethods f1 funcs))
	(newline)
	(outputfuncs (formatmethods f2 funcs))
	(newline)
	(outputfuncs (formatmethods f3 funcs))
	(newline)
	nil
)	

;(t)
(let ((infile (open "test.cpp.lpp" :if-does-not-exist nil))
	  (parts '(nil nil))
	  (currentpart nil)
	  (doneparts ())
	  )
  (when infile
    (loop 
		for line = (read-line infile nil)
        while line 
		do (setq currentpart (cons line currentpart))
	)
	(loop for line in (reverse currentpart)
		  do (format t "~a~%" line)
	)
    (close infile))
)