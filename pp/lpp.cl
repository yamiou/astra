(defpackage :com.orbbec.sdk
  (:use :common-lisp))
  
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

(defun formatmethod (lineformat funcdata) 
	(replace-all 
		(replace-all 
			(replace-all lineformat 
				"^RETURN^" (code-returntype funcdata)
			)
			"^FUNC^" (code-funcname funcdata)
		)
		"^PARAMS^" (code-params funcdata)
	)
)

(defun formatmethods (lineformat funclist) 
	(map 'list 
		(partial #'formatmethod lineformat) funclist
	)
)

(setq c1 (make-code :returntype "sk_status"
					:funcname "open_frame" 
					:params "sk_stream* stream, sk_frame** frame" 
		)
)

(setq c2 (make-code :returntype "sk_status"
					:funcname "close_frame" 
					:params "sk_frame** frame" 
		)
)

(setq f1 "^RETURN^ ^FUNC^(^PARAMS^);
")
(setq f2 "^RETURN^ sensekit_^FUNC^(^PARAMS^) {
	return g_Context->^FUNC^(^PARAMS^);
}
")
(setq f3 "^RETURN^ (*^FUNC^)(void*, ^PARAMS^);
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

(t)