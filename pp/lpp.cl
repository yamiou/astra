(defstruct param type name)
(defstruct funcdef funcname params returntype)

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
		(funcdef-returntype funcdata) 
		(funcdef-funcname funcdata) 
		(funcdef-params funcdata)
	)
)

(defun format-paramitem-type (p)
	(format nil "~A" (param-type p))
)

(defun format-paramlist-type (params)
	(if params
		  (concatenate 'string ", " 
			(format nil "~{~A~^, ~}" (mapcar #'format-paramitem-type params))
		   )
		   ""
    )
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
						"^RETURN^" (funcdef-returntype funcdata)
					)
					"^FUNC^" (funcdef-funcname funcdata)
				)
				"^PARAMS^" (format-paramlist-full (funcdef-params funcdata))
			)
			"^PARAM-NAMES^" (format-paramlist-name (funcdef-params funcdata))
		)
		"^PARAM-TYPES^" (format-paramlist-type (funcdef-params funcdata))
	)
)

(defun formatmethods (lineformat funclist) 
	(mapcar 
		(partial #'formatmethod lineformat) funclist
	)
)

(setq funcs '())

(defun add-func (&rest args)
	(setq funcs
		  (cons 
				(apply #'make-funcdef args)
			funcs)
	)
)

(load "apidef.cl")

(setq f1 "^RETURN^ ^FUNC^(^PARAMS^);
")
(setq f2 "^RETURN^ sensekit_^FUNC^(^PARAMS^) {
	return g_Context->^FUNC^(^PARAM-NAMES^);
}
")
(setq f3 "^RETURN^ (*^FUNC^)(void*^PARAM-TYPES^);
")

(defun concat-string-list (s)
	(format nil "~{~A~^~%~}" s)
)

(defun outputfuncs (f) 
	(write-string (concat-string-list f))
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
(setq begin-marker "^^^BEGINREPLACE^^^")
(setq end-marker "^^^ENDREPLACE^^^")
(setq auto-header-marker "^^^AUTOGENHEADER^^^")

(defun filter-line (line filename)
	(replace-all line auto-header-marker (format nil "THIS FILE AUTO-GENERATED FROM ~A. DO NOT EDIT." filename))
)

(defmacro prepend-into (var value)
	`(setq ,var (cons ,value ,var))
)

(defun process-file (infilename)
	(let ((infile (open infilename :if-does-not-exist nil))
		  (filelines nil)
		  (templatelines nil)
		  (template-status 0)
		  (doneparts ())
		  )
	  (when infile
		(loop 
			for line = (read-line infile nil)
			while line 
			do 
				(if (= template-status 0)
					(if (equal begin-marker line)
						;then
						(progn 
							(setq template-status 1)
						)
						;else
						(prepend-into filelines (filter-line line infilename))
					)
					(if (equal end-marker line)
						;then
						(progn 
							(setq template-status 0)
							;expand template
							;(prepend-into filelines (concat-string-list (formatmethods templatelines funcs)))
							(mapcar 
								(lambda (v) (prepend-into filelines v))
								(formatmethods (concat-string-list (reverse templatelines)) funcs)
							)
							;(outputfuncs (formatmethods (concat-string-list (reverse templatelines)) funcs))
						)
						;else
						(prepend-into templatelines line)
					)
				)
		)
		(loop for line in (reverse filelines)
			  do (format t "~A~%" line)
		)
		(close infile))
	)
)

;(t)
(defun t2 ()
	(process-file "test2.cpp.lpp")
)
(t2)