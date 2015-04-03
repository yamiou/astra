
(defstruct param type name deref)
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

(defun format-paramitem-name (deref p)
	(if (and deref (param-deref p))
		;then
		(format nil "*~A" (param-name p))
		;else
		(format nil "~A" (param-name p))
	)
)

(defun format-paramlist-name (deref params)
	(format nil "~{~A~^, ~}" (mapcar (partial #'format-paramitem-name deref) params))
)

(defun format-paramitem-full (p)
	(format nil "~A ~A" (param-type p) (param-name p))
)

(defun format-paramlist-full (params)
	(format nil "~{~A~^, ~}" (mapcar #'format-paramitem-full params))
)

(defstruct lppmacro macro filter)
;TODO add PARAMS-REF for *& transforms
(setq replacement-macros (list
	(make-lppmacro :macro "^RETURN^"				:filter (lambda (fd) (funcdef-returntype fd)))
	(make-lppmacro :macro "^FUNC^"	 				:filter (lambda (fd) (funcdef-funcname fd)))
	(make-lppmacro :macro "^PARAMS^"				:filter (lambda (fd) (format-paramlist-full (funcdef-params fd))))
	(make-lppmacro :macro "^PARAMS-TYPES^" 			:filter (lambda (fd) (format-paramlist-type (funcdef-params fd))))
	(make-lppmacro :macro "^PARAM-NAMES^" 			:filter (lambda (fd) (format-paramlist-name nil (funcdef-params fd))))
	(make-lppmacro :macro "^PARAM-NAMES-DEREF^" 	:filter (lambda (fd) (format-paramlist-name T (funcdef-params fd))))
))

(defun formatmethod (lineformat funcdata)
	(loop for m in replacement-macros
			do
				(setq lineformat (replace-all lineformat (lppmacro-macro m) (funcall (lppmacro-filter m) funcdata)))
	)
	lineformat
)

(defun formatmethod2 (lineformat funcdata) 
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

(defun clear-funcs ()
	(setq funcs nil)
	(setq funcs-tail funcs)
)
(clear-funcs)

(defun add-func (&rest args)
	(let ((func-data (apply #'make-funcdef args)))
		(if (endp funcs)
			;then
			(progn
				(setq funcs (cons func-data nil))
				(setq funcs-tail funcs)
			)
			;else
			(progn
				(setf (cdr funcs-tail) (cons func-data nil))
				(setq funcs-tail (cdr funcs-tail))
			)
		)
	)
)

(load "apidef.cl")

(defun concat-string-list (s)
	(format nil "~{~A~^~%~}" s)
)

(defun outputfuncs (f) 
	(write-line (concat-string-list f))
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

(defun strip-end-characters (str numChars)
	(let ((index (- (length str) numChars)))
		(if (>= index 1)
			(subseq str 0 index)
			nil
		)
	)
)

(defun process-file (infilename)
	(let ((infile (open infilename :if-does-not-exist nil))
		  (outfile (open (strip-end-characters infilename 4) :direction :output :if-exists :supersede))
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
						)
						;else
						(prepend-into templatelines line)
					)
				)
		)
		(loop for line in (reverse filelines)
			  do (write-line line outfile)
		)
		(close infile)
		(close outfile))
	)
)

(defun _ () (load "lpp.cl" :verbose nil))

(defun t2 ()
	(process-file "SenseKit.cpp.lpp")
	(process-file "StreamServiceProxyBase.cpp.lpp")
	"done"
)
(t2)