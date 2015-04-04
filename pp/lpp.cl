
(defstruct param type name deref)
(defstruct funcdef funcname params returntype)

(setq begin-marker "^^^BEGINREPLACE^^^")
(setq end-marker "^^^ENDREPLACE^^^")
(setq auto-header-marker "^^^AUTOGENHEADER^^^")
(setq token-marker #\^)
(setq token-arg-marker #\:)
(setq token-arg-delimiter #\,)
(setq voidparam (make-param :type "void*" :name "service"))

(defstruct lppmacro macro filter)

(setq macro-hash (make-hash-table :test 'equal))
(defun add-macro (&rest args)
    (let ((m (apply #'make-lppmacro args)))
        (setf (gethash (lppmacro-macro m) macro-hash) m)
    )
)

(add-macro :macro "RETURN"	:filter (lambda (fd len args) (funcdef-returntype fd)))
(add-macro :macro "FUNC"	:filter (lambda (fd len args) (funcdef-funcname fd)))
(add-macro :macro "PARAMS"	:filter (lambda (fd len args) (format-params (funcdef-params fd) len args)))
; PARAMS arguments: types, names, deref, ref, void, nowrap
;;;;;;;;;

(defun partial (func &rest args1)
   (lambda (&rest args2) (apply func (append args1 args2)))
)

(defun newline ()
	(write-string (format nil "~%"))
)

(defun concat-string-list (s)
	(format nil "~{~A~^~%~}" s)
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
            while pos))
)

(defun is-arg-set (target-arg args)
    (not (null (find target-arg args :test 'equal)))
)

(defun format-paramitem-type (args p)
	(format nil "~A" (param-type p))
)

(defun format-paramitem-name (args p)
    ;in name only format, we can dereference the argument (pointer)
    ;but only if the macro argument requested it  
    ;and this parameter can be dereferenced
	(if (and (is-arg-set "deref" args) (param-deref p))
		;then
		(format nil "*~A" (param-name p))
		;else
		(format nil "~A" (param-name p))
	)
)

(defun format-paramitem-full (args p)
	(let* ((type-decl (param-type p))
           (last-index (1- (length type-decl)))
          )
        (format nil "~A ~A" 
                ;if macro requested reference mode, 
                ; this parameter is dereferencable, 
                ; and the last char is a *
                (if (and (is-arg-set "ref" args) (param-deref p) (equal "*" (subseq type-decl last-index)))
                    ;then, replace the last * with a &
                    (concatenate 'string (subseq type-decl 0 last-index) "&")
                    ;else
                    type-decl
                )
                (param-name p))
	)
)

(defun format-params (params token-start-length args)
    ;full, types, names, deref, ref, void, nowrap
    (let*  ((arg-types  (is-arg-set "types" args))
            (arg-names  (is-arg-set "names" args))
            (arg-deref  (is-arg-set "deref" args))
            (arg-ref    (is-arg-set "ref" args))
            (arg-void   (is-arg-set "void" args))
            (arg-nowrap (is-arg-set "nowrap" args))
            (filtered-params (if arg-void 
                                 (cons voidparam params) ;then, prepend "void* service"
                                 params                  ;else, don't
                              ))
            (types-only (and arg-types (not arg-names)))
            (names-only (and arg-names (not arg-types)))
            (full-decl  (not (xor arg-names arg-types)))
            (format-param-func (cond  (full-decl  #'format-paramitem-full)
                                (types-only #'format-paramitem-type)
                                (names-only #'format-paramitem-name)
                          ))
            (do-wrap    (not (or names-only arg-nowrap)))
            (format-line (if do-wrap "~{~A~^,~%~}" "~{~A~^, ~}"))
           )
       (when filtered-params
            (format nil format-line 
                (cons
                    ;first parameter => no indentation
                    (apply format-param-func (list args (car filtered-params)))
                    (mapcar 
                        (lambda (p) 
                            (concatenate 'string 
                                ;second parameter, indent according to wrap policy
                                (when do-wrap (make-string token-start-length :initial-element #\ ))
                                (apply format-param-func (list args p))
                            )
                        )
                        (cdr filtered-params)
                    )
                )
            )
        )
    )
)

(defun split-by-delimiter (string delimiter)
	(loop 	for start = 0 then (1+ finish)
			for finish = (position delimiter string :start start)
			if (and (not (eq (length string) start)) (not (eq start finish))) collect (subseq string start finish)
			until (null finish)
	)
)

(defun process-tokens (line funcdata &optional (line-length 0))
    (let ((token-marker-left (position token-marker line))
         )
        (if (null token-marker-left)
            ;then, no left token marker. return line
            line
            ;else, process more
            (let* ((token-start (1+ token-marker-left))
                  (token-stop (position token-marker line :start token-start))
                 )
                (if (null token-stop)
                    ;then, no right token marker. return line
                    line
                    ;else, process more
    
        (let* ((token-full (subseq line token-start token-stop))
               (seperator-index (position token-arg-marker token-full))
               (token (subseq token-full 0 seperator-index))
               (token-args (if (not (null seperator-index))
                              (split-by-delimiter (subseq token-full (1+ seperator-index) nil) token-arg-delimiter)
                              '(nil)
                          ))
               (line-begin (subseq line 0 token-marker-left))
               (line-end (subseq line (1+ token-stop) nil))
               (m (gethash token macro-hash))
              )
              ;m (the macro struct) is nil if no macro found for the token
              (if (null m)
                    ;then
                    line
                    ;else
                    (let* ((token-start-length (+ line-length token-marker-left))
                           (expanded-token (funcall (lppmacro-filter m) funcdata token-start-length token-args))
                          )
                        (concatenate 'string 
                            line-begin 
                            expanded-token
                            (process-tokens line-end funcdata (+ token-start-length (length expanded-token)))
                        )
                    )
               )
        )
    ))))
)

(defun expand-template (template-list func-data)
    (concat-string-list
        (mapcar 
            (lambda (template-line) (process-tokens template-line func-data))
            template-list
        )
    )
)

(defun expand-methods-with-template (template-list func-data-list)
    (mapcar 
		(lambda (func-data) (expand-template template-list func-data))
        func-data-list
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

(defun outputfuncs (f) 
	(write-line (concat-string-list f))
)

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
							(mapcar 
								(lambda (v) (prepend-into filelines v))
								(expand-methods-with-template (reverse templatelines) funcs)
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
	(process-file "StreamServiceProxyBase.h.lpp")
	(process-file "SenseKitContext.h.lpp")
	"done"
)
(t2)