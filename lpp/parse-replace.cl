(defun file-string (path)
  (with-open-file (infile path :if-does-not-exist nil)
    (cond ((not (null infile))
            (format nil "~{~A~^~%~}"
              (loop
                for line = (read-line infile nil)
                while line
                collect line
              )
            )
          )
          (t nil) ; file not found
    )
  )
)
      
(defun write-file (file-name content)
    (with-open-file (outfile file-name
        :direction :output
        :if-exists :overwrite
        :if-does-not-exist :create)
    (format outfile content)))
    
(defmacro get-next-substr-index (str &key start-marker end-marker start-index end-index (next-index 0))
  `(progn
    (let ((start-marker-index (search ,start-marker ,str :start2 ,next-index))
         )
      (cond ((null start-marker-index) nil)
            (t
              (setq ,start-index (+ start-marker-index (length ,start-marker)))
              (setq ,end-index (search ,end-marker ,str :start2 ,start-index))
              (cond ((null ,end-index) nil)
                    (t
                      (+ ,end-index (length ,end-marker))
                    )
              )
            )
      )
    )
  )
)
(defun concat-string-list (s)
  (format nil "~{~A~}" s))

(defstruct rdata start-marker end-marker replacement)

(defun parse-replace (str template)
    (concat-string-list
      (let ((sm (rdata-start-marker template))
            (em (rdata-end-marker template))
            (replacement (rdata-replacement template))
            )
           (loop  with index = 0
                  do (setq next-index (get-next-substr-index str :start-marker sm
                               :end-marker em
                               :start-index si
                               :end-index ei
                               :next-index index))
                  if (null next-index)
                    collect (progn (write-line (format nil "~A -> ~A" index (length str)))
                              (subseq str index)
                             )
                  while next-index
                  ;collect (subseq str index next-index)
                  collect (subseq str index si)
                  collect replacement
                  collect (subseq str ei next-index)
                  do (progn (write-line (format nil "~A -> [~A ~A] -> ~A" index si ei next-index))
                        (setq index next-index)
                      )
           )
      )
    )
)

(defun apply-parse-file (file-name-in file-name-out template)
  (write-file file-name-out (parse-replace (file-string file-name-in) template))
)

(setq tp1 (make-rdata :start-marker "[" 
                     :end-marker "]"
                     :replacement "_123_"))

(setq tp2 (make-rdata :start-marker "<AdditionalIncludeDirectories>" 
                     :end-marker "</AdditionalIncludeDirectories>"
                     :replacement "_123_"))

(setq f1 "abc [123] fdsa [9090] lala")
(setq fni "DepthReader.vcxproj")
(setq fno "DepthReader.vcxproj.out")

(defun _ () (load "parse-replace.cl" :verbose nil))

(defun t1 ()
  (write-line (parse-replace f1 tp1))
)
(defun t2 ()
  (apply-parse-file fni fno tp2)
)
(when (>= (length *args*) 2)
  (setq path (car *args*))
  (setq template (cdr *args*))
  (parse-replace (file-string path) template)
)

(t2)
