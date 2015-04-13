(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

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
  
(defun parse-replace (str &optional template)
;;  (load template :verbose nil)
    (concat-string-list
      (let ( (str-out nil)
           )
           ;(setq sm "<AdditionalIncludeDirectories>")
           ;(setq em "</AdditionalIncludeDirectories>")
           (setq sm "[")
           (setq em "]")
           (loop  with index = 0
                  do (setq next-index (get-next-substr-index str :start-marker sm
                               :end-marker em
                               :start-index si
                               :end-index ei
                               :next-index index))
                  while next-index
                  collect (subseq str index si)
                  collect "_"
                  collect (subseq str ei next-index)
                  do (write-line (format nil "~A -> ~A" si ei))
                     (setq index next-index)
           )
      )
    )
)


(defun _ () (load "parse-replace.cl" :verbose nil))
(setq f1 "abc [123] fdsa [9090] lala")
(defun t1 ()
  (write-line (parse-replace f1))
)
(when (>= (length *args*) 2)
  (setq path (car *args*))
  (setq template (cdr *args*))
  (parse-replace (file-string path) template)
)

(t1)
