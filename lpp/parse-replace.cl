(use-package :regexp)

(load "cl-fad/load.lisp" :verbose nil)

(defstruct rdata start-marker end-marker replacement-callback include-markers)

(setq project-file-extension "vcxproj")

(defun regexp-replace (string pat repl)
  (reduce #'(lambda (x y) (string-concat x repl y))
          (regexp:regexp-split pat string))
)
          
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
      :if-exists :supersede
      :if-does-not-exist :create)
  (format outfile content)))
    
(defmacro get-next-substr-index (str &key start-marker end-marker start-index end-index include-markers (next-index 0))
  `(progn
    (let ((start-m (match ,start-marker ,str :start ,next-index))
         )
      (cond ((null start-m) nil)
            (t
              (setq ,start-index  (if include-markers
                                     (match-start start-m)
                                     (match-end start-m)
                                  )
              )
              (let ((end-m (match ,end-marker ,str :start (match-end start-m))))
                (cond ((null end-m) nil)
                      (t
                        (setq ,end-index  (if include-markers
                                             (match-end end-m)
                                             (match-start end-m)
                                          )
                        )
                        (match-end end-m)
                      )
                )
              )
            )
      )
    )
  )
)
(defun concat-string-list (s)
  (format nil "~{~A~}" s))

(defun parse-replace (str template)
    (concat-string-list
      (let ((sm (rdata-start-marker template))
            (em (rdata-end-marker template))
            (callback (rdata-replacement-callback template))
            (include-markers (rdata-include-markers template))
            )
           ;(write-line (format nil "Replacing: ~A to ~A" sm em))
           (loop  with index = 0
                  do (setq next-index (get-next-substr-index str :start-marker sm
                               :end-marker em
                               :start-index si
                               :end-index ei
                               :next-index index
                               :include-markers include-markers))
                  if (null next-index)
                    collect (progn ;(write-line (format nil "~A -> ~A" index (length str)))
                              (subseq str index)
                             )
                  while next-index
                  collect (subseq str index si)
                  collect (funcall callback (subseq str si ei))
                  collect (subseq str ei next-index)
                  do (progn ;(write-line (format nil "~A -> [~A ~A] -> ~A" index si ei next-index))
                        (setq index next-index)
                      )
           )
      )
    )
)

(defun apply-replacement-templates (str templates)
  (cond ((null templates) str)
        (t (apply-replacement-templates 
            (parse-replace str (car templates))
            (cdr templates)
           )
        )
  )
)

(defun apply-parse-file (file-name-in file-name-out templates)
  (write-file file-name-out 
    (apply-replacement-templates (file-string file-name-in) templates)
  )
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

(setq rdata-list nil)
(defun add-rdata (&rest args)
  (let ((rd (apply #'make-rdata args)))
    (setq rdata-list (cons rd rdata-list))
  )
)
(defun get-macro (key) (gethash key macro-hash))

(setq tp1 (make-rdata :start-marker "<data[^>]*>" 
                     :end-marker "</data>"
                     :replacement-callback (lambda (str) "_123_")
                     :include-markers T))
(setq build-dir "[Cc]:[\\/]projects[\\/]orbbec[\\/]SenseKitSDK-0.1.0-win32[\\/]samples[\\/]build")
(setq lambda-path-to-sln-dir (lambda (str) (regexp-replace str build-dir "$(SolutionDir)")))
(setq lambda-remove (lambda (str) ""))

(add-rdata :start-marker "<AdditionalIncludeDirectories>" 
           :end-marker "</AdditionalIncludeDirectories>"
           :replacement-callback (lambda (str) (replace-all str "\\$$(SolutionDir)" "$(SolutionDir)")))
(add-rdata :start-marker "<OutDir[^>]*>" 
           :end-marker "</OutDir>"
           :replacement-callback lambda-path-to-sln-dir)
(add-rdata :start-marker "<ImportLibrary[^>]*>" 
           :end-marker "</ImportLibrary>"
           :replacement-callback lambda-path-to-sln-dir)
(add-rdata :start-marker "<ProgramDataBaseFile[^>]*>" 
           :end-marker "</ProgramDataBaseFile>"
           :replacement-callback lambda-path-to-sln-dir)
(add-rdata :start-marker "[^<>]*<ItemGroup[^>]*>[^<]*<CustomBuild[^>]*CMakeLists[.]txt[^>]*>"
           :end-marker "</CustomBuild>[^<]*</ItemGroup>"
           :replacement-callback lambda-remove
           :include-markers T)
(add-rdata :start-marker "[^<>]*<ItemGroup[^>]*>[^<]*<ProjectReference[^>]*ZERO_CHECK[.]vcxproj[^>]*>"
           :end-marker "</ProjectReference>[^<]*</ItemGroup>"
           :replacement-callback lambda-remove
           :include-markers T)

(defun mapc-directory-tree (fn directory)
  (dolist (entry (cl-fad:list-directory directory))
    (when (cl-fad:directory-pathname-p entry)
      (mapc-directory-tree fn entry))
    (funcall fn entry)))

(defun get-temp-filename (fn)
  (format nil "~A.tmp" fn))
  
(defun process-file (filename-in)
  (let ((filename-out (get-temp-filename filename-in)))
    (apply-parse-file filename-in filename-out rdata-list)
    (delete-file filename-in)
    (rename-file filename-out filename-in)
  )
)

(defun process (dir)
  (let ((target-directory (if (null dir)
                              (ext:cd)
                              dir)))
    (write-line (format nil "Base directory: ~A~%" target-directory))
    (mapc-directory-tree (lambda (x)
                           (when (equal (pathname-type x) project-file-extension)
                             (write-line (format nil "~A"
                                                 (enough-namestring x target-directory)
                                                 ))
                             (process-file (namestring x))))
                         target-directory)))

(defun _ () (load "parse-replace.cl" :verbose nil))

(when (>= (length *args*) 1)
  (let ((path (car *args*)))
    (process path)
  )
)
