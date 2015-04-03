
(add-func 	:returntype "sensekit_status_t"
			:funcname "initialize" 
			:params '()
)

(add-func 	:returntype "void"
			:funcname "terminate" 
			:params '()
)

(add-func 	:returntype "sensekit_status_t"
			:funcname "streamset_open"
			:params (list (make-param :type "const char*" :name "connectionString")
						 (make-param :type "sensekit_streamset_t**" :name "streamSet" :deref T)
					)
)

(add-func 	:returntype "sensekit_status_t"
			:funcname "streamset_close"
			:params (list (make-param :type "sensekit_streamset_t**" :name "streamSet" :deref T)
					)
)

(add-func 	:returntype "char*"
			:funcname "get_status_string"
			:params (list (make-param :type "sensekit_status_t" :name "status")
					)
)

(add-func 	:returntype "sensekit_status_t"
			:funcname "reader_create"
			:params (list (make-param :type "sensekit_streamset_t*" :name "streamSet")
						 (make-param :type "sensekit_reader_t**" :name "reader" :deref T)
					)
)

(add-func 	:returntype "sensekit_status_t"
			:funcname "reader_destroy"
			:params (list (make-param :type "sensekit_reader_t**" :name "reader" :deref T)
					)
)

(add-func 	:returntype "sensekit_status_t"
			:funcname "reader_get_stream"
			:params (list (make-param :type "sensekit_reader_t**" :name "reader" :deref T)
					)
)
