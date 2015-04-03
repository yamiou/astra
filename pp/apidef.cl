
(add-func 	:returntype "sensekit_status_t"
			:funcname "open_frame" 
			:params (list (make-param :type "sk_stream*" :name "stream")
						  (make-param :type "sk_frame**" :name "frame" :deref T)
					)
)

(add-func 	:returntype "sensekit_status_t"
			:funcname "close_frame" 
			:params (list (make-param :type "sk_frame**" :name "frame" :deref T ))
)

(add-func 	:returntype "sensekit_status_t"
			:funcname "initialize" 
			:params '()
)

(add-func 	:returntype "sensekit_status_t"
			:funcname "terminate" 
			:params '()
)