;definitions 3manually adapted from sensekit_capi.h

;SENSEKIT_API sensekit_status_t sensekit_initialize();
(add-func 	:returntype "sensekit_status_t"
			:funcname "initialize" 
			:params '()
)

;SENSEKIT_API void sensekit_terminate();
(add-func 	:returntype "void"
			:funcname "terminate" 
			:params '()
)

;SENSEKIT_API sensekit_status_t sensekit_streamset_open(const char* connectionString,
;                                                       sensekit_streamset_t** streamSet);
(add-func 	:returntype "sensekit_status_t"
			:funcname "streamset_open"
			:params (list (make-param :type "const char*" :name "connectionString")
						 (make-param :type "sensekit_streamset_t**" :name "streamSet" :deref T)
					)
)

;SENSEKIT_API sensekit_status_t sensekit_streamset_close(sensekit_streamset_t** streamSet);
(add-func 	:returntype "sensekit_status_t"
			:funcname "streamset_close"
			:params (list (make-param :type "sensekit_streamset_t**" :name "streamSet" :deref T)
					)
)

;SENSEKIT_API char* sensekit_get_status_string(sensekit_status_t status);
(add-func 	:returntype "char*"
			:funcname "get_status_string"
			:params (list (make-param :type "sensekit_status_t" :name "status")
					)
)

;SENSEKIT_API sensekit_status_t sensekit_reader_create(sensekit_streamset_t* streamSet,
;                                                      sensekit_reader_t** reader);
(add-func 	:returntype "sensekit_status_t"
			:funcname "reader_create"
			:params (list (make-param :type "sensekit_streamset_t*" :name "streamSet")
						 (make-param :type "sensekit_reader_t**" :name "reader" :deref T)
					)
)

;SENSEKIT_API sensekit_status_t sensekit_reader_destroy(sensekit_reader_t** reader);
(add-func 	:returntype "sensekit_status_t"
			:funcname "reader_destroy"
			:params (list (make-param :type "sensekit_reader_t**" :name "reader" :deref T)
					)
)

;SENSEKIT_API sensekit_status_t sensekit_reader_get_stream(sensekit_reader_t* reader,
;                                                          sensekit_stream_type_t type,
;                                                          sensekit_stream_subtype_t subType,
;                                                          sensekit_streamconnection_t** connection);
(add-func 	:returntype "sensekit_status_t"
			:funcname "reader_get_stream"
			:params (list (make-param :type "sensekit_reader_t*" :name "reader")
						  (make-param :type "sensekit_stream_type_t" :name "type")
						  (make-param :type "sensekit_stream_subtype_t" :name "subType")
						  (make-param :type "sensekit_streamconnection_t**" :name "connection" :deref T)
					)
)

;SENSEKIT_API sensekit_status_t sensekit_stream_get_description(sensekit_streamconnection_t* connection,
;                                                               sensekit_stream_desc_t* description);

;SENSEKIT_API sensekit_status_t sensekit_stream_start(sensekit_streamconnection_t* connection);
;SENSEKIT_API sensekit_status_t sensekit_stream_stop(sensekit_streamconnection_t* connection);

;SENSEKIT_API sensekit_status_t sensekit_reader_open_frame(sensekit_reader_t* reader,
;                                                          int timeoutMillis,
;                                                          sensekit_reader_frame_t** frame); //0 = return immediately

;SENSEKIT_API sensekit_status_t sensekit_reader_close_frame(sensekit_reader_frame_t** frame); //frame set to null

;SENSEKIT_API sensekit_status_t sensekit_reader_get_frame(sensekit_reader_frame_t* frame,
;                                                         sensekit_stream_type_t type,
;                                                         sensekit_stream_subtype_t subType,
;                                                         sensekit_frame_ref_t** frameRef);

;SENSEKIT_API sensekit_status_t sensekit_stream_set_parameter(sensekit_streamconnection_t* connection,
;                                                             sensekit_parameter_id parameterId,
;                                                             size_t byteLength,
;                                                             sensekit_parameter_data_t* data);

;SENSEKIT_API sensekit_status_t sensekit_stream_get_parameter_size(sensekit_streamconnection_t* connection,
;                                                                  sensekit_parameter_id parameterId,
;                                                                  size_t* byteLength);

;SENSEKIT_API sensekit_status_t sensekit_stream_get_parameter_data(sensekit_streamconnection_t* connection,
;                                                                  sensekit_parameter_id parameterId,
;                                                                  size_t byteLength,
;                                                                  sensekit_parameter_data_t* data);

;SENSEKIT_API sensekit_status_t sensekit_temp_update();
