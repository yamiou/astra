;; definitions manually adapted from sensekit_capi.h

(add-void-param :funcset "stream"
                :type "void*"
                :name "streamService")

(add-void-param :funcset "plugin"
                :type "void*"
                :name "pluginService")

;; sensekit_status_t register_stream_added_callback(StreamAddedCallback callback, CallbackId* callbackId)
(add-func       :funcset "plugin"
                :returntype "sensekit_status_t"
                :funcname "register_stream_added_callback"
                :params (list (make-param :type "StreamAddedCallback" :name "callback")
                              (make-param :type "CallbackId*" :name "callbackId" :deref T)))

;; sensekit_status_t register_stream_removing_callback(StreamRemovingCallback callback, CallbackId* callbackId)
(add-func       :funcset "plugin"
                :returntype "sensekit_status_t"
                :funcname "register_stream_removing_callback"
                :params (list (make-param :type "StreamRemovingCallback" :name "callback")
                              (make-param :type "CallbackId*" :name "callbackId" :deref T)))

;; sensekit_status_t unregister_stream_added_callback(CallbackId callbackId)
(add-func       :funcset "plugin"
                :returntype "sensekit_status_t"
                :funcname "unregister_stream_added_callback"
                :params (list (make-param :type "CallbackId" :name "callback")))

;; sensekit_status_t unregister_stream_removing_callback(CallbackId callbackId)
(add-func       :funcset "plugin"
                :returntype "sensekit_status_t"
                :funcname "unregister_stream_removing_callback"
                :params (list (make-param :type "CallbackId" :name "callback")))

;; sensekit_status_t create_stream_set(sensekit_streamset_t& setHandle)
(add-func       :funcset "plugin"
                :returntype "sensekit_status_t"
                :funcname "create_stream_set"
                :params (list (make-param :type "sensekit_streamset_t&" :name "setHandle")))

;; sensekit_status_t destroy_stream_set(sensekit_streamset_t& setHandle)
(add-func       :funcset "plugin"
                :returntype "sensekit_status_t"
                :funcname "destroy_stream_set"
                :params (list (make-param :type "sensekit_streamset_t&" :name "setHandle")))

;; sensekit_status_t create_stream(sensekit_streamset_t setHandle,
;;                                 sensekit_stream_desc_t desc,
;;                                 stream_callbacks_t pluginCallbacks,
;;                                 sensekit_stream_t* handle)
(add-func       :funcset "plugin"
                :returntype "sensekit_status_t"
                :funcname "create_stream"
                :params (list (make-param :type "sensekit_streamset_t" :name "setHandle")
                              (make-param :type "sensekit_stream_desc_t" :name "desc")
                              (make-param :type "stream_callbacks_t" :name "pluginCallbacks")
                              (make-param :type "sensekit_stream_t*" :name "handle" :deref T)))

;; sensekit_status_t destroy_stream(sensekit_stream_t& handle)
(add-func       :funcset "plugin"
                :returntype "sensekit_status_t"
                :funcname "destroy_stream"
                :params (list (make-param :type "sensekit_stream_t&" :name "handle")))

;; sensekit_status_t create_stream_bin(sensekit_stream_t streamHandle,
;;                                     size_t lengthInBytes,
;;                                     sensekit_bin_t* binHandle,
;;                                     sensekit_frame_t** binBuffer)
(add-func       :funcset "plugin"
                :returntype "sensekit_status_t"
                :funcname "create_stream_bin"
                :params (list (make-param :type "sensekit_stream_t" :name "streamHandle")
                              (make-param :type "size_t" :name "lengthInBytes")
                              (make-param :type "sensekit_bin_t*" :name "binHandle" :deref t)
                              (make-param :type "sensekit_frame_t**" :name "binBuffer" :deref t)))

;; sensekit_status_t destroy_stream_bin(sensekit_stream_t handle,
;;                                      sensekit_bin_t* binHandle,
;;                                      sensekit_frame_t** binBuffer)
(add-func       :funcset "plugin"
                :returntype "sensekit_status_t"
                :funcname "destroy_stream_bin"
                :params (list (make-param :type "sensekit_stream_t" :name "streamHandle")
                              (make-param :type "sensekit_bin_t*" :name "binHandle" :deref t)
                              (make-param :type "sensekit_frame_t**" :name "binBuffer" :deref t)))

;; sensekit_status_t cycle_bin_buffers(sensekit_bin_t binHandle,
;;                                     sensekit_frame_t** binBuffer)
(add-func       :funcset "plugin"
                :returntype "sensekit_status_t"
                :funcname "cycle_bin_buffers"
                :params (list (make-param :type "sensekit_bin_t" :name "binHandle")
                              (make-param :type "sensekit_frame_t**" :name "binBuffer" :deref t)))

;; sensekit_status_t link_connection_to_bin(sensekit_streamconnection_t* connection,
;;                                          sensekit_bin_t binHandle)
(add-func       :funcset "plugin"
                :returntype "sensekit_status_t"
                :funcname "link_connection_to_bin"
                :params (list (make-param :type "sensekit_streamconnection_t*" :name "connection")
                              (make-param :type "sensekit_bin_t" :name "binHandle")))

;; SENSEKIT_API sensekit_status_t sensekit_initialize();
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "initialize"
                :params '())

;; SENSEKIT_API sensekit_status_t sensekit_terminate();
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "terminate"
                :params '())

;;SENSEKIT_API sensekit_status_t sensekit_streamset_open(const char* connectionString,
;;                                                       sensekit_streamset_t** streamSet);
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "streamset_open"
                :params (list (make-param :type "const char*" :name "connectionString")
                              (make-param :type "sensekit_streamset_t*" :name "streamSet" :deref T)))

;; SENSEKIT_API sensekit_status_t sensekit_streamset_close(sensekit_streamset_t** streamSet);
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "streamset_close"
                :params (list (make-param :type "sensekit_streamset_t*" :name "streamSet" :deref T)))

;; SENSEKIT_API char* sensekit_get_status_string(sensekit_status_t status);
(add-func       :funcset "stream"
                :returntype "char*"
                :funcname "get_status_string"
                :params (list (make-param :type "sensekit_status_t" :name "status")))

;; SENSEKIT_API sensekit_status_t sensekit_reader_create(sensekit_streamset_t streamSet,
;;                                                       sensekit_reader_t* reader);
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "reader_create"
                :params (list (make-param :type "sensekit_streamset_t" :name "streamSet")
                              (make-param :type "sensekit_reader_t*" :name "reader" :deref T)))

;; SENSEKIT_API sensekit_status_t sensekit_reader_destroy(sensekit_reader_t* reader);
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "reader_destroy"
                :params (list (make-param :type "sensekit_reader_t*" :name "reader" :deref T)))

;; SENSEKIT_API sensekit_status_t sensekit_reader_get_stream(sensekit_reader_t reader,
;;                                                           sensekit_stream_type_t type,
;;                                                           sensekit_stream_subtype_t subType,
;;                                                           sensekit_streamconnection_t** connection);
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "reader_get_stream"
                :params (list (make-param :type "sensekit_reader_t" :name "reader")
                              (make-param :type "sensekit_stream_type_t" :name "type")
                              (make-param :type "sensekit_stream_subtype_t" :name "subType")
                              (make-param :type "sensekit_streamconnection_t**" :name "connection" :deref T)))

;; SENSEKIT_API sensekit_status_t sensekit_stream_get_description(sensekit_streamconnection_t* connection,
;;                                                                sensekit_stream_desc_t* description);
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "stream_get_description"
                :params (list (make-param :type "sensekit_streamconnection_t*" :name "connection")
                              (make-param :type "sensekit_stream_desc_t*" :name "description")))

;; SENSEKIT_API sensekit_status_t sensekit_stream_start(sensekit_streamconnection_t* connection);
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "stream_start"
                :params (list (make-param :type "sensekit_streamconnection_t*" :name "connection")))

;; SENSEKIT_API sensekit_status_t sensekit_stream_stop(sensekit_streamconnection_t* connection);
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "stream_stop"
                :params (list (make-param :type "sensekit_streamconnection_t*" :name "connection")))

;; SENSEKIT_API sensekit_status_t sensekit_reader_open_frame(sensekit_reader_t reader,
;;                                                           int timeoutMillis,
;;                                                           sensekit_reader_frame_t* frame); //0 = return immediately
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "reader_open_frame"
                :params (list (make-param :type "sensekit_reader_t" :name "reader")
                              (make-param :type "int" :name "timeoutMillis")
                              (make-param :type "sensekit_reader_frame_t*" :name "frame" :deref T)))

;; SENSEKIT_API sensekit_status_t sensekit_reader_close_frame(sensekit_reader_frame_t* frame); //frame set to null
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "reader_close_frame"
                :params (list (make-param :type "sensekit_reader_frame_t*" :name "frame" :deref T)))

;; SENSEKIT_API sensekit_status_t reader_register_frame_ready_callback(sensekit_reader_t reader,
;;                                                                     FrameReadyCallback callback,
;;                                                                     sensekit_reader_callback_id_t* callbackId)
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "reader_register_frame_ready_callback"
                :params (list (make-param :type "sensekit_reader_t" :name "reader")
                              (make-param :type "FrameReadyCallback" :name "callback")
                              (make-param :type "sensekit_reader_callback_id_t*" :name "callbackId" :deref T)))

;; SENSEKIT_API sensekit_status_t reader_unregister_frame_ready_callback(sensekit_reader_callback_id_t* callbackId)
(add-func       :funcset "stream"
                :returntype "sensekit_status_t" 
                :funcname "reader_unregister_frame_ready_callback"
                :params (list (make-param :type "sensekit_reader_callback_id_t*" :name "callbackId" :deref T)))

;; SENSEKIT_API sensekit_status_t sensekit_reader_get_frame(sensekit_reader_frame_t frame,
;;                                                          sensekit_stream_type_t type,
;;                                                          sensekit_stream_subtype_t subType,
;;                                                          sensekit_frame_ref_t** frameRef);
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "reader_get_frame"
                :params (list (make-param :type "sensekit_reader_frame_t" :name "frame")
                              (make-param :type "sensekit_stream_type_t" :name "type")
                              (make-param :type "sensekit_stream_subtype_t" :name "subType")
                              (make-param :type "sensekit_frame_ref_t**" :name "frameRef" :deref T)))

;; SENSEKIT_API sensekit_status_t sensekit_stream_set_parameter(sensekit_streamconnection_t* connection,
;;                                                              sensekit_parameter_id parameterId,
;;                                                              size_t byteLength,
;;                                                              sensekit_parameter_data_t* data);
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "stream_set_parameter"
                :params (list (make-param :type "sensekit_streamconnection_t*" :name "connection")
                              (make-param :type "sensekit_parameter_id" :name "parameterId")
                              (make-param :type "size_t" :name "byteLength")
                              (make-param :type "sensekit_parameter_data_t*" :name "data")))

;; SENSEKIT_API sensekit_status_t sensekit_stream_get_parameter_size(sensekit_streamconnection_t* connection,
;;                                                                   sensekit_parameter_id parameterId,
;;                                                                   size_t* byteLength);
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "stream_get_parameter_size"
                :params (list (make-param :type "sensekit_streamconnection_t*" :name "connection")
                              (make-param :type "sensekit_parameter_id" :name "parameterId")
                              (make-param :type "size_t*" :name "byteLength" :deref T)))

;; SENSEKIT_API sensekit_status_t sensekit_stream_get_parameter_data(sensekit_streamconnection_t* connection,
;;                                                                   sensekit_parameter_id parameterId,
;;                                                                   size_t byteLength,
;;                                                                   sensekit_parameter_data_t* data);
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "stream_get_parameter_data"
                :params (list (make-param :type "sensekit_streamconnection_t*" :name "connection")
                              (make-param :type "sensekit_parameter_id" :name "parameterId")
                              (make-param :type "size_t" :name "byteLength")
                              (make-param :type "sensekit_parameter_data_t*" :name "data")))

;; SENSEKIT_API sensekit_status_t sensekit_temp_update();
(add-func       :funcset "stream"
                :returntype "sensekit_status_t"
                :funcname "temp_update"
                :params '())
