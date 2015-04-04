/* THIS FILE AUTO-GENERATED FROM StreamServiceProxyBase.h.lpp. DO NOT EDIT. */
#ifndef STREAMSERVICEPROXYBASE_H
#define STREAMSERVICEPROXYBASE_H

#include <sensekit_core.h>

struct StreamServiceProxyBase
{
    void* streamService;

    sensekit_status_t (*initialize)(void*);
	
    sensekit_status_t (*terminate)(void*);
	
    sensekit_status_t (*streamset_open)(void*,
                                        const char*,
                                        sensekit_streamset_t**);
	
    sensekit_status_t (*streamset_close)(void*,
                                         sensekit_streamset_t**);
	
    char* (*get_status_string)(void*,
                               sensekit_status_t);
	
    sensekit_status_t (*reader_create)(void*,
                                       sensekit_streamset_t*,
                                       sensekit_reader_t**);
	
    sensekit_status_t (*reader_destroy)(void*,
                                        sensekit_reader_t**);
	
    sensekit_status_t (*reader_get_stream)(void*,
                                           sensekit_reader_t*,
                                           sensekit_stream_type_t,
                                           sensekit_stream_subtype_t,
                                           sensekit_streamconnection_t**);
	
    sensekit_status_t (*stream_get_description)(void*,
                                                sensekit_streamconnection_t*,
                                                sensekit_stream_desc_t*);
	
    sensekit_status_t (*stream_start)(void*,
                                      sensekit_streamconnection_t*);
	
    sensekit_status_t (*stream_stop)(void*,
                                     sensekit_streamconnection_t*);
	
    sensekit_status_t (*reader_open_frame)(void*,
                                           sensekit_reader_t*,
                                           int,
                                           sensekit_reader_frame_t**);
	
    sensekit_status_t (*reader_close_frame)(void*,
                                            sensekit_reader_frame_t**);
	
    sensekit_status_t (*reader_get_frame)(void*,
                                          sensekit_reader_frame_t*,
                                          sensekit_stream_type_t,
                                          sensekit_stream_subtype_t,
                                          sensekit_frame_ref_t**);
	
    sensekit_status_t (*stream_set_parameter)(void*,
                                              sensekit_streamconnection_t*,
                                              sensekit_parameter_id,
                                              size_t,
                                              sensekit_parameter_data_t*);
	
    sensekit_status_t (*stream_get_parameter_size)(void*,
                                                   sensekit_streamconnection_t*,
                                                   sensekit_parameter_id,
                                                   size_t*);
	
    sensekit_status_t (*stream_get_parameter_data)(void*,
                                                   sensekit_streamconnection_t*,
                                                   sensekit_parameter_id,
                                                   size_t,
                                                   sensekit_parameter_data_t*);
	
    sensekit_status_t (*temp_update)(void*);
	
};

#endif /* STREAMSERVICEPROXYBASE_H */
