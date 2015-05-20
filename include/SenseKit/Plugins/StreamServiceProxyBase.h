/* THIS FILE AUTO-GENERATED FROM StreamServiceProxyBase.h.lpp. DO NOT EDIT. */
#ifndef STREAMSERVICEPROXYBASE_H
#define STREAMSERVICEPROXYBASE_H

#include <SenseKit/sensekit_types.h>

struct StreamServiceProxyBase
{
    void* streamService;

    sensekit_status_t (*initialize)(void*);

    sensekit_status_t (*terminate)(void*);

    sensekit_status_t (*streamset_open)(void*,
                                        const char*,
                                        sensekit_streamset_t*);

    sensekit_status_t (*streamset_close)(void*,
                                         sensekit_streamset_t*);

    char* (*get_status_string)(void*,
                               sensekit_status_t);

    sensekit_status_t (*reader_create)(void*,
                                       sensekit_streamset_t,
                                       sensekit_reader_t*);

    sensekit_status_t (*reader_destroy)(void*,
                                        sensekit_reader_t*);

    sensekit_status_t (*reader_get_stream)(void*,
                                           sensekit_reader_t,
                                           sensekit_stream_type_t,
                                           sensekit_stream_subtype_t,
                                           sensekit_streamconnection_t*);

    sensekit_status_t (*stream_get_description)(void*,
                                                sensekit_streamconnection_t,
                                                sensekit_stream_desc_t*);

    sensekit_status_t (*stream_start)(void*,
                                      sensekit_streamconnection_t);

    sensekit_status_t (*stream_stop)(void*,
                                     sensekit_streamconnection_t);

    sensekit_status_t (*reader_open_frame)(void*,
                                           sensekit_reader_t,
                                           int,
                                           sensekit_reader_frame_t*);

    sensekit_status_t (*reader_close_frame)(void*,
                                            sensekit_reader_frame_t*);

    sensekit_status_t (*reader_register_frame_ready_callback)(void*,
                                                              sensekit_reader_t,
                                                              sensekit_frame_ready_callback_t,
                                                              void*,
                                                              sensekit_reader_callback_id_t*);

    sensekit_status_t (*reader_unregister_frame_ready_callback)(void*,
                                                                sensekit_reader_callback_id_t*);

    sensekit_status_t (*reader_get_frame)(void*,
                                          sensekit_reader_frame_t,
                                          sensekit_stream_type_t,
                                          sensekit_stream_subtype_t,
                                          sensekit_frame_t**);

    sensekit_status_t (*stream_set_parameter)(void*,
                                              sensekit_streamconnection_t,
                                              sensekit_parameter_id,
                                              size_t,
                                              sensekit_parameter_data_t);

    sensekit_status_t (*stream_get_parameter)(void*,
                                              sensekit_streamconnection_t,
                                              sensekit_parameter_id,
                                              size_t*,
                                              sensekit_result_token_t*);

    sensekit_status_t (*stream_get_result)(void*,
                                           sensekit_streamconnection_t,
                                           sensekit_result_token_t,
                                           size_t,
                                           sensekit_parameter_data_t);

    sensekit_status_t (*stream_invoke)(void*,
                                       sensekit_streamconnection_t,
                                       sensekit_command_id,
                                       size_t,
                                       sensekit_parameter_data_t,
                                       size_t*,
                                       sensekit_result_token_t*);

    sensekit_status_t (*temp_update)(void*);

};

#endif /* STREAMSERVICEPROXYBASE_H */
