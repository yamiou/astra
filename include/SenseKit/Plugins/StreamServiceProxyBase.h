#ifndef STREAMSERVICEPROXYBASE_H
#define STREAMSERVICEPROXYBASE_H

#include <sensekit_core.h>

struct StreamServiceProxyBase
{
    void* streamService;
    sensekit_status_t (*open_streamset)(void*,
                                        const char*,
                                        sensekit_streamset_t**);

    sensekit_status_t (*close_streamset)(void*,
                                         sensekit_streamset_t**);

    sensekit_status_t (*create_reader)(void*,
                                       sensekit_streamset_t*,
                                       sensekit_reader_t**);

    sensekit_status_t (*destroy_reader)(void*,
                                        sensekit_reader_t**);

    sensekit_status_t (*get_stream)(void*,
                                    sensekit_reader_t*,
                                    sensekit_stream_type_t,
                                    sensekit_stream_subtype_t,
                                    sensekit_streamconnection_t**);

    sensekit_status_t (*start_stream)(void*,
                                      sensekit_streamconnection_t*);

    sensekit_status_t (*stop_stream)(void*,
                                      sensekit_streamconnection_t*);

    sensekit_status_t (*open_frame)(void*,
                                    sensekit_streamconnection_t*,
                                    int,
                                    sensekit_frame_ref_t**);

    sensekit_status_t (*close_frame)(void*,
                                     sensekit_frame_ref_t**);

    sensekit_status_t (*temp_update)(void*);

    sensekit_status_t (*set_parameter)(void*,
                                       sensekit_streamconnection_t*,
                                       sensekit_parameter_id,
                                       size_t,
                                       sensekit_parameter_data_t*);

    sensekit_status_t (*get_parameter_size)(void*,
                                            sensekit_streamconnection_t*,
                                            sensekit_parameter_id,
                                            size_t*);

    sensekit_status_t (*get_parameter_data)(void*,
                                            sensekit_streamconnection_t*,
                                            sensekit_parameter_id,
                                            size_t,
                                            sensekit_parameter_data_t*);
};

#endif /* STREAMSERVICEPROXYBASE_H */
