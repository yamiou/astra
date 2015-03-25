#ifndef STREAMSERVICEPROXYBASE_H
#define STREAMSERVICEPROXYBASE_H

#include "sensekit_types.h"

struct StreamServiceProxyBase
{
    void* streamService;
    sensekit_status_t (*open_streamset)(void* service, const char* uri, sensekit_streamset_t** streamset);
    sensekit_status_t (*close_streamset)(void* service, sensekit_streamset_t** streamset);
    sensekit_status_t (*open_stream)(void* service, sensekit_streamset_t* streamset, sensekit_stream_type_t type,
                                     sensekit_stream_subtype_t subtype,
                                     sensekit_streamconnection_t** stream_connection);
    sensekit_status_t (*close_stream)(void* service, sensekit_streamconnection_t** stream_connection);
    sensekit_status_t (*open_frame)(void* service, sensekit_streamconnection_t* stream_connection,
                                    int timeout, sensekit_frame_ref_t** frameRef);
    sensekit_status_t (*close_frame)(void* service, sensekit_frame_ref_t** frameRef);
    sensekit_status_t (*temp_update)(void* service);
    sensekit_status_t (*set_parameter)(void* service, sensekit_streamconnection_t* stream_connection,
                                       sensekit_parameter_id parameter_id,
                                       size_t byte_length, sensekit_parameter_data_t* data);
    sensekit_status_t (*get_parameter_size)(void* service, sensekit_streamconnection_t* stream_connection,
                                            sensekit_parameter_id parameter_id, /*out*/size_t* byte_length);
    sensekit_status_t (*get_parameter_data)(void* service, sensekit_streamconnection_t* stream_connection,
                                            sensekit_parameter_id parameter_id, size_t byte_length,
                                            sensekit_parameter_data_t* data);
};

#endif /* STREAMSERVICEPROXYBASE_H */
