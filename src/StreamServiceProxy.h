#ifndef STREAMSERVICEPROXY_H
#define STREAMSERVICEPROXY_H

#include "StreamServiceProxyBase.h"

namespace sensekit {

    class StreamServiceProxy : public StreamServiceProxyBase
    {
    public:
        sensekit_status_t open_streamset(const char* uri, sensekit_streamset_t** streamset)
            {
                return StreamServiceProxyBase::open_streamset(streamService, uri, streamset);
            }

        sensekit_status_t close_streamset(sensekit_streamset_t** streamset)
            {
                return StreamServiceProxyBase::close_streamset(streamService, streamset);
            }

        sensekit_status_t open_stream(sensekit_streamset_t* streamset, sensekit_stream_type_t type, sensekit_stream_subtype_t subtype, sensekit_streamconnection_t** stream_connection)
            {
                return StreamServiceProxyBase::open_stream(streamService, streamset, type, subtype, stream_connection);
            }

        sensekit_status_t close_stream(sensekit_streamconnection_t** stream_connection)
            {
                return StreamServiceProxyBase::close_stream(streamService, stream_connection);
            }

        sensekit_status_t open_frame(sensekit_streamconnection_t* stream_connection, int timeout, sensekit_frame_ref_t** frameRef)
            {
                return StreamServiceProxyBase::open_frame(streamService, stream_connection, timeout, frameRef);
            }

        sensekit_status_t close_frame(sensekit_frame_ref_t** frameRef)
            {
                return StreamServiceProxyBase::close_frame(streamService, frameRef);
            }
        sensekit_status_t temp_update()
            {
                return StreamServiceProxyBase::temp_update(streamService);
            }
        sensekit_status_t set_parameter(sensekit_streamconnection_t* stream_connection, sensekit_parameter_id parameter_id, size_t byte_length, sensekit_parameter_data_t* data)
            {
                return StreamServiceProxyBase::set_parameter(streamService, stream_connection, parameter_id, byte_length, data);
            }
        sensekit_status_t get_parameter_size(sensekit_streamconnection_t* stream_connection, sensekit_parameter_id parameter_id, /*out*/size_t* byte_length)
            {
                return StreamServiceProxyBase::get_parameter_size(streamService, stream_connection, parameter_id, byte_length);
            }
        sensekit_status_t get_parameter_data(sensekit_streamconnection_t* stream_connection, sensekit_parameter_id parameter_id, size_t byte_length, sensekit_parameter_data_t* data)
            {
                return StreamServiceProxyBase::get_parameter_data(streamService, stream_connection, parameter_id, byte_length, data);
            }
    };
}

#endif /* STREAMSERVICEPROXY_H */
