#ifndef STREAMSERVICEPROXY_H
#define STREAMSERVICEPROXY_H

#include "StreamServiceProxyBase.h"

namespace sensekit {

    class StreamServiceProxy : public StreamServiceProxyBase
    {
    public:
        sensekit_status_t open_streamset(const char* uri, sensekit_streamset_t** streamSet)
            {
                return StreamServiceProxyBase::open_streamset(streamService, uri, streamSet);
            }

        sensekit_status_t close_streamset(sensekit_streamset_t** streamSet)
            {
                return StreamServiceProxyBase::close_streamset(streamService, streamSet);
            }

        sensekit_status_t get_stream(sensekit_reader_t* reader,
                                     sensekit_stream_type_t type,
                                     sensekit_stream_subtype_t subType,
                                     sensekit_streamconnection_t** connection)
            {
                return StreamServiceProxyBase::get_stream(streamService, reader, type, subType, connection);
            }

        sensekit_status_t create_reader(sensekit_streamset_t* streamSet, sensekit_reader_t** reader)
            {
                return StreamServiceProxyBase::create_reader(streamService, streamSet, reader);
            }

        sensekit_status_t destroy_reader(sensekit_reader_t** reader)
            {
                return StreamServiceProxyBase::destroy_reader(streamService, reader);
            }

        sensekit_status_t open_frame(sensekit_streamconnection_t* connection,
                                     int timeoutMillis,
                                     sensekit_frame_ref_t** frameRef)
            {
                return StreamServiceProxyBase::open_frame(streamService, connection, timeoutMillis, frameRef);
            }

        sensekit_status_t close_frame(sensekit_frame_ref_t** frameRef)
            {
                return StreamServiceProxyBase::close_frame(streamService, frameRef);
            }

        sensekit_status_t temp_update()
            {
                return StreamServiceProxyBase::temp_update(streamService);
            }

        sensekit_status_t set_parameter(sensekit_streamconnection_t* connection,
                                        sensekit_parameter_id parameterId,
                                        size_t byteLength,
                                        sensekit_parameter_data_t* data)
            {
                return StreamServiceProxyBase::set_parameter(streamService, connection, parameterId, byteLength, data);
            }

        sensekit_status_t get_parameter_size(sensekit_streamconnection_t* connection,
                                             sensekit_parameter_id parameterId,
                                             size_t* byteLength)
            {
                return StreamServiceProxyBase::get_parameter_size(streamService, connection, parameterId, byteLength);
            }

        sensekit_status_t get_parameter_data(sensekit_streamconnection_t* connection,
                                             sensekit_parameter_id parameterId,
                                             size_t byteLength,
                                             sensekit_parameter_data_t* data)
            {
                return StreamServiceProxyBase::get_parameter_data(streamService,
                                                                  connection,
                                                                  parameterId,
                                                                  byteLength,
                                                                  data);
            }
    };
}

#endif /* STREAMSERVICEPROXY_H */
