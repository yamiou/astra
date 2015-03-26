#ifndef STREAMSERVICEDELEGATE_H
#define STREAMSERVICEDELEGATE_H

#include <SenseKit.h>
#include "SenseKitContext.h"

namespace sensekit {

    class StreamServiceDelegate
    {
    public:
        static sensekit_status_t open_streamset(void* service, const char* uri, sensekit_streamset_t** streamset)
            {
                return static_cast<SenseKitContext*>(service)->open_streamset(uri, *streamset);
            }
        static sensekit_status_t close_streamset(void* service, sensekit_streamset_t** streamset)
            {
                return static_cast<SenseKitContext*>(service)->close_streamset(*streamset);
            }
        static sensekit_status_t open_stream(void* service, sensekit_streamset_t* streamset, sensekit_stream_type_t type, sensekit_stream_subtype_t subtype, sensekit_streamconnection_t** stream_connection)
            {
                return static_cast<SenseKitContext*>(service)->open_stream(streamset, type, subtype, *stream_connection);
            }
        static sensekit_status_t close_stream(void* service, sensekit_streamconnection_t** stream_connection)
            {
                return static_cast<SenseKitContext*>(service)->close_stream(*stream_connection);
            }
        static sensekit_status_t open_frame(void* service, sensekit_streamconnection_t* stream_connection, int timeout, sensekit_frame_ref_t** frameRef)
            {
                return static_cast<SenseKitContext*>(service)->open_frame(stream_connection, timeout, *frameRef);
            }
        static sensekit_status_t close_frame(void* service, sensekit_frame_ref_t** frameRef)
            {
                return static_cast<SenseKitContext*>(service)->close_frame(*frameRef);
            }
        static sensekit_status_t temp_update(void* service)
            {
                return static_cast<SenseKitContext*>(service)->temp_update();
            }
        static sensekit_status_t set_parameter(void* service, sensekit_streamconnection_t* stream_connection, sensekit_parameter_id parameter_id, size_t byte_length, sensekit_parameter_data_t* data)
            {
                return static_cast<SenseKitContext*>(service)->set_parameter(stream_connection, parameter_id, byte_length, data);
            }
        static sensekit_status_t get_parameter_size(void* service, sensekit_streamconnection_t* stream_connection, sensekit_parameter_id parameter_id, /*out*/size_t* byte_length)
            {
                return static_cast<SenseKitContext*>(service)->get_parameter_size(stream_connection, parameter_id, *byte_length);
            }
        static sensekit_status_t get_parameter_data(void* service, sensekit_streamconnection_t* stream_connection, sensekit_parameter_id parameter_id, size_t byte_length, sensekit_parameter_data_t* data)
            {
                return static_cast<SenseKitContext*>(service)->get_parameter_data(stream_connection, parameter_id, byte_length, data);
            }
    };

}

#endif /* STREAMSERVICEDELEGATE_H */
