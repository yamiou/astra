#include <SenseKit.h>
#include <SenseKitAPI.h>
#include <Plugins/StreamServiceProxy.h>

SENSEKIT_BEGIN_DECLS

SENSEKIT_API sensekit_status_t sensekit_open_streamset(const char* connection_string, /*out*/ sensekit_streamset_t** streamset)
{
    return reinterpret_cast<sensekit::StreamServiceProxy*>(g_proxyPtr)->open_streamset(connection_string, streamset);
}

SENSEKIT_API sensekit_status_t sensekit_close_streamset(sensekit_streamset_t** streamset)
{
    return reinterpret_cast<sensekit::StreamServiceProxy*>(g_proxyPtr)->close_streamset(streamset);
}

SENSEKIT_API sensekit_status_t sensekit_stream_open(sensekit_streamset_t* streamset, sensekit_stream_type_t type,
                                                    sensekit_stream_subtype_t subtype, sensekit_streamconnection_t** stream_connection)
{
    return reinterpret_cast<sensekit::StreamServiceProxy*>(g_proxyPtr)->open_stream(streamset, type, subtype, stream_connection);
}

SENSEKIT_API sensekit_status_t sensekit_stream_close(sensekit_streamconnection_t** stream_connection)
{
    return reinterpret_cast<sensekit::StreamServiceProxy*>(g_proxyPtr)->close_stream(stream_connection);
}

SENSEKIT_API sensekit_status_t sensekit_stream_frame_open(sensekit_streamconnection_t* stream_connection, int timeout, sensekit_frame_ref_t** frame)
{
    return reinterpret_cast<sensekit::StreamServiceProxy*>(g_proxyPtr)->open_frame(stream_connection, timeout, frame);
}

SENSEKIT_API sensekit_status_t sensekit_stream_frame_close(sensekit_frame_ref_t** frame)
{
    return reinterpret_cast<sensekit::StreamServiceProxy*>(g_proxyPtr)->close_frame(frame);
}

SENSEKIT_API sensekit_status_t sensekit_temp_update()
{
    return reinterpret_cast<sensekit::StreamServiceProxy*>(g_proxyPtr)->temp_update();
}

SENSEKIT_API sensekit_status_t sensekit_stream_set_parameter(sensekit_streamconnection_t* stream_connection, sensekit_parameter_id parameter_id, size_t byte_length, sensekit_parameter_data_t* data)
{
    return reinterpret_cast<sensekit::StreamServiceProxy*>(g_proxyPtr)->set_parameter(stream_connection, parameter_id, byte_length, data);
}

SENSEKIT_API sensekit_status_t sensekit_stream_get_parameter_size(sensekit_streamconnection_t* stream_connection, sensekit_parameter_id parameter_id, /*out*/size_t* byte_length)
{
    return reinterpret_cast<sensekit::StreamServiceProxy*>(g_proxyPtr)->get_parameter_size(stream_connection, parameter_id, byte_length);
}

SENSEKIT_API sensekit_status_t sensekit_stream_get_parameter_data(sensekit_streamconnection_t* stream_connection, sensekit_parameter_id parameter_id, size_t byte_length, sensekit_parameter_data_t* data)
{
    return reinterpret_cast<sensekit::StreamServiceProxy*>(g_proxyPtr)->get_parameter_data(stream_connection, parameter_id, byte_length, data);
}

SENSEKIT_END_DECLS
