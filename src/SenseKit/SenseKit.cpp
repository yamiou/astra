#include "SenseKitContext.h"

static sensekit::SenseKitContext g_Context;

SENSEKIT_BEGIN_DECLS

SENSEKIT_API sensekit_context_t* sensekit_initialize()
{
    g_Context.initialize();
    return reinterpret_cast<sensekit_context_t*>(g_Context.get_streamServiceProxy());
}

SENSEKIT_API void sensekit_terminate()
{
    g_Context.terminate();
}

SENSEKIT_API sensekit_status_t sensekit_open_streamset(const char* connection_string, /*out*/ sensekit_streamset_t** streamset)
{
    return g_Context.open_streamset(connection_string, *streamset);
}

SENSEKIT_API sensekit_status_t sensekit_close_streamset(sensekit_streamset_t** streamset)
{
    return g_Context.close_streamset(*streamset);
}

SENSEKIT_API sensekit_status_t sensekit_stream_open(sensekit_streamset_t* streamset, sensekit_stream_type_t type,
                                                    sensekit_stream_subtype_t subtype, sensekit_streamconnection_t** stream_connection)
{
    return g_Context.open_stream(streamset, type, subtype, *stream_connection);
}

SENSEKIT_API sensekit_status_t sensekit_stream_close(sensekit_streamconnection_t** stream_connection)
{
    return g_Context.close_stream(*stream_connection);
}

SENSEKIT_API sensekit_status_t sensekit_stream_frame_open(sensekit_streamconnection_t* stream_connection, int timeout, sensekit_frame_ref_t** frame)
{
    return g_Context.open_frame(stream_connection, timeout, *frame);
}

SENSEKIT_API sensekit_status_t sensekit_stream_frame_close(sensekit_frame_ref_t** frame)
{
    return g_Context.close_frame(*frame);
}

SENSEKIT_API sensekit_status_t sensekit_temp_update()
{
    return g_Context.temp_update();
}

SENSEKIT_API sensekit_status_t sensekit_stream_set_parameter(sensekit_streamconnection_t* stream_connection, sensekit_parameter_id parameter_id, size_t byte_length, sensekit_parameter_data_t* data)
{
    return g_Context.set_parameter(stream_connection, parameter_id, byte_length, data);
}

SENSEKIT_API sensekit_status_t sensekit_stream_get_parameter_size(sensekit_streamconnection_t* stream_connection, sensekit_parameter_id parameter_id, /*out*/size_t* byte_length)
{
    return g_Context.get_parameter_size(stream_connection, parameter_id, *byte_length);
}

SENSEKIT_API sensekit_status_t sensekit_stream_get_parameter_data(sensekit_streamconnection_t* stream_connection, sensekit_parameter_id parameter_id, size_t byte_length, sensekit_parameter_data_t* data)
{
    return g_Context.get_parameter_data(stream_connection, parameter_id, byte_length, data);
}

SENSEKIT_END_DECLS
