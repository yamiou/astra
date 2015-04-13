/* THIS FILE AUTO-GENERATED FROM SenseKit.cpp.lpp. DO NOT EDIT. */
#include "SenseKitContext.h"
#include <SenseKit/Plugins/StreamServiceProxyBase.h>
#include <SenseKitAPI.h>

static sensekit::SenseKitContext g_Context;

SENSEKIT_BEGIN_DECLS

SENSEKIT_API sensekit_status_t sensekit_initialize()
{
    return g_Context.initialize();
}

SENSEKIT_API sensekit_status_t sensekit_terminate()
{
    return g_Context.terminate();
}

SENSEKIT_API sensekit_status_t sensekit_streamset_open(const char* connectionString,
                                                       sensekit_streamset_t* streamSet)
{
    return g_Context.streamset_open(connectionString, *streamSet);
}

SENSEKIT_API sensekit_status_t sensekit_streamset_close(sensekit_streamset_t* streamSet)
{
    return g_Context.streamset_close(*streamSet);
}

SENSEKIT_API char* sensekit_get_status_string(sensekit_status_t status)
{
    return g_Context.get_status_string(status);
}

SENSEKIT_API sensekit_status_t sensekit_reader_create(sensekit_streamset_t streamSet,
                                                      sensekit_reader_t* reader)
{
    return g_Context.reader_create(streamSet, *reader);
}

SENSEKIT_API sensekit_status_t sensekit_reader_destroy(sensekit_reader_t* reader)
{
    return g_Context.reader_destroy(*reader);
}

SENSEKIT_API sensekit_status_t sensekit_reader_get_stream(sensekit_reader_t reader,
                                                          sensekit_stream_type_t type,
                                                          sensekit_stream_subtype_t subType,
                                                          sensekit_streamconnection_t* connection)
{
    return g_Context.reader_get_stream(reader, type, subType, *connection);
}

SENSEKIT_API sensekit_status_t sensekit_stream_get_description(sensekit_streamconnection_t connection,
                                                               sensekit_stream_desc_t* description)
{
    return g_Context.stream_get_description(connection, description);
}

SENSEKIT_API sensekit_status_t sensekit_stream_start(sensekit_streamconnection_t connection)
{
    return g_Context.stream_start(connection);
}

SENSEKIT_API sensekit_status_t sensekit_stream_stop(sensekit_streamconnection_t connection)
{
    return g_Context.stream_stop(connection);
}

SENSEKIT_API sensekit_status_t sensekit_reader_open_frame(sensekit_reader_t reader,
                                                          int timeoutMillis,
                                                          sensekit_reader_frame_t* frame)
{
    return g_Context.reader_open_frame(reader, timeoutMillis, *frame);
}

SENSEKIT_API sensekit_status_t sensekit_reader_close_frame(sensekit_reader_frame_t* frame)
{
    return g_Context.reader_close_frame(*frame);
}

SENSEKIT_API sensekit_status_t sensekit_reader_register_frame_ready_callback(sensekit_reader_t reader,
                                                                             sensekit_frame_ready_callback_t callback,
                                                                             void* clientTag,
                                                                             sensekit_reader_callback_id_t* callbackId)
{
    return g_Context.reader_register_frame_ready_callback(reader, callback, clientTag, *callbackId);
}

SENSEKIT_API sensekit_status_t sensekit_reader_unregister_frame_ready_callback(sensekit_reader_callback_id_t* callbackId)
{
    return g_Context.reader_unregister_frame_ready_callback(*callbackId);
}

SENSEKIT_API sensekit_status_t sensekit_reader_get_frame(sensekit_reader_frame_t frame,
                                                         sensekit_stream_type_t type,
                                                         sensekit_stream_subtype_t subType,
                                                         sensekit_frame_ref_t** frameRef)
{
    return g_Context.reader_get_frame(frame, type, subType, *frameRef);
}

SENSEKIT_API sensekit_status_t sensekit_stream_set_parameter(sensekit_streamconnection_t connection,
                                                             sensekit_parameter_id parameterId,
                                                             size_t byteLength,
                                                             sensekit_parameter_data_t* data)
{
    return g_Context.stream_set_parameter(connection, parameterId, byteLength, data);
}

SENSEKIT_API sensekit_status_t sensekit_stream_get_parameter_size(sensekit_streamconnection_t connection,
                                                                  sensekit_parameter_id parameterId,
                                                                  size_t* byteLength)
{
    return g_Context.stream_get_parameter_size(connection, parameterId, *byteLength);
}

SENSEKIT_API sensekit_status_t sensekit_stream_get_parameter_data(sensekit_streamconnection_t connection,
                                                                  sensekit_parameter_id parameterId,
                                                                  size_t byteLength,
                                                                  sensekit_parameter_data_t* data)
{
    return g_Context.stream_get_parameter_data(connection, parameterId, byteLength, data);
}

SENSEKIT_API sensekit_status_t sensekit_temp_update()
{
    return g_Context.temp_update();
}

SENSEKIT_END_DECLS
