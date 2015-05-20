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
                                                          sensekit_stream_subtype_t subtype,
                                                          sensekit_streamconnection_t* connection)
{
    return g_Context.reader_get_stream(reader, type, subtype, *connection);
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
                                                         sensekit_stream_subtype_t subtype,
                                                         sensekit_frame_t** subFrame)
{
    return g_Context.reader_get_frame(frame, type, subtype, *subFrame);
}

SENSEKIT_API sensekit_status_t sensekit_stream_set_parameter(sensekit_streamconnection_t connection,
                                                             sensekit_parameter_id parameterId,
                                                             size_t inByteLength,
                                                             sensekit_parameter_data_t inData)
{
    return g_Context.stream_set_parameter(connection, parameterId, inByteLength, inData);
}

SENSEKIT_API sensekit_status_t sensekit_stream_get_parameter(sensekit_streamconnection_t connection,
                                                             sensekit_parameter_id parameterId,
                                                             size_t* resultByteLength,
                                                             sensekit_result_token_t* token)
{
    return g_Context.stream_get_parameter(connection, parameterId, *resultByteLength, *token);
}

SENSEKIT_API sensekit_status_t sensekit_stream_get_result(sensekit_streamconnection_t connection,
                                                          sensekit_result_token_t token,
                                                          size_t dataByteLength,
                                                          sensekit_parameter_data_t dataDestination)
{
    return g_Context.stream_get_result(connection, token, dataByteLength, dataDestination);
}

SENSEKIT_API sensekit_status_t sensekit_stream_invoke(sensekit_streamconnection_t connection,
                                                      sensekit_command_id commandId,
                                                      size_t inByteLength,
                                                      sensekit_parameter_data_t inData,
                                                      size_t* resultByteLength,
                                                      sensekit_result_token_t* token)
{
    return g_Context.stream_invoke(connection, commandId, inByteLength, inData, *resultByteLength, *token);
}

SENSEKIT_API sensekit_status_t sensekit_temp_update()
{
    return g_Context.temp_update();
}

SENSEKIT_END_DECLS
