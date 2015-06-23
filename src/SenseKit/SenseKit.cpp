/* THIS FILE AUTO-GENERATED FROM SenseKit.cpp.lpp. DO NOT EDIT. */
#include "SenseKitContext.h"
#include <SenseKit/Plugins/StreamServiceProxyBase.h>
#include <SenseKitAPI.h>
#include <SenseKit/host_events.h>

static sensekit::SenseKitContext* g_ContextPtr = nullptr;
static bool g_Initialized = false;

SENSEKIT_BEGIN_DECLS

SENSEKIT_API sensekit_status_t sensekit_initialize()
{
    if (g_Initialized)
        return SENSEKIT_STATUS_SUCCESS;

    g_Initialized = true;
    g_ContextPtr = new sensekit::SenseKitContext();
    return g_ContextPtr->initialize();
}

SENSEKIT_API sensekit_status_t sensekit_terminate()
{
    if (!g_Initialized)
        return SENSEKIT_STATUS_SUCCESS;

    sensekit_status_t rc =  g_ContextPtr->terminate();
    delete g_ContextPtr;
    g_ContextPtr = nullptr;
    g_Initialized = false;
    return rc;
}

SENSEKIT_API sensekit_status_t sensekit_streamset_open(const char* connectionString,
                                                       sensekit_streamsetconnection_t* streamSet)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->streamset_open(connectionString, *streamSet);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_streamset_close(sensekit_streamsetconnection_t* streamSet)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->streamset_close(*streamSet);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_reader_create(sensekit_streamsetconnection_t streamSet,
                                                      sensekit_reader_t* reader)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_create(streamSet, *reader);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_reader_destroy(sensekit_reader_t* reader)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_destroy(*reader);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_reader_get_stream(sensekit_reader_t reader,
                                                          sensekit_stream_type_t type,
                                                          sensekit_stream_subtype_t subtype,
                                                          sensekit_streamconnection_t* connection)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_get_stream(reader, type, subtype, *connection);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_stream_get_description(sensekit_streamconnection_t connection,
                                                               sensekit_stream_desc_t* description)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->stream_get_description(connection, description);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_stream_start(sensekit_streamconnection_t connection)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->stream_start(connection);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_stream_stop(sensekit_streamconnection_t connection)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->stream_stop(connection);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_reader_open_frame(sensekit_reader_t reader,
                                                          int timeoutMillis,
                                                          sensekit_reader_frame_t* frame)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_open_frame(reader, timeoutMillis, *frame);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_reader_close_frame(sensekit_reader_frame_t* frame)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_close_frame(*frame);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_reader_register_frame_ready_callback(sensekit_reader_t reader,
                                                                             sensekit_frame_ready_callback_t callback,
                                                                             void* clientTag,
                                                                             sensekit_reader_callback_id_t* callbackId)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_register_frame_ready_callback(reader, callback, clientTag, *callbackId);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_reader_unregister_frame_ready_callback(sensekit_reader_callback_id_t* callbackId)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_unregister_frame_ready_callback(*callbackId);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_reader_get_frame(sensekit_reader_frame_t frame,
                                                         sensekit_stream_type_t type,
                                                         sensekit_stream_subtype_t subtype,
                                                         sensekit_frame_t** subFrame)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_get_frame(frame, type, subtype, *subFrame);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_stream_set_parameter(sensekit_streamconnection_t connection,
                                                             sensekit_parameter_id parameterId,
                                                             size_t inByteLength,
                                                             sensekit_parameter_data_t inData)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->stream_set_parameter(connection, parameterId, inByteLength, inData);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_stream_get_parameter(sensekit_streamconnection_t connection,
                                                             sensekit_parameter_id parameterId,
                                                             size_t* resultByteLength,
                                                             sensekit_result_token_t* token)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->stream_get_parameter(connection, parameterId, *resultByteLength, *token);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_stream_get_result(sensekit_streamconnection_t connection,
                                                          sensekit_result_token_t token,
                                                          size_t dataByteLength,
                                                          sensekit_parameter_data_t dataDestination)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->stream_get_result(connection, token, dataByteLength, dataDestination);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_stream_invoke(sensekit_streamconnection_t connection,
                                                      sensekit_command_id commandId,
                                                      size_t inByteLength,
                                                      sensekit_parameter_data_t inData,
                                                      size_t* resultByteLength,
                                                      sensekit_result_token_t* token)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->stream_invoke(connection, commandId, inByteLength, inData, *resultByteLength, *token);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_temp_update()
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->temp_update();
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_notify_host_event(sensekit_event_id id, const void* data, size_t dataSize)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->notify_host_event(id, data, dataSize);
    }
    else
    {
        return SENSEKIT_STATUS_UNINITIALIZED;
    }
}

SENSEKIT_API sensekit_status_t sensekit_notify_resource_available(const char* resourceURI)
{
    return sensekit_notify_host_event(SENSEKIT_EVENT_RESOURCE_AVAILABLE, resourceURI, strlen(resourceURI));
}

SENSEKIT_API sensekit_status_t sensekit_notify_resource_unavailable(const char* resourceURI)
{
    return sensekit_notify_host_event(SENSEKIT_EVENT_RESOURCE_UNAVAILABLE, resourceURI, strlen(resourceURI));
}

SENSEKIT_END_DECLS
