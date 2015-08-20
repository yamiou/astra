/* THIS FILE AUTO-GENERATED FROM Astra.cpp.lpp. DO NOT EDIT. */
#include "AstraContext.h"
#include <Astra/Plugins/StreamServiceProxyBase.h>
#include <AstraAPI.h>
#include <Astra/host_events.h>
#include <memory>

static std::unique_ptr<astra::AstraContext> g_ContextPtr(nullptr);
static bool g_Initialized = false;

ASTRA_BEGIN_DECLS

ASTRA_API astra_status_t astra_initialize()
{
    if (g_Initialized)
        return ASTRA_STATUS_SUCCESS;

    g_Initialized = true;
    g_ContextPtr = std::make_unique<astra::AstraContext>();

    return g_ContextPtr->initialize();
}

ASTRA_API astra_status_t astra_terminate()
{
    if (!g_Initialized)
        return ASTRA_STATUS_SUCCESS;

    astra_status_t rc =  g_ContextPtr->terminate();
    g_Initialized = false;

    return rc;
}

ASTRA_API astra_status_t astra_streamset_open(const char* connectionString,
                                              astra_streamsetconnection_t* streamSet)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->streamset_open(connectionString, *streamSet);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_streamset_close(astra_streamsetconnection_t* streamSet)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->streamset_close(*streamSet);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_reader_create(astra_streamsetconnection_t streamSet,
                                             astra_reader_t* reader)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_create(streamSet, *reader);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_reader_destroy(astra_reader_t* reader)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_destroy(*reader);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_reader_get_stream(astra_reader_t reader,
                                                 astra_stream_type_t type,
                                                 astra_stream_subtype_t subtype,
                                                 astra_streamconnection_t* connection)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_get_stream(reader, type, subtype, *connection);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_stream_get_description(astra_streamconnection_t connection,
                                                      astra_stream_desc_t* description)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->stream_get_description(connection, description);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_stream_start(astra_streamconnection_t connection)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->stream_start(connection);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_stream_stop(astra_streamconnection_t connection)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->stream_stop(connection);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_reader_open_frame(astra_reader_t reader,
                                                 int timeoutMillis,
                                                 astra_reader_frame_t* frame)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_open_frame(reader, timeoutMillis, *frame);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_reader_close_frame(astra_reader_frame_t* frame)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_close_frame(*frame);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_reader_register_frame_ready_callback(astra_reader_t reader,
                                                                    astra_frame_ready_callback_t callback,
                                                                    void* clientTag,
                                                                    astra_reader_callback_id_t* callbackId)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_register_frame_ready_callback(reader, callback, clientTag, *callbackId);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_reader_unregister_frame_ready_callback(astra_reader_callback_id_t* callbackId)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_unregister_frame_ready_callback(*callbackId);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_reader_get_frame(astra_reader_frame_t frame,
                                                astra_stream_type_t type,
                                                astra_stream_subtype_t subtype,
                                                astra_frame_t** subFrame)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->reader_get_frame(frame, type, subtype, *subFrame);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_stream_set_parameter(astra_streamconnection_t connection,
                                                    astra_parameter_id parameterId,
                                                    size_t inByteLength,
                                                    astra_parameter_data_t inData)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->stream_set_parameter(connection, parameterId, inByteLength, inData);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_stream_get_parameter(astra_streamconnection_t connection,
                                                    astra_parameter_id parameterId,
                                                    size_t* resultByteLength,
                                                    astra_result_token_t* token)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->stream_get_parameter(connection, parameterId, *resultByteLength, *token);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_stream_get_result(astra_streamconnection_t connection,
                                                 astra_result_token_t token,
                                                 size_t dataByteLength,
                                                 astra_parameter_data_t dataDestination)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->stream_get_result(connection, token, dataByteLength, dataDestination);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_stream_invoke(astra_streamconnection_t connection,
                                             astra_command_id commandId,
                                             size_t inByteLength,
                                             astra_parameter_data_t inData,
                                             size_t* resultByteLength,
                                             astra_result_token_t* token)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->stream_invoke(connection, commandId, inByteLength, inData, *resultByteLength, *token);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_temp_update()
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->temp_update();
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_notify_host_event(astra_event_id id, const void* data, size_t dataSize)
{
    if (g_ContextPtr)
    {
        return g_ContextPtr->notify_host_event(id, data, dataSize);
    }
    else
    {
        return ASTRA_STATUS_UNINITIALIZED;
    }
}

ASTRA_API astra_status_t astra_notify_resource_available(const char* resourceURI)
{
    return astra_notify_host_event(ASTRA_EVENT_RESOURCE_AVAILABLE, resourceURI, strlen(resourceURI));
}

ASTRA_API astra_status_t astra_notify_resource_unavailable(const char* resourceURI)
{
    return astra_notify_host_event(ASTRA_EVENT_RESOURCE_UNAVAILABLE, resourceURI, strlen(resourceURI));
}

ASTRA_END_DECLS
