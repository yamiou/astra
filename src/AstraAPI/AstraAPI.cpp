/* THIS FILE AUTO-GENERATED FROM AstraAPI.cpp.lpp. DO NOT EDIT. */
#include "AstraAPI.h"
#include <Astra/Plugins/StreamServiceProxy.h>
#include <cassert>

ASTRA_BEGIN_DECLS

ASTRA_LOCAL StreamServiceProxyBase* __astra_api_proxy_ptr = nullptr;

inline astra::StreamServiceProxy* get_api_proxy()
{
    return reinterpret_cast<astra::StreamServiceProxy*>(__astra_api_proxy_ptr);
}

ASTRA_API_PROXY StreamServiceProxyBase* astra_api_get_proxy()
{
    return __astra_api_proxy_ptr;
}

ASTRA_API_PROXY void astra_api_set_proxy(StreamServiceProxyBase* proxy)
{
    __astra_api_proxy_ptr = proxy;
}

ASTRA_API astra_status_t astra_streamset_open(const char* connectionString,
                                              astra_streamsetconnection_t* streamSet)
{
    return get_api_proxy()->streamset_open(connectionString, streamSet);
}

ASTRA_API astra_status_t astra_streamset_close(astra_streamsetconnection_t* streamSet)
{
    return get_api_proxy()->streamset_close(streamSet);
}

ASTRA_API astra_status_t astra_reader_create(astra_streamsetconnection_t streamSet,
                                             astra_reader_t* reader)
{
    return get_api_proxy()->reader_create(streamSet, reader);
}

ASTRA_API astra_status_t astra_reader_destroy(astra_reader_t* reader)
{
    return get_api_proxy()->reader_destroy(reader);
}

ASTRA_API astra_status_t astra_reader_get_stream(astra_reader_t reader,
                                                 astra_stream_type_t type,
                                                 astra_stream_subtype_t subtype,
                                                 astra_streamconnection_t* connection)
{
    return get_api_proxy()->reader_get_stream(reader, type, subtype, connection);
}

ASTRA_API astra_status_t astra_stream_get_description(astra_streamconnection_t connection,
                                                      astra_stream_desc_t* description)
{
    return get_api_proxy()->stream_get_description(connection, description);
}

ASTRA_API astra_status_t astra_stream_start(astra_streamconnection_t connection)
{
    return get_api_proxy()->stream_start(connection);
}

ASTRA_API astra_status_t astra_stream_stop(astra_streamconnection_t connection)
{
    return get_api_proxy()->stream_stop(connection);
}

ASTRA_API astra_status_t astra_reader_open_frame(astra_reader_t reader,
                                                 int timeoutMillis,
                                                 astra_reader_frame_t* frame)
{
    return get_api_proxy()->reader_open_frame(reader, timeoutMillis, frame);
}

ASTRA_API astra_status_t astra_reader_close_frame(astra_reader_frame_t* frame)
{
    return get_api_proxy()->reader_close_frame(frame);
}

ASTRA_API astra_status_t astra_reader_register_frame_ready_callback(astra_reader_t reader,
                                                                    astra_frame_ready_callback_t callback,
                                                                    void* clientTag,
                                                                    astra_reader_callback_id_t* callbackId)
{
    return get_api_proxy()->reader_register_frame_ready_callback(reader, callback, clientTag, callbackId);
}

ASTRA_API astra_status_t astra_reader_unregister_frame_ready_callback(astra_reader_callback_id_t* callbackId)
{
    return get_api_proxy()->reader_unregister_frame_ready_callback(callbackId);
}

ASTRA_API astra_status_t astra_reader_get_frame(astra_reader_frame_t frame,
                                                astra_stream_type_t type,
                                                astra_stream_subtype_t subtype,
                                                astra_frame_t** subFrame)
{
    return get_api_proxy()->reader_get_frame(frame, type, subtype, subFrame);
}

ASTRA_API astra_status_t astra_stream_set_parameter(astra_streamconnection_t connection,
                                                    astra_parameter_id parameterId,
                                                    size_t inByteLength,
                                                    astra_parameter_data_t inData)
{
    return get_api_proxy()->stream_set_parameter(connection, parameterId, inByteLength, inData);
}

ASTRA_API astra_status_t astra_stream_get_parameter(astra_streamconnection_t connection,
                                                    astra_parameter_id parameterId,
                                                    size_t* resultByteLength,
                                                    astra_result_token_t* token)
{
    return get_api_proxy()->stream_get_parameter(connection, parameterId, resultByteLength, token);
}

ASTRA_API astra_status_t astra_stream_get_result(astra_streamconnection_t connection,
                                                 astra_result_token_t token,
                                                 size_t dataByteLength,
                                                 astra_parameter_data_t dataDestination)
{
    return get_api_proxy()->stream_get_result(connection, token, dataByteLength, dataDestination);
}

ASTRA_API astra_status_t astra_stream_invoke(astra_streamconnection_t connection,
                                             astra_command_id commandId,
                                             size_t inByteLength,
                                             astra_parameter_data_t inData,
                                             size_t* resultByteLength,
                                             astra_result_token_t* token)
{
    return get_api_proxy()->stream_invoke(connection, commandId, inByteLength, inData, resultByteLength, token);
}

ASTRA_API astra_status_t astra_temp_update()
{
    return get_api_proxy()->temp_update();
}

ASTRA_END_DECLS
