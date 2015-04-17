/* THIS FILE AUTO-GENERATED FROM SenseKitAPI.cpp.lpp. DO NOT EDIT. */
#include "SenseKitAPI.h"
#include <SenseKit/Plugins/PluginKit.h>
#include <cassert>

SENSEKIT_BEGIN_DECLS

SENSEKIT_LOCAL StreamServiceProxyBase* __sensekit_api_proxy_ptr = nullptr;

inline sensekit::StreamServiceProxy* get_api_proxy()
{
    return reinterpret_cast<sensekit::StreamServiceProxy*>(__sensekit_api_proxy_ptr);
}

SENSEKIT_API_PROXY StreamServiceProxyBase* sensekit_api_get_proxy()
{
    return __sensekit_api_proxy_ptr;
}

SENSEKIT_API_PROXY void sensekit_api_set_proxy(StreamServiceProxyBase* proxy)
{
    __sensekit_api_proxy_ptr = proxy;
}

SENSEKIT_API sensekit_status_t sensekit_initialize()
{
    return get_api_proxy()->initialize();
}

SENSEKIT_API sensekit_status_t sensekit_terminate()
{
    return get_api_proxy()->terminate();
}

SENSEKIT_API sensekit_status_t sensekit_streamset_open(const char* connectionString,
                                                       sensekit_streamset_t* streamSet)
{
    return get_api_proxy()->streamset_open(connectionString, streamSet);
}

SENSEKIT_API sensekit_status_t sensekit_streamset_close(sensekit_streamset_t* streamSet)
{
    return get_api_proxy()->streamset_close(streamSet);
}

SENSEKIT_API char* sensekit_get_status_string(sensekit_status_t status)
{
    return get_api_proxy()->get_status_string(status);
}

SENSEKIT_API sensekit_status_t sensekit_reader_create(sensekit_streamset_t streamSet,
                                                      sensekit_reader_t* reader)
{
    return get_api_proxy()->reader_create(streamSet, reader);
}

SENSEKIT_API sensekit_status_t sensekit_reader_destroy(sensekit_reader_t* reader)
{
    return get_api_proxy()->reader_destroy(reader);
}

SENSEKIT_API sensekit_status_t sensekit_reader_get_stream(sensekit_reader_t reader,
                                                          sensekit_stream_type_t type,
                                                          sensekit_stream_subtype_t subType,
                                                          sensekit_streamconnection_t* connection)
{
    return get_api_proxy()->reader_get_stream(reader, type, subType, connection);
}

SENSEKIT_API sensekit_status_t sensekit_stream_get_description(sensekit_streamconnection_t connection,
                                                               sensekit_stream_desc_t* description)
{
    return get_api_proxy()->stream_get_description(connection, description);
}

SENSEKIT_API sensekit_status_t sensekit_stream_start(sensekit_streamconnection_t connection)
{
    return get_api_proxy()->stream_start(connection);
}

SENSEKIT_API sensekit_status_t sensekit_stream_stop(sensekit_streamconnection_t connection)
{
    return get_api_proxy()->stream_stop(connection);
}

SENSEKIT_API sensekit_status_t sensekit_reader_open_frame(sensekit_reader_t reader,
                                                          int timeoutMillis,
                                                          sensekit_reader_frame_t* frame)
{
    return get_api_proxy()->reader_open_frame(reader, timeoutMillis, frame);
}

SENSEKIT_API sensekit_status_t sensekit_reader_close_frame(sensekit_reader_frame_t* frame)
{
    return get_api_proxy()->reader_close_frame(frame);
}

SENSEKIT_API sensekit_status_t sensekit_reader_register_frame_ready_callback(sensekit_reader_t reader,
                                                                             sensekit_frame_ready_callback_t callback,
                                                                             void* clientTag,
                                                                             sensekit_reader_callback_id_t* callbackId)
{
    return get_api_proxy()->reader_register_frame_ready_callback(reader, callback, clientTag, callbackId);
}

SENSEKIT_API sensekit_status_t sensekit_reader_unregister_frame_ready_callback(sensekit_reader_callback_id_t* callbackId)
{
    return get_api_proxy()->reader_unregister_frame_ready_callback(callbackId);
}

SENSEKIT_API sensekit_status_t sensekit_reader_get_frame(sensekit_reader_frame_t frame,
                                                         sensekit_stream_type_t type,
                                                         sensekit_stream_subtype_t subType,
                                                         sensekit_frame_ref_t** frameRef)
{
    return get_api_proxy()->reader_get_frame(frame, type, subType, frameRef);
}

SENSEKIT_API sensekit_status_t sensekit_stream_set_parameter(sensekit_streamconnection_t connection,
                                                             sensekit_parameter_id parameterId,
                                                             size_t inByteLength,
                                                             sensekit_parameter_data_t inData)
{
    return get_api_proxy()->stream_set_parameter(connection, parameterId, inByteLength, inData);
}

SENSEKIT_API sensekit_status_t sensekit_stream_get_parameter(sensekit_streamconnection_t connection,
                                                             sensekit_parameter_id parameterId,
                                                             size_t* resultByteLength,
                                                             sensekit_result_token_t* token)
{
    return get_api_proxy()->stream_get_parameter(connection, parameterId, resultByteLength, token);
}

SENSEKIT_API sensekit_status_t sensekit_stream_get_result(sensekit_streamconnection_t connection,
                                                          sensekit_result_token_t token,
                                                          size_t dataByteLength,
                                                          sensekit_parameter_data_t dataDestination)
{
    return get_api_proxy()->stream_get_result(connection, token, dataByteLength, dataDestination);
}

SENSEKIT_API sensekit_status_t sensekit_stream_invoke(sensekit_streamconnection_t connection,
                                                      sensekit_command_id commandId,
                                                      size_t inByteLength,
                                                      sensekit_parameter_data_t inData,
                                                      size_t* resultByteLength,
                                                      sensekit_result_token_t* token)
{
    return get_api_proxy()->stream_invoke(connection, commandId, inByteLength, inData, resultByteLength, token);
}

SENSEKIT_API sensekit_status_t sensekit_temp_update()
{
    return get_api_proxy()->temp_update();
}

SENSEKIT_END_DECLS
