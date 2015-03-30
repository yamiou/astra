#include <SenseKit.h>
#include <SenseKitAPI.h>
#include <Plugins/StreamServiceProxy.h>

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

SENSEKIT_API_PROXY sensekit_status_t sensekit_streamset_open(const char* connection_string,
                                                             sensekit_streamset_t** streamset)
{
    return get_api_proxy()->open_streamset(connection_string,
                                           streamset);
}

SENSEKIT_API_PROXY sensekit_status_t sensekit_streamset_close(sensekit_streamset_t** streamset)
{
    return get_api_proxy()->close_streamset(streamset);
}

SENSEKIT_API_PROXY sensekit_status_t sensekit_stream_open(sensekit_streamset_t* streamset,
                                                          sensekit_stream_type_t type,
                                                          sensekit_stream_subtype_t subtype,
                                                          sensekit_streamconnection_t** connection)
{
    return get_api_proxy()->open_stream(streamset,
                                        type,
                                        subtype,
                                        connection);
}

SENSEKIT_API_PROXY sensekit_status_t sensekit_stream_close(sensekit_streamconnection_t** stream_connection)
{
    return get_api_proxy()->close_stream(stream_connection);
}

SENSEKIT_API_PROXY sensekit_status_t sensekit_stream_frame_open(sensekit_streamconnection_t* connection,
                                                                int timeout,
                                                                sensekit_frame_ref_t** frame)
{
    return get_api_proxy()->open_frame(connection, timeout, frame);
}

SENSEKIT_API_PROXY sensekit_status_t sensekit_stream_frame_close(sensekit_frame_ref_t** frame)
{
    return get_api_proxy()->close_frame(frame);
}

SENSEKIT_API_PROXY sensekit_status_t sensekit_temp_update()
{
    return get_api_proxy()->temp_update();
}

SENSEKIT_API_PROXY sensekit_status_t sensekit_stream_set_parameter(sensekit_streamconnection_t* connection,
                                                                   sensekit_parameter_id parameter_id,
                                                                   size_t byte_length,
                                                                   sensekit_parameter_data_t* data)
{
    return get_api_proxy()->set_parameter(connection, parameter_id, byte_length, data);
}

SENSEKIT_API_PROXY sensekit_status_t sensekit_stream_get_parameter_size(sensekit_streamconnection_t* connection,
                                                                        sensekit_parameter_id parameter_id,
                                                                        size_t* byte_length)
{
    return get_api_proxy()->get_parameter_size(connection, parameter_id, byte_length);
}

SENSEKIT_API_PROXY sensekit_status_t sensekit_stream_get_parameter_data(sensekit_streamconnection_t* connection,
                                                                        sensekit_parameter_id parameter_id,
                                                                        size_t byte_length,
                                                                        sensekit_parameter_data_t* data)
{
    return get_api_proxy()->get_parameter_data(connection, parameter_id, byte_length, data);
}

SENSEKIT_END_DECLS
