#include "astra_context.hpp"
#include "astra_context_impl.hpp"
#include "astra_create_stream_proxy.hpp"
#include <Astra/Plugins/StreamServiceProxyBase.h>
#include <AstraAPI.h>

namespace astra {

    context::context()
          : m_impl(std::make_unique<context_impl>()),
            m_proxy(create_stream_proxy(this))
            {}

    context::~context() {}

    astra_status_t context::initialize()
    {
        astra_api_set_proxy(proxy());
        return m_impl->initialize();
    }

    astra_status_t context::terminate()
    {
        return m_impl->terminate();
    }

    StreamServiceProxyBase* context::proxy()
    {
        return m_proxy.get();
    }

    astra_status_t context::streamset_open(const char* connectionString,
                                           astra_streamsetconnection_t& streamSet)
    {
        return m_impl->streamset_open(connectionString, streamSet);
    }

    astra_status_t context::streamset_close(astra_streamsetconnection_t& streamSet)
    {
        return m_impl->streamset_close(streamSet);
    }

    astra_status_t context::reader_create(astra_streamsetconnection_t streamSet,
                                          astra_reader_t& reader)
    {
        return m_impl->reader_create(streamSet, reader);
    }

    astra_status_t context::reader_destroy(astra_reader_t& reader)
    {
        return m_impl->reader_destroy(reader);
    }

    astra_status_t context::reader_get_stream(astra_reader_t reader,
                                              astra_stream_type_t type,
                                              astra_stream_subtype_t subtype,
                                              astra_streamconnection_t& connection)
    {
        return m_impl->reader_get_stream(reader, type, subtype, connection);
    }

    astra_status_t context::stream_get_description(astra_streamconnection_t connection,
                                                   astra_stream_desc_t* description)
    {
        return m_impl->stream_get_description(connection, description);
    }

    astra_status_t context::stream_start(astra_streamconnection_t connection)
    {
        return m_impl->stream_start(connection);
    }

    astra_status_t context::stream_stop(astra_streamconnection_t connection)
    {
        return m_impl->stream_stop(connection);
    }

    astra_status_t context::reader_open_frame(astra_reader_t reader,
                                              int timeoutMillis,
                                              astra_reader_frame_t& frame)
    {
        return m_impl->reader_open_frame(reader, timeoutMillis, frame);
    }

    astra_status_t context::reader_close_frame(astra_reader_frame_t& frame)
    {
        return m_impl->reader_close_frame(frame);
    }

    astra_status_t context::reader_register_frame_ready_callback(astra_reader_t reader,
                                                                 astra_frame_ready_callback_t callback,
                                                                 void* clientTag,
                                                                 astra_reader_callback_id_t& callbackId)
    {
        return m_impl->reader_register_frame_ready_callback(reader, callback, clientTag, callbackId);
    }

    astra_status_t context::reader_unregister_frame_ready_callback(astra_reader_callback_id_t& callbackId)
    {
        return m_impl->reader_unregister_frame_ready_callback(callbackId);
    }

    astra_status_t context::reader_get_frame(astra_reader_frame_t frame,
                                             astra_stream_type_t type,
                                             astra_stream_subtype_t subtype,
                                             astra_frame_t*& subFrame)
    {
        return m_impl->reader_get_frame(frame, type, subtype, subFrame);
    }

    astra_status_t context::stream_set_parameter(astra_streamconnection_t connection,
                                                 astra_parameter_id parameterId,
                                                 size_t inByteLength,
                                                 astra_parameter_data_t inData)
    {
        return m_impl->stream_set_parameter(connection, parameterId, inByteLength, inData);
    }

    astra_status_t context::stream_get_parameter(astra_streamconnection_t connection,
                                                 astra_parameter_id parameterId,
                                                 size_t& resultByteLength,
                                                 astra_result_token_t& token)
    {
        return m_impl->stream_get_parameter(connection, parameterId, resultByteLength, token);
    }

    astra_status_t context::stream_get_result(astra_streamconnection_t connection,
                                              astra_result_token_t token,
                                              size_t dataByteLength,
                                              astra_parameter_data_t dataDestination)
    {
        return m_impl->stream_get_result(connection, token, dataByteLength, dataDestination);
    }

    astra_status_t context::stream_invoke(astra_streamconnection_t connection,
                                          astra_command_id commandId,
                                          size_t inByteLength,
                                          astra_parameter_data_t inData,
                                          size_t& resultByteLength,
                                          astra_result_token_t& token)
    {
        return m_impl->stream_invoke(connection, commandId, inByteLength, inData, resultByteLength, token);
    }

    astra_status_t context::temp_update()
    {
        return m_impl->temp_update();
    }


    astra_status_t context::notify_host_event(astra_event_id id, const void* data, size_t dataSize)
    {
        return m_impl->notify_host_event(id, data, dataSize);
    }
}
