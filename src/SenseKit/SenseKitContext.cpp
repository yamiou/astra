#include "SenseKitContext.h"
#include "SenseKitContextImpl.h"
#include "CreateStreamProxy.h"
#include <SenseKit/Plugins/StreamServiceProxyBase.h>
#include <SenseKitAPI.h>

namespace sensekit {

    SenseKitContext::SenseKitContext()
          : m_impl(std::make_unique<SenseKitContextImpl>()),
            m_proxy(create_stream_proxy(this))
            {}

    SenseKitContext::~SenseKitContext() {}

    sensekit_status_t SenseKitContext::initialize()
    {
        sensekit_api_set_proxy(proxy());
        return m_impl->initialize();
    }

    sensekit_status_t SenseKitContext::terminate()
    {
        return m_impl->terminate();
    }

    StreamServiceProxyBase* SenseKitContext::proxy()
    {
        return m_proxy.get();
    }

    sensekit_status_t SenseKitContext::streamset_open(const char* connectionString,
                                                      sensekit_streamsetconnection_t& streamSet)
    {
        return m_impl->streamset_open(connectionString, streamSet);
    }

    sensekit_status_t SenseKitContext::streamset_close(sensekit_streamsetconnection_t& streamSet)
    {
        return m_impl->streamset_close(streamSet);
    }

    sensekit_status_t SenseKitContext::reader_create(sensekit_streamsetconnection_t streamSet,
                                                     sensekit_reader_t& reader)
    {
        return m_impl->reader_create(streamSet, reader);
    }

    sensekit_status_t SenseKitContext::reader_destroy(sensekit_reader_t& reader)
    {
        return m_impl->reader_destroy(reader);
    }

    sensekit_status_t SenseKitContext::reader_get_stream(sensekit_reader_t reader,
                                                         sensekit_stream_type_t type,
                                                         sensekit_stream_subtype_t subtype,
                                                         sensekit_streamconnection_t& connection)
    {
        return m_impl->reader_get_stream(reader, type, subtype, connection);
    }

    sensekit_status_t SenseKitContext::stream_get_description(sensekit_streamconnection_t connection,
                                                              sensekit_stream_desc_t* description)
    {
        return m_impl->stream_get_description(connection, description);
    }

    sensekit_status_t SenseKitContext::stream_start(sensekit_streamconnection_t connection)
    {
        return m_impl->stream_start(connection);
    }

    sensekit_status_t SenseKitContext::stream_stop(sensekit_streamconnection_t connection)
    {
        return m_impl->stream_stop(connection);
    }

    sensekit_status_t SenseKitContext::reader_open_frame(sensekit_reader_t reader,
                                                         int timeoutMillis,
                                                         sensekit_reader_frame_t& frame)
    {
        return m_impl->reader_open_frame(reader, timeoutMillis, frame);
    }

    sensekit_status_t SenseKitContext::reader_close_frame(sensekit_reader_frame_t& frame)
    {
        return m_impl->reader_close_frame(frame);
    }

    sensekit_status_t SenseKitContext::reader_register_frame_ready_callback(sensekit_reader_t reader,
                                                                            sensekit_frame_ready_callback_t callback,
                                                                            void* clientTag,
                                                                            sensekit_reader_callback_id_t& callbackId)
    {
        return m_impl->reader_register_frame_ready_callback(reader, callback, clientTag, callbackId);
    }

    sensekit_status_t SenseKitContext::reader_unregister_frame_ready_callback(sensekit_reader_callback_id_t& callbackId)
    {
        return m_impl->reader_unregister_frame_ready_callback(callbackId);
    }

    sensekit_status_t SenseKitContext::reader_get_frame(sensekit_reader_frame_t frame,
                                                        sensekit_stream_type_t type,
                                                        sensekit_stream_subtype_t subtype,
                                                        sensekit_frame_t*& subFrame)
    {
        return m_impl->reader_get_frame(frame, type, subtype, subFrame);
    }

    sensekit_status_t SenseKitContext::stream_set_parameter(sensekit_streamconnection_t connection,
                                                            sensekit_parameter_id parameterId,
                                                            size_t inByteLength,
                                                            sensekit_parameter_data_t inData)
    {
        return m_impl->stream_set_parameter(connection, parameterId, inByteLength, inData);
    }

    sensekit_status_t SenseKitContext::stream_get_parameter(sensekit_streamconnection_t connection,
                                                            sensekit_parameter_id parameterId,
                                                            size_t& resultByteLength,
                                                            sensekit_result_token_t& token)
    {
        return m_impl->stream_get_parameter(connection, parameterId, resultByteLength, token);
    }

    sensekit_status_t SenseKitContext::stream_get_result(sensekit_streamconnection_t connection,
                                                         sensekit_result_token_t token,
                                                         size_t dataByteLength,
                                                         sensekit_parameter_data_t dataDestination)
    {
        return m_impl->stream_get_result(connection, token, dataByteLength, dataDestination);
    }

    sensekit_status_t SenseKitContext::stream_invoke(sensekit_streamconnection_t connection,
                                                     sensekit_command_id commandId,
                                                     size_t inByteLength,
                                                     sensekit_parameter_data_t inData,
                                                     size_t& resultByteLength,
                                                     sensekit_result_token_t& token)
    {
        return m_impl->stream_invoke(connection, commandId, inByteLength, inData, resultByteLength, token);
    }

    sensekit_status_t SenseKitContext::temp_update()
    {
        return m_impl->temp_update();
    }


    sensekit_status_t SenseKitContext::notify_host_event(sensekit_event_id id, const void* data, size_t dataSize)
    {
        return notify_host_event(id, data, dataSize);
    }
}
