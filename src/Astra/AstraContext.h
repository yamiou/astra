/* THIS FILE AUTO-GENERATED FROM AstraContext.h.lpp. DO NOT EDIT. */
#ifndef ASTRACONTEXT_H
#define ASTRACONTEXT_H

#include <Astra/astra_types.h>
#include <memory>

struct StreamServiceProxyBase;

namespace astra {

    class StreamSet;
    class StreamSetCatalog;
    class AstraContextImpl;

    class AstraContext
    {
    public:
        AstraContext();
        ~AstraContext();

        AstraContext& operator=(const AstraContext& rhs) = delete;
        AstraContext(const AstraContext& context) = delete;

        astra_status_t initialize();
        astra_status_t terminate();

        astra_status_t streamset_open(const char* connectionString,
                                      astra_streamsetconnection_t& streamSet);

        astra_status_t streamset_close(astra_streamsetconnection_t& streamSet);

        astra_status_t reader_create(astra_streamsetconnection_t streamSet,
                                     astra_reader_t& reader);

        astra_status_t reader_destroy(astra_reader_t& reader);

        astra_status_t reader_get_stream(astra_reader_t reader,
                                         astra_stream_type_t type,
                                         astra_stream_subtype_t subtype,
                                         astra_streamconnection_t& connection);

        astra_status_t stream_get_description(astra_streamconnection_t connection,
                                              astra_stream_desc_t* description);

        astra_status_t stream_start(astra_streamconnection_t connection);

        astra_status_t stream_stop(astra_streamconnection_t connection);

        astra_status_t reader_open_frame(astra_reader_t reader,
                                         int timeoutMillis,
                                         astra_reader_frame_t& frame);

        astra_status_t reader_close_frame(astra_reader_frame_t& frame);

        astra_status_t reader_register_frame_ready_callback(astra_reader_t reader,
                                                            astra_frame_ready_callback_t callback,
                                                            void* clientTag,
                                                            astra_reader_callback_id_t& callbackId);

        astra_status_t reader_unregister_frame_ready_callback(astra_reader_callback_id_t& callbackId);

        astra_status_t reader_get_frame(astra_reader_frame_t frame,
                                        astra_stream_type_t type,
                                        astra_stream_subtype_t subtype,
                                        astra_frame_t*& subFrame);

        astra_status_t stream_set_parameter(astra_streamconnection_t connection,
                                            astra_parameter_id parameterId,
                                            size_t inByteLength,
                                            astra_parameter_data_t inData);

        astra_status_t stream_get_parameter(astra_streamconnection_t connection,
                                            astra_parameter_id parameterId,
                                            size_t& resultByteLength,
                                            astra_result_token_t& token);

        astra_status_t stream_get_result(astra_streamconnection_t connection,
                                         astra_result_token_t token,
                                         size_t dataByteLength,
                                         astra_parameter_data_t dataDestination);

        astra_status_t stream_invoke(astra_streamconnection_t connection,
                                     astra_command_id commandId,
                                     size_t inByteLength,
                                     astra_parameter_data_t inData,
                                     size_t& resultByteLength,
                                     astra_result_token_t& token);

        astra_status_t temp_update();

        StreamServiceProxyBase* proxy();

        astra_status_t notify_host_event(astra_event_id id, const void* data, size_t dataSize);

    private:
        std::unique_ptr<AstraContextImpl> m_impl;
        std::unique_ptr<StreamServiceProxyBase> m_proxy;
    };
}

#endif /* ASTRACONTEXT_H */
