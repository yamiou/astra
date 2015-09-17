/* THIS FILE AUTO-GENERATED FROM AstraContextImpl.h.lpp. DO NOT EDIT. */
#ifndef ASTRACONTEXTIMPL_H
#define ASTRACONTEXTIMPL_H

#include <Astra/astra_types.h>
#include <atomic>
#include <memory>
#include <unordered_map>
#include <string>

#include "PluginManager.h"
#include "StreamSet.h"
#include "StreamReader.h"
#include "shared_library.h"
#include "Logger.h"
#include "StreamSetCatalog.h"

struct StreamServiceProxyBase;

namespace astra {

    class AstraContextImpl
    {
    public:
        AstraContextImpl() = default;
        AstraContextImpl& operator=(const AstraContextImpl& rhs) = delete;
        AstraContextImpl(const AstraContextImpl& context) = delete;

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

        astra_status_t notify_host_event(astra_event_id id, const void* data, size_t dataSize);

    private:
        bool m_initialized{false};

        using PluginManagerPtr = std::unique_ptr<PluginManager>;
        PluginManagerPtr m_pluginManager;

        using ReaderList = std::vector<StreamReader*>;

        ReaderList m_activeReaders;
        StreamSetCatalog m_setCatalog;
    };
}

#endif /* ASTRACONTEXTIMPL_H */
