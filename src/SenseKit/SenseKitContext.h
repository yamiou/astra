/* THIS FILE AUTO-GENERATED FROM SenseKitContext.h.lpp. DO NOT EDIT. */
#ifndef SENSEKITCONTEXT_H
#define SENSEKITCONTEXT_H

#include <SenseKit/sensekit_types.h>
#include <atomic>
#include <memory>
#include <unordered_map>
#include <string>

#include "PluginManager.h"
#include "StreamSet.h"
#include "StreamReader.h"
#include "Core/shared_library.h"
#include "Logger.h"
#include "StreamSetCatalog.h"

struct StreamServiceProxyBase;

namespace sensekit {

    class SenseKitContext
    {
    public:
        SenseKitContext() = default;
        SenseKitContext& operator=(const SenseKitContext& rhs) = delete;
        SenseKitContext(const SenseKitContext& context) = delete;

        sensekit_status_t initialize();
        sensekit_status_t terminate();

        sensekit_status_t streamset_open(const char* connectionString,
                                         sensekit_streamsetconnection_t& streamSet);

        sensekit_status_t streamset_close(sensekit_streamsetconnection_t& streamSet);

        sensekit_status_t reader_create(sensekit_streamsetconnection_t streamSet,
                                        sensekit_reader_t& reader);

        sensekit_status_t reader_destroy(sensekit_reader_t& reader);

        sensekit_status_t reader_get_stream(sensekit_reader_t reader,
                                            sensekit_stream_type_t type,
                                            sensekit_stream_subtype_t subtype,
                                            sensekit_streamconnection_t& connection);

        sensekit_status_t stream_get_description(sensekit_streamconnection_t connection,
                                                 sensekit_stream_desc_t* description);

        sensekit_status_t stream_start(sensekit_streamconnection_t connection);

        sensekit_status_t stream_stop(sensekit_streamconnection_t connection);

        sensekit_status_t reader_open_frame(sensekit_reader_t reader,
                                            int timeoutMillis,
                                            sensekit_reader_frame_t& frame);

        sensekit_status_t reader_close_frame(sensekit_reader_frame_t& frame);

        sensekit_status_t reader_register_frame_ready_callback(sensekit_reader_t reader,
                                                               sensekit_frame_ready_callback_t callback,
                                                               void* clientTag,
                                                               sensekit_reader_callback_id_t& callbackId);

        sensekit_status_t reader_unregister_frame_ready_callback(sensekit_reader_callback_id_t& callbackId);

        sensekit_status_t reader_get_frame(sensekit_reader_frame_t frame,
                                           sensekit_stream_type_t type,
                                           sensekit_stream_subtype_t subtype,
                                           sensekit_frame_t*& subFrame);

        sensekit_status_t stream_set_parameter(sensekit_streamconnection_t connection,
                                               sensekit_parameter_id parameterId,
                                               size_t inByteLength,
                                               sensekit_parameter_data_t inData);

        sensekit_status_t stream_get_parameter(sensekit_streamconnection_t connection,
                                               sensekit_parameter_id parameterId,
                                               size_t& resultByteLength,
                                               sensekit_result_token_t& token);

        sensekit_status_t stream_get_result(sensekit_streamconnection_t connection,
                                            sensekit_result_token_t token,
                                            size_t dataByteLength,
                                            sensekit_parameter_data_t dataDestination);

        sensekit_status_t stream_invoke(sensekit_streamconnection_t connection,
                                        sensekit_command_id commandId,
                                        size_t inByteLength,
                                        sensekit_parameter_data_t inData,
                                        size_t& resultByteLength,
                                        sensekit_result_token_t& token);

        sensekit_status_t temp_update();

        StreamServiceProxyBase* get_streamServiceProxy() { return m_streamServiceProxy; }

        sensekit_status_t notify_host_event(sensekit_event_id id, const void* data, size_t dataSize);

        StreamSet& create_stream(const char* streamUri);

        StreamSetCatalog& get_setCatalog() { return m_setCatalog; }

    private:
        bool m_initialized{false};

        using LoggerPtr = std::unique_ptr<Logger>;
        LoggerPtr m_logger;

        StreamServiceProxyBase* m_streamServiceProxy;

        using PluginManagerPtr = std::unique_ptr<PluginManager>;
        PluginManagerPtr m_pluginManager;

        using ReaderList = std::vector<StreamReader*>;

        ReaderList m_activeReaders;
        StreamSetCatalog m_setCatalog;
    };
}

#endif /* SENSEKITCONTEXT_H */
