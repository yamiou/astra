/* THIS FILE AUTO-GENERATED FROM SenseKitContext.h.lpp. DO NOT EDIT. */
#ifndef SENSEKITCONTEXT_H
#define SENSEKITCONTEXT_H

#include <SenseKit/sensekit_types.h>
#include <atomic>
#include "PluginService.h"
#include "StreamSet.h"
#include "StreamReader.h"
#include "Core/shared_library.h"
#include "Logger.h"

struct StreamServiceProxyBase;
struct PluginServiceProxyBase;

namespace sensekit {

    using initialize_fn = void(*)(PluginServiceProxyBase*);
    using terminate_fn = void(*)();
    using update_fn = void(*)();

    struct PluginFuncs
    {
        initialize_fn initialize{nullptr};
        terminate_fn terminate{nullptr};
        update_fn update{nullptr};
        LibHandle libHandle{nullptr};

        bool isValid()
            {
                return initialize != nullptr &&
                    terminate != nullptr &&
                    update != nullptr;
            }
    };

    class SenseKitContext
    {
    public:
        SenseKitContext();
        ~SenseKitContext();

        sensekit_status_t initialize();

        sensekit_status_t terminate();

        sensekit_status_t streamset_open(const char* connectionString,
                                         sensekit_streamset_t& streamSet);

        sensekit_status_t streamset_close(sensekit_streamset_t& streamSet);

        char* get_status_string(sensekit_status_t status);

        sensekit_status_t reader_create(sensekit_streamset_t streamSet,
                                        sensekit_reader_t& reader);

        sensekit_status_t reader_destroy(sensekit_reader_t& reader);

        sensekit_status_t reader_get_stream(sensekit_reader_t reader,
                                            sensekit_stream_type_t type,
                                            sensekit_stream_subtype_t subType,
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
                                           sensekit_stream_subtype_t subType,
                                           sensekit_frame_ref_t*& frameRef);

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


        void raise_existing_streams_added(stream_added_callback_t callback, void* clientTag);

        StreamSet& get_rootSet() { return m_rootSet; }

        StreamServiceProxyBase* get_streamServiceProxy() { return m_streamServiceProxy; }

    private:
        bool m_initialized{false};

        Logger m_logger;
        StreamSet m_rootSet;

        PluginService m_pluginService;
        PluginServiceProxyBase* m_pluginServiceProxy;
        StreamServiceProxyBase* m_streamServiceProxy;

        using PluginList = std::vector<PluginFuncs>;
        PluginList m_pluginList;

        using ReaderPtr = std::unique_ptr<StreamReader>;
        using ReaderList = std::vector<ReaderPtr>;

        ReaderList m_readers;
    };
}

#endif /* SENSEKITCONTEXT_H */
