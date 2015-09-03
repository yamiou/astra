#ifndef STREAMCONNECTION_H
#define STREAMCONNECTION_H

#include <Astra/astra_types.h>
#include <Astra/Plugins/plugin_capi.h>
#include "ParameterBin.h"
#include "StreamBin.h"
#include "Logger.h"
#include "Registry.h"

namespace astra {

    class Stream;

    class StreamConnection : public TrackedInstance<StreamConnection>
    {
    public:
        using FrameReadyCallback = std::function<void(StreamConnection*, astra_frame_index_t)>;

        StreamConnection(Stream* stream);
        ~StreamConnection();

        StreamConnection& operator=(const StreamConnection& rhs) = delete;
        StreamConnection(const StreamConnection& connection) = delete;

        void start();
        void stop();

        bool is_started() const { return m_started; }

        astra_frame_t* lock();
        void unlock();

        void set_bin(StreamBin* bin);
        StreamBin* get_bin() const { return m_bin; }

        Stream* get_stream() const { return m_stream; }

        astra_streamconnection_t get_handle() { return &m_connection; }

        astra_bin_t get_bin_handle();

        static StreamConnection* get_ptr(astra_streamconnection_t conn)
        {
            return Registry::get<StreamConnection>(conn->handle);
        }

        const astra_stream_desc_t& get_description() const;

        astra_callback_id_t register_frame_ready_callback(FrameReadyCallback callback);
        void unregister_frame_ready_callback(astra_callback_id_t& callbackId);

        void set_parameter(astra_parameter_id id,
                           size_t inByteLength,
                           astra_parameter_data_t inData);

        void get_parameter(astra_parameter_id id,
                           size_t& resultByteLength,
                           astra_result_token_t& token);

        astra_status_t get_result(astra_result_token_t token,
                                     size_t dataByteLength,
                                     astra_parameter_data_t dataDestination);

        void invoke(astra_command_id commandId,
                    size_t inByteLength,
                    astra_parameter_data_t inData,
                    size_t& resultByteLength,
                    astra_result_token_t& token);

    private:
        void on_bin_front_buffer_ready(StreamBin* bin, astra_frame_index_t frameIndex);
        void clear_pending_parameter_result();
        void cache_parameter_bin_token(astra_parameter_bin_t parameterBinHandle,
                                       size_t& resultByteLength,
                                       astra_result_token_t& token);

        _astra_streamconnection m_connection;
        astra_frame_t* m_currentFrame{nullptr};

        bool m_locked{false};
        bool m_started{false};

        Stream* m_stream{nullptr};
        StreamBin* m_bin{nullptr};
        ParameterBin* m_pendingParameterResult{nullptr};

        StreamBin::FrontBufferReadyCallback m_binFrontBufferReadyCallback;
        astra_callback_id_t m_binFrontBufferReadyCallbackId;

        Signal<StreamConnection*, astra_frame_index_t> m_frameReadySignal;
    };
}

#endif /* STREAMCONNECTION_H */
