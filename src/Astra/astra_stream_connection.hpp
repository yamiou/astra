#ifndef ASTRA_STREAM_CONNECTION_H
#define ASTRA_STREAM_CONNECTION_H

#include <Astra/astra_types.h>
#include <Astra/Plugins/plugin_capi.h>
#include "astra_parameter_bin.hpp"
#include "astra_stream_bin.hpp"
#include "astra_logger.hpp"
#include "astra_registry.hpp"

namespace astra {

    class stream;

    class stream_connection : public tracked_instance<stream_connection>
    {
    public:
        using FrameReadyCallback = std::function<void(stream_connection*, astra_frame_index_t)>;

        stream_connection(stream* stream);
        ~stream_connection();

        stream_connection& operator=(const stream_connection& rhs) = delete;
        stream_connection(const stream_connection& connection) = delete;

        void start();
        void stop();

        bool is_started() const { return m_started; }

        astra_frame_t* lock();
        void unlock();

        void set_bin(stream_bin* bin);
        stream_bin* get_bin() const { return m_bin; }

        stream* get_stream() const { return m_stream; }

        astra_streamconnection_t get_handle() { return &m_connection; }

        astra_bin_t get_bin_handle();

        static stream_connection* get_ptr(astra_streamconnection_t conn)
        {
            return registry::get<stream_connection>(conn->handle);
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
        void on_bin_front_buffer_ready(stream_bin* bin, astra_frame_index_t frameIndex);
        void clear_pending_parameter_result();
        void cache_parameter_bin_token(astra_parameter_bin_t parameterBinHandle,
                                       size_t& resultByteLength,
                                       astra_result_token_t& token);

        _astra_streamconnection m_connection;
        astra_frame_t* m_currentFrame{nullptr};

        bool m_locked{false};
        bool m_started{false};

        stream* m_stream{nullptr};
        stream_bin* m_bin{nullptr};
        parameter_bin* m_pendingParameterResult{nullptr};

        stream_bin::FrontBufferReadyCallback m_binFrontBufferReadyCallback;
        astra_callback_id_t m_binFrontBufferReadyCallbackId;

        signal<stream_connection*, astra_frame_index_t> m_frameReadySignal;
    };
}

#endif /* ASTRA_STREAM_CONNECTION_H */
