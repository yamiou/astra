#ifndef ASTRA_STREAM_BIN_H
#define ASTRA_STREAM_BIN_H

#include <exception>
#include <array>
#include <Astra/astra_types.h>
#include "astra_signal.hpp"
#include <Astra/Plugins/plugin_capi.h>
#include "astra_logger.hpp"

namespace astra {

    class stream_bin
    {
    public:
        using FrontBufferReadyCallback = std::function<void(stream_bin*,astra_frame_index_t)>;

        stream_bin(size_t bufferSizeInBytes);
        ~stream_bin();

        stream_bin(const stream_bin& bin) = delete;
        stream_bin& operator=(const stream_bin& rhs) = delete;

        //exposed to plugins
        astra_frame_t* get_backBuffer()
            {
                return &m_buffers[m_backBufferIndex];
            }

        astra_frame_t* cycle_buffers();

        astra_frame_t* lock_front_buffer();
        void unlock_front_buffer();

        astra_callback_id_t register_front_buffer_ready_callback(FrontBufferReadyCallback callback);
        void unregister_front_buffer_ready_callback(astra_callback_id_t& callbackId);

        void inc_active() { m_activeCount++; }
        void dec_active()
            {
                m_activeCount--;
                if (m_activeCount < 0)
                {
                    throw std::exception();
                }
            }

        void inc_connected() { m_connectedCount++; }
        void dec_connected()
            {
                m_connectedCount--;
                if (m_connectedCount < 0)
                {
                    throw std::exception();
                }
            }

        bool is_active() { return m_activeCount > 0; }
        bool has_clients_connected() { return m_connectedCount > 0; }
        size_t bufferSize() { return m_bufferSize; }

        astra_bin_t get_handle() { return reinterpret_cast<astra_bin_t>(this); }

        static stream_bin* get_ptr(astra_bin_t bin)
            { return reinterpret_cast<stream_bin*>(bin); }

    private:
        inline bool is_front_buffer_locked() { return m_frontBufferLockCount > 0; }
        void init_buffers(size_t bufferLengthInBytes);
        void deinit_buffers();
        void init_buffer(astra_frame_t& frame, size_t bufferLengthInBytes);
        void deinit_buffer(astra_frame_t& frame);
        astra_frame_t* get_frontBuffer();
        astra_frame_t* get_middleBuffer();
        void raiseFrameReadySignal();

        size_t m_bufferSize{0};

        size_t m_frontBufferIndex{0};
        size_t m_middleBufferIndex{1};
        size_t m_backBufferIndex{2};

        uint32_t m_frontBufferLockCount{0};

        const static size_t BUFFER_COUNT = 3;
        astra_frame_t m_buffers[BUFFER_COUNT];

        int m_connectedCount{0};
        int m_activeCount{0};

        signal<stream_bin*, astra_frame_index_t> m_frontBufferReadySignal;
    };
}

#endif /* ASTRA_STREAM_BIN_H */
