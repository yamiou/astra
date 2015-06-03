#ifndef STREAMBIN_H
#define STREAMBIN_H

#include <exception>
#include <array>
#include <SenseKit/sensekit_types.h>
#include "Core/Signal.h"
#include <SenseKit/Plugins/plugin_capi.h>
#include "Logger.h"

namespace sensekit {

    class StreamBin;
    using FrontBufferReadyCallback = std::function<void(StreamBin*,sensekit_frame_index_t)>;

    class StreamBin
    {
    public:
        StreamBin(size_t bufferSizeInBytes);
        ~StreamBin();

        StreamBin(const StreamBin& bin) = delete;
        StreamBin& operator=(const StreamBin& rhs) = delete;

        //exposed to plugins
        sensekit_frame_t* get_backBuffer()
            {
                return &m_buffers[m_backBufferIndex];
            }

        sensekit_frame_t* cycle_buffers();

        sensekit_frame_t* lock_front_buffer();
        void unlock_front_buffer();

        sensekit_callback_id_t register_front_buffer_ready_callback(FrontBufferReadyCallback callback);
        void unregister_front_buffer_ready_callback(sensekit_callback_id_t& callbackId);

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

        sensekit_bin_t get_handle() { return reinterpret_cast<sensekit_bin_t>(this); }

        static StreamBin* get_ptr(sensekit_bin_t bin)
            { return reinterpret_cast<StreamBin*>(bin); }

    private:
        inline bool is_front_buffer_locked() { return m_frontBufferLockCount > 0; }
        void init_buffers(size_t bufferLengthInBytes);
        void deinit_buffers();
        void init_buffer(sensekit_frame_t& frame, size_t bufferLengthInBytes);
        void deinit_buffer(sensekit_frame_t& frame);
        sensekit_frame_t* get_frontBuffer();
        sensekit_frame_t* get_middleBuffer();
        void raiseFrameReadySignal();

        size_t m_bufferSize{0};

        size_t m_frontBufferIndex{0};
        size_t m_middleBufferIndex{1};
        size_t m_backBufferIndex{2};

        uint32_t m_frontBufferLockCount{0};

        const static size_t BUFFER_COUNT = 3;
        sensekit_frame_t m_buffers[BUFFER_COUNT];

        int m_connectedCount{0};
        int m_activeCount{0};

        Logger m_logger;

        Signal<StreamBin*, sensekit_frame_index_t> m_frontBufferReadySignal;
    };
}

#endif /* STREAMBIN_H */
