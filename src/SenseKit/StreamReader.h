#ifndef STREAMREADER_H
#define STREAMREADER_H

#include <SenseKit/sensekit_types.h>
#include "Registry.h"
#include "StreamSet.h"
#include "StreamConnection.h"
#include <unordered_map>
#include <vector>
#include <cassert>
#include "Core/Signal.h"
#include "Logger.h"
#include "sensekit_private.h"

namespace sensekit {

    class StreamDescHash
    {
    public:
        std::size_t operator()(const sensekit_stream_desc_t desc) const
            {
                std::size_t h1 = std::hash<sensekit_stream_type_t>()(desc.type);
                std::size_t h2 = std::hash<sensekit_stream_subtype_t>()(desc.subtype);

                return h1 ^ (h2 << 1);
            }
    };

    class StreamDescEqualTo
    {
    public:
        std::size_t operator()(const sensekit_stream_desc_t& lhs,
                               const sensekit_stream_desc_t& rhs) const
            {
                return lhs.type == rhs.type && lhs.subtype == rhs.subtype;
            }
    };

    struct ReaderConnectionData
    {
        StreamConnection* connection;
        sensekit_callback_id_t scFrameReadyCallbackId;
        bool isNewFrameReady;
        sensekit_frame_index_t currentFrameIndex;
    };

    class StreamReader : public TrackedInstance<StreamReader>
    {
    public:
        StreamReader(StreamSetConnection& connection);
        ~StreamReader();

        StreamReader& operator=(const StreamReader& rhs) = delete;
        StreamReader(const StreamReader& reader) = delete;

        inline StreamSetConnection& get_connection() const { return m_connection; }

        inline sensekit_reader_t get_handle() { return reinterpret_cast<sensekit_reader_t>(this); }

        StreamConnection* get_stream(sensekit_stream_desc_t& desc);
        sensekit_frame_t* get_subframe(sensekit_stream_desc_t& desc);

        sensekit_callback_id_t register_frame_ready_callback(sensekit_frame_ready_callback_t callback, void* clientTag);
        void unregister_frame_ready_callback(sensekit_callback_id_t& callbackId);

        //TODO: locking currently not threadsafe

        sensekit_status_t lock(int timeoutMillis, sensekit_reader_frame_t& readerFrame);
        sensekit_status_t unlock(sensekit_reader_frame_t& readerFrame);

        static inline StreamReader* get_ptr(sensekit_reader_t reader) { return Registry::get<StreamReader>(reader); }
        static inline StreamReader* from_frame(sensekit_reader_frame_t& frame)
        {
            if (frame == nullptr)
            {
                return nullptr;
            }

            return get_ptr(frame->reader);
        }

    private:
        enum class block_result
        {
            TIMEOUT,
            FRAMEREADY
        };

        block_result block_until_frame_ready_or_timeout(int timeoutMillis);

        sensekit_reader_frame_t lock_frame_for_event_callback();
        sensekit_reader_frame_t lock_frame_for_poll();
        sensekit_reader_frame_t acquire_available_reader_frame();

        sensekit_status_t unlock_frame_and_check_connections(sensekit_reader_frame_t& readerFrame);
        sensekit_status_t return_locked_frame(sensekit_reader_frame_t& readerFrame);

        void ensure_connections_locked();
        sensekit_status_t unlock_connections_if_able();

        StreamConnection* find_stream_of_type(sensekit_stream_desc_t& desc);
        StreamConnection::FrameReadyCallback get_sc_frame_ready_callback();
        void on_connection_frame_ready(StreamConnection* connection, sensekit_frame_index_t frameIndex);
        void check_for_all_frames_ready();
        void raise_frame_ready();

        bool m_locked{false};
        bool m_isFrameReadyForLock{false};
        sensekit_frame_index_t m_lastFrameIndex{-1};
        StreamSetConnection& m_connection;

        using ConnectionMap = std::unordered_map<sensekit_stream_desc_t,
                                                 ReaderConnectionData*,
                                                 StreamDescHash,
                                                 StreamDescEqualTo>;
        ConnectionMap m_streamMap;

        using FramePtr  = std::unique_ptr<_sensekit_reader_frame>;
        using FrameList = std::vector<FramePtr>;

        FrameList m_frameList;
        int32_t m_lockedFrameCount{0};

        Signal<sensekit_reader_t, sensekit_reader_frame_t> m_frameReadySignal;

        StreamConnection::FrameReadyCallback m_scFrameReadyCallback;

        Logger m_logger;
    };
}

#endif /* STREAMREADER_H */
