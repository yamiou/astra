#ifndef STREAMREADER_H
#define STREAMREADER_H

#include <Astra/astra_types.h>
#include "Registry.h"
#include <unordered_map>
#include <vector>
#include <cassert>
#include "Core/Signal.h"
#include "astra_private.h"
#include "StreamConnection.h"

namespace astra {

    class StreamSetConnection;
    //class StreamConnection;

    class StreamDescHash
    {
    public:
        std::size_t operator()(const astra_stream_desc_t desc) const
            {
                std::size_t h1 = std::hash<astra_stream_type_t>()(desc.type);
                std::size_t h2 = std::hash<astra_stream_subtype_t>()(desc.subtype);

                return h1 ^ (h2 << 1);
            }
    };

    class StreamDescEqualTo
    {
    public:
        std::size_t operator()(const astra_stream_desc_t& lhs,
                               const astra_stream_desc_t& rhs) const
            {
                return lhs.type == rhs.type && lhs.subtype == rhs.subtype;
            }
    };

    struct ReaderConnectionData
    {
        StreamConnection* connection;
        astra_callback_id_t scFrameReadyCallbackId;
        bool isNewFrameReady;
        astra_frame_index_t currentFrameIndex;
    };

    class StreamReader : public TrackedInstance<StreamReader>
    {
    public:
        StreamReader(StreamSetConnection& connection);
        ~StreamReader();

        StreamReader& operator=(const StreamReader& rhs) = delete;
        StreamReader(const StreamReader& reader) = delete;

        inline StreamSetConnection& get_connection() const { return m_connection; }

        inline astra_reader_t get_handle() { return reinterpret_cast<astra_reader_t>(this); }

        StreamConnection* get_stream(astra_stream_desc_t& desc);
        astra_frame_t* get_subframe(astra_stream_desc_t& desc);

        astra_callback_id_t register_frame_ready_callback(astra_frame_ready_callback_t callback, void* clientTag);
        void unregister_frame_ready_callback(astra_callback_id_t& callbackId);

        //TODO: locking currently not threadsafe

        astra_status_t lock(int timeoutMillis, astra_reader_frame_t& readerFrame);
        astra_status_t unlock(astra_reader_frame_t& readerFrame);

        static inline StreamReader* get_ptr(astra_reader_t reader) { return Registry::get<StreamReader>(reader); }
        static inline StreamReader* from_frame(astra_reader_frame_t& frame)
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

        astra_reader_frame_t lock_frame_for_event_callback();
        astra_reader_frame_t lock_frame_for_poll();
        astra_reader_frame_t acquire_available_reader_frame();

        astra_status_t unlock_frame_and_check_connections(astra_reader_frame_t& readerFrame);
        astra_status_t return_locked_frame(astra_reader_frame_t& readerFrame);

        void ensure_connections_locked();
        astra_status_t unlock_connections_if_able();

        StreamConnection* find_stream_of_type(astra_stream_desc_t& desc);
        StreamConnection::FrameReadyCallback get_sc_frame_ready_callback();
        void on_connection_frame_ready(StreamConnection* connection, astra_frame_index_t frameIndex);
        void check_for_all_frames_ready();
        void raise_frame_ready();

        bool m_locked{false};
        bool m_isFrameReadyForLock{false};
        astra_frame_index_t m_lastFrameIndex{-1};
        StreamSetConnection& m_connection;

        using ConnectionMap = std::unordered_map<astra_stream_desc_t,
                                                 ReaderConnectionData*,
                                                 StreamDescHash,
                                                 StreamDescEqualTo>;
        ConnectionMap m_streamMap;

        using FramePtr  = std::unique_ptr<_astra_reader_frame>;
        using FrameList = std::vector<FramePtr>;

        FrameList m_frameList;
        int32_t m_lockedFrameCount{0};

        Signal<astra_reader_t, astra_reader_frame_t> m_frameReadySignal;

        StreamConnection::FrameReadyCallback m_scFrameReadyCallback;
    };
}

#endif /* STREAMREADER_H */
