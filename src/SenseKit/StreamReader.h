#ifndef STREAMREADER_H
#define STREAMREADER_H

#include <SenseKit/sensekit_types.h>
#include "StreamSet.h"
#include "StreamConnection.h"
#include <unordered_map>
#include <cassert>
#include "Core/Signal.h"

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

    class StreamReader
    {
    public:
        explicit StreamReader(StreamSet& streamSet);

        ~StreamReader();

        StreamSet& get_streamSet() const { return m_streamSet; }

        StreamConnection* get_stream(sensekit_stream_desc_t& desc);
        sensekit_frame_ref_t* get_subframe(sensekit_stream_desc_t& desc);

        sensekit_callback_id_t register_frame_ready_callback(sensekit_frame_ready_callback_t callback, void* clientTag);
        void unregister_frame_ready_callback(sensekit_callback_id_t& callbackId);

        //TODO: locking currently not threadsafe

        sensekit_status_t lock(int timeoutMillis);
        void unlock();

        sensekit_reader_t get_handle() { return reinterpret_cast<sensekit_reader_t>(this); }
        static StreamReader* get_ptr(sensekit_reader_t reader) { return reinterpret_cast<StreamReader*>(reader); }
        static StreamReader* from_frame(sensekit_reader_frame_t frame) { return reinterpret_cast<StreamReader*>(frame); }

    private:
        enum class blockresult
        {
            BLOCKRESULT_TIMEOUT = 0,
            BLOCKRESULT_FRAMEREADY = 1
        };

        blockresult block_until_frame_ready_or_timeout(int timeoutMillis);
        void check_for_all_frames_ready();
        void raise_frame_ready();
        void lock_private();

        StreamConnection* find_stream_of_type(sensekit_stream_desc_t& desc);
        StreamConnection::FrameReadyCallback get_sc_frame_ready_callback();
        void on_connection_frame_ready(StreamConnection* connection, sensekit_frame_index_t frameIndex);

        using ConnectionMap = std::unordered_map<sensekit_stream_desc_t,
                                                 ReaderConnectionData*,
                                                 StreamDescHash,
                                                 StreamDescEqualTo>;

        bool m_locked{false};
        bool m_isFrameReadyForLock{ false };
        sensekit_frame_index_t m_lastFrameIndex{ -1 };
        sensekit_reader_frame_t* m_currentFrame{nullptr};
        StreamSet& m_streamSet;
        ConnectionMap m_streamMap;
        Signal<sensekit_reader_t, sensekit_reader_frame_t> m_frameReadySignal;

        StreamConnection::FrameReadyCallback m_scFrameReadyCallback;
    };
}

#endif /* STREAMREADER_H */
