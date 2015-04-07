#ifndef STREAMREADER_H
#define STREAMREADER_H

#include <sensekit_core.h>
#include "StreamSet.h"
#include "StreamConnection.h"
#include <unordered_map>
#include <cassert>

namespace sensekit {

    class StreamDescHash
    {
    public:
        std::size_t operator()(const sensekit_stream_desc_t desc) const
            {
                std::size_t h1 = std::hash<sensekit_stream_type_t>()(desc.type);
                std::size_t h2 = std::hash<sensekit_stream_subtype_t>()(desc.subType);

                return h1 ^ (h2 << 1);
            }
    };

    class StreamDescEqualTo
    {
    public:
        std::size_t operator()(const sensekit_stream_desc_t& lhs,
                               const sensekit_stream_desc_t& rhs) const
            {
                return lhs.type == rhs.type && lhs.subType == rhs.subType;
            }
    };


    class StreamReader
    {
    public:
        explicit StreamReader(StreamSet& streamSet)
            : m_streamSet(streamSet) { }

        ~StreamReader();

        StreamSet& get_streamSet() const { return m_streamSet; }

        sensekit_streamconnection_t* get_stream(sensekit_stream_desc_t& desc);
        sensekit_frame_ref_t* get_subframe(sensekit_stream_desc_t& desc);

        //TODO: locking currently not threadsafe

        void lock();
        void unlock();

        sensekit_reader_t get_handle() { return reinterpret_cast<sensekit_reader_t>(this); }
        static StreamReader* get_ptr(sensekit_reader_t reader) { return reinterpret_cast<StreamReader*>(reader); }
        static StreamReader* from_frame(sensekit_reader_frame_t frame) { return reinterpret_cast<StreamReader*>(frame); }

    private:
        StreamConnection* find_stream_of_type(sensekit_stream_desc_t& desc);

        bool destroy_stream_connection(StreamConnection* connection);

        using ConnectionMap = std::unordered_map<sensekit_stream_desc_t,
                                                 StreamConnection*,
                                                 StreamDescHash,
                                                 StreamDescEqualTo>;

        bool m_locked{false};
        sensekit_reader_frame_t* m_currentFrame{nullptr};
        StreamSet& m_streamSet;
        ConnectionMap m_streamMap;
    };
}

#endif /* STREAMREADER_H */
