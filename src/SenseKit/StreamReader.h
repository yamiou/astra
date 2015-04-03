#ifndef STREAMREADER_H
#define STREAMREADER_H

#include <sensekit_core.h>
#include "StreamSet.h"
#include "SynchronizedFrame.h"
#include <set>
#include <cassert>

namespace sensekit {

    class StreamReader
    {
    public:
        explicit StreamReader(StreamSet& streamSet)
            : m_streamSet(streamSet) { }

        StreamSet& get_streamSet() const { return m_streamSet; }

        sensekit_streamconnection_t* get_stream(sensekit_stream_desc_t description);

        sensekit_reader_frame_t* lock()
            {
                sensekit_reader_frame_t* frame = new sensekit_reader_frame_t;
                int count = 0;

                for(auto connection : m_streamConnections)
                {
                    frame->streamFrames[count++] = connection->lock();
                }

                frame->reader =
                    reinterpret_cast<sensekit_reader_t*>(this);

                frame->numStreams = count;

                return frame;
            }

        void unlock(sensekit_reader_frame_t* frame)
            {
                assert(frame != nullptr);

                for(int i = 0; i < frame->numStreams; i++)
                {
                    StreamConnection* underlyingConnection =
                        reinterpret_cast<StreamConnection*>(
                            frame->streamFrames[i]->streamConnection);

                    assert(underlyingConnection != nullptr);

                    underlyingConnection->unlock(frame->streamFrames[i]);
                }

                delete frame;
            }

    private:
        StreamConnection* find_stream_of_type(sensekit_stream_type_t type,
                                              sensekit_stream_subtype_t subType);

        bool close_stream(StreamConnection* connection);

        using ConnectionSet = std::set<StreamConnection*>;

        StreamSet& m_streamSet;
        ConnectionSet  m_streamConnections;
    };
}

#endif /* STREAMREADER_H */
