#ifndef HAND_H
#define HAND_H

#include <SenseKit/SenseKit.h>
#include <stdexcept>
#include <SenseKitUL/StreamTypes.h>
#include "hand_capi.h"

namespace sensekit {

    class HandPoint
    {
        
    };
    class HandStream : public DataStream
    {
    public:
        explicit HandStream(sensekit_streamconnection_t connection)
            : DataStream(connection)
            { }

        static const sensekit_stream_type_t id = SENSEKIT_STREAM_HAND;
    };

    class HandFrame
    {
    public:
        HandFrame(sensekit_reader_frame_t readerFrame)
            {
                if (readerFrame != nullptr)
                {
                    sensekit_hand_get_frame(readerFrame, &m_handFrame);
                    sensekit_handframe_get_frameindex(m_handFrame, &m_frameIndex);
                    sensekit_handframe_get_hands_ptr(m_handFrame, &m_handPtr, &m_numHands);
                }
            }

        bool is_valid() { return m_handFrame != nullptr; }
        
        sensekit_frame_index_t get_frameIndex() { throwIfInvalidFrame(); return m_frameIndex; }
        const sensekit_handpoint_t* hands() { throwIfInvalidFrame(); return m_handPtr; }
        size_t get_numHands() { throwIfInvalidFrame(); return m_numHands; }

        void copy_to(sensekit_handpoint_t* buffer)
            {
                throwIfInvalidFrame();
                sensekit_handframe_copy_hands(m_handFrame, buffer);
            }

    private:
        void throwIfInvalidFrame()
            {
                if (m_handFrame == nullptr)
                {
                    throw std::logic_error("Cannot operate on an invalid frame");
                }
            }
        sensekit_handframe_t m_handFrame{nullptr};
        sensekit_frame_index_t m_frameIndex;
        sensekit_handpoint_t* m_handPtr;
        size_t m_numHands;
    };


}

#endif /* HAND_H */
