#ifndef HAND_H
#define HAND_H

#include <SenseKit/SenseKit.h>
#include <stdexcept>
#include <SenseKitUL/StreamTypes.h>
#include "hand_capi.h"
#include <SenseKitUL/Vectorx.h>

namespace sensekit {
    class HandPoint
    {
    public:
        explicit HandPoint(sensekit_handpoint_t handPoint) : 
            m_handPoint(handPoint),
            m_depthPosition(cvector_to_vector(handPoint.depthPosition)),
            m_worldPosition(cvector_to_vector(handPoint.worldPosition)),
            m_worldDeltaPosition(cvector_to_vector(handPoint.worldDeltaPosition))
        { }

        inline int32_t get_trackingId() const { return m_handPoint.trackingId; }
        inline sensekit_handstatus_t get_status() const { return m_handPoint.status; }
        inline Vector2i get_depthPosition() const { return m_depthPosition; }
        inline Vector3f get_worldPosition() const { return m_worldPosition; }
        inline Vector3f get_worldDeltaPosition() const { return m_worldDeltaPosition; }

    private:
        
        const sensekit_handpoint_t m_handPoint;
        const Vector2i m_depthPosition;
        const Vector3f m_worldPosition;
        const Vector3f m_worldDeltaPosition;
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
