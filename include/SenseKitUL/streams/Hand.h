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

        inline int32_t trackingId() const { return m_handPoint.trackingId; }
        inline sensekit_handstatus_t status() const { return m_handPoint.status; }
        inline const Vector2i& depthPosition() const { return m_depthPosition; }
        inline const Vector3f& worldPosition() const { return m_worldPosition; }
        inline const Vector3f& worldDeltaPosition() const { return m_worldDeltaPosition; }

    private:
        
        sensekit_handpoint_t m_handPoint;
        Vector2i m_depthPosition;
        Vector3f m_worldPosition;
        Vector3f m_worldDeltaPosition;
    };
    using HandPointList = std::vector < HandPoint > ;

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
                    
                    size_t maxNumHands;
                    sensekit_handframe_get_num_hands(m_handFrame, &maxNumHands);

                    m_handPoints.reserve(maxNumHands);
                }
            }

        bool is_valid() { return m_handFrame != nullptr; }

        size_t handpoint_count()
        {
            verify_handpointlist();
            return m_handPoints.size();
        }

        const HandPointList& handpoints()
        {
            verify_handpointlist();
            return m_handPoints;
        }

        sensekit_frame_index_t frameIndex() { throwIfInvalidFrame(); return m_frameIndex; }

    private:
        void throwIfInvalidFrame()
            {
                if (m_handFrame == nullptr)
                {
                    throw std::logic_error("Cannot operate on an invalid frame");
                }
            }

        void verify_handpointlist()
        {
            if (m_handPointsInitialized)
            {
                return;
            }
            m_handPointsInitialized = true;

            sensekit_handpoint_t* handPtr;
            size_t maxNumHands;

            sensekit_handframe_get_hands_ptr(m_handFrame, &handPtr, &maxNumHands);

            for (int i = 0; i < maxNumHands; ++i, ++handPtr)
            {
                sensekit_handpoint_t& p = *handPtr;
                if (p.status != sensekit_handstatus_t::HAND_STATUS_NOTTRACKING)
                {
                    m_handPoints.push_back(HandPoint(p));
                }
            }
        }

        bool m_handPointsInitialized{ false };
        HandPointList m_handPoints;
        sensekit_handframe_t m_handFrame{nullptr};
        sensekit_frame_index_t m_frameIndex;
    };


}

#endif /* HAND_H */
