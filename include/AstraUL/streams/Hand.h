#ifndef HAND_H
#define HAND_H

#include <Astra/Astra.h>
#include <stdexcept>
#include <AstraUL/skul_ctypes.h>
#include <AstraUL/streams/hand_capi.h>
#include <AstraUL/Vector.h>

namespace astra {

    class HandPoint
    {
    public:
        explicit HandPoint(astra_handpoint_t handPoint) :
            m_handPoint(handPoint),
            m_depthPosition(handPoint.depthPosition),
            m_worldPosition(handPoint.worldPosition),
            m_worldDeltaPosition(handPoint.worldDeltaPosition)
        { }

        inline int32_t trackingId() const { return m_handPoint.trackingId; }
        inline astra_handstatus_t status() const { return m_handPoint.status; }
        inline const Vector2i& depthPosition() const { return m_depthPosition; }
        inline const Vector3f& worldPosition() const { return m_worldPosition; }
        inline const Vector3f& worldDeltaPosition() const { return m_worldDeltaPosition; }

    private:
        astra_handpoint_t m_handPoint;
        Vector2i m_depthPosition;
        Vector3f m_worldPosition;
        Vector3f m_worldDeltaPosition;
    };

    using HandPointList = std::vector<HandPoint>;

    class HandStream : public DataStream
    {
    public:
        explicit HandStream(astra_streamconnection_t connection)
            : DataStream(connection),
            m_handStream(connection)
        { }

        static const astra_stream_type_t id = ASTRA_STREAM_HAND;

        bool get_include_candidate_points()
        {
            bool includeCandidatePoints;
            astra_handstream_get_include_candidate_points(m_handStream, &includeCandidatePoints);
            return includeCandidatePoints;
        }

        void set_include_candidate_points(bool includeCandidatePoints)
        {
            astra_handstream_set_include_candidate_points(m_handStream, includeCandidatePoints);
        }
    private:
        astra_handstream_t m_handStream;
    };

    class HandFrame
    {
    public:
        template<typename TFrameType>
        static TFrameType acquire(astra_reader_frame_t readerFrame,
                                  astra_stream_subtype_t subtype)
        {
            if (readerFrame != nullptr)
            {
                astra_handframe_t handFrame;
                astra_frame_get_handframe_with_subtype(readerFrame, subtype, &handFrame);
                return TFrameType(handFrame);
            }

            return TFrameType(nullptr);
        }

        HandFrame(astra_handframe_t handFrame)
        {
            m_handFrame = handFrame;
            if (m_handFrame)
            {
                astra_handframe_get_frameindex(m_handFrame, &m_frameIndex);

                size_t maxHandCount;
                astra_handframe_get_hand_count(m_handFrame, &maxHandCount);

                m_handPoints.reserve(maxHandCount);
            }
        }

        bool is_valid() { return m_handFrame != nullptr; }
        astra_handframe_t handle() { return m_handFrame; }

        size_t handpoint_count()
        {
            throwIfInvalidFrame();
            verify_handpointlist();
            return m_handPoints.size();
        }

        const HandPointList& handpoints()
        {
            throwIfInvalidFrame();
            verify_handpointlist();
            return m_handPoints;
        }

        astra_frame_index_t frameIndex() { throwIfInvalidFrame(); return m_frameIndex; }

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

            astra_handpoint_t* handPtr;
            size_t maxHandCount;

            astra_handframe_get_shared_hand_array(m_handFrame, &handPtr, &maxHandCount);

            for (size_t i = 0; i < maxHandCount; ++i, ++handPtr)
            {
                astra_handpoint_t& p = *handPtr;
                if (p.status != astra_handstatus_t::HAND_STATUS_NOTTRACKING)
                {
                    m_handPoints.push_back(HandPoint(p));
                }
            }
        }

        bool m_handPointsInitialized{ false };
        HandPointList m_handPoints;
        astra_handframe_t m_handFrame{nullptr};
        astra_frame_index_t m_frameIndex;
    };
}

#endif /* HAND_H */
