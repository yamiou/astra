#ifndef POINTPROCESSOR_H
#define POINTPROCESSOR_H

#include <opencv2/core/affine.hpp>
#include "TrackingData.h"
#include "CoordinateConverter.h"

namespace sensekit { namespace plugins { namespace hands {

    struct TrackedPoint;

    class PointProcessor
    {
    public:
        PointProcessor(const CoordinateConverter& converter);
        virtual ~PointProcessor();

        void updateTrackedPoints(TrackingMatrices& matrices);

        void removeOldOrDeadPoints();
        void removeDuplicatePoints();
        void updateTrackedPointOrCreateNewPointFromSeedPosition(TrackingMatrices& matrices,
                                                                const cv::Point& seedPosition);

        std::vector<TrackedPoint>& get_trackedPoints() { return m_trackedPoints; }

        void reset();

        float get_trackingBandwidthDepth() { return m_trackingBandwidthDepth; }


    private:

        void updateTrackedPoint(TrackingMatrices& matrices, TrackedPoint& trackedPoint);
        void validateAndUpdateTrackedPoint(TrackingMatrices& matrices,
                                           TrackedPoint& tracked,
                                           const cv::Point& targetPoint);
        bool isValidPointArea(TrackingMatrices& matrices, cv::Point targetPoint);

        const CoordinateConverter& m_converter;
        float m_trackingBandwidthDepth;
        float m_initialBandwidthDepth;
        float m_maxMatchDistLostActive;
        float m_maxMatchDistDefault;
        int m_iterationMaxInitial;
        int m_iterationMaxTracking;
        float m_minArea;
        float m_maxArea;
        float m_areaBandwidth;
        float m_areaBandwidthDepth;

        int m_nextTrackingId{ 0 };
        std::vector<TrackedPoint> m_trackedPoints;

    };

}}}

#endif // POINTPROCESSOR_H