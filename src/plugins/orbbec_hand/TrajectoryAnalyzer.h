#ifndef TRAJECTORYANALYZER_H
#define TRAJECTORYANALYZER_H

#include "TrackedPoint.h"
#include "HandSettings.h"
#include <SenseKit/Plugins/PluginLogger.h>

namespace sensekit { namespace plugins { namespace hand {

    class TrajectoryAnalyzer
    {
    public:
        TrajectoryAnalyzer(int trackingId, PluginLogger& pluginLogger, HandSettings& settings);
        ~TrajectoryAnalyzer();
        void update(TrackedPoint& point);

        void reset_wave();
        void reset_steady();

        bool pointSteady() const { return m_pointSteady; }

        int trackingId() const { return m_trackingId; }

        bool isWaveGesture() const { return m_isWaveGesture; }

    private:

        float get_delta_angle(float x, float y);
        bool is_valid_heading_dist(const cv::Point3f& currentWorldPosition);
        float get_degree_difference(cv::Point3f& v1, cv::Point3f& v2);
        
        int m_trackingId;
        PluginLogger& m_logger;


        bool m_pointSteady;
        int m_numSteadyFrames;
        cv::Point3f m_avgDeltaHeading;
        cv::Point3f m_lastAvgDeltaHeading;
        bool m_avgDeltaHeadingValid;
        bool m_lastAvgDeltaHeadingValid;
        bool m_isTrackingHeading;
        cv::Point3f m_headingTrackStart;
        bool m_isInflecting;
        int m_numWaveInflections;
        bool m_isWaveGesture;
        int m_framesSinceInflection;
        
        float m_maxSteadyDelta;
        int m_minSteadyFrames;
        int m_minHeadingDist;
        float m_worldDelta{0};
        float m_deltaHeadingFactor;
        float m_minHeadingDiffForInflection;
        float m_maxHeadingDiffForContinuation;
        int m_minWaveInflectionsForGesture;
        int m_maxFramesBetweenInflections;
    };
}}}

#endif // TRAJECTORYANALYZER_H
