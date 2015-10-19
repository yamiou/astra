#ifndef HND_TRAJECTORY_ANALYZER_H
#define HND_TRAJECTORY_ANALYZER_H

#include "hnd_tracked_point.hpp"
#include "hnd_settings.hpp"
#include <astra_core/Plugins/PluginLogger.h>

namespace astra { namespace hand {

    class trajectory_analyzer
    {
    public:
        trajectory_analyzer(int trackingId, trajectory_analyzer_settings& settings);
        ~trajectory_analyzer();

        void update(tracked_point& point);

        void reset_wave();
        void set_for_next_inflection();
        void reset_steady();

        bool pointSteady() const { return pointSteady_; }
        int trackingId() const { return trackingId_; }
        bool is_wave_gesture() const { return isWaveGesture_; }

    private:
        float get_delta_angle(float x, float y);
        bool is_valid_heading_dist(const cv::Point3f& currentWorldPosition);
        float get_degree_difference(cv::Point3f& v1, cv::Point3f& v2);

        int trackingId_;

        bool pointSteady_;
        int numSteadyFrames_;
        cv::Point3f accumulatedDeltaHeading_;
        cv::Point3f lastAccumulatedDeltaHeading_;
        bool avgDeltaHeadingValid_;
        bool lastAvgDeltaHeadingValid_;
        bool isTrackingHeading_;
        cv::Point3f headingTrackStart_;
        bool isInflecting_;
        int numWaveInflections_;
        bool isWaveGesture_;
        int framesSinceInflection_;
        cv::Point3f recentDeltaHeading_;

        float maxSteadyDelta_;
        int minSteadyFrames_;
        int minHeadingDist_;
        float deltaHeadingFactor_;
        float minHeadingDiffForInflection_;
        float maxHeadingDiffForContinuation_;
        int minWaveInflectionsForGesture_;
        int maxFramesBetweenInflections_;
    };
}}

#endif // HND_TRAJECTORY_ANALYZER_H
