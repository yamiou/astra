#ifndef HANDSETTINGS_H
#define HANDSETTINGS_H

#include <string>

namespace sensekit { namespace plugins { namespace hand {

    struct DepthUtilitySettings
    {
        float depthSmoothingFactor{ 0.05 };
        float velocityThresholdFactor{ 0.005 };
        float maxDepthJumpPercent{ 0.1 };
        int erodeSize{ 1 };
        float depthAdjustmentFactor{ 2.5 };
        float minDepth { 1000 };
        float maxDepth { 4000 };
    };

    struct TrajectoryAnalyzerSettings
    {
        float maxSteadyDelta{ 5 };
        int minSteadyFrames{ 15 };
        int minHeadingDist{ 125 };
        float deltaHeadingFactor{ 0.5 };
        float minHeadingDiffForInflection{ 135 };
        float maxHeadingDiffForContinuation{ 45 };
        int minWaveInflectionsForGesture{ 2 };
        int maxFramesBetweenInflections{ 90 };
    };

    struct PointProcessorSettings
    {
        TrajectoryAnalyzerSettings trajectoryAnalyzerSettings;

        float segmentationBandwidthDepthNear{ 200 };
        float segmentationBandwidthDepthFar{ 200 };
        float maxMatchDistLostActive{ 500 }; //mm
        float maxMatchDistDefault{ 200 }; //mm
        int iterationMaxInitial{ 1 };
        int iterationMaxTracking{ 1 };
        int iterationMaxRefinement{ 1 };
        float minArea{ 0 }; //mm^2
        float maxArea{ 30000 }; //mm^2
        float areaBandwidth{ 250 }; //mm
        float areaBandwidthDepth{ 100 }; //mm
        float maxSegmentationDist{ 250 }; //mm
        float steadyDeadBandRadius{ 75 }; //mm
        float targetEdgeDistance{ 40 }; //mm
        float heightScoreFactor{ 0.5 };
        float depthScoreFactor{ 1.0 };
        float edgeDistanceScoreFactor{ 100.0 };
        float pointInertiaFactor{ 100.0 };
        float pointInertiaRadius{ 100.0 }; //mm
        int maxInactiveFramesForCandidatePoints{ 60 };
        int maxInactiveFramesForLostPoints{ 15 };
        int maxInactiveFramesForActivePoints{ 480 };
        float pointSmoothingFactor{ 0.95 };
        float pointDeadBandSmoothingFactor{ 0.05 };
        float pointSmoothingDeadZone{ 50 }; //mm
        float foregroundRadius1{ 100 };
        float foregroundRadius2{ 150 };
        float foregroundRadiusMaxPercent1{ 0.35 };
        float foregroundRadiusMaxPercent2{ 0.15 };
        int maxFailedTestsInProbation{ 5 };
        int probationFrameCount{ 30 };
        int maxFailedTestsInProbationActivePoints{ 3 };
        float secondChanceMinDistance{ 100 };
        float mergePointDistance { 100 }; //mm
    };

    struct HandSettings
    {
        int processingSizeWidth{ 160 };
        int processingSizeHeight{ 120 };

        DepthUtilitySettings depthUtilitySettings;
        PointProcessorSettings pointProcessorSettings;
    };

    HandSettings parse_settings(std::string path);
}}}

#endif // HANDSETTINGS_H
