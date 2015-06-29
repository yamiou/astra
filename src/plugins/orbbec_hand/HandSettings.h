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
        float minDepth { 500 };
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

    struct AreaTestSettings
    {
        float areaBandwidth{ 150 }; //mm
        float areaBandwidthDepth{ 100 }; //mm
        float minArea{ 3000 }; //mm^2
        float maxArea{ 35000 }; //mm^2
    };

    struct NaturalEdgeTestSettings
    {
        float naturalEdgeBandwidth{ 150 }; //mm
        float minPercentNaturalEdges { 0.8 }; //mm^2
    };

    struct CircumferenceTestSettings
    {
        float foregroundRadius1{ 100 };
        float foregroundRadius2{ 150 };
        float foregroundRadiusMinPercent1{ 0.08 };
        float foregroundRadiusMinPercent2{ 0.04 };
        float foregroundRadiusMaxPercent1{ 0.35 };
        float foregroundRadiusMaxPercent2{ 0.15 };
    };

    struct SegmentationSettings
    {
        AreaTestSettings areaTestSettings;
        CircumferenceTestSettings circumferenceTestSettings;
        NaturalEdgeTestSettings naturalEdgeTestSettings;

        float segmentationBandwidthDepthNear{ 500 };
        float segmentationBandwidthDepthFar{ 100 };
        float maxSegmentationDist{ 250 }; //mm
        float heightScoreFactor{ 0.5 };
        float depthScoreFactor{ 1000.0 };
        float targetEdgeDistance{ 40 }; //mm
        float edgeDistanceScoreFactor{ 100.0 };
        float pointInertiaFactor{ 40.0 };
        float pointInertiaRadius{ 40.0 }; //mm
    };

    struct PointProcessorSettings
    {
        TrajectoryAnalyzerSettings trajectoryAnalyzerSettings;
        SegmentationSettings segmentationSettings;

        float maxMatchDistLostActive{ 500 }; //mm
        float maxMatchDistDefault{ 500 }; //mm
        float steadyDeadBandRadius{ 75 }; //mm
        int maxInactiveFramesForCandidatePoints{ 60 };
        int maxInactiveFramesForLostPoints{ 15 };
        int maxInactiveFramesForActivePoints{ 480 };
        float pointSmoothingFactor{ 0.75 };
        float pointDeadBandSmoothingFactor{ 0.05 };
        float pointSmoothingDeadZone{ 50 }; //mm
        int maxFailedTestsInProbation{ 5 };
        int probationFrameCount{ 30 };
        int maxFailedTestsInProbationActivePoints{ 3 };
        float secondChanceMinDistance{ 100 };
        float mergePointDistance { 100 }; //mm
        int maxHandPointUpdatesPerFrame { 10 };
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
