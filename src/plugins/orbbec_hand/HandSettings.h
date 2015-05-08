#ifndef HANDSETTINGS_H
#define HANDSETTINGS_H

namespace sensekit { namespace plugins { namespace hand {

    struct HandSettings
    {
        float depthSmoothingFactor{0.05};
        float velocityThresholdFactor{0.005};
        float maxDepthJumpPercent{0.1};
        int erodeSize{1};
        float depthAdjustmentFactor{0.0025};

        int processingSizeWidth{160};
        int processingSizeHeight{120};

        float segmentationBandwidthDepthNear{500};
        float segmentationBandwidthDepthFar{100};
        float maxMatchDistLostActive{500};
        float maxMatchDistDefault{200};
        int iterationMaxInitial{1};
        int iterationMaxTracking{1};
        int iterationMaxRefinement{1};
        float minArea{0};
        float maxArea{30000};
        float areaBandwidth{250};
        float areaBandwidthDepth{100};
        float maxSegmentationDist{500};
        float steadyDeadBandRadius{75};
        float targetEdgeDistance{40};
        float heightScoreFactor{0.5};
        float depthScoreFactor{2.0};
        float edgeDistanceScoreFactor{4.0};
        float pointInertiaFactor{50.0};
        float pointInertiaRadius{60};
        int maxInactiveFramesToBeConsideredActive{10};
        int minActiveFramesToLockTracking{30};
        int maxInactiveFramesForCandidatePoints{60};
        int maxInactiveFramesForLostPoints{240};
        int maxInactiveFramesForActivePoints{480};
        float pointSmoothingFactor{0.75};
        float pointDeadBandSmoothingFactor{0.05};
        float pointSmoothingDeadZone{50};
        float foregroundRadius1{100};
        float foregroundRadius2{150};
        float foregroundRadiusMaxPercent1{0.35};
        float foregroundRadiusMaxPercent2{0.15};
        int maxFailedTestsInProbation{5};
        int probationFrameCount{30};
        int maxFailedTestsInProbationActivePoints{3};
    };
}}}

#endif // HANDSETTINGS_H