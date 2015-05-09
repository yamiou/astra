#ifndef HANDSETTINGS_H
#define HANDSETTINGS_H

#include "../../SenseKit/vendor/cpptoml.h"

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
        float maxSegmentationDist{250};
        float steadyDeadBandRadius{75};
        float targetEdgeDistance{40};
        float heightScoreFactor{0.5};
        float depthScoreFactor{1.0};
        float edgeDistanceScoreFactor{4.0};
        float pointInertiaFactor{50.0};
        float pointInertiaRadius{60};
        int maxInactiveFramesForCandidatePoints{60};
        int maxInactiveFramesForLostPoints{15};
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
        float secondChanceMinDistance{100};
    };

    class SettingsParser
    {
    public:
        void parse(std::string path, HandSettings& settings)
        {
            cpptoml::table t;

            try
            {
                t = cpptoml::parse_file(path);
            }
            catch (const cpptoml::parse_exception& e)
            {
                return;
            }

            getIntFromTable(settings.processingSizeWidth, "processingSizeWidth", t);
            getIntFromTable(settings.processingSizeHeight, "processingSizeHeight", t);

            getFloatFromTable(settings.depthSmoothingFactor, "depthSmoothingFactor", t);
            getFloatFromTable(settings.velocityThresholdFactor, "velocityThresholdFactor", t);
            getFloatFromTable(settings.maxDepthJumpPercent, "maxDepthJumpPercent", t);
            getIntFromTable(settings.erodeSize, "erodeSize", t);
            getFloatFromTable(settings.depthAdjustmentFactor, "depthAdjustmentFactor", t);

            getFloatFromTable(settings.segmentationBandwidthDepthNear, "segmentationBandwidthDepthNear", t);
            getFloatFromTable(settings.segmentationBandwidthDepthFar, "segmentationBandwidthDepthFar", t);
            getFloatFromTable(settings.maxMatchDistLostActive, "maxMatchDistLostActive", t);
            getFloatFromTable(settings.maxMatchDistDefault, "maxMatchDistDefault", t);
            getIntFromTable(settings.iterationMaxInitial, "iterationMaxInitial", t);
            getIntFromTable(settings.iterationMaxTracking, "iterationMaxTracking", t);
            getIntFromTable(settings.iterationMaxRefinement, "iterationMaxRefinement", t);
            getFloatFromTable(settings.minArea, "minArea", t);
            getFloatFromTable(settings.maxArea, "maxArea", t);
            getFloatFromTable(settings.areaBandwidth, "areaBandwidth", t);
            getFloatFromTable(settings.areaBandwidthDepth, "areaBandwidthDepth", t);
            getFloatFromTable(settings.maxSegmentationDist, "maxSegmentationDist", t);
            getFloatFromTable(settings.steadyDeadBandRadius, "steadyDeadBandRadius", t);
            getFloatFromTable(settings.targetEdgeDistance, "targetEdgeDistance", t);
            getFloatFromTable(settings.heightScoreFactor, "heightScoreFactor", t);
            getFloatFromTable(settings.depthScoreFactor, "depthScoreFactor", t);
            getFloatFromTable(settings.edgeDistanceScoreFactor, "edgeDistanceScoreFactor", t);
            getFloatFromTable(settings.pointInertiaFactor, "pointInertiaFactor", t);
            getFloatFromTable(settings.pointInertiaRadius, "pointInertiaRadius", t);
            getIntFromTable(settings.maxInactiveFramesToBeConsideredActive, "maxInactiveFramesToBeConsideredActive", t);
            getIntFromTable(settings.minActiveFramesToLockTracking, "minActiveFramesToLockTracking", t);
            getIntFromTable(settings.maxInactiveFramesForCandidatePoints, "maxInactiveFramesForCandidatePoints", t);
            getIntFromTable(settings.maxInactiveFramesForLostPoints, "maxInactiveFramesForLostPoints", t);
            getIntFromTable(settings.maxInactiveFramesForActivePoints, "maxInactiveFramesForActivePoints", t);
            getFloatFromTable(settings.pointSmoothingFactor, "pointSmoothingFactor", t);
            getFloatFromTable(settings.pointDeadBandSmoothingFactor, "pointDeadBandSmoothingFactor", t);
            getFloatFromTable(settings.pointSmoothingDeadZone, "pointSmoothingDeadZone", t);
            getFloatFromTable(settings.foregroundRadius1, "foregroundRadius1", t);
            getFloatFromTable(settings.foregroundRadius2, "foregroundRadius2", t);
            getFloatFromTable(settings.foregroundRadiusMaxPercent1, "foregroundRadiusMaxPercent1", t);
            getFloatFromTable(settings.foregroundRadiusMaxPercent2, "foregroundRadiusMaxPercent2", t);
            getIntFromTable(settings.maxFailedTestsInProbation, "maxFailedTestsInProbation", t);
            getIntFromTable(settings.probationFrameCount, "probationFrameCount", t);
            getIntFromTable(settings.maxFailedTestsInProbationActivePoints, "maxFailedTestsInProbationActivePoints", t);
            getFloatFromTable(settings.secondChanceMinDistance, "secondChanceMinDistance", t);
        }

    private:
        void getFloatFromTable(float& setting, std::string key, cpptoml::table& t)
        {
            if (t.contains(key))
            {
                setting = static_cast<float>(t.get(key)->as<double>()->get());
            }
        }

        void getIntFromTable(int& setting, std::string key, cpptoml::table& t)
        {
            if (t.contains(key))
            {
                setting = static_cast<int>(t.get(key)->as<int64_t>()->get());
            }
        }
    };
}}}

#endif // HANDSETTINGS_H