#ifndef SETTINGSPARSER_H
#define SETTINGSPARSER_H

#include "../../SenseKit/vendor/cpptoml.h"
#include "HandSettings.h"
#include <string>
#include <cstdint>

namespace sensekit { namespace plugins { namespace hand {

    template<typename T>
    T get_from_table(cpptoml::table& t, std::string key, T defaultValue)
    {
        if (t.contains_qualified(key))
        {
            return t.get_qualified(key)->as<T>()->get();
        }
        return defaultValue;
    }

    inline float get_float_from_table(cpptoml::table& t, std::string key, float defaultValue)
    {
        double value = get_from_table<double>(t, key, defaultValue);

        return static_cast<float>(value);
    }

    inline int get_int_from_table(cpptoml::table& t, std::string key, int defaultValue)
    {
        int64_t value = get_from_table<int64_t>(t, key, defaultValue);

        return static_cast<int>(value);
    }

    inline HandSettings parse_settings(std::string path)
    {
        HandSettings settings;

        cpptoml::table t;

        try
        {
            t = cpptoml::parse_file(path);
        }
        catch (const cpptoml::parse_exception& e)
        {
            return settings;
        }

        settings.processingSizeWidth = get_int_from_table(t, "handtracker.processingSizeWidth", settings.processingSizeWidth);
        settings.processingSizeHeight = get_int_from_table(t, "handtracker.processingSizeHeight", settings.processingSizeHeight);

        settings.depthUtilitySettings.depthSmoothingFactor = get_float_from_table(t, "depthutility.depthSmoothingFactor", settings.depthUtilitySettings.depthSmoothingFactor);
        settings.depthUtilitySettings.velocityThresholdFactor = get_float_from_table(t, "depthutility.velocityThresholdFactor", settings.depthUtilitySettings.velocityThresholdFactor);
        settings.depthUtilitySettings.maxDepthJumpPercent = get_float_from_table(t, "depthutility.maxDepthJumpPercent", settings.depthUtilitySettings.maxDepthJumpPercent);
        settings.depthUtilitySettings.erodeSize = get_int_from_table(t, "depthutility.erodeSize", settings.depthUtilitySettings.erodeSize);
        settings.depthUtilitySettings.depthAdjustmentFactor = get_float_from_table(t, "depthutility.depthAdjustmentFactor", settings.depthUtilitySettings.depthAdjustmentFactor);

        settings.pointProcessorSettings.segmentationBandwidthDepthNear = get_float_from_table(t, "pointprocessor.segmentationBandwidthDepthNear", settings.pointProcessorSettings.segmentationBandwidthDepthNear);
        settings.pointProcessorSettings.segmentationBandwidthDepthFar = get_float_from_table(t, "pointprocessor.segmentationBandwidthDepthFar", settings.pointProcessorSettings.segmentationBandwidthDepthFar);
        settings.pointProcessorSettings.maxMatchDistLostActive = get_float_from_table(t, "pointprocessor.maxMatchDistLostActive", settings.pointProcessorSettings.maxMatchDistLostActive);
        settings.pointProcessorSettings.maxMatchDistDefault = get_float_from_table(t, "pointprocessor.maxMatchDistDefault", settings.pointProcessorSettings.maxMatchDistDefault);
        settings.pointProcessorSettings.iterationMaxInitial = get_int_from_table(t, "pointprocessor.iterationMaxInitial", settings.pointProcessorSettings.iterationMaxInitial);
        settings.pointProcessorSettings.iterationMaxTracking = get_int_from_table(t, "pointprocessor.iterationMaxTracking", settings.pointProcessorSettings.iterationMaxTracking);
        settings.pointProcessorSettings.iterationMaxRefinement = get_int_from_table(t, "pointprocessor.iterationMaxRefinement", settings.pointProcessorSettings.iterationMaxRefinement);
        settings.pointProcessorSettings.minArea = get_float_from_table(t, "pointprocessor.minArea", settings.pointProcessorSettings.minArea);
        settings.pointProcessorSettings.maxArea = get_float_from_table(t, "pointprocessor.maxArea", settings.pointProcessorSettings.maxArea);
        settings.pointProcessorSettings.areaBandwidth = get_float_from_table(t, "pointprocessor.areaBandwidth", settings.pointProcessorSettings.areaBandwidth);
        settings.pointProcessorSettings.areaBandwidthDepth = get_float_from_table(t, "pointprocessor.areaBandwidthDepth", settings.pointProcessorSettings.areaBandwidthDepth);
        settings.pointProcessorSettings.maxSegmentationDist = get_float_from_table(t, "pointprocessor.maxSegmentationDist", settings.pointProcessorSettings.maxSegmentationDist);
        settings.pointProcessorSettings.steadyDeadBandRadius = get_float_from_table(t, "pointprocessor.steadyDeadBandRadius", settings.pointProcessorSettings.steadyDeadBandRadius);
        settings.pointProcessorSettings.targetEdgeDistance = get_float_from_table(t, "pointprocessor.targetEdgeDistance", settings.pointProcessorSettings.targetEdgeDistance);
        settings.pointProcessorSettings.heightScoreFactor = get_float_from_table(t, "pointprocessor.heightScoreFactor", settings.pointProcessorSettings.heightScoreFactor);
        settings.pointProcessorSettings.depthScoreFactor = get_float_from_table(t, "pointprocessor.depthScoreFactor", settings.pointProcessorSettings.depthScoreFactor);
        settings.pointProcessorSettings.edgeDistanceScoreFactor = get_float_from_table(t, "pointprocessor.edgeDistanceScoreFactor", settings.pointProcessorSettings.edgeDistanceScoreFactor);
        settings.pointProcessorSettings.pointInertiaFactor = get_float_from_table(t, "pointprocessor.pointInertiaFactor", settings.pointProcessorSettings.pointInertiaFactor);
        settings.pointProcessorSettings.pointInertiaRadius = get_float_from_table(t, "pointprocessor.pointInertiaRadius", settings.pointProcessorSettings.pointInertiaRadius);
        settings.pointProcessorSettings.maxInactiveFramesForCandidatePoints = get_int_from_table(t, "pointprocessor.maxInactiveFramesForCandidatePoints", settings.pointProcessorSettings.maxInactiveFramesForCandidatePoints);
        settings.pointProcessorSettings.maxInactiveFramesForLostPoints = get_int_from_table(t, "pointprocessor.maxInactiveFramesForLostPoints", settings.pointProcessorSettings.maxInactiveFramesForLostPoints);
        settings.pointProcessorSettings.maxInactiveFramesForActivePoints = get_int_from_table(t, "pointprocessor.maxInactiveFramesForActivePoints", settings.pointProcessorSettings.maxInactiveFramesForActivePoints);
        settings.pointProcessorSettings.pointSmoothingFactor = get_float_from_table(t, "pointprocessor.pointSmoothingFactor", settings.pointProcessorSettings.pointSmoothingFactor);
        settings.pointProcessorSettings.pointDeadBandSmoothingFactor = get_float_from_table(t, "pointprocessor.pointDeadBandSmoothingFactor", settings.pointProcessorSettings.pointDeadBandSmoothingFactor);
        settings.pointProcessorSettings.pointSmoothingDeadZone = get_float_from_table(t, "pointprocessor.pointSmoothingDeadZone", settings.pointProcessorSettings.pointSmoothingDeadZone);
        settings.pointProcessorSettings.foregroundRadius1 = get_float_from_table(t, "pointprocessor.foregroundRadius1", settings.pointProcessorSettings.foregroundRadius1);
        settings.pointProcessorSettings.foregroundRadius2 = get_float_from_table(t, "pointprocessor.foregroundRadius2", settings.pointProcessorSettings.foregroundRadius2);
        settings.pointProcessorSettings.foregroundRadiusMaxPercent1 = get_float_from_table(t, "pointprocessor.foregroundRadiusMaxPercent1", settings.pointProcessorSettings.foregroundRadiusMaxPercent1);
        settings.pointProcessorSettings.foregroundRadiusMaxPercent2 = get_float_from_table(t, "pointprocessor.foregroundRadiusMaxPercent2", settings.pointProcessorSettings.foregroundRadiusMaxPercent2);
        settings.pointProcessorSettings.maxFailedTestsInProbation = get_int_from_table(t, "pointprocessor.maxFailedTestsInProbation", settings.pointProcessorSettings.maxFailedTestsInProbation);
        settings.pointProcessorSettings.probationFrameCount = get_int_from_table(t, "pointprocessor.probationFrameCount", settings.pointProcessorSettings.probationFrameCount);
        settings.pointProcessorSettings.maxFailedTestsInProbationActivePoints = get_int_from_table(t, "pointprocessor.maxFailedTestsInProbationActivePoints", settings.pointProcessorSettings.maxFailedTestsInProbationActivePoints);
        settings.pointProcessorSettings.secondChanceMinDistance = get_float_from_table(t, "pointprocessor.secondChanceMinDistance", settings.pointProcessorSettings.secondChanceMinDistance);

        settings.pointProcessorSettings.trajectoryAnalyzerSettings.maxSteadyDelta = get_float_from_table(t, "trajectoryanalyzer.maxSteadyDelta", settings.pointProcessorSettings.trajectoryAnalyzerSettings.maxSteadyDelta);
        settings.pointProcessorSettings.trajectoryAnalyzerSettings.minSteadyFrames = get_int_from_table(t, "trajectoryanalyzer.minSteadyFrames", settings.pointProcessorSettings.trajectoryAnalyzerSettings.minSteadyFrames);
        settings.pointProcessorSettings.trajectoryAnalyzerSettings.minHeadingDist = get_int_from_table(t, "trajectoryanalyzer.minHeadingDist", settings.pointProcessorSettings.trajectoryAnalyzerSettings.minHeadingDist);
        settings.pointProcessorSettings.trajectoryAnalyzerSettings.deltaHeadingFactor = get_float_from_table(t, "trajectoryanalyzer.deltaHeadingFactor", settings.pointProcessorSettings.trajectoryAnalyzerSettings.deltaHeadingFactor);
        settings.pointProcessorSettings.trajectoryAnalyzerSettings.minHeadingDiffForInflection = get_float_from_table(t, "trajectoryanalyzer.minHeadingDiffForInflection", settings.pointProcessorSettings.trajectoryAnalyzerSettings.minHeadingDiffForInflection);
        settings.pointProcessorSettings.trajectoryAnalyzerSettings.maxHeadingDiffForContinuation = get_float_from_table(t, "trajectoryanalyzer.maxHeadingDiffForContinuation", settings.pointProcessorSettings.trajectoryAnalyzerSettings.maxHeadingDiffForContinuation);
        settings.pointProcessorSettings.trajectoryAnalyzerSettings.minWaveInflectionsForGesture = get_int_from_table(t, "trajectoryanalyzer.minWaveInflectionsForGesture", settings.pointProcessorSettings.trajectoryAnalyzerSettings.minWaveInflectionsForGesture);
        settings.pointProcessorSettings.trajectoryAnalyzerSettings.maxFramesBetweenInflections = get_int_from_table(t, "trajectoryanalyzer.maxFramesBetweenInflections", settings.pointProcessorSettings.trajectoryAnalyzerSettings.maxFramesBetweenInflections);

        return settings;
    }
}}}

#endif // SETTINGSPARSER_H