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

    float get_float_from_table(cpptoml::table& t, std::string key, float defaultValue);
    int get_int_from_table(cpptoml::table& t, std::string key, int defaultValue);
    DepthUtilitySettings parse_depth_utility_settings(cpptoml::table t);
    PointProcessorSettings parse_point_processor_settings(cpptoml::table t);
    TrajectoryAnalyzerSettings parse_trajectory_analyzer_settings(cpptoml::table t);
    HandSettings parse_settings(std::string path);
}}}

#endif // SETTINGSPARSER_H