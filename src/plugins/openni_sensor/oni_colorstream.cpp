#include "oni_colorstream.hpp"

namespace orbbec { namespace ni {

    colorstream::colorstream(astra::PluginServiceProxy& pluginService,
                             astra_streamset_t streamSet,
                             openni::Device& oniDevice,
                             stream_listener& listener)
        : devicestream(pluginService,
                       streamSet,
                       astra::stream_description(
                           ASTRA_STREAM_COLOR,
                           DEFAULT_SUBTYPE),
                       oniDevice,
                       openni::SENSOR_COLOR,
                       listener)
    {}
}}
