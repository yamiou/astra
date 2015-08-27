#include "oni_colorstream.hpp"

namespace orbbec { namespace ni {

    colorstream::colorstream(astra::PluginServiceProxy& pluginService,
                             astra_streamset_t streamSet,
                             openni::Device& oniDevice)
        : devicestream(pluginService,
                       streamSet,
                       astra::StreamDescription(
                           ASTRA_STREAM_COLOR,
                           DEFAULT_SUBTYPE),
                       oniDevice,
                       openni::SENSOR_COLOR)
    {}
}}
