#include "oni_infrared_stream.hpp"

namespace orbbec { namespace ni {

    infrared_stream::infrared_stream(astra::PluginServiceProxy& pluginService,
                                     astra_streamset_t streamSet,
                                     openni::Device& oniDevice,
                                     stream_listener& listener)
        : devicestream(pluginService,
                       streamSet,
                       astra::StreamDescription(
                           ASTRA_STREAM_INFRARED,
                           DEFAULT_SUBTYPE),
                       oniDevice,
                       openni::SENSOR_IR,
                       listener)
    {}
}}
