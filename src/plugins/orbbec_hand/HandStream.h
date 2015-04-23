#ifndef HANDSTREAM_H
#define HANDSTREAM_H

#include <SenseKit/Plugins/SingleBinStream.h>
#include <SenseKitUL/streams/hand_types.h>
#include <SenseKitUL/skul_ctypes.h>

namespace sensekit { namespace plugins { namespace hand {

    class HandStream : public SingleBinStream<sensekit_handframe_wrapper_t,
                                              sensekit_handpoint_t>

    {
    public:
        HandStream(PluginServiceProxy& pluginService,
                   Sensor& streamSet,
                   size_t maxHandCount)
            : SingleBinStream(pluginService,
                              streamSet,
                              StreamDescription(SENSEKIT_STREAM_HAND,
                                                DEFAULT_SUBTYPE),
                              sizeof(sensekit_handpoint_t) * maxHandCount)
        { }
    };

}}}

#endif /* HANDSTREAM_H */
