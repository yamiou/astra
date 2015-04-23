#ifndef STYLIZEDDEPTHSTREAM_H
#define STYLIZEDDEPTHSTREAM_H

#include <SenseKit/Plugins/PluginKit.h>
#include <SenseKit/sensekit_types.h>

namespace sensekit { namespace plugins { namespace depth {

    class StylizedDepthStream
    {
    public:
        StylizedDepthStream(PluginServiceProxy& pluginService,
                            Sensor streamSet,
                            sensekit_stream_t sourceStream)
        {};
        virtual ~StylizedDepthStream() {};

        sensekit_stream_t get_handle() { return nullptr; }
    };

}}}

#endif /* STYLIZEDDEPTHSTREAM_H */
