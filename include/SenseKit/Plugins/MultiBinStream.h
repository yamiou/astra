#ifndef MULTIBINSTREAM_H
#define MULTIBINSTREAM_H

#include "Stream.h"

namespace sensekit { namespace plugins {

    template<typename TFrameType, typename TBlockType>
    class MultiBinStream : public Stream
    {
    public:
        MultiBinStream(PluginServiceProxy& pluginService,
                       Sensor streamSet,
                       StreamDescription description)
            : Stream(pluginService, streamSet, description)
        {}

    };

}}


#endif /* MULTIBINSTREAM_H */
