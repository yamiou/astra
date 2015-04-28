#ifndef MULTIBINSTREAM_H
#define MULTIBINSTREAM_H

#include "Stream.h"
#include "StreamBin.h"
#include <memory>
#include <vector>

namespace sensekit { namespace plugins {

    template<typename TFrameType, typename TBlockType>
    class MultiBinStream : public Stream
    {
    public:
        using bin_type = StreamBin<TFrameType>;

        MultiBinStream(PluginServiceProxy& pluginService,
                       Sensor streamSet,
                       StreamDescription description)
            : Stream(pluginService, streamSet, description)
        {}

    private:
        using BinPtr = std::unique_ptr<bin_type>;
        using BinPtrList = std::vector<BinPtr>;

        BinPtrList m_bins;
    };

}}


#endif /* MULTIBINSTREAM_H */
