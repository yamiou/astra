#ifndef XS_POINTSTREAM_H
#define XS_POINTSTREAM_H

#include <astra_core/Plugins/SingleBinStream.h>
#include <astra/capi/streams/point_types.h>
#include <astra/capi/astra_ctypes.h>
#include <astra/capi/streams/stream_types.h>
#include <Shiny.h>

namespace astra { namespace xs {

    class pointstream : public astra::plugins::SingleBinStream<astra_imageframe_wrapper_t>
    {
    public:
        pointstream(PluginServiceProxy& pluginService,
                    astra_streamset_t streamSet,
                    uint32_t width,
                    uint32_t height)
            : SingleBinStream(pluginService,
                              streamSet,
                              stream_description(ASTRA_STREAM_POINT,
                                                DEFAULT_SUBTYPE),
                              width * height * sizeof(astra_vector3f_t))
        {}
    };
}}

#endif /* XS_POINTSTREAM_H */
