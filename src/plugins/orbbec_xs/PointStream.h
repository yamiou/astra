#ifndef POINTSTREAM_H
#define POINTSTREAM_H

#include <Astra/Plugins/SingleBinStream.h>
#include <AstraUL/streams/point_types.h>
#include <AstraUL/skul_ctypes.h>
#include <AstraUL/Plugins/stream_types.h>
#include <Shiny.h>

namespace astra { namespace plugins { namespace xs {

    class PointStream : public SingleBinStream<astra_imageframe_wrapper_t>

    {
    public:
        PointStream(PluginServiceProxy& pluginService,
                    astra_streamset_t streamSet,
                    uint32_t width,
                    uint32_t height)
            : SingleBinStream(pluginService,
                              streamSet,
                              StreamDescription(ASTRA_STREAM_POINT,
                                                DEFAULT_SUBTYPE),
                              width * height * sizeof(astra_vector3f_t))
        {}

    private:
        virtual void on_connection_removed(astra_bin_t bin,
                                           astra_streamconnection_t connection) override
        {
            SingleBinStream::on_connection_removed(bin, connection);
        }
    };
}}}

#endif /* POINTSTREAM_H */
