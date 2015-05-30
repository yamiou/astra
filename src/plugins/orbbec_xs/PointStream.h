#ifndef POINTSTREAM_H
#define POINTSTREAM_H

#include <SenseKit/Plugins/SingleBinStream.h>
#include <SenseKitUL/streams/point_types.h>
#include <SenseKitUL/skul_ctypes.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <Shiny.h>

namespace sensekit { namespace plugins { namespace xs {

    class PointStream : public SingleBinStream<sensekit_imageframe_wrapper_t,
                                               sensekit_vector3f_t>

    {
    public:
        PointStream(PluginServiceProxy& pluginService,
                    sensekit_streamset_t streamSet,
                    uint32_t width,
                    uint32_t height)
            : SingleBinStream(pluginService,
                              streamSet,
                              StreamDescription(SENSEKIT_STREAM_POINT,
                                                DEFAULT_SUBTYPE),
                              width * height * sizeof(sensekit_vector3f_t))
        {
            PROFILE_FUNC();
        }

    private:
        virtual void on_connection_removed(sensekit_bin_t bin,
                                           sensekit_streamconnection_t connection) override
        {
            PROFILE_FUNC();

            SingleBinStream::on_connection_removed(bin, connection);
        }
    };
}}}

#endif /* POINTSTREAM_H */
