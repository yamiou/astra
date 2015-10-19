#ifndef SKELETONSTREAM_H
#define SKELETONSTREAM_H

#include <astra_core/Plugins/PluginKit.h>
#include <astra_core/Astra.h>
#include <astra/capi/astra_ctypes.h>
#include <astra/streams/skeleton_types.h>

namespace astra { namespace plugins { namespace skeleton {

    class skeletonstream : public astra::plugins::SingleBinStream<astra_skeletonframe_wrapper_t,
                                                                  astra_skeleton_joint_t>
    {
    public:
        skeletonstream(astra::PluginServiceProxy& pluginService,
                       astra_streamset_t streamSet,
                       astra_stream_t sourceStream,
                       size_t skeletonCount)
            : SingleBinStream(pluginService,
                              streamSet,
                              astra::stream_description(ASTRA_STREAM_SKELETON,
                                                        DEFAULT_SUBTYPE),
                              sizeof(astra_skeleton_t) * skeletonCount)

        {
            enable_callbacks();
        }
    };
}}}

#endif /* SKELETONSTREAM_H */
