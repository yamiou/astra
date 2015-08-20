#ifndef SKELETONSTREAM_H
#define SKELETONSTREAM_H

#include <Astra/Plugins/PluginKit.h>
#include <Astra/Astra.h>
#include <AstraUL/skul_ctypes.h>
#include <AstraUL/streams/skeleton_types.h>

namespace astra { namespace plugins { namespace skeleton {

    class SkeletonStream : public astra::plugins::SingleBinStream<astra_skeletonframe_wrapper_t,
                                                                     astra_skeleton_joint_t>
    {
    public:
        SkeletonStream(astra::PluginServiceProxy& pluginService,
                       astra_streamset_t streamSet,
                       astra_stream_t sourceStream,
                       size_t skeletonCount)
            : SingleBinStream(pluginService,
                              streamSet,
                              astra::StreamDescription(ASTRA_STREAM_SKELETON,
                                                          DEFAULT_SUBTYPE),
                              sizeof(astra_skeleton_t) * skeletonCount)

        {
            enable_callbacks();
        }
    };
}}}

#endif /* SKELETONSTREAM_H */
