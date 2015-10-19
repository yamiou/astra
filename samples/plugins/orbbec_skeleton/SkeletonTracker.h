#ifndef SKELETONTRACKER_H
#define SKELETONTRACKER_H

#include <astra_core/Plugins/PluginKit.h>
#include <astra_core/Astra.h>
#include <astra/capi/astra_ctypes.h>
#include <astra/streams/Depth.h>
#include <astra/streams/skeleton_types.h>
#include "skeletonstream.h"

namespace astra { namespace plugins { namespace skeleton {

    class SkeletonTracker : public astra::frame_listener
    {
    public:
        static const size_t MAX_SKELETONS;

        SkeletonTracker(PluginServiceProxy& pluginService,
                        astra_streamset_t streamSet,
                        astra_stream_t sourceStream)
            : sourceStreamHandle_(sourceStream),
              sensor_(get_uri_for_streamset(pluginService, streamSet)),
              pluginService_(pluginService)
        {
            reader_ = sensor_.create_reader();
            depthStream_ = reader_.stream<astra::depthstream>();
            depthStream_.start();

            reader_.addListener(*this);
            skeletonStream_ = std::make_unique<skeletonstream>(pluginService_,
                                                               streamSet,
                                                               sourceStreamHandle_,
                                                               SkeletonTracker::MAX_SKELETONS);
        }

        astra_stream_t sourceStream() { return sourceStreamHandle_; }

        virtual void on_frame_ready(astra::StreaReader_& reader, astra::frame& frame) override;

    private:
        astra_stream_t sourceStreamHandle_;
        depthstream depthStream_{nullptr};
        streamset sensor_;
        stream_reader reader_;
        PluginServiceProxy& pluginService_;

        using skeletonstream_ptr = std::unique_ptr<skeletonstream>;
        skeletonstream_ptr skeletonStream_;
    };


}}}


#endif /* SKELETONTRACKER_H */
