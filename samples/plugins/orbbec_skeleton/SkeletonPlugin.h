#ifndef SKELETONPLUGIN_H
#define SKELETONPLUGIN_H

#include <astra_core/Plugins/PluginKit.h>
#include <astra/astra.hpp>
#include "SkeletonTracker.h"
#include <memory>
#include <vector>

namespace astra { namespace plugins { namespace skeleton {

    class SkeletonPlugin : public astra::PluginBase
    {
    public:
        static const size_t MAX_SKELETONS = 5;

        SkeletonPlugin(PluginServiceProxy* pluginProxy)
            : PluginBase(pluginProxy, "orbbec_skeleton")
        {
            register_for_stream_events();
        }

        virtual ~SkeletonPlugin()
        {
            unregister_for_stream_events();
        }

    private:
        virtual void on_stream_added(astra_streamset_t setHandle,
                                     astra_stream_t streamHandle,
                                     astra_stream_desc_t desc) override;

        virtual void on_stream_removed(astra_streamset_t setHandle,
                                       astra_stream_t streamHandle,
                                       astra_stream_desc_t desc) override;

        using SkeletonTrackerPtr = std::unique_ptr<SkeletonTracker>;
        using SkeletonTrackerList = std::vector<SkeletonTrackerPtr>;

        SkeletonTrackerList m_skeletonTrackers;
    };
}}}


#endif /* SKELETONPLUGIN_H */
