#include "SkeletonPlugin.h"
#include <astra_core/Astra.h>

EXPORT_PLUGIN(astra::plugins::skeleton::SkeletonPlugin);

namespace astra { namespace plugins { namespace skeleton {

    void SkeletonPlugin::on_stream_added(astra_streamset_t setHandle,
                                         astra_stream_t streamHandle,
                                         astra_stream_desc_t desc)
    {
        if (desc.type != ASTRA_STREAM_DEPTH)
            return; // if new stream is not depth, we don't care.

        m_skeletonTrackers.push_back(std::make_unique<SkeletonTracker>(get_pluginService(),
                                                                     setHandle,
                                                                     streamHandle));
    }

    void SkeletonPlugin::on_stream_removed(astra_streamset_t setHandle,
                                           astra_stream_t streamHandle,
                                           astra_stream_desc_t desc)
    {
        auto it = std::find_if(m_skeletonTrackers.cbegin(),
                               m_skeletonTrackers.cend(),
                               [&streamHandle] (const SkeletonTrackerPtr& trackerPtr)
                               {
                                   return trackerPtr->sourceStream() == streamHandle;
                               });

        if (it != m_skeletonTrackers.cend())
        {
            m_skeletonTrackers.erase(it);
        }
    }

}}}
