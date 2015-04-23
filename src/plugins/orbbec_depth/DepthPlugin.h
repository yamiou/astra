#ifndef SKELETONPLUGIN_H
#define SKELETONPLUGIN_H

#include <SenseKit/Plugins/PluginKit.h>
#include <SenseKitUL/SenseKitUL.h>
#include "StylizedDepthStream.h"
#include <memory>
#include <vector>

namespace sensekit { namespace plugins { namespace depth {

    class DepthPlugin : public sensekit::PluginBase
    {
    public:
        static const size_t MAX_SKELETONS = 5;

        DepthPlugin(PluginServiceProxy* pluginProxy)
            : PluginBase(pluginProxy)
        {
            register_for_stream_events();
        }

        virtual ~DepthPlugin()
        {
            unregister_for_stream_events();
        }

    private:
        virtual void on_stream_added(sensekit_streamset_t setHandle,
                                     sensekit_stream_t streamHandle,
                                     sensekit_stream_desc_t desc) override;

        virtual void on_stream_removed(sensekit_streamset_t setHandle,
                                       sensekit_stream_t streamHandle,
                                       sensekit_stream_desc_t desc) override;

        using DepthStreamPtr = std::unique_ptr<StylizedDepthStream>;
        using DepthStreamList = std::vector<DepthStreamPtr>;

        DepthStreamList m_depthStreams;
    };
}}}


#endif /* SKELETONPLUGIN_H */
