#ifndef XSPLUGIN_H
#define XSPLUGIN_H

#include <SenseKit/Plugins/PluginKit.h>
#include <SenseKitUL/SenseKitUL.h>
#include "StylizedDepthStream.h"
#include "PointProcessor.h"
#include <memory>
#include <unordered_map>

namespace sensekit { namespace plugins { namespace xs {

    class XSPlugin : public sensekit::PluginBase
    {
    public:
        XSPlugin(PluginServiceProxy* pluginProxy)
            : PluginBase(pluginProxy, "orbbec_xs")
        {
            register_for_stream_events();
        }

        virtual ~XSPlugin()
        {
            unregister_for_stream_events();
        }

    private:
        virtual void on_stream_added(sensekit_streamset_t setHandle,
                                     sensekit_stream_t streamHandle,
                                     sensekit_stream_desc_t streamDesc) override;

        virtual void on_stream_removed(sensekit_streamset_t setHandle,
                                       sensekit_stream_t streamHandle,
                                       sensekit_stream_desc_t streamDesc) override;

        using PointProcessorPtr = std::unique_ptr<PointProcessor>;
        using PointProcessorMap =  std::unordered_map<sensekit_stream_t,
                                                     PointProcessorPtr,
                                                     StreamHandleHash,
                                                     StreamHandleEqualTo>;

        PointProcessorMap m_pointProcessorMap;
    };
}}}


#endif /* XSPLUGIN_H */
