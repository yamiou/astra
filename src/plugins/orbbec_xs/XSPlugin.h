#ifndef XSPLUGIN_H
#define XSPLUGIN_H

#include <Astra/Plugins/PluginKit.h>
#include <AstraUL/AstraUL.h>
#include "StylizedDepthStream.h"
#include "PointProcessor.h"
#include <memory>
#include <unordered_map>

namespace astra { namespace plugins { namespace xs {

    class XSPlugin : public astra::PluginBase
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
        virtual void on_stream_added(astra_streamset_t setHandle,
                                     astra_stream_t streamHandle,
                                     astra_stream_desc_t streamDesc) override;

        virtual void on_stream_removed(astra_streamset_t setHandle,
                                       astra_stream_t streamHandle,
                                       astra_stream_desc_t streamDesc) override;

        using PointProcessorPtr = std::unique_ptr<PointProcessor>;
        using PointProcessorMap =  std::unordered_map<astra_stream_t,
                                                     PointProcessorPtr,
                                                     StreamHandleHash,
                                                     StreamHandleEqualTo>;

        PointProcessorMap m_pointProcessorMap;
    };
}}}


#endif /* XSPLUGIN_H */
