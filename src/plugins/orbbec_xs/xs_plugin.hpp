#ifndef XS_PLUGIN_H
#define XS_PLUGIN_H

#include <Astra/Plugins/PluginKit.h>
#include <AstraUL/AstraUL.h>
#include "xs_point_processor.hpp"
#include <memory>
#include <map>

namespace astra { namespace xs {

    class plugin : public astra::PluginBase
    {
    public:
        plugin(PluginServiceProxy* pluginProxy)
            : PluginBase(pluginProxy, "orbbec_xs")
        {
            LOG_INFO("astra.xs.plugin", "Initializing xs plugin");
            register_for_stream_events();

        }

        virtual ~plugin()
        {
            unregister_for_stream_events();
            LOG_INFO("astra.xs.plugin", "Terminated xs plugin");
        }

    private:
        virtual void on_stream_added(astra_streamset_t setHandle,
                                     astra_stream_t streamHandle,
                                     astra_stream_desc_t streamDesc) override;

        virtual void on_stream_removed(astra_streamset_t setHandle,
                                       astra_stream_t streamHandle,
                                       astra_stream_desc_t streamDesc) override;

        using point_processor_ptr = std::unique_ptr<point_processor>;
        using point_processor_map = std::map<astra_stream_t,
                                              point_processor_ptr>;

        point_processor_map pointProcessorMap_;
    };
}}

#endif /* XS_PLUGIN_H */
