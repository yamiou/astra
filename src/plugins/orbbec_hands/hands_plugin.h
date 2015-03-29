#ifndef HANDS_PLUGIN_H
#define HANDS_PLUGIN_H

#include <Plugins/PluginBase.h>

namespace sensekit
{
    namespace hands
    {

        class HandsPlugin : public PluginBase
        {
        public:
            HandsPlugin(PluginServiceProxy* pluginProxy)
                : PluginBase(pluginProxy) { }

    	    virtual void temp_update() override {}
        };
    }
}

EXPORT_PLUGIN(sensekit::hands::HandsPlugin);

#endif /* HANDS_PLUGIN_H */
