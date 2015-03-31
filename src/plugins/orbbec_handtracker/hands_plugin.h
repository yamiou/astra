#ifndef HANDS_PLUGIN_H
#define HANDS_PLUGIN_H

#include <Plugins/PluginBase.h>

class HandTrackerPlugin : public sensekit::PluginBase
{
public:
    HandTrackerPlugin(sensekit::PluginServiceProxy* pluginProxy)
        : PluginBase(pluginProxy) { }

    virtual void temp_update() override {}
};

#endif /* HANDS_PLUGIN_H */
