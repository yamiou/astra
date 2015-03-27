#ifndef HANDS_PLUGIN_H
#define HANDS_PLUGIN_H

#include <Plugins/PluginBase.h>

class HandTrackerPlugin : public sensekit::PluginBase
{
public:
    HandTrackerPlugin(sensekit::StreamServiceProxy* streamProxy, sensekit::PluginServiceProxy* pluginProxy)
        : PluginBase(streamProxy, pluginProxy) { }

    virtual void temp_update() override {}
};

#endif /* HANDS_PLUGIN_H */
