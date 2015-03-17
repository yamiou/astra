#ifndef PLUGINBASE_H
#define PLUGINBASE_H

#include "PluginService.h"

namespace sensekit
{
    class PluginBase
    {
    public:
        PluginBase() { };
        virtual ~PluginBase() { };

        //stream core calls these on plugins
        //TODO transition this init call to the PluginBase ctor
        //TODO the void* was Context2* but compiler puked...circular reference?
        virtual void orbbec_plugin_init(void* context, PluginService* pluginService) = 0;
        virtual void orbbec_plugin_cleanup() = 0;
        virtual void temp_update() = 0;
    };
}

#endif /* PLUGINBASE_H */