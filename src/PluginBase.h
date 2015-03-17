#ifndef PLUGINBASE_H
#define PLUGINBASE_H

#include "PluginService.h"

namespace sensekit
{
    class Context;

    class PluginBase
    {
    public:
        PluginBase() { };
        virtual ~PluginBase() { };

        //stream core calls these on plugins
        //TODO transition this init call to the PluginBase ctor
        virtual void initialize(Context* context, PluginService* pluginService) = 0;
        virtual void cleanup() = 0;
        virtual void temp_update() = 0;
    };
}

#endif /* PLUGINBASE_H */