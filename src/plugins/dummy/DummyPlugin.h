#ifndef DUMMYPLUGIN_H
#define DUMMYPLUGIN_H

#include <Plugins/plugin_api.h>
#include <SenseKitUL.h>

#include <iostream>
using std::cout;
using std::endl;

namespace sensekit {

    class DummyPlugin : public PluginBase
    {
    public:
        DummyPlugin(PluginServiceProxy* pluginService)
            : PluginBase(pluginService) { }
    };
}

#endif /* DUMMYPLUGIN_H */
