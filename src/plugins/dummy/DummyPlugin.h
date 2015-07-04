#ifndef DUMMYPLUGIN_H
#define DUMMYPLUGIN_H

#include <SenseKit/Plugins/PluginKit.h>
#include <SenseKitUL/SenseKitUL.h>

namespace sensekit {

    class DummyPlugin : public PluginBase
    {
    public:
        DummyPlugin(PluginServiceProxy* pluginService)
            : PluginBase(pluginService, "dummy_plugin") { }
    };
}

#endif /* DUMMYPLUGIN_H */
