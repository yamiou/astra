#ifndef DUMMYPLUGIN_H
#define DUMMYPLUGIN_H

#include <Astra/Plugins/PluginKit.h>
#include <AstraUL/AstraUL.h>

namespace astra {

    class DummyPlugin : public PluginBase
    {
    public:
        DummyPlugin(PluginServiceProxy* pluginService)
            : PluginBase(pluginService, "dummy_plugin") { }
    };
}

#endif /* DUMMYPLUGIN_H */
