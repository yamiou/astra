#ifndef DUMMYPLUGIN_H
#define DUMMYPLUGIN_H

#include <Plugins/PluginBase.h>
#include <SenseKitUL.h>

#include <iostream>
using std::cout;
using std::endl;

namespace sensekit {

    class DummyPlugin : public PluginBase
    {
    public:
        DummyPlugin(StreamServiceProxy* streamProxy, PluginServiceProxy* pluginService)
            : PluginBase(pluginService) { }

        virtual void temp_update() override { cout << "dumb." << endl; }

    private:
        StreamServiceProxyBase* m_streamProxy;
    };
}

#endif /* DUMMYPLUGIN_H */
