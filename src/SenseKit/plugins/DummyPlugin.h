#ifndef DUMMYPLUGIN_H
#define DUMMYPLUGIN_H

#include "PluginBase.h"
#include <SenseKitUL.h>

#include <iostream>
using std::cout;
using std::endl;

namespace sensekit {

    class DummyPlugin : public PluginBase
    {
    public:
        DummyPlugin(StreamServiceProxy* streamProxy, PluginServiceProxy* pluginService)
            : PluginBase(streamProxy, pluginService) { }

        virtual void temp_update() override { cout << "dumb." << endl; }

    private:
        StreamServiceProxyBase* m_streamProxy;
    };
}

#endif /* DUMMYPLUGIN_H */
