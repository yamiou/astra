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
        DummyPlugin(PluginServiceProxy* pluginService)
            : PluginBase(pluginService) { }

        virtual void temp_update() override { cout << "dumb." << endl; }
    };
}

EXPORT_PLUGIN(sensekit::DummyPlugin);

#endif /* DUMMYPLUGIN_H */
