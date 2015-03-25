#ifndef DUMMYPLUGIN_H
#define DUMMYPLUGIN_H

#include "PluginBase.h"
#include <SenseKitSL.h>

    namespace sensekit {

        class DummyPlugin : public PluginBase
        {
        public:
            DummyPlugin(StreamServiceProxy* streamProxy, PluginServiceProxy* pluginService)
                : PluginBase(streamProxy, pluginService) { }

            virtual void temp_update() override { }

        private:
            StreamServiceProxyBase* m_streamProxy;
        };
    }


#endif /* DUMMYPLUGIN_H */
