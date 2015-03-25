#ifndef DUMMYPLUGIN_H
#define DUMMYPLUGIN_H

#include "PluginBase.h"

    namespace sensekit {

        class DummyPlugin : public PluginBase
        {
        public:
            DummyPlugin();
            virtual ~DummyPlugin();
        };
    }


#endif /* DUMMYPLUGIN_H */
