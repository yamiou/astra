#ifndef STREAMPLAYERPLUGIN_H
#define STREAMPLAYERPLUGIN_H

#include <SenseKit/Plugins/PluginKit.h>
#include "PlaybackStreamSet.h"

namespace sensekit { namespace plugins { namespace streamplayer {

    class StreamPlayerPlugin : public PluginBase
    {
    public:
        StreamPlayerPlugin(PluginServiceProxy* pluginProxy);

        virtual ~StreamPlayerPlugin();
        virtual void temp_update() override;
    private:
        void create_streamset();

        sensekit_status_t read_streams();

        using SetPtr = std::unique_ptr<PlaybackStreamSetBase>;
        using SetList = std::vector<SetPtr>;

        SetList m_sets;
    };
    
}}}


#endif /* STREAMPLAYERPLUGIN_H */
