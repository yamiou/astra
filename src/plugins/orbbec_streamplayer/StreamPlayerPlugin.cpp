#include "StreamPlayerPlugin.h"
#include <SenseKit/SenseKit.h>
#include "PlaybackStreamSet.h"

EXPORT_PLUGIN(sensekit::plugins::streamplayer::StreamPlayerPlugin)

namespace sensekit { namespace plugins { namespace streamplayer {

    StreamPlayerPlugin::StreamPlayerPlugin(PluginServiceProxy* pluginService)
        : PluginBase(pluginService, "orbbec_streamplayer")
    {
        create_streamset();
    }

    StreamPlayerPlugin::~StreamPlayerPlugin()
    {
        m_sets.clear();
    }

    void StreamPlayerPlugin::create_streamset()
    {
        PlaybackStreamSetBase* set = new PlaybackStreamSet(get_pluginService(), "stream_player");
        set->open();

        m_sets.push_back(SetPtr(set));
    }

    void StreamPlayerPlugin::temp_update()
    {
        read_streams();
    }

    sensekit_status_t StreamPlayerPlugin::read_streams()
    {
        for (auto& set : m_sets)
        {
            set->read();
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

}}}
