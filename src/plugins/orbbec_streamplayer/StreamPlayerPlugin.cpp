#include "StreamPlayerPlugin.h"
#include <astra_core/astra_core.hpp>
#include "PlaybackStreamSet.h"

EXPORT_PLUGIN(astra::plugins::streamplayer::StreamPlayerPlugin)

namespace astra { namespace plugins { namespace streamplayer {

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
        PlaybackStreamSetBase* set = new PlaybackStreamSet(pluginService(), "stream_player");
        set->open();

        m_sets.push_back(SetPtr(set));
    }

    void StreamPlayerPlugin::temp_update()
    {
        read_streams();
    }

    astra_status_t StreamPlayerPlugin::read_streams()
    {
        for (auto& set : m_sets)
        {
            set->read();
        }

        return ASTRA_STATUS_SUCCESS;
    }

}}}
