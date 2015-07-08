#ifndef PLAYBACKSTREAMSET_H
#define PLAYBACKSTREAMSET_H

#include <SenseKit/Plugins/plugin_capi.h>
#include <memory>
#include <vector>
#include <string>
#include "PlaybackStream.h"
#include "DepthStream.h"
#include <common/serialization/FrameStreamReader.h>
#include <common/serialization/FrameInputStream.h>

namespace sensekit { namespace plugins { namespace streamplayer {
    
    const char STREAMPLAYERPLUGIN_FILE_PATH[] = "plugins/test.df";

    class PlaybackStreamSetBase
    {
    public:
        PlaybackStreamSetBase(PluginServiceProxy& pluginService, std::string uri)
            : m_pluginService(pluginService),
              m_uri(uri)
        {
            m_pluginService.create_stream_set(uri.c_str(), m_streamSetHandle);
        }
        virtual ~PlaybackStreamSetBase()
        {
            m_pluginService.destroy_stream_set(m_streamSetHandle);
        }

        virtual sensekit_status_t open() = 0;
        virtual sensekit_status_t close() = 0;
        virtual sensekit_status_t read() = 0;
    protected:
        PluginServiceProxy& m_pluginService;
        sensekit_streamset_t m_streamSetHandle;
        std::string m_uri;

        using StreamPtr = std::unique_ptr<PlaybackStreamBase>;
        using StreamPtrList = std::vector<StreamPtr>;

        StreamPtrList m_streams;        
    };

    class PlaybackStreamSet : public PlaybackStreamSetBase
    {
    public:
        PlaybackStreamSet(PluginServiceProxy& pluginService, std::string uri) :
            PlaybackStreamSetBase(pluginService, uri)
        {
            
        }

        virtual ~PlaybackStreamSet()
        {
            close();
        }

        virtual sensekit_status_t open() override
        {
            if (m_isOpen)
            {
                return SENSEKIT_STATUS_SUCCESS;
            }

            try
            {
                m_frameStream = std::unique_ptr<FrameInputStream>(open_frame_input_stream(STREAMPLAYERPLUGIN_FILE_PATH));
                m_frameStreamReader = std::make_unique<FrameStreamReader>(m_frameStream.get());

                open_stream();

                m_isOpen = true;
            }
            catch (ResourceNotFoundException)
            {
                return SENSEKIT_STATUS_DEVICE_ERROR;
            }

            return SENSEKIT_STATUS_SUCCESS;
        }

        virtual sensekit_status_t close() override final
        {
            if (!m_isOpen)
            {
                return SENSEKIT_STATUS_SUCCESS;
            }

            for (StreamPtr& s : m_streams)
            {
                s->close();
            }

            m_streams.clear();

            m_isOpen = false;

            return SENSEKIT_STATUS_SUCCESS;
        }

        virtual sensekit_status_t read() override
        {
            if (!m_isOpen || m_streams.size() == 0)
            {
                return SENSEKIT_STATUS_SUCCESS;
            }

            for (StreamPtr& s : m_streams)
            {
                s->read();
            }

            return SENSEKIT_STATUS_SUCCESS;
        }

    private:
        sensekit_status_t open_stream()
        {
            switch (m_frameStreamReader->get_stream_type())
            {
                case SENSEKIT_STREAM_DEPTH:
                {
                    DepthStream* stream = new DepthStream(*m_frameStreamReader, m_pluginService, m_streamSetHandle);
                    stream->open();
                    m_streams.push_back(StreamPtr(stream));
                    break;
                }
                default:
                {
                    return SENSEKIT_STATUS_DEVICE_ERROR;
                }
            }

            return SENSEKIT_STATUS_SUCCESS;
        }


        bool m_isOpen{ false };

        std::unique_ptr<FrameStreamReader> m_frameStreamReader;
        std::unique_ptr<FrameInputStream> m_frameStream;
    };
}}}

#endif /* PLAYBACKSTREAMSET_H */