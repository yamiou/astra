#ifndef PLAYBACKSTREAM_H
#define PLAYBACKSTREAM_H

#include <SenseKit/Plugins/Stream.h>
#include <SenseKit/Plugins/StreamBin.h>
#include <SenseKit/Plugins/plugin_capi.h>
#include <SenseKitUL/streams/image_parameters.h>
#include <common/serialization/FrameStreamReader.h>

using namespace sensekit::serialization;

namespace sensekit { namespace plugins { namespace streamplayer {

    class PlaybackStreamBase : public Stream
    {
    public:
        PlaybackStreamBase(PluginServiceProxy& pluginService,
                            sensekit_streamset_t streamSet,
                            StreamDescription desc)
                            : Stream(pluginService,
                            streamSet,
                            desc) { }

        virtual ~PlaybackStreamBase() { };

        virtual sensekit_status_t read() = 0;
        virtual sensekit_status_t open() = 0;
        virtual sensekit_status_t close() = 0;
        virtual sensekit_status_t start() = 0;
        virtual sensekit_status_t stop() = 0;
    };

    template<typename TFrameWrapper>
    class PlaybackStream : public PlaybackStreamBase
    {
    public:
        using wrapper_type = TFrameWrapper;

        PlaybackStream(FrameStreamReader& depthStreamParser,
                        PluginServiceProxy& pluginService,
                        sensekit_streamset_t streamSet,
                        StreamDescription desc);

        virtual ~PlaybackStream();

        virtual sensekit_status_t close() override final;
        virtual sensekit_status_t start() override { return SENSEKIT_STATUS_SUCCESS; }
        virtual sensekit_status_t stop() override { return SENSEKIT_STATUS_SUCCESS; }
        virtual sensekit_status_t open() override;
        virtual sensekit_status_t read() override;

        bool is_streaming();

        void on_connection_added(sensekit_streamconnection_t connection) override;
        void on_connection_removed(sensekit_bin_t bin,
                                    sensekit_streamconnection_t connection) override;

        virtual void on_get_parameter(sensekit_streamconnection_t connection,
                                        sensekit_parameter_id id,
                                        sensekit_parameter_bin_t& parameterBin) override;

    protected:
        virtual sensekit_status_t on_open()
        {
            return SENSEKIT_STATUS_SUCCESS;
        }

        FrameStreamReader& m_frameStreamReader;

    private:
        bool m_isOpen{ false };
        bool m_isStreaming{ true };

        int m_frameIndex{ 0 };

        using BinType = StreamBin<wrapper_type>;
        std::unique_ptr<BinType> m_bin;
    };

    template<typename TFrameWrapper>
    PlaybackStream<TFrameWrapper>::PlaybackStream(FrameStreamReader& frameStreamReader,
                                                    PluginServiceProxy& pluginService,
                                                    sensekit_streamset_t streamSet,
                                                    StreamDescription desc) :
                                                    PlaybackStreamBase(pluginService,
                                                    streamSet,
                                                    desc),
                                                    m_frameStreamReader(frameStreamReader)
    {

    }

    template<typename TFrameWrapper>
    PlaybackStream<TFrameWrapper>::~PlaybackStream()
    {
        close();
    }

    template<typename TFrameWrapper>
    sensekit_status_t PlaybackStream<TFrameWrapper>::open()
    {
        if (m_isOpen)
        {
            return SENSEKIT_STATUS_SUCCESS;
        }

        m_bin = std::make_unique<StreamBin<wrapper_type> >(
            get_pluginService(),
            get_handle(),
            m_frameStreamReader.get_buffer_length() - sizeof(wrapper_type));

        on_open();

        m_isOpen = true;

        enable_callbacks();

        return SENSEKIT_STATUS_SUCCESS;
    }

    template<typename TFrameWrapper>
    sensekit_status_t PlaybackStream<TFrameWrapper>::close()
    {
        if (!m_isOpen)
            return SENSEKIT_STATUS_SUCCESS;

        m_isOpen = m_isStreaming = false;

        return SENSEKIT_STATUS_SUCCESS;
    }

    template<typename TFrameWrapper>
    bool PlaybackStream<TFrameWrapper>::is_streaming()
    {
        return m_isOpen && m_isStreaming;
    }

    template<typename TFrameWrapper>
    void PlaybackStream<TFrameWrapper>::on_connection_added(
        sensekit_streamconnection_t connection)
    {
        m_bin->link_connection(connection);
    }

    template<typename TFrameWrapper>
    void PlaybackStream<TFrameWrapper>::on_connection_removed(
        sensekit_bin_t bin, sensekit_streamconnection_t connection)
    {
        m_bin->unlink_connection(connection);

        if (!m_bin->has_connections())
        {
            m_bin = nullptr;
        }
    }

    template<typename TFrameWrapper>
    void PlaybackStream<TFrameWrapper>::on_get_parameter(sensekit_streamconnection_t connection,
                                                            sensekit_parameter_id id,
                                                            sensekit_parameter_bin_t& parameterBin)
    {
        switch (id)
        {
            case SENSEKIT_PARAMETER_IMAGE_HFOV:
            {
                size_t resultByteLength = sizeof(float);

                sensekit_parameter_data_t parameterData;
                sensekit_status_t rc = get_pluginService().get_parameter_bin(resultByteLength,
                                                                                &parameterBin,
                                                                                &parameterData);
                if (rc == SENSEKIT_STATUS_SUCCESS)
                {
                    float* hFov = reinterpret_cast<float*>(parameterData);
                    *hFov = 1.02259994;
                }
                break;
            }
            case SENSEKIT_PARAMETER_IMAGE_VFOV:
            {
                size_t resultByteLength = sizeof(float);

                sensekit_parameter_data_t parameterData;
                sensekit_status_t rc = get_pluginService().get_parameter_bin(resultByteLength,
                                                                                &parameterBin,
                                                                                &parameterData);
                if (rc == SENSEKIT_STATUS_SUCCESS)
                {
                    float* vFov = reinterpret_cast<float*>(parameterData);
                    *vFov = 0.796615660;
                }
                break;
            }
        }
    }

    template<typename TFrameWrapper>
    sensekit_status_t PlaybackStream<TFrameWrapper>::read()
    {
        if (!is_streaming()) return SENSEKIT_STATUS_SUCCESS;

        if (!m_frameStreamReader.read())
        {
            return SENSEKIT_STATUS_SUCCESS;
        }

        sensekit::serialization::Frame& decodedFrame = m_frameStreamReader.peek();

        auto framePair = m_bin->begin_write_ex(m_frameIndex);

        wrapper_type* frameWrapper = framePair.second;
        sensekit_frame_t* frame = framePair.first;

        memcpy(frame->data, decodedFrame.rawFrameWrapper, decodedFrame.byteLength);
        frameWrapper->frame.data = &(frameWrapper->frame_data);
        frameWrapper->frame.frame = frame;

        m_bin->end_write();

        ++m_frameIndex;

        return SENSEKIT_STATUS_SUCCESS;
    }

}}}

#endif /* PLAYBACKSTREAM_H */