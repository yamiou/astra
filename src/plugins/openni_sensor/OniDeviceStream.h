#ifndef ONIDEVICESTREAM_H
#define ONIDEVICESTREAM_H

#include <Astra/Astra.h>
#include <Astra/Plugins/plugin_capi.h>
#include <Astra/Plugins/Stream.h>
#include <Astra/Plugins/StreamBin.h>
#include <AstraUL/streams/image_parameters.h>
#include <OpenNI.h>
#include <AstraUL/streams/image_types.h>
#include <memory>
#include <Shiny.h>
#include <cstring>
#include <vector>
#include "oni_mappers.hpp"

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif

namespace astra { namespace plugins {

    class OniDeviceStreamBase : public Stream
    {
    public:
        OniDeviceStreamBase(PluginServiceProxy& pluginService,
                            astra_streamset_t streamSet,
                            StreamDescription desc)
            : Stream(pluginService,
                     streamSet,
                     desc)
        {
            PROFILE_FUNC();
        }

        virtual astra_status_t read_frame() = 0;
        virtual astra_status_t open() = 0;
        virtual astra_status_t close() = 0;
        virtual openni::VideoStream* get_oni_stream() = 0;
        virtual astra_status_t start() = 0;
        virtual astra_status_t stop() = 0;
    };

    template<typename TFrameWrapper, typename TBufferBlockType>
    class OniDeviceStream : public OniDeviceStreamBase
    {
    public:
        using wrapper_type = TFrameWrapper;
        using block_type = TBufferBlockType;

        OniDeviceStream(PluginServiceProxy& pluginService,
                        astra_streamset_t streamSet,
                        StreamDescription desc,
                        openni::Device& oniDevice,
                        openni::SensorType oniSensorType)
            : OniDeviceStreamBase(pluginService,
                                  streamSet,
                                  desc),
              m_oniDevice(oniDevice),
              m_oniSensorType(oniSensorType)
        {
            PROFILE_FUNC();
        }

        virtual ~OniDeviceStream()
        {
            PROFILE_FUNC();
            close();
        }

        virtual astra_status_t open() override final
        {
            PROFILE_FUNC();
            if (m_isOpen)
                return ASTRA_STATUS_SUCCESS;

            SINFO("OniDeviceStream", "creating oni stream of type: %d", get_description().get_type());
            openni::Status rc = m_oniStream.create(m_oniDevice, m_oniSensorType);

            if (rc != openni::STATUS_OK)
            {
                return ASTRA_STATUS_DEVICE_ERROR;
            }

            SINFO("OniDeviceStream", "created oni stream of type: %d", get_description().get_type());

            const openni::SensorInfo& pInfo = m_oniStream.getSensorInfo();
            auto& modes = pInfo.getSupportedVideoModes();

            SINFO("OniDeviceStream", "stream type %d supports modes:", get_description().get_type());

            for(int i = 0; i < modes.getSize(); i++)
            {
                const openni::VideoMode& oniMode = modes[i];

                if (std::get<0>(convert_format(oniMode.getPixelFormat())) != 0)
                {
                    astra_imagestream_mode_t mode = convert_mode(oniMode);
                    mode.id = i+1;
                    m_modes.push_back(mode);
                }

                SINFO("OniDeviceStream", "- w: %d h: %d fps: %d pf: %d",
                      oniMode.getResolutionX(),
                      oniMode.getResolutionY(),
                      oniMode.getFps(),
                      oniMode.getPixelFormat());
            }

            m_oniVideoMode = m_oniStream.getVideoMode();
            m_mode = convert_mode(m_oniVideoMode);

            SINFO("OniDeviceStream", "Selected mode: w: %d h: %d fps: %d pf: %d",
                  m_mode.width,
                  m_mode.height,
                  m_mode.fps,
                  m_mode.pixelFormat);

            assert(m_mode.pixelFormat != 0);

            m_bufferLength =
                m_mode.width *
                m_mode.height *
                m_mode.bytesPerPixel;

            m_bin = std::make_unique<StreamBin<wrapper_type> >(get_pluginService(),
                                                               get_handle(),
                                                               m_bufferLength);

            on_open();

            m_isOpen = true;

            enable_callbacks();
            return ASTRA_STATUS_SUCCESS;
        }

        virtual astra_status_t close() override final
        {
            PROFILE_FUNC();
            if (!m_isOpen)
                return ASTRA_STATUS_SUCCESS;

            stop();

            on_close();

            SINFO("OniDeviceStream", "destroying oni stream of type: %d", get_description().get_type());
            m_oniStream.destroy();

            m_isOpen = m_isStreaming = false;

            return ASTRA_STATUS_SUCCESS;
        }

        virtual astra_status_t start() override final
        {
            PROFILE_FUNC();
            if (!m_isOpen || m_isStreaming)
                return ASTRA_STATUS_SUCCESS;

            SINFO("OniDeviceStream", "starting oni stream of type: %d", get_description().get_type());
            m_oniStream.start();
            SINFO("OniDeviceStream", "started oni stream of type: %d", get_description().get_type());

            m_isStreaming = true;

            return ASTRA_STATUS_SUCCESS;
        }

        virtual astra_status_t stop() override final
        {
            PROFILE_FUNC();
            if (!m_isOpen || !m_isStreaming)
                return ASTRA_STATUS_SUCCESS;

            SINFO("OniDeviceStream", "stopping oni stream of type: %d", get_description().get_type());
            m_oniStream.stop();
            SINFO("OniDeviceStream", "stopped oni stream of type: %d", get_description().get_type());

            m_isStreaming = false;

            return ASTRA_STATUS_SUCCESS;
        }

        inline bool is_streaming() const { return m_isOpen && m_isStreaming; }

        virtual void on_get_parameter(astra_streamconnection_t connection,
                                      astra_parameter_id id,
                                      astra_parameter_bin_t& parameterBin) override
        {
            PROFILE_FUNC();

            switch (id)
            {
            case ASTRA_PARAMETER_IMAGE_HFOV:
            {
                size_t resultByteLength = sizeof(float);

                astra_parameter_data_t parameterData;
                astra_status_t rc = get_pluginService().get_parameter_bin(resultByteLength,
                                                                             &parameterBin,
                                                                             &parameterData);
                if (rc == ASTRA_STATUS_SUCCESS)
                {
                    float* hFov = reinterpret_cast<float*>(parameterData);
                    *hFov = m_oniStream.getHorizontalFieldOfView();
                }
                break;
            }
            case ASTRA_PARAMETER_IMAGE_VFOV:
            {
                size_t resultByteLength = sizeof(float);

                astra_parameter_data_t parameterData;
                astra_status_t rc = get_pluginService().get_parameter_bin(resultByteLength,
                                                                             &parameterBin,
                                                                             &parameterData);
                if (rc == ASTRA_STATUS_SUCCESS)
                {
                    float* vFov = reinterpret_cast<float*>(parameterData);
                    *vFov = m_oniStream.getVerticalFieldOfView();
                }
                break;
            }
            case ASTRA_PARAMETER_IMAGE_MIRRORING:
            {
                size_t resultByteLength = sizeof(bool);

                astra_parameter_data_t parameterData;
                astra_status_t rc = get_pluginService().get_parameter_bin(resultByteLength,
                                                                             &parameterBin,
                                                                             &parameterData);
                if (rc == ASTRA_STATUS_SUCCESS)
                {
                    bool mirroring = m_oniStream.getMirroringEnabled();
                    *reinterpret_cast<bool*>(parameterData) = mirroring;
                }

                break;
            }
            case ASTRA_PARAMETER_IMAGE_MODES:
            {
                std::size_t resultSize = sizeof(astra_imagestream_mode_t) * m_modes.size();

                astra_parameter_data_t parameterData;
                astra_status_t rc = get_pluginService().get_parameter_bin(resultSize,
                                                                             &parameterBin,
                                                                             &parameterData);

                astra_imagestream_mode_t* result = static_cast<astra_imagestream_mode_t*>(parameterData);

                if (rc == ASTRA_STATUS_SUCCESS)
                {
                    for(int i = 0; i < m_modes.size(); i++)
                    {
                        result[i] = m_modes[i];
                    }
                }

                break;
            }
            }
        }

        void on_set_parameter(astra_streamconnection_t connection,
                              astra_parameter_id id,
                              size_t inByteLength,
                              astra_parameter_data_t inData) override
        {
            switch (id)
            {
            case ASTRA_PARAMETER_IMAGE_MIRRORING:
            {
                bool enable = *static_cast<bool*>(inData);
                m_oniStream.setMirroringEnabled(enable);
                break;
            }
            case ASTRA_PARAMETER_IMAGE_MODE:
                astra_imagestream_mode_t* mode = static_cast<astra_imagestream_mode_t*>(inData);
                auto oniMode = convert_mode(*mode);
                auto rc = m_oniStream.setVideoMode(oniMode);

                if (rc == ::openni::STATUS_OK)
                {
                    SINFO("OniDeviceStream", "stream mode changed");
                    m_oniVideoMode = oniMode;
                    m_mode = *mode;
                }
                break;
            }
        }

        virtual void on_new_buffer(wrapper_type* wrapper)
        {
            PROFILE_FUNC();
            if (wrapper == nullptr)
                return;

            auto& md = wrapper->frame.metadata;

            md.pixelFormat = m_mode.pixelFormat;
            md.width = m_mode.width;
            md.height = m_mode.height;
            md.bytesPerPixel = m_mode.bytesPerPixel;
        }

        virtual astra_status_t read_frame() override;

        virtual openni::VideoStream* get_oni_stream() override { return &m_oniStream; }

        virtual void on_connection_added(astra_streamconnection_t connection) override;
        virtual void on_connection_removed(astra_bin_t bin,
                                           astra_streamconnection_t connection) override;

    protected:
        openni::Device& m_oniDevice;
        openni::SensorType m_oniSensorType;
        openni::VideoStream m_oniStream;
        openni::VideoMode m_oniVideoMode;
        astra_imagestream_mode_t m_mode;

    private:
        virtual void on_open() {}
        virtual void on_close() {}

        bool m_isOpen{false};
        bool m_isStreaming{false};

        using BinType = StreamBin<wrapper_type>;
        std::unique_ptr<BinType> m_bin;

        size_t m_bufferLength{0};

        astra_stream_t m_streamHandle{nullptr};
        astra_frame_index_t m_frameIndex{0};

        std::vector<astra_imagestream_mode_t> m_modes;
    };

    template<typename TFrameWrapper, typename TBufferBlockType>
    void OniDeviceStream<TFrameWrapper,
                         TBufferBlockType>::on_connection_added(astra_streamconnection_t connection)
    {
        PROFILE_FUNC();
        assert(m_bin != nullptr);
        m_bin->link_connection(connection);
    }

    template<typename TFrameWrapper, typename TBufferBlockType>
    void OniDeviceStream<TFrameWrapper,
                         TBufferBlockType>::on_connection_removed(astra_bin_t bin,
                                                                  astra_streamconnection_t connection)
    {
        PROFILE_FUNC();
        m_bin->unlink_connection(connection);

        if (!m_bin->has_connections())
        {
            m_bin = nullptr;
        }
    }

    template<typename TFrameWrapper, typename TBufferBlockType>
    astra_status_t OniDeviceStream<TFrameWrapper, TBufferBlockType>::read_frame()
    {
        PROFILE_FUNC();
        if (!is_streaming()) return ASTRA_STATUS_SUCCESS;

        openni::VideoFrameRef ref;
        PROFILE_BEGIN(oni_stream_readFrame);
        auto status = m_oniStream.readFrame(&ref);
        PROFILE_END();

        if (status == ::openni::STATUS_OK)
        {
            const block_type* oniFrameData = static_cast<const block_type*>(ref.getData());

            size_t byteSize = MIN(ref.getDataSize(), m_bufferLength);

            wrapper_type* wrapper = m_bin->begin_write(m_frameIndex);

            wrapper->frame.frame = nullptr;
            wrapper->frame.data =
                reinterpret_cast<block_type*>(&(wrapper->frame_data));

            on_new_buffer(wrapper);

            std::memcpy(wrapper->frame.data, oniFrameData, byteSize);

            PROFILE_BEGIN(oni_stream_end_write);
            m_bin->end_write();
            PROFILE_END();

            ++m_frameIndex;
        }

        return ASTRA_STATUS_SUCCESS;
    }
}}

#endif /* ONIDEVICESTREAM_H */
