#ifndef ONIDEVICESTREAM_H
#define ONIDEVICESTREAM_H

#include <SenseKit/SenseKit.h>
#include <SenseKit/Plugins/plugin_capi.h>
#include <SenseKit/Plugins/Stream.h>
#include <SenseKit/Plugins/StreamBin.h>
#include <SenseKitUL/streams/image_parameters.h>
#include <OpenNI.h>
#include <SenseKitUL/streams/image_types.h>
#include <memory>
#include <Shiny.h>

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif

namespace sensekit { namespace plugins {

    class OniDeviceStreamBase : public Stream
    {
    public:
        OniDeviceStreamBase(PluginServiceProxy& pluginService,
                            Sensor streamSet,
                            StreamDescription desc)
            : Stream(pluginService,
                     streamSet,
                     desc)
        {
             PROFILE_FUNC();
        }

        virtual sensekit_status_t read_frame() = 0;
        virtual sensekit_status_t open() = 0;
        virtual sensekit_status_t close() = 0;
        virtual openni::VideoStream* get_oni_stream() = 0;
        virtual sensekit_status_t start() = 0;
        virtual sensekit_status_t stop() = 0;
    };

    template<typename TFrameWrapper, typename TBufferBlockType>
    class OniDeviceStream : public OniDeviceStreamBase
    {
    public:
        using wrapper_type = TFrameWrapper;
        using block_type = TBufferBlockType;

        OniDeviceStream(PluginServiceProxy& pluginService,
                        Sensor streamSet,
                        StreamDescription desc,
                        openni::Device& oniDevice,
                        openni::SensorType oniSensorType,
                        size_t numComponentPerPixel)
            : OniDeviceStreamBase(pluginService,
                                  streamSet,
                                  desc),
              m_oniDevice(oniDevice),
              m_oniSensorType(oniSensorType),
              m_bytesPerPixel(sizeof(block_type) * numComponentPerPixel),
              m_numComponentPerPixel(numComponentPerPixel)
        {
            PROFILE_FUNC();
            enable_callbacks();
        }

        virtual ~OniDeviceStream()
        {
            PROFILE_FUNC();
            close();
        }

        virtual sensekit_status_t open() override final
        {
            PROFILE_FUNC();
            if (m_isOpen)
                return SENSEKIT_STATUS_SUCCESS;

            get_logger().info("creating oni stream of type: %d", get_description().get_type());
            openni::Status rc = m_oniStream.create(m_oniDevice, m_oniSensorType);

            if (rc != openni::STATUS_OK)
            {
                return SENSEKIT_STATUS_DEVICE_ERROR;
            }

            get_logger().info("created oni stream of type: %d", get_description().get_type());

            m_oniVideoMode = m_oniStream.getVideoMode();

            m_bufferLength =
                m_oniVideoMode.getResolutionX() *
                m_oniVideoMode.getResolutionY() *
                m_bytesPerPixel;

            m_bin = std::make_unique<StreamBin<wrapper_type> >(get_pluginService(),
                                                               get_handle(),
                                                               m_bufferLength);

            on_open();

            m_isOpen = true;

            return SENSEKIT_STATUS_SUCCESS;
        }

        virtual sensekit_status_t close() override final
        {
            PROFILE_FUNC();
            if (!m_isOpen)
                return SENSEKIT_STATUS_SUCCESS;

            stop();

            on_close();

            get_logger().info("destroying oni stream of type: %d", get_description().get_type());
            m_oniStream.destroy();

            m_isOpen = m_isStreaming = false;

            return SENSEKIT_STATUS_SUCCESS;
        }

        virtual sensekit_status_t start() override final
        {
            PROFILE_FUNC();
            if (!m_isOpen || m_isStreaming)
                return SENSEKIT_STATUS_SUCCESS;

            get_logger().info("starting oni stream of type: %d", get_description().get_type());
            m_oniStream.start();
            get_logger().info("started oni stream of type: %d", get_description().get_type());

            m_isStreaming = true;

            return SENSEKIT_STATUS_SUCCESS;
        }

        virtual sensekit_status_t stop() override final
        {
            PROFILE_FUNC();
            if (!m_isOpen || !m_isStreaming)
                return SENSEKIT_STATUS_SUCCESS;

            get_logger().info("stopping oni stream of type: %d", get_description().get_type());
            m_oniStream.stop();
            get_logger().info("stopped oni stream of type: %d", get_description().get_type());

            m_isStreaming = false;

            return SENSEKIT_STATUS_SUCCESS;
        }

        inline bool is_streaming() const { return m_isOpen && m_isStreaming; }

        virtual void on_get_parameter(sensekit_streamconnection_t connection,
                                      sensekit_parameter_id id,
                                      sensekit_parameter_bin_t& parameterBin) override
        {
            PROFILE_FUNC();
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
                    *hFov = m_oniStream.getHorizontalFieldOfView();
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
                    *vFov = m_oniStream.getVerticalFieldOfView();
                }
                break;
            }
            }
        }

        virtual void on_new_buffer(wrapper_type* wrapper)
        {
            PROFILE_FUNC();
            if (wrapper == nullptr)
                return;

            sensekit_image_metadata_t metadata;

            metadata.width = m_oniVideoMode.getResolutionX();
            metadata.height = m_oniVideoMode.getResolutionY();
            metadata.bytesPerPixel = m_bytesPerPixel;

            wrapper->frame.metadata = metadata;
        }

        virtual sensekit_status_t read_frame() override;

        virtual openni::VideoStream* get_oni_stream() override { return &m_oniStream; }

        virtual void on_connection_added(sensekit_streamconnection_t connection) override;
        virtual void on_connection_removed(sensekit_bin_t bin,
                                   sensekit_streamconnection_t connection) override;

    protected:
        openni::Device& m_oniDevice;
        openni::SensorType m_oniSensorType;
        openni::VideoStream m_oniStream;
        openni::VideoMode m_oniVideoMode;

    private:
        virtual void on_open() {}
        virtual void on_close() {}

        bool m_isOpen{false};
        bool m_isStreaming{false};

        using BinType = StreamBin<wrapper_type>;
        std::unique_ptr<BinType> m_bin;

        size_t m_bufferLength{0};
        size_t m_bytesPerPixel{0};
        size_t m_numComponentPerPixel{0};

        sensekit_stream_t m_streamHandle{nullptr};
        sensekit_frame_index_t m_frameIndex{0};
    };

    template<typename TFrameWrapper, typename TBufferBlockType>
    void OniDeviceStream<TFrameWrapper,
                         TBufferBlockType>::on_connection_added(sensekit_streamconnection_t connection)
    {
        PROFILE_FUNC();
        m_bin->link_connection(connection);
    }

    template<typename TFrameWrapper, typename TBufferBlockType>
    void OniDeviceStream<TFrameWrapper,
                         TBufferBlockType>::on_connection_removed(sensekit_bin_t bin,
                                                                  sensekit_streamconnection_t connection)
    {
        PROFILE_FUNC();
        m_bin->unlink_connection(connection);

        if (!m_bin->has_connections())
        {
            m_bin = nullptr;
        }
    }

    template<typename TFrameWrapper, typename TBufferBlockType>
    sensekit_status_t OniDeviceStream<TFrameWrapper, TBufferBlockType>::read_frame()
    {
        PROFILE_FUNC();
        if (!is_streaming()) return SENSEKIT_STATUS_SUCCESS;

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

            memcpy(wrapper->frame.data, oniFrameData, byteSize);

            PROFILE_BEGIN(oni_stream_end_write);
            m_bin->end_write();
            PROFILE_END();

            ++m_frameIndex;
        }

        return SENSEKIT_STATUS_SUCCESS;
    }
}}

#endif /* ONIDEVICESTREAM_H */
