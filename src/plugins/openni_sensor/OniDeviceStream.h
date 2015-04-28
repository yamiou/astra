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
                     desc) { }

        virtual void read_frame() = 0;
        virtual ::openni::VideoStream* get_oni_stream() = 0;
        virtual void start() = 0;
        virtual void stop() = 0;
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
                        ::openni::Device& oniDevice,
                        ::openni::SensorType oniSensorType,
                        size_t numComponentPerPixel)
            : OniDeviceStreamBase(pluginService,
                                  streamSet,
                                  desc),
              m_oniDevice(oniDevice),
              m_numComponentPerPixel(numComponentPerPixel),
              m_bytesPerPixel(sizeof(block_type) * numComponentPerPixel)
        {
            m_oniStream.create(m_oniDevice, oniSensorType);
            m_oniVideoMode = m_oniStream.getVideoMode();
            m_bufferLength =
                m_oniVideoMode.getResolutionX() *
                m_oniVideoMode.getResolutionY() *
                m_bytesPerPixel;

            m_bin = std::make_unique<StreamBin<wrapper_type> >(get_pluginService(),
                                                               get_handle(),
                                                               m_bufferLength);
        }

        virtual ~OniDeviceStream()
        {
            stop();
            get_logger().info("destroying oni stream of type: %d", get_description().get_type());
            m_oniStream.destroy();
        }

        virtual void start() override final
        {
            get_logger().info("starting oni stream of type: %d", get_description().get_type());
            m_oniStream.start();
        }

        virtual void stop() override final
        {
            get_logger().info("stopping oni stream of type: %d", get_description().get_type());
            m_oniStream.stop();
        }

        virtual void on_connection_added(sensekit_streamconnection_t connection) override;

        virtual void on_connection_removed(sensekit_bin_t bin,
                                           sensekit_streamconnection_t connection) override;

        virtual void on_get_parameter(sensekit_streamconnection_t connection,
                                      sensekit_parameter_id id,
                                      sensekit_parameter_bin_t& parameterBin) override
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
            if (wrapper == nullptr)
                return;

            sensekit_image_metadata_t metadata;

            metadata.width = m_oniVideoMode.getResolutionX();
            metadata.height = m_oniVideoMode.getResolutionY();
            metadata.bytesPerPixel = m_bytesPerPixel;

            wrapper->frame.metadata = metadata;
        }

        virtual void read_frame() override;

        virtual ::openni::VideoStream* get_oni_stream() override { return &m_oniStream; }


    protected:
        ::openni::Device& m_oniDevice;
        ::openni::VideoStream m_oniStream;
        ::openni::VideoMode m_oniVideoMode;

    private:
        std::unique_ptr<StreamBin<wrapper_type> > m_bin;

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
        m_bin->link_connection(connection);
    }

    template<typename TFrameWrapper, typename TBufferBlockType>
    void OniDeviceStream<TFrameWrapper,
                         TBufferBlockType>::on_connection_removed(sensekit_bin_t bin,
                                                                  sensekit_streamconnection_t connection)
    {
        m_bin->unlink_connection(connection);

        if (!m_bin->has_connections())
        {
            m_bin = nullptr;
        }
    }

    template<typename TFrameWrapper, typename TBufferBlockType>
    void OniDeviceStream<TFrameWrapper, TBufferBlockType>::read_frame()
    {
        openni::VideoFrameRef ref;
        if (m_oniStream.readFrame(&ref) == ::openni::STATUS_OK)
        {
            const block_type* oniFrameData = static_cast<const block_type*>(ref.getData());

            size_t byteSize = MIN(ref.getDataSize(), m_bufferLength);

            wrapper_type* wrapper = m_bin->begin_write(m_frameIndex);

            wrapper->frame.data =
                reinterpret_cast<block_type*>(&(wrapper->frame_data));

            on_new_buffer(wrapper);

            memcpy(wrapper->frame.data, oniFrameData, byteSize);

            m_bin->end_write();

            ++m_frameIndex;
        }
    }
}}

#endif /* ONIDEVICESTREAM_H */
