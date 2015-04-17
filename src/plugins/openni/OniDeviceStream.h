#ifndef ONIDEVICESTREAM_H
#define ONIDEVICESTREAM_H

#include <SenseKit/SenseKit.h>
#include <SenseKit/Plugins/plugin_capi.h>
#include <SenseKit/Plugins/Stream.h>
#include <SenseKitUL/streams/video_parameters.h>
#include <OpenNI.h>
#include <SenseKitUL/streams/image_types.h>

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
                            Sensor& streamSet,
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
                        Sensor& streamSet,
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
        }

        virtual ~OniDeviceStream()
        {
            stop();
            get_logger().info("destroying oni stream of type: %d", get_description().get_type());
            m_oniStream.destroy();
        }

        virtual void start() override final
        {
            get_logger().info("starting stream of type: %d", get_description().get_type());
            m_oniStream.start();
        }

        virtual void stop() override final
        {
            get_logger().info("stop stream of type: %d", get_description().get_type());
            m_oniStream.stop();
        }

        virtual void set_new_buffer(sensekit_frame_t* newBuffer);

        virtual void on_connection_added(sensekit_streamconnection_t connection) override;

        virtual void on_connection_removed(sensekit_bin_t bin,
                                           sensekit_streamconnection_t connection) override;

        virtual void set_parameter(sensekit_streamconnection_t connection,
                                   sensekit_parameter_id id,
                                   size_t byteLength,
                                   sensekit_parameter_data_t data) override
        {

        }

        virtual void get_parameter(sensekit_streamconnection_t connection,
                                   sensekit_parameter_id id,
                                   sensekit_parameter_bin_t& parameterBin) override
        {
            switch (id)
            {
            case STREAM_PARAMETER_HFOV:
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
            }
            break;
            case STREAM_PARAMETER_VFOV:
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
            }
            break;
            }
        }

        virtual void invoke(sensekit_streamconnection_t connection,
                            sensekit_command_id commandId,
                            size_t inByteLength,
                            sensekit_parameter_data_t inData,
                            sensekit_parameter_bin_t& parameterBin) override
        {

        }

        void on_new_buffer(sensekit_frame_t* newBuffer,
                                   wrapper_type* wrapper)
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
        size_t m_bufferLength{ 0 };
        size_t m_bytesPerPixel{ 0 };
        size_t m_numComponentPerPixel{ 0 };
        sensekit_stream_t m_streamHandle{ nullptr };
        sensekit_bin_t m_binHandle{nullptr};
        sensekit_frame_t* m_currentBuffer{nullptr};
        wrapper_type* m_currentFrame{nullptr};
        sensekit_frame_index_t m_frameIndex {0};

    };

    template<typename TFrameWrapper, typename TBufferBlockType>
    void OniDeviceStream<TFrameWrapper,
                         TBufferBlockType>::on_connection_added(sensekit_streamconnection_t connection)
    {
        if (m_binHandle == nullptr)
        {
            size_t binSize = sizeof(wrapper_type) + m_bufferLength;
            create_bin(binSize, m_binHandle, m_currentBuffer);
            set_new_buffer(m_currentBuffer);
        }

        link_connection_to_bin(connection, m_binHandle);
    }

    template<typename TFrameWrapper, typename TBufferBlockType>
    void OniDeviceStream<TFrameWrapper,
                         TBufferBlockType>::on_connection_removed(sensekit_bin_t bin,
                                                                  sensekit_streamconnection_t connection)
    {
        link_connection_to_bin(connection,
                               static_cast<sensekit_bin_t>(nullptr));

        //don't destroy bin if other connections are linked assigned to it
        if (!bin_has_connections(bin))
        {
            destroy_bin(m_binHandle, m_currentBuffer);
        }
    }


    template<typename TFrameWrapper, typename TBufferBlockType>
    void OniDeviceStream<TFrameWrapper,
                         TBufferBlockType>::set_new_buffer(sensekit_frame_t* newBuffer)
    {
        m_currentBuffer = newBuffer;
        m_currentFrame = nullptr;

        if (m_currentBuffer != nullptr)
        {
            m_currentBuffer->frameIndex = m_frameIndex;
            m_currentFrame = reinterpret_cast<wrapper_type*>(m_currentBuffer->data);

            if (m_currentFrame != nullptr)
            {
                m_currentFrame->frame.data =
                    reinterpret_cast<block_type*>(&(m_currentFrame->frame_data));

                on_new_buffer(m_currentBuffer, m_currentFrame);
            }
        }
    }

    template<typename TFrameWrapper, typename TBufferBlockType>
    void OniDeviceStream<TFrameWrapper, TBufferBlockType>::read_frame()
    {
        if (m_binHandle == nullptr)
            return;

        openni::VideoFrameRef ref;
        if (m_oniStream.readFrame(&ref) == ::openni::STATUS_OK)
        {
            const block_type* oniFrameData = static_cast<const block_type*>(ref.getData());

            block_type* frameData = static_cast<block_type*>(m_currentFrame->frame.data);
            size_t byteSize = MIN(ref.getDataSize(), m_bufferLength);

            memcpy(frameData, oniFrameData, byteSize);

            ++m_frameIndex;

            sensekit_frame_t* nextBuffer;
            cycle_bin(m_binHandle, nextBuffer);
            set_new_buffer(nextBuffer);
        }
    }
}}


#endif /* ONIDEVICESTREAM_H */
