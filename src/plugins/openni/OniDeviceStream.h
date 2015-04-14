#ifndef ONIDEVICESTREAM_H
#define ONIDEVICESTREAM_H

#include <SenseKit/SenseKit.h>
#include <SenseKit/Plugins/plugin_api.h>
#include <SenseKit/Plugins/Stream.h>
#include <OpenNI.h>
#include <iostream>

using std::cout;
using std::endl;


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
                            ::openni::Device& oniDevice)
                : OniDeviceStreamBase(pluginService,
                                      streamSet,
                                      desc),
                  m_oniDevice(oniDevice) { }

            virtual ~OniDeviceStream()
                {
                    stop();
                    cout << "destroying oni stream of type: " << get_description().get_type() << endl;
                    m_oniStream.destroy();
                }

            virtual void start() override
                {
                    m_oniStream.start();
                    cout << "starting stream of type: " << get_description().get_type() << endl;
                }

            virtual void stop() override
                {
                    cout << "stop stream of type: " << get_description().get_type() << endl;
                    m_oniStream.stop();
                }

            virtual void set_new_buffer(sensekit_frame_t* newBuffer);

            virtual void on_connection_added(sensekit_streamconnection_t connection) override;

            virtual void on_connection_removed(sensekit_bin_t bin,
                                               sensekit_streamconnection_t connection) override;

            virtual void on_new_buffer(sensekit_frame_t* newBuffer,
                                       wrapper_type* wrapper) { }

            virtual void read_frame() override;

            virtual ::openni::VideoStream* get_oni_stream() override { return &m_oniStream; }


        protected:
            ::openni::Device& m_oniDevice;
            ::openni::VideoStream m_oniStream;
            ::openni::VideoMode m_oniVideoMode;
            size_t m_bufferLength{0};

        private:
            sensekit_stream_t m_streamHandle{nullptr};
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

                block_type* frameData = m_currentFrame->frame.data;
                size_t dataSize = MIN(ref.getDataSize(), m_bufferLength);

                memcpy(frameData, oniFrameData, dataSize);

                ++m_frameIndex;

                sensekit_frame_t* nextBuffer;
                cycle_bin(m_binHandle, nextBuffer);
                set_new_buffer(nextBuffer);
            }
        }
    }}


#endif /* ONIDEVICESTREAM_H */
