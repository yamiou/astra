#include "OpenNIPlugin.h"
#include <iostream>
#include <StreamTypes.h>
#include "../../SenseKit/sensekit_internal.h"

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif

using std::cout;
using std::endl;

EXPORT_PLUGIN(sensekit::openni::OpenNIPlugin);

namespace sensekit
{
    namespace openni
    {
        void OpenNIPlugin::init_openni()
        {
            ::openni::Status rc = ::openni::STATUS_OK;

            const char* deviceURI = ::openni::ANY_DEVICE;

            cout << "Initializing openni" << endl;
            rc = ::openni::OpenNI::initialize();
            
            cout << "Opening device" << endl;
            rc = m_device.open(deviceURI);

            if (rc != ::openni::STATUS_OK)
            {
                cout << "Failed to open" << endl;
                ::openni::OpenNI::shutdown();
            }

            m_deviceInfo = m_device.getDeviceInfo();

            open_sensor_streams();
        }

        OpenNIPlugin::~OpenNIPlugin()
        {
            get_pluginService().destroy_stream(m_depthHandle);
            get_pluginService().destroy_stream(m_colorHandle);
            get_pluginService().destroy_stream_set(m_streamSetHandle);
            close_sensor_streams();

            cout << "closing device" << endl;
            m_device.close();
            cout << "shutting down openni" << endl;
            ::openni::OpenNI::shutdown();
        }

        sensekit_status_t OpenNIPlugin::open_sensor_streams()
        {
            ::openni::Status rc = ::openni::STATUS_OK;

            cout << "opening depth stream" << endl;
            rc = m_depthStream.create(m_device, ::openni::SENSOR_DEPTH);
            if (rc == ::openni::STATUS_OK)
            {
                cout << "starting depth stream." << endl;
                rc = m_depthStream.start();
                if (rc != ::openni::STATUS_OK)
                {
                    cout << "failed to start depth stream" << endl;
                    m_depthStream.destroy();
                }
            }
            else
            {
                cout << "Failed to open depth stream" << endl;
            }

            if (!m_depthStream.isValid())
            {
                cout << "shutting down openni because of failure" << endl;
                ::openni::OpenNI::shutdown();
            }

            cout << "opening color stream" << endl;
            rc = m_colorStream.create(m_device, ::openni::SENSOR_COLOR);
            if (rc == ::openni::STATUS_OK)
            {
                cout << "starting color stream." << endl;
                rc = m_colorStream.start();
                if (rc != ::openni::STATUS_OK)
                {
                    cout << "failed to start color stream" << endl;
                    m_colorStream.destroy();
                }
            }
            else
            {
                cout << "Failed to open color stream" << endl;
            }

            if (!m_colorStream.isValid())
            {
                cout << "shutting down openni because of failure" << endl;
                ::openni::OpenNI::shutdown();
            }

            sensekit_frame_t* nextBuffer = nullptr;

            stream_callbacks_t pluginCallbacks = create_plugin_callbacks(this);

            get_pluginService().create_stream_set(m_streamSetHandle);

            sensekit_stream_desc_t depthDesc;
            depthDesc.type = SENSEKIT_STREAM_DEPTH;
            depthDesc.subType = DEPTH_DEFAULT_SUBTYPE;

            get_pluginService().create_stream(m_streamSetHandle,
                                              depthDesc,
                                              pluginCallbacks,
                                              &m_depthHandle);

            sensekit_stream_desc_t colorDesc;
            colorDesc.type = SENSEKIT_STREAM_COLOR;
            colorDesc.subType = COLOR_DEFAULT_SUBTYPE;

            get_pluginService().create_stream(m_streamSetHandle,
                                              colorDesc,
                                              pluginCallbacks,
                                              &m_colorHandle);

            m_depthMode = m_depthStream.getVideoMode();
            m_colorMode = m_colorStream.getVideoMode();

            m_depthBufferLength  = m_depthMode.getResolutionX() * m_depthMode.getResolutionY() * 2;
            m_colorBufferLength = m_colorMode.getResolutionX() * m_colorMode.getResolutionY() * 3;

            get_pluginService()
                .create_stream_bin(m_depthHandle,
                                   sizeof(sensekit_depthframe_wrapper_t) + m_depthBufferLength,
                                   &m_depthBinHandle,
                                   &nextBuffer);

            set_new_depth_buffer(nextBuffer);

            get_pluginService()
                .create_stream_bin(m_colorHandle,
                                   sizeof(sensekit_colorframe_wrapper_t) + m_colorBufferLength,
                                   &m_colorBinHandle,
                                   &nextBuffer);

            set_new_color_buffer(nextBuffer);

            return SENSEKIT_STATUS_SUCCESS;
        }

        void OpenNIPlugin::set_parameter(sensekit_streamconnection_t streamConnection,
                                         sensekit_parameter_id id, size_t byteLength,
                                         sensekit_parameter_data_t* data)
        {}

        void OpenNIPlugin::get_parameter_size(sensekit_streamconnection_t streamConnection,
                                              sensekit_parameter_id id,
                                              size_t& byteLength)
        {}

        void OpenNIPlugin::get_parameter_data(sensekit_streamconnection_t streamConnection,
                                              sensekit_parameter_id id,
                                              size_t byteLength,
                                              sensekit_parameter_data_t* data)
        {}

        void OpenNIPlugin::connection_added(sensekit_streamconnection_t connection)
        {
            sensekit_stream_desc_t desc;
            sensekit_stream_get_description(connection, &desc);

            switch (desc.type)
            {
            case SENSEKIT_STREAM_DEPTH:
                get_pluginService().link_connection_to_bin(connection, m_depthBinHandle);
                cout << "openniplugin: new connection added, linked to global depth" << endl;
                break;
            case SENSEKIT_STREAM_COLOR:
                get_pluginService().link_connection_to_bin(connection, m_colorBinHandle);
                cout << "openniplugin: new connection added, linked to global color" << endl;
                break;
            }
        }

        void OpenNIPlugin::connection_removed(sensekit_streamconnection_t connection)
        {
            sensekit_stream_desc_t desc;
            sensekit_stream_get_description(connection, &desc);

            switch (desc.type)
            {
            case SENSEKIT_STREAM_DEPTH:
                get_pluginService().link_connection_to_bin(connection, nullptr);
                cout << "openniplugin: connection removed, unlinking from depth" << endl;
                break;
            case SENSEKIT_STREAM_COLOR:
                get_pluginService().link_connection_to_bin(connection, nullptr);
                cout << "openniplugin: connection removed, unlinking from color" << endl;
                break;
            }
        }

        sensekit_status_t OpenNIPlugin::close_sensor_streams()
        {
            cout << "stopping depth stream" << endl;
            m_depthStream.stop();
            cout << "stopping color stream" << endl;
            m_colorStream.stop();
            cout << "destroying depth stream" << endl;
            m_depthStream.destroy();
            cout << "destroying color stream" << endl;
            m_colorStream.destroy();

            return SENSEKIT_STATUS_SUCCESS;
        }

        void OpenNIPlugin::set_new_depth_buffer(sensekit_frame_t* nextBuffer)
        {
            m_currentDepthBuffer = nextBuffer;
            m_currentDepthFrame = nullptr;
            if (m_currentDepthBuffer != nullptr)
            {
                m_currentDepthBuffer->frameIndex = m_frameIndex;
                m_currentDepthFrame = static_cast<sensekit_depthframe_wrapper_t*>(m_currentDepthBuffer->data);
                if (m_currentDepthFrame != nullptr)
                {
                    m_currentDepthFrame->frame.data = reinterpret_cast<int16_t *>(&(m_currentDepthFrame->frame_data));


                    sensekit_depthframe_metadata_t metadata;

                    metadata.width = m_depthMode.getResolutionX();
                    metadata.height = m_depthMode.getResolutionY();
                    metadata.bytesPerPixel = 2;

                    m_currentDepthFrame->frame.metadata = metadata;
                }
            }
        }

        void OpenNIPlugin::set_new_color_buffer(sensekit_frame_t* nextBuffer)
        {
            m_currentColorBuffer = nextBuffer;
            m_currentColorFrame = nullptr;
            if (m_currentColorBuffer != nullptr)
            {
                m_currentColorBuffer->frameIndex = m_frameIndex;
                m_currentColorFrame = static_cast<sensekit_colorframe_wrapper_t*>(m_currentColorBuffer->data);
                if (m_currentColorFrame != nullptr)
                {
                    m_currentColorFrame->frame.data = reinterpret_cast<uint8_t *>(&(m_currentColorFrame->frame_data));

                    sensekit_colorframe_metadata_t metadata;

                    metadata.width = m_colorMode.getResolutionX();
                    metadata.height = m_colorMode.getResolutionY();
                    metadata.bytesPerPixel = 3;

                    m_currentColorFrame->frame.metadata = metadata;
                }
            }
        }

        void OpenNIPlugin::temp_update()
        {
            if (m_currentColorBuffer != nullptr
                && m_currentDepthBuffer != nullptr)
            {
                read_streams();
            }
            else
            {
                std::cout << "buffers not available" << std::endl;
            }
        }

        sensekit_status_t OpenNIPlugin::read_streams()
        {
            int streamIndex = -1;
            int timeout = ::openni::TIMEOUT_NONE;

            ::openni::VideoStream* pStreams[] =  { &m_depthStream, &m_colorStream };
            if (::openni::OpenNI::waitForAnyStream(pStreams, 2, &streamIndex, timeout)
                == ::openni::STATUS_TIME_OUT)
            {
                return SENSEKIT_STATUS_TIMEOUT;
            }

            if (streamIndex == -1)
                return SENSEKIT_STATUS_TIMEOUT;

            sensekit_frame_t* nextBuffer = nullptr;

            if (streamIndex == 0)
            {
                ::openni::VideoFrameRef ref;
                if (m_depthStream.readFrame(&ref) == ::openni::STATUS_OK)
                {
                    const int16_t* datData = static_cast<const int16_t*>(ref.getData());

                    int16_t* frameData = m_currentDepthFrame->frame.data;
                    int dataSize = MIN(ref.getDataSize(), m_depthBufferLength);

                    memcpy(frameData, datData, dataSize);

                    ++m_frameIndex;

                    get_pluginService().cycle_bin_buffers(m_depthBinHandle, &nextBuffer);
                    set_new_depth_buffer(nextBuffer);
                }
            }
            else if (streamIndex == 1)
            {
                ::openni::VideoFrameRef ref;
                if (m_colorStream.readFrame(&ref) == ::openni::STATUS_OK)
                {
                    const uint8_t* datData = static_cast<const uint8_t*>(ref.getData());

                    uint8_t* frameData = m_currentColorFrame->frame.data;

                    int dataSize = MIN(ref.getDataSize(), m_colorBufferLength);
                    memcpy(frameData, datData, dataSize);

                    ++m_frameIndex;

                    get_pluginService().cycle_bin_buffers(m_colorBinHandle, &nextBuffer);
                    set_new_color_buffer(nextBuffer);
                }
            }

            return SENSEKIT_STATUS_SUCCESS;
        }
    }
}