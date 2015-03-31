#include "OpenNIPlugin.h"
#include <iostream>
#include <StreamTypes.h>
#include <streams/color_types.h>

using std::cout;
using std::endl;

EXPORT_PLUGIN(sensekit::openni::OpenNIPlugin);

namespace sensekit
{
    namespace openni
    {
        void OpenNIPlugin::on_initialize()
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

        void OpenNIPlugin::on_cleanup()
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

            get_pluginService().create_stream(m_streamSetHandle,
                                              SENSEKIT_STREAM_DEPTH,
                                              DEPTH_DEFAULT_SUBTYPE,
                                              pluginCallbacks,
                                              m_depthHandle);

            get_pluginService().create_stream(m_streamSetHandle,
                                              SENSEKIT_STREAM_COLOR,
                                              COLOR_DEFAULT_SUBTYPE,
                                              pluginCallbacks,
                                              m_colorHandle);

            m_depthMode = m_depthStream.getVideoMode();
            m_colorMode = m_colorStream.getVideoMode();

            m_depthBufferLength  = m_depthMode.getResolutionX() * m_depthMode.getResolutionY() * 2;
            m_colorBufferLength = m_colorMode.getResolutionX() * m_colorMode.getResolutionY() * 3;

            get_pluginService()
                .create_stream_bin(m_depthHandle,
                                   sizeof(sensekit_depthframe_t) + m_depthBufferLength,
                                   &m_depthBinId,
                                   &nextBuffer);

            set_new_depth_buffer(nextBuffer);

            get_pluginService()
                .create_stream_bin(m_colorHandle,
                                   sizeof(sensekit_colorframe_t) + m_colorBufferLength,
                                   &m_colorBinId,
                                   &nextBuffer);

            set_new_color_buffer(nextBuffer);

            return SENSEKIT_STATUS_SUCCESS;
        }

        void OpenNIPlugin::set_parameter(sensekit_streamconnection_t* streamConnection,
                                         sensekit_parameter_id id, size_t byteLength,
                                         sensekit_parameter_data_t* data)
        {}

        void OpenNIPlugin::get_parameter_size(sensekit_streamconnection_t* streamConnection,
                                              sensekit_parameter_id id,
                                              size_t& byteLength)
        {}

        void OpenNIPlugin::get_parameter_data(sensekit_streamconnection_t* streamConnection,
                                              sensekit_parameter_id id,
                                              size_t byteLength,
                                              sensekit_parameter_data_t* data)
        {}

        void OpenNIPlugin::connection_added(sensekit_streamconnection_t* connection)
        {
            cout << "openniplugin: new connection added" << endl;
        }

        void OpenNIPlugin::connection_removed(sensekit_streamconnection_t* connection)
        {
            cout << "openniplugin: connection removed" << endl;
        }

        sensekit_status_t OpenNIPlugin::close_sensor_streams()
        {
            cout << "stopping depth stream" << endl;
            m_depthStream.stop();
            cout << "destroying depth stream" << endl;
            m_depthStream.destroy();

            cout << "stopping color stream" << endl;
            m_colorStream.stop();
            cout << "destroying color stream" << endl;
            m_colorStream.destroy();

            return SENSEKIT_STATUS_SUCCESS;
        }

        void OpenNIPlugin::set_new_depth_buffer(sensekit_frame_t* nextBuffer)
        {
            m_currentDepthBuffer = nextBuffer;
            m_currentDepthFrame = static_cast<sensekit_depthframe_wrapper_t*>(m_currentDepthBuffer->data);
            m_currentDepthFrame->frame.data = (int16_t *)&(m_currentDepthFrame->frame_data);
            m_currentDepthFrame->frame.frameIndex = m_frameIndex;
            m_currentDepthFrame->frame.width = m_depthMode.getResolutionX();
            m_currentDepthFrame->frame.height = m_depthMode.getResolutionY();
            m_currentDepthFrame->frame.bpp = 2;
        }

        void OpenNIPlugin::set_new_color_buffer(sensekit_frame_t* nextBuffer)
        {
            m_currentColorBuffer = nextBuffer;
            m_currentColorFrame = static_cast<sensekit_colorframe_wrapper_t*>(m_currentColorBuffer->data);
            m_currentColorFrame->frame.data = (uint8_t *)&(m_currentColorFrame->frame_data);
            m_currentColorFrame->frame.frameIndex = m_frameIndex;
            m_currentColorFrame->frame.width = m_colorMode.getResolutionX();
            m_currentColorFrame->frame.height = m_colorMode.getResolutionY();
            m_currentColorFrame->frame.bpp = 3;
        }

        void OpenNIPlugin::temp_update()
        {
            if (!is_initialized())
                return;

            if (nullptr != m_currentDepthFrame &&
                read_next_depth_frame(m_currentDepthFrame)
                == SENSEKIT_STATUS_SUCCESS)
            {
                sensekit_frame_t* nextBuffer = nullptr;
                get_pluginService().cycle_bin_buffers(m_depthHandle, m_depthBinId, &nextBuffer);
                set_new_depth_buffer(nextBuffer);
            }

            if (nullptr != m_currentColorFrame &&
                read_next_color_frame(m_currentColorFrame)
                == SENSEKIT_STATUS_SUCCESS)
            {
                sensekit_frame_t* nextBuffer = nullptr;
                get_pluginService().cycle_bin_buffers(m_colorHandle, m_colorBinId, &nextBuffer);
                set_new_color_buffer(nextBuffer);
            }
        }

        sensekit_status_t OpenNIPlugin::read_next_depth_frame(sensekit_depthframe_wrapper_t* frame)
        {
            int dummy;
            int timeout = 30;
            ::openni::VideoStream* pStream = &m_depthStream;
            if (::openni::OpenNI::waitForAnyStream(&pStream, 1, &dummy, timeout)
                == ::openni::STATUS_TIME_OUT)
            {
                return SENSEKIT_STATUS_TIMEOUT;
            }

            ::openni::VideoFrameRef ref;
            m_depthStream.readFrame(&ref);

            const short* datData = static_cast<const short*>(ref.getData());

            int16_t* frameData = m_currentDepthFrame->frame.data;
            size_t bufferLength = m_depthMode.getResolutionX() * m_depthMode.getResolutionY();

            memcpy(frameData, datData, sizeof(int16_t)*bufferLength);

            frame->frame.frameIndex = m_frameIndex;
            ++m_frameIndex;

            ref.release();

            return SENSEKIT_STATUS_SUCCESS;
        }

        sensekit_status_t OpenNIPlugin::read_next_color_frame(sensekit_colorframe_wrapper_t* frame)
        {
            int dummy;
            int timeout = 30;
            ::openni::VideoStream* pStream = &m_colorStream;
            if (::openni::OpenNI::waitForAnyStream(&pStream, 1, &dummy, timeout)
                == ::openni::STATUS_TIME_OUT)
            {
                return SENSEKIT_STATUS_TIMEOUT;
            }

            ::openni::VideoFrameRef ref;
            m_colorStream.readFrame(&ref);

            const uint8_t* datData = static_cast<const uint8_t*>(ref.getData());

            uint8_t* frameData = m_currentColorFrame->frame.data;
            size_t bufferLength = m_colorMode.getResolutionX() * m_depthMode.getResolutionY() * 3;

            memcpy(frameData, datData, bufferLength);

            frame->frame.frameIndex = m_frameIndex;
            ++m_frameIndex;

            ref.release();

            return SENSEKIT_STATUS_SUCCESS;
        }
    }
}