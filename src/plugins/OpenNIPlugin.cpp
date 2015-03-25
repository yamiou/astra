#include "OpenNIPlugin.h"
#include "PluginBase.h"
#include <iostream>
#include <sensekit_known_streams.h>

using std::cout;
using std::endl;

static sensekit::openni::OpenNIPlugin* g_plugin;

SENSEKIT_BEGIN_DECLS

void initialize(PluginServiceProxyBase* proxy)
{
    g_plugin = new sensekit::openni::OpenNIPlugin(static_cast<sensekit::PluginServiceProxy*>(proxy));
    g_plugin->initialize();
}

void update()
{
    g_plugin->temp_update();
}

void terminate()
{
    g_plugin->cleanup();
    delete g_plugin;
}

SENSEKIT_END_DECLS

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

            open_depth_stream();
        }

        void OpenNIPlugin::on_cleanup()
        {
            get_pluginService().destroy_stream(m_handle);

            close_depth_stream();

            cout << "closing device" << endl;
            m_device.close();
            cout << "shutting down openni" << endl;
            ::openni::OpenNI::shutdown();
        }

        sensekit_status_t OpenNIPlugin::open_depth_stream()
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

            sensekit_frame_t* nextBuffer = nullptr;
            
            StreamPluginCallbacks pluginCallbacks(this);
            
            pluginCallbacks.setParameterCallback = &OpenNIPlugin::set_parameter_thunk;
            pluginCallbacks.getParameterSizeCallback = &OpenNIPlugin::get_parameter_size_thunk;
            pluginCallbacks.getParameterDataCallback = &OpenNIPlugin::get_parameter_data_thunk;

            get_pluginService().create_stream(/*bogus*/nullptr, DEPTH_TYPE, DEPTH_DEFAULT_SUBTYPE, pluginCallbacks, m_handle);

            const ::openni::VideoMode& mode = m_depthStream.getVideoMode();

            m_width = mode.getResolutionX();
            m_height = mode.getResolutionY();
            m_bpp = 2;

            m_bufferLength  = m_width * m_height * m_bpp;

            get_pluginService()
                .create_stream_bin(m_handle,
                                   sizeof(sensekit_depthframe_t) + m_bufferLength,
                                   &m_id,
                                   &nextBuffer);

            set_new_buffer(nextBuffer);
            
            return SENSEKIT_STATUS_SUCCESS;
        }

        void OpenNIPlugin::set_parameter_thunk(void* instance, sensekit_streamconnection_t* streamConnection, sensekit_parameter_id id, size_t byteLength, sensekit_parameter_data_t* data)
        {
            OpenNIPlugin* self = static_cast<OpenNIPlugin*>(instance);
            self->set_parameter(streamConnection, id, byteLength, data);
        }

        void OpenNIPlugin::get_parameter_size_thunk(void* instance, sensekit_streamconnection_t* streamConnection, sensekit_parameter_id id, /*out*/size_t* byteLength)
        {
            OpenNIPlugin* self = static_cast<OpenNIPlugin*>(instance);
            self->get_parameter_size(streamConnection, id, *byteLength);
        }

        void OpenNIPlugin::get_parameter_data_thunk(void* instance, sensekit_streamconnection_t* streamConnection, sensekit_parameter_id id, size_t byteLength, sensekit_parameter_data_t* data)
        {
            OpenNIPlugin* self = static_cast<OpenNIPlugin*>(instance);
            self->get_parameter_data(streamConnection, id, byteLength, data);
        }


        void OpenNIPlugin::set_parameter(sensekit_streamconnection_t* streamConnection, sensekit_parameter_id id, size_t byteLength, sensekit_parameter_data_t* data)
        {
        }

        void OpenNIPlugin::get_parameter_size(sensekit_streamconnection_t* streamConnection, sensekit_parameter_id id, /*out*/size_t& byteLength)
        {
            byteLength = 20;
        }

        void OpenNIPlugin::get_parameter_data(sensekit_streamconnection_t* streamConnection, sensekit_parameter_id id, size_t byteLength, sensekit_parameter_data_t* data)
        {
            char* charData = (char*)data;
            for (int i = 0; i < byteLength; i++)
            {
                charData[i] = i;
            }
        }

        sensekit_status_t OpenNIPlugin::close_depth_stream()
        {
            cout << "stopping depth stream" << endl;
            m_depthStream.stop();
            cout << "destroying depth stream" << endl;
            m_depthStream.destroy();

            return SENSEKIT_STATUS_SUCCESS;
        }

        void OpenNIPlugin::set_new_buffer(sensekit_frame_t* nextBuffer)
        {
            m_currentBuffer = nextBuffer;
            m_currentFrame = static_cast<sensekit_depthframe_wrapper_t*>(m_currentBuffer->data);
            m_currentFrame->frame.data = (int16_t *)&(m_currentFrame->frame_data);
            m_currentFrame->frame.frameIndex = m_frameIndex;
            m_currentFrame->frame.width = m_width;
            m_currentFrame->frame.height = m_height;
            m_currentFrame->frame.bpp = m_bpp;

            //TODO use placement new for m_currentFrame?
            //m_currentFrame = new(m_currentBuffer->data) sensekit_depthframe_t();
        }

        void OpenNIPlugin::temp_update()
        {
            if (!is_initialized())
                return;

            if (nullptr != m_currentFrame
                && read_next_depth_frame(m_currentFrame) == SENSEKIT_STATUS_SUCCESS)
            {
                sensekit_frame_t* nextBuffer = nullptr;
                get_pluginService().cycle_bin_buffers(m_handle, m_id, &nextBuffer);
                set_new_buffer(nextBuffer);
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

            int16_t* frameData = m_currentFrame->frame.data;
            size_t bufferLength = m_width * m_height;

            for(int i = 0; i < bufferLength; i++)
            {
                frameData[i] = datData[i];
            }

            frame->frame.frameIndex = m_frameIndex;
            ++m_frameIndex;

            ref.release();

            return SENSEKIT_STATUS_SUCCESS;
        }
    }
}