#include "OpenNIPlugin.h"
#include <iostream>

using std::cout;
using std::endl;

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
            get_pluginService().unregister_stream(m_handle);

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

            get_pluginService().register_stream(/*bogus*/0, /*bogus*/0, m_handle);

            get_pluginService()
                .orbbec_stream_create_bin(m_handle, sizeof(sensekit_depthframe_t), m_id, nextBuffer);

            set_new_buffer(nextBuffer);

            return SENSEKIT_STATUS_SUCCESS;
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
            m_currentFrame = static_cast<sensekit_depthframe_t*>(m_currentBuffer->data);
            m_currentFrame->sampleValue = 0;
            m_currentFrame->header.frameIndex = m_frameIndex;
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
                get_pluginService().orbbec_swap_bin_buffer(m_handle, m_id, nextBuffer);
                set_new_buffer(nextBuffer);
            }
        }

        sensekit_status_t OpenNIPlugin::read_next_depth_frame(sensekit_depthframe_t* frame)
        {
            int dummy;

            int timeout = 30;
            ::openni::VideoStream* pStream = &m_depthStream;
            if (::openni::OpenNI::waitForAnyStream(&pStream, 1, &dummy, timeout) == ::openni::STATUS_TIME_OUT)
            {
                return SENSEKIT_STATUS_TIMEOUT;
            }

            ::openni::VideoFrameRef ref;
            m_depthStream.readFrame(&ref);

            int halfHeight = ref.getHeight() / 2;
            int halfWidth = ref.getWidth() / 2;

            int index = halfHeight * ref.getWidth() + halfWidth;
            const short* datData = static_cast<const short*>(ref.getData());

            short depthSon = datData[index];

            frame->sampleValue = depthSon;
            frame->header.frameIndex = m_frameIndex;
            ++m_frameIndex;

            ref.release();

            return SENSEKIT_STATUS_SUCCESS;
        }
    }
}