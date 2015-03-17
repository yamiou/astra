#include "MockContext.h"
#include "SenseKit-private.h"
#include <iostream>

using std::cout;
using std::endl;

namespace sensekit {

    sensekit_status_t MockContext::initialize()
    {
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t MockContext::terminate()
    {
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t MockContext::open_sensor(const char *uri, sensekit_sensor_t **sensor)
    {
        openni::Status rc = openni::STATUS_OK;

        const char* deviceURI = openni::ANY_DEVICE;

        cout << "Initializing openni" << endl;
        rc = openni::OpenNI::initialize();
        cout << "Opening device" << endl;
        rc = m_device.open(deviceURI);

        if (rc != openni::STATUS_OK)
        {
            cout << "Failed to open" << endl;
            openni::OpenNI::shutdown();
        }

        m_deviceInfo = m_device.getDeviceInfo();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t MockContext::close_sensor(sensekit_sensor_t **sensor)
    {
        cout << "closing device" << endl;
        m_device.close();
        cout << "shutting down openni" << endl;
        openni::OpenNI::shutdown();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t MockContext::open_depth_stream(sensekit_sensor_t *sensor, sensekit_depthstream_t **stream)
    {
        openni::Status rc = openni::STATUS_OK;

        cout << "opening depth stream" << endl;
        rc = m_depthStream.create(m_device, openni::SENSOR_DEPTH);
        if (rc == openni::STATUS_OK)
        {
            cout << "starting depth stream." << endl;
            rc = m_depthStream.start();
            if (rc != openni::STATUS_OK)
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
            openni::OpenNI::shutdown();
        }

        m_currentFrame = new sensekit_depthframe_t;
        m_currentFrame->sampleValue = 0;
        m_currentFrame->frameIndex = 0;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t MockContext::close_depth_stream(sensekit_depthstream_t **stream)
    {
        cout << "stoping depth stream" << endl;
        m_depthStream.stop();
        cout << "destroying depth stream" << endl;
        m_depthStream.destroy();

        delete m_currentFrame;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t MockContext::open_depth_frame(sensekit_depthstream_t *stream, int timeout, sensekit_depthframe_t **frame)
    {
        int dummy;

        openni::VideoStream* pStream = &m_depthStream;
        openni::OpenNI::waitForAnyStream(&pStream, 1, &dummy, timeout);

        openni::VideoFrameRef ref;
        m_depthStream.readFrame(&ref);

        int halfHeight = ref.getHeight() / 2;
        int halfWidth = ref.getWidth() / 2;

        int index = halfHeight * ref.getWidth() + halfWidth;
        const short* datData = static_cast<const short*>(ref.getData());

        short depthSon = datData[index];

        m_currentFrame->sampleValue = depthSon;
        m_currentFrame->frameIndex++;

        ref.release();

        *frame = m_currentFrame;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t MockContext::close_depth_frame(sensekit_depthframe_t** frame)
    {
        *frame = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }
}
