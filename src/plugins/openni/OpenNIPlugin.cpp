#include "OpenNIPlugin.h"
#include "OniDepthStream.h"
#include "OniColorStream.h"
#include <iostream>
#include <SenseKit/SenseKit.h>
#include <SenseKitUL/StreamTypes.h>
#include "../../SenseKit/sensekit_internal.h"


using std::cout;
using std::endl;

EXPORT_PLUGIN(sensekit::plugins::OpenNIPlugin);

namespace sensekit
{
    namespace plugins
    {
        void OpenNIPlugin::init_openni()
        {
            ::openni::Status rc = ::openni::STATUS_OK;

            ::openni::OpenNI::addDeviceConnectedListener(this);
            ::openni::OpenNI::addDeviceDisconnectedListener(this);

            get_pluginService().create_stream_set(m_streamSetHandle);

            m_sensor = new Sensor(m_streamSetHandle);

            cout << "Initializing openni" << endl;
            rc = ::openni::OpenNI::initialize();
        }

        void OpenNIPlugin::onDeviceConnected(const ::openni::DeviceInfo* info)
        {
            ::openni::Status rc = ::openni::STATUS_OK;

            cout << "device connected, opening device" << endl;
            rc = m_device.open(info->getUri());

            if (rc != ::openni::STATUS_OK)
            {
                cout << "Failed to open" << endl;
                ::openni::OpenNI::shutdown();
            }

            m_deviceInfo = m_device.getDeviceInfo();
            m_isDeviceOpen = true;

            open_sensor_streams();
        }

        void OpenNIPlugin::onDeviceDisconnected(const ::openni::DeviceInfo* info)
        {
            cout << "device disconnected" << endl;
            m_isDeviceOpen = false;

            close_sensor_streams();
        }

        OpenNIPlugin::~OpenNIPlugin()
        {
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
            if (rc == ::openni::STATUS_OK)
            {
                OniDepthStream* stream = new OniDepthStream(get_pluginService(),
                                                            *m_sensor,
                                                            m_device);
                stream->start();
                m_streams.push_back(StreamPtr(stream));

            }
            else
            {
                cout << "Failed to open depth stream" << endl;
            }

            cout << "opening color stream" << endl;

            if (rc == ::openni::STATUS_OK)
            {
                OniColorStream* stream = new OniColorStream(get_pluginService(),
                                                            *m_sensor,
                                                            m_device);
                stream->start();
                m_streams.push_back(StreamPtr(stream));
            }
            else
            {
                cout << "Failed to open color stream" << endl;
            }

            return SENSEKIT_STATUS_SUCCESS;
        }

        sensekit_status_t OpenNIPlugin::close_sensor_streams()
        {
            m_streams.clear();

            return SENSEKIT_STATUS_SUCCESS;
        }

        void OpenNIPlugin::temp_update()
        {
            if (m_isDeviceOpen)
            {
                read_streams();
            }
        }

        sensekit_status_t OpenNIPlugin::read_streams()
        {
            int streamIndex = -1;
            int timeout = ::openni::TIMEOUT_NONE;

            // refactor in to a stream set class
            const int MAX_STREAMS = 2;
            ::openni::VideoStream* pStreams[2];

            int count = 0;
            for(auto& stream : m_streams)
            {
                pStreams[count++] = stream->get_oni_stream();
                if (count == MAX_STREAMS)
                    break;
            }

            if (::openni::OpenNI::waitForAnyStream(pStreams, count, &streamIndex, timeout)
                == ::openni::STATUS_TIME_OUT)
            {
                return SENSEKIT_STATUS_TIMEOUT;
            }

            if (streamIndex == -1)
                return SENSEKIT_STATUS_TIMEOUT;

            count = 0;
            for(auto& stream : m_streams)
            {
                if (pStreams[streamIndex] == stream->get_oni_stream())
                {
                    stream->read_frame();
                    break;
                }
            }

            return SENSEKIT_STATUS_SUCCESS;
        }
    }
}
