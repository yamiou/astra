#include "OniDeviceStreamSet.h"

namespace sensekit { namespace plugins {

        sensekit_status_t OniDeviceStreamSet::open_sensor_streams()
        {
            ::openni::Status rc = ::openni::STATUS_OK;

            cout << "opening depth stream" << endl;
            if (rc == ::openni::STATUS_OK)
            {
                OniDepthStream* stream = new OniDepthStream(m_pluginService,
                                                            *m_sensor,
                                                            m_oniDevice);
                stream->start();
                m_streams.push_back(StreamPtr(stream));
                m_oniStreams[m_streams.size() - 1] = stream->get_oni_stream();
            }
            else
            {
                cout << "Failed to open depth stream" << endl;
            }

            cout << "opening color stream" << endl;

            if (rc == ::openni::STATUS_OK)
            {
                OniColorStream* stream = new OniColorStream(m_pluginService,
                                                            *m_sensor,
                                                            m_oniDevice);
                stream->start();
                m_streams.push_back(StreamPtr(stream));
                m_oniStreams[m_streams.size() - 1] = stream->get_oni_stream();
            }
            else
            {
                cout << "Failed to open color stream" << endl;
            }

            return SENSEKIT_STATUS_SUCCESS;
        }

        sensekit_status_t OniDeviceStreamSet::close_sensor_streams()
        {
            m_streams.clear();
            m_oniStreams.fill(nullptr);

            return SENSEKIT_STATUS_SUCCESS;
        }



    }}
