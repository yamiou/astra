#ifndef MOCKCONTEXT_H
#define MOCKCONTEXT_H

#include <SenseKit.h>
#include <OpenNI.h>

namespace sensekit {

    class MockContext
    {
    public:
        MockContext() {}
        virtual ~MockContext() {}

        sensekit_status_t initialize();
        sensekit_status_t terminate();

        sensekit_status_t open_sensor(const char* uri, sensekit_sensor_t** sensor);
        sensekit_status_t close_sensor(sensekit_sensor_t** sensor);
        sensekit_status_t open_depth_stream(sensekit_sensor_t* sensor, sensekit_depthstream_t** stream);
        sensekit_status_t close_depth_stream(sensekit_depthstream_t** stream);
        sensekit_status_t open_depth_frame(sensekit_depthstream_t* stream, int timeout,  sensekit_depthframe_t** frame);
        sensekit_status_t close_depth_frame(sensekit_depthframe_t** frame);

    private:
        openni::Device m_device;
        openni::VideoStream m_depthStream;
        openni::DeviceInfo m_deviceInfo;

        sensekit_depthframe_t* m_currentFrame{nullptr};
    };
}

#endif /* MOCKCONTEXT_H */
