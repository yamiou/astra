#ifndef SENSEKIT_H
#define SENSEKIT_H

#include <SenseKit/Sensor.h>
#include <SenseKit/StreamDescription.h>
#include <SenseKit/Frame.h>
#include <SenseKit/FrameReadyListener.h>
#include <SenseKit/StreamReader.h>
#include <SenseKit/DataStream.h>

namespace sensekit {

    class SenseKit
    {
    public:
        static sensekit_status_t initialize()
            {
                return sensekit_initialize();
            }

        static sensekit_status_t terminate()
            {
                return sensekit_terminate();
            }
    };
}

#endif // SENSEKIT_H
