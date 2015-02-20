#ifndef OPENNIADAPTER_H
#define OPENNIADAPTER_H

#include "../SenseKit-private.h"
#include "../SensorAdapter.h"
#include <OpenNI.h>

namespace sensekit {

    class OpenNIAdapter : public SensorAdapter
    {
    public:
        OpenNIAdapter() {}
        virtual ~OpenNIAdapter() {}

        virtual sensekit_status_t Open();
        virtual sensekit_status_t Close();

    private:
        openni::Device m_device;
        openni::VideoStream m_colorStream;
        openni::VideoStream m_depthStream;
    };
}


#endif // OPENNIADAPTER_H
