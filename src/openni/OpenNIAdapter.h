#ifndef OPENNIADAPTER_H
#define OPENNIADAPTER_H

#include <OpenNI.h>
#include "../DriverService.h"

namespace sensekit {

    class OpenNIAdapter : public DriverService
    {
    public:
        OpenNIAdapter()
            : m_initialized(false)
            {}

        virtual ~OpenNIAdapter() {}

        virtual sensekit_status_t initialize() override;
        virtual sensekit_status_t destroy() override;
        virtual sensekit_status_t has_device_for_uri(char* uri, bool& deviceAvailable) override;
        virtual sensekit_status_t query_for_device(char* uri, Device** device) override;

    private:
        bool m_initialized;
        openni::Device m_device;
        openni::VideoStream m_colorStream;
        openni::VideoStream m_depthStream;
    };
}


#endif // OPENNIADAPTER_H
