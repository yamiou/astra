#ifndef OPENNIADAPTER_H
#define OPENNIADAPTER_H

#include <OpenNI.h>
#include "../DriverAdapter.h"

namespace sensekit {

    class OpenNIAdapter : public DriverAdapter
    {
    public:
        OpenNIAdapter()
            : m_initialized(false)
            {}

        virtual ~OpenNIAdapter() {}

        virtual sensekit_status_t initialize(device_connected_callback_t connectedCallback,
                                             device_disconnected_callback_t disconnectedCallback,
                                             device_changed_callback_t changedCallback,
                                             void* callbackContext) override;

        virtual sensekit_status_t terminate() override;
        virtual sensekit_status_t has_device_for_uri(char* uri, bool& deviceAvailable) override;
        virtual device_handle_t open_device(char* uri) override;
        virtual driver_status_t close_device(device_handle_t deviceHandle) override;

    private:
        bool m_initialized;
        openni::Device m_device;
        sensekit_device_desc_t m_desc;
        openni::VideoStream m_colorStream;
        openni::VideoStream m_depthStream;
    };
}


#endif // OPENNIADAPTER_H
