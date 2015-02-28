#ifndef STREAMSOURCE_H
#define STREAMSOURCE_H

#include "Device.h"
#include "DriverAdapter.h"

namespace sensekit {

    class StreamSource
    {
    public:

        StreamSource(DriverAdapter& adapter, Device& device);
        virtual ~StreamSource() {}

    private:

        DriverAdapter& m_adapter;
        Device& m_device;

    };
}


#endif /* STREAMSOURCE_H */
