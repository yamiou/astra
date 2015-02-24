#ifndef DEVICEADAPTER_H
#define DEVICEADAPTER_H

#include <SenseKit.h>

namespace sensekit {

    class DriverAdapter
    {
    public:
        DriverAdapter();
        virtual ~DriverAdapter();

        sensekit_status_t initialize();
    private:

    };

}

#endif /* DEVICEADAPTER_H */
