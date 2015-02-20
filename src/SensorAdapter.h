#ifndef SENSORADAPTER_H
#define SENSORADAPTER_H

#include "Sensekit-private.h"

namespace sensekit {

    class SensorAdapter
    {
    public:
        SensorAdapter() {};
        virtual ~SensorAdapter() {};

        virtual sensekit_status_t Open() = 0;
        virtual sensekit_status_t Close() = 0;

    };

}


#endif /* SENSORADAPTER_H */
