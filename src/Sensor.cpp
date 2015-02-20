#include "Sensor.h"

namespace sensekit {

    sensekit_status_t Sensor::Open(void)
    {
        m_adapter->Open();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t Sensor::Close(void)
    {
        m_adapter->Close();

        return SENSEKIT_STATUS_SUCCESS;
    }
}
