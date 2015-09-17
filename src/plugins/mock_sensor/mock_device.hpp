#ifndef MOCK_DEVICE_H
#define MOCK_DEVICE_H

#include <memory>
#include "astra_device.hpp"
#include "astra_sensor_info.hpp"

using astra::devices::device_status;
using astra::devices::device_status_value;

namespace orbbec { namespace mocks {

    class mock_device : public astra::devices::device
    {
    public:
        mock_device();
        virtual ~mock_device();

    private:
        virtual device_status on_initialize() override;
        virtual device_status on_connect() override;
        virtual device_status on_disconnect() override;
    };
}}

#endif /* MOCK_DEVICE_H */
