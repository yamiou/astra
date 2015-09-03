#ifndef MOCK_COLOR_SENSOR_H
#define MOCK_COLOR_SENSOR_H

#include "astra_sensor.hpp"

using astra::devices::device_status;
using astra::devices::device_status_value;
using astra::devices::sensor_info;

namespace orbbec { namespace mocks {

    class mock_color_sensor : public astra::devices::sensor
    {
    public:
        mock_color_sensor();
        virtual ~mock_color_sensor();

        device_status on_initialize() override;
    };
}}

#endif /* MOCK_COLOR_SENSOR_H */
