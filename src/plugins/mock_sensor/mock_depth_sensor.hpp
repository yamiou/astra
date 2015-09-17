#ifndef MOCK_DEPTH_SENSOR_H
#define MOCK_DEPTH_SENSOR_H

#include <memory>

#include "astra_sensor.hpp"
#include "mock_depth_generator.hpp"

using astra::devices::device_status;
using astra::devices::device_status_value;
using astra::devices::sensor_info;

namespace orbbec { namespace mocks {

    class mock_depth_sensor : public astra::devices::sensor
    {
    public:
        mock_depth_sensor();
        virtual ~mock_depth_sensor();

        virtual device_status on_initialize() override;

    private:
        std::unique_ptr<depth_generator> generator_;
    };
}}

#endif /* MOCK_DEPTH_SENSOR_H */
