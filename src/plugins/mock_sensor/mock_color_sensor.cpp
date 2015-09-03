#include "mock_color_sensor.hpp"
#include "mock_device_color_stream.hpp"

namespace orbbec { namespace mocks {

    mock_color_sensor::mock_color_sensor()
        : sensor(sensor_info(astra::devices::sensor_type::color, "color"))
    {
    }

    mock_color_sensor::~mock_color_sensor()
    {
    }

    device_status mock_color_sensor::on_initialize()
    {
        auto color = std::make_shared<mock_color_stream>();
        add_stream(color);

        return device_status_value::ok;
    }
}}
