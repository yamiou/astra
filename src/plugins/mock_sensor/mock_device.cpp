#include "mock_device.hpp"
#include "mock_color_sensor.hpp"
#include "mock_depth_sensor.hpp"
#include "astra_device_info.hpp"

namespace orbbec { namespace mocks {

    mock_device::mock_device()
        : astra::devices::device(astra::devices::device_info("device/mock_device", 0, 0))
    {
    }

    mock_device::~mock_device() = default;

    device_status mock_device::on_initialize()
    {
        auto color = std::make_shared<mock_color_sensor>();
        add_sensor(color);

        auto depth = std::make_shared<mock_depth_sensor>();
        add_sensor(depth);

        return device_status_value::ok;
    }

    device_status mock_device::on_connect()
    {
        return device_status_value::ok;
    }

    device_status mock_device::on_disconnect()
    {
        return device_status_value::ok;
    }
}}
