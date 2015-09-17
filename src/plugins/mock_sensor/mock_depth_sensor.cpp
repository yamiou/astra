#include "mock_depth_sensor.hpp"
#include "mock_device_depth_stream.hpp"
#include "mock_device_ir_stream.hpp"

namespace orbbec { namespace mocks {

    mock_depth_sensor::mock_depth_sensor()
        : sensor(sensor_info(astra::devices::sensor_type::depth, "depth"))
    {
    }

    mock_depth_sensor::~mock_depth_sensor()
    {
    }

    device_status mock_depth_sensor::on_initialize()
    {
        auto depth = std::make_shared<mock_depth_stream>();
        add_stream(depth);

        auto ir = std::make_shared<mock_ir_stream>();
        add_stream(ir);

        return device_status_value::ok;
    }
}}
