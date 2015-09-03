#include "mock_device_ir_stream.hpp"
#include "mock_graphics.hpp"
#include <sstream>

namespace orbbec { namespace mocks {

    mock_ir_stream::mock_ir_stream()
        : base_stream(static_cast<stream_type>(astra::devices::stream_types::infrared))
    {
    }

    mock_ir_stream::~mock_ir_stream()
    {
    }

    device_status mock_ir_stream::on_initialize()
    {
        // Populate modes
        add_available_mode(stream_mode(320, 240, 30, ASTRA_PIXEL_FORMAT_RGB888));
        add_available_mode(stream_mode(320, 240, 60, ASTRA_PIXEL_FORMAT_RGB888));

        add_available_mode(stream_mode(640, 480, 30, ASTRA_PIXEL_FORMAT_RGB888));
        add_available_mode(stream_mode(640, 480, 60, ASTRA_PIXEL_FORMAT_RGB888));

        set_active_mode(*modes_begin());

        generator_ = std::make_unique<infrared_generator>(active_mode().width(), active_mode().height());

        return device_status_value::ok;
    }

    device_status mock_ir_stream::on_read_into(void* dest, std::size_t size, std::int32_t timeout)
    {
        int width = active_mode().width();
        int height = active_mode().height();

        std::stringstream ss;
        ss << "infrared" << std::endl << active_mode() << std::endl;

        if (flags().has_flags())
        {
            ss << "(" << flags() << ")";
        }

        generator_->set_size(width, height);
        generator_->set_overlayText(ss.str());
        generator_->set_overlayColor(0, 255, 255, 255);
        generator_->generate();

        const std::uint8_t* src = reinterpret_cast<const std::uint8_t*>(generator_->pixels());

        orbbec::copy_rgba_to_rgb(reinterpret_cast<std::uint8_t*>(dest), src, width * height);

        return device_status_value::ok;
    }
}}
