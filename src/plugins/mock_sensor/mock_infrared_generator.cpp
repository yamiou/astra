#include "mock_infrared_generator.hpp"
#include <cassert>

namespace orbbec { namespace mocks {

    infrared_generator::infrared_generator(std::uint32_t width, std::uint32_t height)
        : frame_generator(width, height)
    { }

    infrared_generator::~infrared_generator() = default;

    void infrared_generator::on_generate()
    {
        target().clear(sf::Color(128, 128, 128, 255));
    }
}}
