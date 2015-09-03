#include "mock_color_generator.hpp"
#include <cassert>

namespace orbbec { namespace mocks {

    color_generator::color_generator(std::uint32_t width, std::uint32_t height)
        : frame_generator(width, height)
    { }

    color_generator::~color_generator() = default;

    void color_generator::on_generate()
    {
        target().clear(sf::Color(128, 0, 0, 255));
    }
}}
