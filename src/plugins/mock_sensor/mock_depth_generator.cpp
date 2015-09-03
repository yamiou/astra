#include "mock_depth_generator.hpp"
#include <cassert>

namespace orbbec { namespace mocks {

    depth_generator::depth_generator(std::uint32_t width, std::uint32_t height)
        : frame_generator(width, height)
    { }

    depth_generator::~depth_generator() = default;

    void depth_generator::on_generate()
    {
        target().clear(sf::Color(128, 0, 0, 0));
    }
}}
