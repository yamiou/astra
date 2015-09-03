#ifndef MOCK_COLOR_GENERATOR_H
#define MOCK_COLOR_GENERATOR_H

#include <SFML/Graphics.hpp>
#include "mock_frame_generator.hpp"

namespace orbbec { namespace mocks {

    class color_generator : public frame_generator
    {
    public:
        color_generator(std::uint32_t width, std::uint32_t height);
        virtual ~color_generator();

    private:
        virtual void on_generate() override;
    };
}}

#endif /* MOCK_COLOR_GENERATOR_H */
