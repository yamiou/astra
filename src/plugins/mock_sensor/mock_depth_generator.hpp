#ifndef MOCK_DEPTH_GENERATOR_H
#define MOCK_DEPTH_GENERATOR_H

#include <SFML/Graphics.hpp>
#include "mock_frame_generator.hpp"

namespace orbbec { namespace mocks {

    class depth_generator : public frame_generator
    {
    public:
        depth_generator(std::uint32_t width, std::uint32_t height);
        virtual ~depth_generator();

    private:
        virtual void on_generate() override;
    };
}}

#endif /* MOCK_DEPTH_GENERATOR_H */
