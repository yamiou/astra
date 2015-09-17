#ifndef MOCK_INFRARED_GENERATOR_H
#define MOCK_INFRARED_GENERATOR_H

#include <SFML/Graphics.hpp>
#include "mock_frame_generator.hpp"

namespace orbbec { namespace mocks {

    class infrared_generator : public frame_generator
    {
    public:
        infrared_generator(std::uint32_t width, std::uint32_t height);
        virtual ~infrared_generator();

    private:
        virtual void on_generate() override;
    };
}}

#endif /* MOCK_INFRARED_GENERATOR_H */
