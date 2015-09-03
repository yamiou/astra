#include "mock_frame_generator.hpp"
#include <cassert>

namespace orbbec { namespace mocks {

    frame_generator::frame_generator(std::uint32_t width, std::uint32_t height)
        : width_(width), height_(height)
    {
        font_.loadFromFile("Inconsolata.otf");

        text_.setFont(font_);
        text_.setCharacterSize(28);
        text_.setColor(sf::Color::White);

        set_size(width, height);
    }

    frame_generator::~frame_generator() = default;

    const std::uint8_t* frame_generator::pixels() const
    {
        return image_.getPixelsPtr();
    }

    void frame_generator::set_size(std::uint32_t width, std::uint32_t height)
    {
        assert(width != 0);
        assert(height != 0);

        // 8k is enough for anyone
        assert(width <= 8096);
        assert(height <= 8096);

        width_ = width;
        height_ = height;

        if (target_.getSize() != sf::Vector2u(width_, height_))
        {
            target_.create(width_, height_);
        }
    }

    void frame_generator::generate()
    {
        target_.clear();

        on_generate();

        if (overlayText().length() > 0)
        {
            auto rect = text_.getLocalBounds();
            text_.setPosition(width_ / 2.0f - rect.width / 2.0f,
                              height_ / 2.0f - rect.height / 2.0f);

            target_.draw(text_);
        }

        target_.display();
        image_ = target_.getTexture().copyToImage();
    }
}}
