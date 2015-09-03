#include "astra_stream_mode.hpp"

namespace astra { namespace devices {

    stream_mode::stream_mode(std::uint32_t width,
                             std::uint32_t height,
                             std::uint32_t framesPerSecond,
                             std::uint32_t pixelFormatCode)
        : width_(width),
          height_(height),
          framesPerSecond_(framesPerSecond),
          pixelFormatCode_(pixelFormatCode)
    {
    }

    std::uint32_t stream_mode::width() const
    {
        return width_;
    }

    void stream_mode::set_width(std::uint32_t width)
    {
        width_ = width;
    }

    std::uint32_t stream_mode::height() const
    {
        return height_;
    }

    void stream_mode::set_height(std::uint32_t height)
    {
        height_ = height;
    }

    std::uint32_t stream_mode::framesPerSecond() const
    {
        return framesPerSecond_;
    }

    void stream_mode::set_framesPerSecond(std::uint32_t framesPerSecond)
    {
        framesPerSecond_ = framesPerSecond;
    }

    std::uint32_t stream_mode::pixelFormatCode() const
    {
        return pixelFormatCode_;
    }

    void stream_mode::set_pixelFormatCode(std::uint32_t pixelFormatCode)
    {
        pixelFormatCode_ = pixelFormatCode;
    }

    bool operator==(const stream_mode& lhs, const stream_mode& rhs)
    {
        return
            lhs.width() == rhs.width() &&
            lhs.height() == rhs.height() &&
            lhs.framesPerSecond() == rhs.framesPerSecond() &&
            lhs.pixelFormatCode() == rhs.pixelFormatCode();
    }

    bool operator!=(const stream_mode& lhs, const stream_mode& rhs)
    {
        return !(lhs == rhs);
    }



}}
