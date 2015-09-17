#ifndef ASTRA_STREAM_MODE_H
#define ASTRA_STREAM_MODE_H

#include <cstdint>

namespace astra { namespace devices {

    class stream_mode
    {
    public:
        stream_mode(std::uint32_t width,
                    std::uint32_t height,
                    std::uint32_t framesPerSecond,
                    std::uint32_t pixelFormatCode);

        std::uint32_t width() const;
        void set_width(std::uint32_t width);

        std::uint32_t height() const;
        void set_height(std::uint32_t height);

        std::uint32_t framesPerSecond() const;
        void set_framesPerSecond(std::uint32_t framesPerSecond);

        std::uint32_t pixelFormatCode() const;
        void set_pixelFormatCode(std::uint32_t pixelFormatCode);
    private:
        std::uint32_t width_{0};
        std::uint32_t height_{0};
        std::uint32_t framesPerSecond_{0};
        std::uint32_t pixelFormatCode_{0};
    };

    bool operator==(const stream_mode& lhs, const stream_mode& rhs);
    bool operator!=(const stream_mode& lhs, const stream_mode& rhs);

}}

#endif /* ASTRA_STREAM_MODE_H */
