#ifndef MOCK_GRAPHICS_H
#define MOCK_GRAPHICS_H

#include <cstdint>
#include <cstddef>

namespace orbbec {

    inline void copy_rgba_to_rgb(std::uint8_t* dest, const std::uint8_t* src, std::size_t pixelCount)
    {
        for(int i = 0; i < pixelCount; i++, dest+=3, src+=4)
        {
            dest[0] = src[0];
            dest[1] = src[1];
            dest[2] = src[2];
        }
    }

    inline void copy_rgb_to_rgba(
        std::uint8_t* dest,
        const std::uint8_t* src,
        std::size_t pixelCount,
        std::uint8_t alpha = 255)
    {
        for(int i = 0; i < pixelCount; i++, dest+=4, src+=3)
        {
            dest[0] = src[0];
            dest[1] = src[1];
            dest[2] = src[2];
            dest[3] = alpha;
        }
    }
}

#endif /* MOCK_GRAPHICS_H */
