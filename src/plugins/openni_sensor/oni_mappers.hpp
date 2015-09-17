#ifndef ONI_MAPPERS_H
#define ONI_MAPPERS_H

#include <tuple>
#include <OpenNI.h>
#include <AstraUL/streams/Image.h>
#include <cstdint>

namespace orbbec { namespace ni {

    std::tuple<astra_pixel_format_t, uint8_t> convert_format(const openni::PixelFormat& oniFormat);
    std::tuple<openni::PixelFormat, uint8_t> convert_format(const astra_pixel_format_t& format);

    astra::ImageStreamMode convert_mode(const openni::VideoMode& oniMode);
    openni::VideoMode convert_mode(const astra::ImageStreamMode& mode);
}}

#endif /* ONI_MAPPERS_H */
