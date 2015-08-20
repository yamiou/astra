#ifndef ONI_MAPPERS_H
#define ONI_MAPPERS_H

#include <tuple>
#include <OpenNI.h>
#include <AstraUL/streams/image_types.h>
#include <cstdint>

namespace astra { namespace plugins {

    std::tuple<astra_pixel_format_t, uint8_t> convert_format(const openni::PixelFormat& oniFormat);
    std::tuple<openni::PixelFormat, uint8_t> convert_format(const astra_pixel_format_t& format);

    astra_imagestream_mode_t convert_mode(const openni::VideoMode& oniMode);
    openni::VideoMode convert_mode(const astra_imagestream_mode_t& mode);
}}

#endif /* ONI_MAPPERS_H */
