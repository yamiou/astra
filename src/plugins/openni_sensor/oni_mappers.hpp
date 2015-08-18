#ifndef ONI_MAPPERS_H
#define ONI_MAPPERS_H

#include <tuple>
#include <OpenNI.h>
#include <SenseKitUL/streams/image_types.h>
#include <cstdint>

namespace sensekit { namespace plugins {

    std::tuple<sensekit_pixel_format_t, uint8_t> convert_format(const openni::PixelFormat& oniFormat);
    std::tuple<openni::PixelFormat, uint8_t> convert_format(const sensekit_pixel_format_t& format);

    sensekit_imagestream_mode_t convert_mode(const openni::VideoMode& oniMode);
    openni::VideoMode convert_mode(const sensekit_imagestream_mode_t& mode);
}}

#endif /* ONI_MAPPERS_H */
