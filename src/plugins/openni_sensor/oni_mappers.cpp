#include "oni_mappers.hpp"

namespace sensekit { namespace plugins {

    std::tuple<sensekit_pixel_format_t, uint8_t> convert_format(const openni::PixelFormat& oniFormat)
    {
        switch(oniFormat)
        {
        case openni::PixelFormat::PIXEL_FORMAT_RGB888:
            return std::make_tuple(sensekit_pixel_formats::SENSEKIT_PIXEL_FORMAT_RGB888, 3);
        case openni::PixelFormat::PIXEL_FORMAT_YUV422:
            return std::make_tuple(sensekit_pixel_formats::SENSEKIT_PIXEL_FORMAT_YUV422, 2);
        case openni::PixelFormat::PIXEL_FORMAT_GRAY8:
            return std::make_tuple(sensekit_pixel_formats::SENSEKIT_PIXEL_FORMAT_GRAY8, 1);
        case openni::PixelFormat::PIXEL_FORMAT_GRAY16:
            return std::make_tuple(sensekit_pixel_formats::SENSEKIT_PIXEL_FORMAT_GRAY16, 2);
        case openni::PixelFormat::PIXEL_FORMAT_DEPTH_1_MM:
            return std::make_tuple(sensekit_pixel_formats::SENSEKIT_PIXEL_FORMAT_DEPTH_MM, 2);
        case openni::PixelFormat::PIXEL_FORMAT_JPEG:
            return std::make_tuple(sensekit_pixel_formats::SENSEKIT_PIXEL_FORMAT_UNKNOWN, 0);
        case openni::PixelFormat::PIXEL_FORMAT_YUYV:
            return std::make_tuple(sensekit_pixel_formats::SENSEKIT_PIXEL_FORMAT_YUYV, 2);
        case openni::PixelFormat::PIXEL_FORMAT_DEPTH_100_UM:
        default:
            return std::make_tuple(sensekit_pixel_formats::SENSEKIT_PIXEL_FORMAT_UNKNOWN, 0);
        }
    }

    std::tuple<openni::PixelFormat, uint8_t> convert_format(const sensekit_pixel_format_t& format)
    {
        switch(format)
        {
        case sensekit_pixel_formats::SENSEKIT_PIXEL_FORMAT_RGB888:
            return std::make_tuple(openni::PixelFormat::PIXEL_FORMAT_RGB888, 3);
        case sensekit_pixel_formats::SENSEKIT_PIXEL_FORMAT_YUV422:
            return std::make_tuple(openni::PixelFormat::PIXEL_FORMAT_YUV422, 2);
        case sensekit_pixel_formats::SENSEKIT_PIXEL_FORMAT_GRAY8:
            return std::make_tuple(openni::PixelFormat::PIXEL_FORMAT_GRAY8, 1);
        case sensekit_pixel_formats::SENSEKIT_PIXEL_FORMAT_GRAY16:
            return std::make_tuple(openni::PixelFormat::PIXEL_FORMAT_GRAY16, 2);
        case sensekit_pixel_formats::SENSEKIT_PIXEL_FORMAT_DEPTH_MM:
            return std::make_tuple(openni::PixelFormat::PIXEL_FORMAT_DEPTH_1_MM, 2);
        case sensekit_pixel_formats::SENSEKIT_PIXEL_FORMAT_UNKNOWN:
            return std::make_tuple(openni::PixelFormat::PIXEL_FORMAT_JPEG, 0);
        case sensekit_pixel_formats::SENSEKIT_PIXEL_FORMAT_YUYV:
            return std::make_tuple(openni::PixelFormat::PIXEL_FORMAT_YUYV, 2);
        default:
            return std::make_tuple(openni::PixelFormat(0), 0);
        }
    }

    sensekit_imagestream_mode_t convert_mode(const openni::VideoMode& oniMode)
    {
        sensekit_imagestream_mode_t mode;

        mode.width = oniMode.getResolutionX();
        mode.height = oniMode.getResolutionY();
        mode.fps = oniMode.getFps();

        sensekit_pixel_format_t format;
        std::uint8_t bpp;

        std::tie(format, bpp) = convert_format(oniMode.getPixelFormat());
        mode.pixelFormat = format;
        mode.bytesPerPixel = bpp;

        return mode;
    }

    openni::VideoMode convert_mode(const sensekit_imagestream_mode_t& mode)
    {
        openni::VideoMode oniMode;
        oniMode.setResolution(mode.width, mode.height);
        oniMode.setFps(mode.fps);

        openni::PixelFormat format;

        std::tie(format, std::ignore) = convert_format(mode.pixelFormat);
        oniMode.setPixelFormat(format);

        return oniMode;
    }
}}
