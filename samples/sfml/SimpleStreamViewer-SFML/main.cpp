// This file is part of the Orbbec Astra SDK [https://orbbec3d.com]
// Copyright (c) 2015 Orbbec 3D
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Be excellent to each other.
#include <SFML/Graphics.hpp>
#include <astra_core/astra_core.hpp>
#include <astra/astra.hpp>
#include "../../common/lit_depth_visualizer.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
#include <key_handler.h>

enum ColorMode
{
    MODE_COLOR,
    MODE_IR_16,
    MODE_IR_RGB,
};

class MultiFrameListener : public astra::FrameListener
{
public:
    using BufferPtr = std::unique_ptr<uint8_t[]>;

    struct stream_view
    {
        sf::Sprite sprite;
        sf::Texture texture;
        BufferPtr buffer;
        int width{0};
        int height{0};
    };

    MultiFrameListener()
    {
        lastTimepoint_ = clock_type::now();
    }

    void init_texture(int width, int height, stream_view& view)
    {
        if (view.buffer == nullptr || width != view.width || height != view.height)
        {
            view.width = width;
            view.height = height;

            // texture is RGBA
            int byteLength = width * height * 4;

            view.buffer = BufferPtr(new uint8_t[byteLength]);

            clear_view(view);

            view.texture.create(width, height);
            view.sprite.setTexture(view.texture);
            view.sprite.setPosition(0, 0);
        }
    }

    void clear_view(stream_view& view)
    {
        int byteLength = view.width * view.height * 4;

        memset(view.buffer.get(), 0, byteLength);
    }

    void check_fps()
    {
        const double frameWeight = 0.2;

        auto newTimepoint = clock_type::now();
        auto frameDuration = std::chrono::duration_cast<duration_type>(newTimepoint - lastTimepoint_);

        frameDuration_ = frameDuration * frameWeight + frameDuration_ * (1 - frameWeight);
        lastTimepoint_ = newTimepoint;

        double fps = 1.0 / frameDuration_.count();

        auto precision = std::cout.precision();
        std::cout << std::fixed
                  << std::setprecision(1)
                  << fps << " fps ("
                  << std::setprecision(2)
                  << frameDuration.count() * 1000 << " ms)"
                  << std::setprecision(precision)
                  << std::endl;
    }

    void update_depth(astra::Frame& frame)
    {
        astra::PointFrame pointFrame = frame.get<astra::PointFrame>();

        if (!pointFrame.is_valid())
        {
            clear_view(depthView_);
            depthView_.texture.update(depthView_.buffer.get());
            return;
        }

        int depthWidth = pointFrame.resolutionX();
        int depthHeight = pointFrame.resolutionY();

        init_texture(depthWidth, depthHeight, depthView_);

        if (isPaused_)
        {
            return;
        }

        visualizer_.update(pointFrame);

        astra::RgbPixel* vizBuffer = visualizer_.get_output();
        uint8_t* buffer = depthView_.buffer.get();
        for (int i = 0; i < depthWidth * depthHeight; i++)
        {
            int rgbaOffset = i * 4;
            buffer[rgbaOffset] = vizBuffer[i].r;
            buffer[rgbaOffset + 1] = vizBuffer[i].g;
            buffer[rgbaOffset + 2] = vizBuffer[i].b;
            buffer[rgbaOffset + 3] = 255;
        }

        depthView_.texture.update(depthView_.buffer.get());
    }

    void update_color(astra::Frame& frame)
    {
        astra::ColorFrame colorFrame = frame.get<astra::ColorFrame>();

        if (!colorFrame.is_valid())
        {
            clear_view(colorView_);
            colorView_.texture.update(colorView_.buffer.get());
            return;
        }

        int colorWidth = colorFrame.resolutionX();
        int colorHeight = colorFrame.resolutionY();

        init_texture(colorWidth, colorHeight, colorView_);

        if (isPaused_)
        {
            return;
        }

        const astra::RgbPixel* color = colorFrame.data();
        uint8_t* buffer = colorView_.buffer.get();
        for(int i = 0; i < colorWidth * colorHeight; i++)
        {
            int rgbaOffset = i * 4;
            buffer[rgbaOffset] = color[i].r;
            buffer[rgbaOffset + 1] = color[i].g;
            buffer[rgbaOffset + 2] = color[i].b;
            buffer[rgbaOffset + 3] = 255;
        }

        colorView_.texture.update(colorView_.buffer.get());
    }

    void update_ir_16(astra::Frame& frame)
    {
        astra::InfraredFrame16 irFrame = frame.get<astra::InfraredFrame16>();

        if (!irFrame.is_valid())
        {
            clear_view(colorView_);
            colorView_.texture.update(colorView_.buffer.get());
            return;
        }

        int irWidth = irFrame.resolutionX();
        int irHeight = irFrame.resolutionY();

        init_texture(irWidth, irHeight, colorView_);

        if (isPaused_)
        {
            return;
        }

        const uint16_t* ir_values = irFrame.data();
        uint8_t* buffer = colorView_.buffer.get();
        for (int i = 0; i < irWidth * irHeight; i++)
        {
            int rgbaOffset = i * 4;
            const uint16_t value = ir_values[i];
            const uint8_t red = static_cast<uint8_t>(value >> 2);
            const uint8_t blue = 0x66 - red / 2;
            buffer[rgbaOffset] = red;
            buffer[rgbaOffset + 1] = 0;
            buffer[rgbaOffset + 2] = blue;
            buffer[rgbaOffset + 3] = 255;
        }

        colorView_.texture.update(colorView_.buffer.get());
    }

    void update_ir_rgb(astra::Frame& frame)
    {
        astra::InfraredFrameRgb irFrame = frame.get<astra::InfraredFrameRgb>();

        if (!irFrame.is_valid())
        {
            clear_view(colorView_);
            colorView_.texture.update(colorView_.buffer.get());
            return;
        }

        int irWidth = irFrame.resolutionX();
        int irHeight = irFrame.resolutionY();

        init_texture(irWidth, irHeight, colorView_);

        if (isPaused_)
        {
            return;
        }

        const astra::RgbPixel* irRGB = irFrame.data();
        uint8_t* buffer = colorView_.buffer.get();
        for (int i = 0; i < irWidth * irHeight; i++)
        {
            int rgbaOffset = i * 4;
            buffer[rgbaOffset] = irRGB[i].r;
            buffer[rgbaOffset + 1] = irRGB[i].g;
            buffer[rgbaOffset + 2] = irRGB[i].b;
            buffer[rgbaOffset + 3] = 255;
        }

        colorView_.texture.update(colorView_.buffer.get());
    }

    virtual void on_frame_ready(astra::StreamReader& reader,
                                astra::Frame& frame) override
    {
        update_depth(frame);

        switch (colorMode_)
        {
        case MODE_COLOR:
            update_color(frame);
            break;
        case MODE_IR_16:
            update_ir_16(frame);
            break;
        case MODE_IR_RGB:
            update_ir_rgb(frame);
            break;
        }

        check_fps();
    }

    void drawTo(sf::RenderWindow& window)
    {
        int viewSize = (int)(window.getView().getSize().x / 2.0f);

        if (depthView_.buffer != nullptr)
        {
            float depthScale = viewSize / (float)depthView_.width;
            float height = depthView_.height * depthScale;
            int horzCenter = window.getView().getCenter().y - height / 2.0f;
            depthView_.sprite.setScale(depthScale, depthScale);
            depthView_.sprite.setPosition(0, horzCenter);
            window.draw(depthView_.sprite);
        }

        if (colorView_.buffer != nullptr)
        {
            float colorScale = viewSize / (float)colorView_.width;
            float height = colorView_.height * colorScale;
            int horzCenter = window.getView().getCenter().y - height / 2.0f;
            colorView_.sprite.setScale(colorScale, colorScale);

            if (overlayDepth_)
            {
                colorView_.sprite.setPosition(0, horzCenter);
                colorView_.sprite.setColor(sf::Color(255, 255, 255, 128));
            }
            else
            {
                colorView_.sprite.setPosition(viewSize, horzCenter);
                colorView_.sprite.setColor(sf::Color(255, 255, 255, 255));
            }
            window.draw(colorView_.sprite);
        }
    }

    void toggle_depth_overlay()
    {
        overlayDepth_ = !overlayDepth_;
    }

    bool get_overlayDepth() const
    {
        return overlayDepth_;
    }

    void toggle_Paused()
    {
        isPaused_ = !isPaused_;
    }

    bool get_isPaused() const
    {
        return isPaused_;
    }

    ColorMode get_mode() const { return colorMode_; }
    void set_mode(ColorMode mode) { colorMode_ = mode; }

private:
    samples::common::lit_depth_visualizer visualizer_;

    using duration_type = std::chrono::duration<double>;
    duration_type frameDuration_{0.0};

    using clock_type = std::chrono::system_clock;
    std::chrono::time_point<clock_type> lastTimepoint_;

    stream_view depthView_;
    stream_view colorView_;
    ColorMode colorMode_;
    bool overlayDepth_{ false };
    bool isPaused_{ false };
};

astra::DepthStream configure_depth(astra::StreamReader& reader)
{
    auto depthStream = reader.stream<astra::DepthStream>();

    //We don't have to set the mode to start the stream, but if you want to here is how:
    astra::ImageStreamMode depthMode;

    depthMode.set_width(640);
    depthMode.set_height(480);
    depthMode.set_pixelFormat(astra_pixel_formats::ASTRA_PIXEL_FORMAT_DEPTH_MM);
    depthMode.set_fps(30);

    depthStream.set_mode(depthMode);

    return depthStream;
}

astra::InfraredStream configure_ir(astra::StreamReader& reader, bool useRGB)
{
    auto irStream = reader.stream<astra::InfraredStream>();

    astra::ImageStreamMode irMode;

    irMode.set_width(640);
    irMode.set_height(480);
    if (useRGB)
    {
        irMode.set_pixelFormat(astra_pixel_formats::ASTRA_PIXEL_FORMAT_RGB888);
    }
    else
    {
        irMode.set_pixelFormat(astra_pixel_formats::ASTRA_PIXEL_FORMAT_GRAY16);
    }

    irMode.set_fps(30);

    irStream.set_mode(irMode);

    return irStream;
}

astra::ColorStream configure_color(astra::StreamReader& reader)
{
    auto colorStream = reader.stream<astra::ColorStream>();

    astra::ImageStreamMode colorMode;

    colorMode.set_width(640);
    colorMode.set_height(480);
    colorMode.set_pixelFormat(astra_pixel_formats::ASTRA_PIXEL_FORMAT_RGB888);
    colorMode.set_fps(30);

    colorStream.set_mode(colorMode);

    return colorStream;
}

int main(int argc, char** argv)
{
    astra::initialize();

    set_key_handler();

#ifdef _WIN32
    auto fullscreenStyle = sf::Style::None;
#else
    auto fullscreenStyle = sf::Style::Fullscreen;
#endif

    sf::VideoMode fullscreen_mode = sf::VideoMode::getFullscreenModes()[0];
    sf::VideoMode windowed_mode(1800, 675);
    bool is_fullscreen = false;
    sf::RenderWindow window(windowed_mode, "Stream Viewer");

    astra::StreamSet streamSet;
    astra::StreamReader reader = streamSet.create_reader();

    reader.stream<astra::PointStream>().start();

    auto depthStream = configure_depth(reader);
    depthStream.start();

    auto colorStream = configure_color(reader);
    colorStream.start();

    auto irStream = configure_ir(reader, false);

    MultiFrameListener listener;
    listener.set_mode(MODE_COLOR);

    reader.add_listener(listener);

    while (window.isOpen())
    {
        astra_temp_update();

        sf::Event event;
        while (window.pollEvent(event))
        {
            switch (event.type)
            {
            case sf::Event::Closed:
                window.close();
                break;
            case sf::Event::KeyPressed:
                {
                    switch (event.key.code)
                    {
                    case sf::Keyboard::Escape:
                        window.close();
                        break;
                    case sf::Keyboard::F:
                        if (is_fullscreen)
                        {
                            is_fullscreen = false;
                            window.create(windowed_mode, "Stream Viewer", sf::Style::Default);
                        }
                        else
                        {
                            is_fullscreen = true;
                            window.create(fullscreen_mode, "Stream Viewer", fullscreenStyle);
                        }
                        break;
                    case sf::Keyboard::R:
                        depthStream.enable_registration(!depthStream.registration_enabled());
                        break;
                    case sf::Keyboard::M:
                        {
                            bool newMirroring = !depthStream.mirroring_enabled();
                            depthStream.enable_mirroring(newMirroring);
                            colorStream.enable_mirroring(newMirroring);
                            irStream.enable_mirroring(newMirroring);
                        }
                        break;
                    case sf::Keyboard::G:
                        colorStream.stop();
                        configure_ir(reader, false);
                        listener.set_mode(MODE_IR_16);
                        irStream.start();
                        break;
                    case sf::Keyboard::I:
                        colorStream.stop();
                        configure_ir(reader, true);
                        listener.set_mode(MODE_IR_RGB);
                        irStream.start();
                        break;
                    case sf::Keyboard::O:
                        listener.toggle_depth_overlay();
                        if (listener.get_overlayDepth()) {
                            depthStream.enable_registration(true);
                        }
                        break;
                    case sf::Keyboard::P:
                        listener.toggle_Paused();
                        break;
                    case sf::Keyboard::C:
                        if (event.key.control)
                        {
                            window.close();
                        }
                        else
                        {
                            irStream.stop();
                            listener.set_mode(MODE_COLOR);
                            colorStream.start();
                        }
                        break;
                    default:
                        break;
                    }
                    break;
                }
            default:
                break;
            }
        }

        // clear the window with black color
        window.clear(sf::Color::Black);

        listener.drawTo(window);
        window.display();

        if (!shouldContinue)
        {
            window.close();
        }
    }

    astra::terminate();
    return 0;
}
