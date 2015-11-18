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

class MultiFrameListener : public astra::frame_listener
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

    void update_depth(astra::frame& frame)
    {
        astra::pointframe pointFrame = frame.get<astra::pointframe>();

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

        astra_rgb_pixel_t* vizBuffer = visualizer_.get_output();
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

    void update_color(astra::frame& frame)
    {
        astra::colorframe colorFrame = frame.get<astra::colorframe>();

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

        const astra::rgb_pixel* color = colorFrame.data();
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

    void update_ir_16(astra::frame& frame)
    {
        astra::infraredframe_16 irFrame = frame.get<astra::infraredframe_16>();

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

    void update_ir_rgb(astra::frame& frame)
    {
        astra::infraredframe_rgb irFrame = frame.get<astra::infraredframe_rgb>();

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

        const astra::rgb_pixel* irRGB = irFrame.data();
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

    virtual void on_frame_ready(astra::stream_reader& reader,
                                astra::frame& frame) override
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

    void drawTo(sf::RenderWindow& window, sf::Vector2f origin, sf::Vector2f size)
    {
        int viewSize = (int)(size.x / 2.0f);

        sf::Vector2f windowSize = window.getView().getSize();

        if (depthView_.buffer != nullptr)
        {
            float depthScale = viewSize / (float)depthView_.width;
            //float height = depthView_.height * depthScale;
            int horzCenter = origin.y * windowSize.y;
            depthView_.sprite.setScale(depthScale, depthScale);
            depthView_.sprite.setPosition(origin.x * windowSize.x, horzCenter);
            window.draw(depthView_.sprite);
        }

        if (colorView_.buffer != nullptr)
        {
            float colorScale = viewSize / (float)colorView_.width;
            //float height = colorView_.height * colorScale;
            int horzCenter = origin.y * windowSize.y;
            colorView_.sprite.setScale(colorScale, colorScale);

            if (overlayDepth_)
            {
                colorView_.sprite.setPosition(origin.x * windowSize.x, horzCenter);
                colorView_.sprite.setColor(sf::Color(255, 255, 255, 128));
            }
            else
            {
                colorView_.sprite.setPosition(origin.x * windowSize.x + viewSize, horzCenter);
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

astra::depthstream configure_depth(astra::stream_reader& reader)
{
    auto depthStream = reader.stream<astra::depthstream>();

    //We don't have to set the mode to start the stream, but if you want to here is how:
    astra::imagestream_mode depthMode;

    depthMode.set_width(640);
    depthMode.set_height(480);
    depthMode.set_pixelFormat(astra_pixel_formats::ASTRA_PIXEL_FORMAT_DEPTH_MM);
    depthMode.set_fps(30);

    depthStream.set_mode(depthMode);

    return depthStream;
}

astra::infraredstream configure_ir(astra::stream_reader& reader, bool useRGB)
{
    auto irStream = reader.stream<astra::infraredstream>();

    astra::imagestream_mode irMode;

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

astra::colorstream configure_color(astra::stream_reader& reader)
{
    auto colorStream = reader.stream<astra::colorstream>();

    astra::imagestream_mode colorMode;

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
    sf::VideoMode windowed_mode(1800, 675*2);
    bool is_fullscreen = false;
    sf::RenderWindow window(windowed_mode, "Multi Sensor Viewer");

    astra::streamset streamset1("device/sensor0");
    astra::streamset streamset2("device/sensor1");
    astra::stream_reader reader1 = streamset1.create_reader();
    astra::stream_reader reader2 = streamset2.create_reader();

    reader1.stream<astra::pointstream>().start();
    reader2.stream<astra::pointstream>().start();

    auto depthStream1 = configure_depth(reader1);
    depthStream1.start();

    auto colorStream1 = configure_color(reader1);
    colorStream1.start();

    auto irStream1 = configure_ir(reader1, false);

    auto depthStream2 = configure_depth(reader2);
    depthStream2.start();

    auto colorStream2 = configure_color(reader2);
    colorStream2.start();

    auto irStream2 = configure_ir(reader2, false);

    MultiFrameListener listener1;
    listener1.set_mode(MODE_COLOR);

    MultiFrameListener listener2;
    listener2.set_mode(MODE_COLOR);

    reader1.add_listener(listener1);
    reader2.add_listener(listener2);

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
                            window.create(windowed_mode, "Multi Sensor Viewer", sf::Style::Default);
                        }
                        else
                        {
                            is_fullscreen = true;
                            window.create(fullscreen_mode, "Multi Sensor Viewer", fullscreenStyle);
                        }
                        break;
                    case sf::Keyboard::R:
                        depthStream1.enable_registration(!depthStream1.registration_enabled());
                        depthStream2.enable_registration(!depthStream2.registration_enabled());
                        break;
                    case sf::Keyboard::M:
                        {
                            bool newMirroring1 = !depthStream1.mirroring_enabled();
                            depthStream1.enable_mirroring(newMirroring1);
                            colorStream1.enable_mirroring(newMirroring1);
                            irStream1.enable_mirroring(newMirroring1);

                            bool newMirroring2 = !depthStream2.mirroring_enabled();
                            depthStream2.enable_mirroring(newMirroring2);
                            colorStream2.enable_mirroring(newMirroring2);
                            irStream2.enable_mirroring(newMirroring2);
                        }
                        break;
                    case sf::Keyboard::G:
                        colorStream1.stop();
                        configure_ir(reader1, false);
                        listener1.set_mode(MODE_IR_16);
                        irStream1.start();

                        colorStream2.stop();
                        configure_ir(reader2, false);
                        listener2.set_mode(MODE_IR_16);
                        irStream2.start();


                        break;
                    case sf::Keyboard::I:
                        colorStream1.stop();
                        configure_ir(reader1, true);
                        listener1.set_mode(MODE_IR_RGB);
                        irStream1.start();

                        colorStream2.stop();
                        configure_ir(reader2, true);
                        listener2.set_mode(MODE_IR_RGB);
                        irStream2.start();
                        break;
                    case sf::Keyboard::O:
                        listener1.toggle_depth_overlay();
                        if (listener1.get_overlayDepth()) {
                            depthStream1.enable_registration(true);
                        }
                        listener2.toggle_depth_overlay();
                        if (listener2.get_overlayDepth()) {
                            depthStream2.enable_registration(true);
                        }
                        break;
                    case sf::Keyboard::P:
                        listener1.toggle_Paused();
                        listener2.toggle_Paused();
                        break;
                    case sf::Keyboard::C:
                        if (event.key.control)
                        {
                            window.close();
                        }
                        else
                        {
                            irStream1.stop();
                            listener1.set_mode(MODE_COLOR);
                            colorStream1.start();

                            irStream2.stop();
                            listener2.set_mode(MODE_COLOR);
                            colorStream2.start();
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

        listener1.drawTo(window, sf::Vector2f(0.0f, 0.0f), sf::Vector2f(window.getSize().x, window.getSize().y / 2.0f));
        listener2.drawTo(window, sf::Vector2f(0.0f, 0.5f), sf::Vector2f(window.getSize().x, window.getSize().y / 2.0f));
        window.display();

        if (!shouldContinue)
        {
            window.close();
        }
    }

    astra::terminate();
    return 0;
}
