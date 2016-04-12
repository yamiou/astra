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
#include <astra/astra.hpp>
#include "../../common/lit_depth_visualizer.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
#include <key_handler.h>
#include <sstream>

class DepthFrameListener : public astra::FrameListener
{
public:
    DepthFrameListener(const astra::CoordinateMapper& coordinateMapper)
        : coordinateMapper_(coordinateMapper)
    {
        prev_ = ClockType::now();
        font_.loadFromFile("Inconsolata.otf");
    }

    void init_texture(int width, int height)
    {
        if (displayBuffer_ == nullptr || width != displayWidth_ || height != displayHeight_)
        {
            displayWidth_ = width;
            displayHeight_ = height;

            // texture is RGBA
            int byteLength = displayWidth_ * displayHeight_ * 4;

            displayBuffer_ = BufferPtr(new uint8_t[byteLength]);
            memset(displayBuffer_.get(), 0, byteLength);

            texture_.create(displayWidth_, displayHeight_);
            sprite_.setTexture(texture_);
            sprite_.setPosition(0, 0);
        }
    }

    void check_fps()
    {
        const float frameWeight = .2f;

        const ClockType::time_point now = ClockType::now();
        const float elapsedMillis = std::chrono::duration_cast<DurationType>(now - prev_).count();

        elapsedMillis_ = elapsedMillis * frameWeight + elapsedMillis_ * (1.f - frameWeight);
        prev_ = now;

        const double fps = 1000.f / elapsedMillis;

        const auto precision = std::cout.precision();

        std::cout << std::fixed
                  << std::setprecision(1)
                  << fps << " fps ("
                  << std::setprecision(1)
                  << elapsedMillis_ << " ms)"
                  << std::setprecision(precision)
                  << std::endl;
    }

    virtual void on_frame_ready(astra::StreamReader& reader,
                                astra::Frame& frame) override
    {
        astra::PointFrame pointFrame = frame.get<astra::PointFrame>();

        int width = pointFrame.resolutionX();
        int height = pointFrame.resolutionY();

        init_texture(width, height);

        check_fps();

        if (isPaused_)
        {
            return;
        }

        copy_depth_data(frame);

        visualizer_.update(pointFrame);

        astra_rgb_pixel_t* vizBuffer = visualizer_.get_output();
        for (int i = 0; i < width * height; i++)
        {
            int rgbaOffset = i * 4;
            displayBuffer_[rgbaOffset] = vizBuffer[i].r;
            displayBuffer_[rgbaOffset + 1] = vizBuffer[i].b;
            displayBuffer_[rgbaOffset + 2] = vizBuffer[i].g;
            displayBuffer_[rgbaOffset + 3] = 255;
        }
        texture_.update(displayBuffer_.get());
    }

    void copy_depth_data(astra::Frame& frame)
    {
        astra::DepthFrame depthFrame = frame.get<astra::DepthFrame>();

        if (depthFrame.is_valid())
        {
            int width = depthFrame.resolutionX();
            int height = depthFrame.resolutionY();
            if (depthData_ == nullptr || width != depthWidth_ || height != depthHeight_)
            {
                depthWidth_ = width;
                depthHeight_ = height;

                // texture is RGBA
                int byteLength = depthWidth_ * depthHeight_ * sizeof(uint16_t);

                depthData_ = DepthPtr(new int16_t[byteLength]);
            }
            depthFrame.copy_to(depthData_.get());
        }
    }

    void update_mouse_position(sf::RenderWindow& window)
    {
        sf::Vector2i position = sf::Mouse::getPosition(window);
        auto windowSize = window.getSize();

        mouseNormX_ = position.x / static_cast<float>(windowSize.x);
        mouseNormY_ = position.y / static_cast<float>(windowSize.y);
    }

    void drawText(sf::RenderWindow& window, sf::Text& text, sf::Color color, int x, int y)
    {
        text.setColor(sf::Color::Black);
        text.setPosition(x + 5, y + 5);
        window.draw(text);

        text.setColor(color);
        text.setPosition(x, y);
        window.draw(text);
    }

    void drawMouseOverlay(sf::RenderWindow& window, float depthWScale, float depthHScale)
    {
        if (!isMouseOverlayEnabled_ || depthData_ == nullptr)
        {
            return;
        }
        int mouseX = depthWidth_ * mouseNormX_;
        int mouseY = depthHeight_ * mouseNormY_;

        if (mouseX >= depthWidth_ || mouseY >= depthHeight_ ||
            mouseX < 0 || mouseY < 0)
        {
            return;
        }

        size_t index = (depthWidth_ * mouseY + mouseX);

        int z = depthData_[index];

        float worldX, worldY, worldZ;
        coordinateMapper_.convert_depth_to_world(static_cast<float>(mouseX),
                                                 static_cast<float>(mouseY),
                                                 static_cast<float>(z),
                                                 &worldX,
                                                 &worldY,
                                                 &worldZ);

        std::stringstream str;
        str << std::fixed << std::setprecision(0)
            << "(" << mouseX << ", " << mouseY << ") "
            << "X:" << worldX << " Y:" << worldY << " Z:" << worldZ;

        sf::Text text(str.str(), font_);

        int characterSize = 40;
        text.setCharacterSize(characterSize);
        text.setStyle(sf::Text::Bold);

        float display_x = 10;
        float display_y = window.getView().getSize().y - 10 - characterSize;
        drawText(window, text, sf::Color::White, display_x, display_y);
    }

    void drawTo(sf::RenderWindow& window)
    {
        if (displayBuffer_ != nullptr)
        {
            float depthWScale = window.getView().getSize().x / displayWidth_;
            float depthHScale = window.getView().getSize().y / displayHeight_;

            sprite_.setScale(depthWScale, depthHScale);

            window.draw(sprite_);

            drawMouseOverlay(window, depthWScale, depthHScale);
        }
    }

    void toggle_isPaused()
    {
        isPaused_ = !isPaused_;
    }

    bool get_isPaused() const
    {
        return isPaused_;
    }

    void toggle_isMouseOverlayEnabled()
    {
        isMouseOverlayEnabled_ = !isMouseOverlayEnabled_;
    }

    bool get_isMouseOverlayEnabled() const
    {
        return isMouseOverlayEnabled_;
    }

private:
    samples::common::lit_depth_visualizer visualizer_;

    using DurationType = std::chrono::milliseconds;
    using ClockType = std::chrono::high_resolution_clock;

    ClockType::time_point prev_;
    float elapsedMillis_{.0f};

    sf::Texture texture_;
    sf::Sprite sprite_;
    sf::Font font_;

    const astra::CoordinateMapper& coordinateMapper_;

    int displayWidth_{0};
    int displayHeight_{0};

    using BufferPtr = std::unique_ptr<uint8_t[]>;
    BufferPtr displayBuffer_{nullptr};

    int depthWidth_{0};
    int depthHeight_{0};

    using DepthPtr = std::unique_ptr<int16_t[]>;
    DepthPtr depthData_{nullptr};

    float mouseNormX_{0};
    float mouseNormY_{0};
    bool isPaused_{false};
    bool isMouseOverlayEnabled_{true};
};

int main(int argc, char** argv)
{
    astra::initialize();

    set_key_handler();

    sf::RenderWindow window(sf::VideoMode(1280, 960), "Depth Viewer");

#ifdef _WIN32
    auto fullscreenStyle = sf::Style::None;
#else
    auto fullscreenStyle = sf::Style::Fullscreen;
#endif

    sf::VideoMode fullscreen_mode = sf::VideoMode::getFullscreenModes()[0];
    sf::VideoMode windowed_mode(1280, 1024);
    bool is_fullscreen = false;

    astra::StreamSet streamSet;
    astra::StreamReader reader = streamSet.create_reader();
    reader.stream<astra::PointStream>().start();

    auto depthStream = reader.stream<astra::DepthStream>();
    depthStream.start();

    auto coordinateMapper = depthStream.coordinateMapper();
    DepthFrameListener listener(coordinateMapper);

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
                if (event.key.code == sf::Keyboard::C && event.key.control)
                {
                    window.close();
                }
                switch (event.key.code)
                {
                case sf::Keyboard::Escape:
                    window.close();
                    break;
                case sf::Keyboard::F:
                    if (is_fullscreen)
                    {
                        is_fullscreen = false;
                        window.create(windowed_mode, "Depth Viewer", sf::Style::Default);
                    }
                    else
                    {
                        is_fullscreen = true;
                        window.create(fullscreen_mode, "Depth Viewer", fullscreenStyle);
                    }
                    break;
                case sf::Keyboard::R:
                    depthStream.enable_registration(!depthStream.registration_enabled());
                    break;
                case sf::Keyboard::M:
                    depthStream.enable_mirroring(!depthStream.mirroring_enabled());
                    break;
                case sf::Keyboard::P:
                    listener.toggle_isPaused();
                    break;
                case sf::Keyboard::Space:
                    listener.toggle_isMouseOverlayEnabled();
                    break;
                default:
                    break;
                }
                break;
            }
            case sf::Event::MouseMoved:
            {
                listener.update_mouse_position(window);
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
