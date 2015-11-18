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
#include <sstream>
#include <iomanip>
#include <deque>
#include <unordered_map>
#include <key_handler.h>

class sfLine : public sf::Drawable
{
public:
    sfLine(const sf::Vector2f& point1, const sf::Vector2f& point2, sf::Color color, float thickness)
        : color(color)
    {
        sf::Vector2f direction = point2 - point1;
        sf::Vector2f unitDirection = direction / std::sqrt(direction.x*direction.x + direction.y*direction.y);
        sf::Vector2f unitPerpendicular(-unitDirection.y, unitDirection.x);

        sf::Vector2f offset = (thickness / 2.f)*unitPerpendicular;

        vertices[0].position = point1 + offset;
        vertices[1].position = point2 + offset;
        vertices[2].position = point2 - offset;
        vertices[3].position = point1 - offset;

        for (int i = 0; i<4; ++i)
            vertices[i].color = color;
    }

    void draw(sf::RenderTarget &target, sf::RenderStates states) const
    {
        target.draw(vertices, 4, sf::Quads);
    }

private:
    sf::Vertex vertices[4];
    sf::Color color;
};

class HandFrameListener : public astra::frame_listener
{
public:
    using PointList = std::deque < astra::vector2i >;
    using PointMap = std::unordered_map < int, PointList >;

    HandFrameListener()
    {
        font_.loadFromFile("Inconsolata.otf");
    }

    HandFrameListener(const HandFrameListener&) = delete;
    HandFrameListener& operator=(const HandFrameListener&) = delete;

    void init_texture(int width, int height)
    {
        if (displayBuffer_ == nullptr || width != depthWidth_ || height != depthHeight_)
        {
            depthWidth_ = width;
            depthHeight_ = height;
            int byteLength = depthWidth_ * depthHeight_ * 4;

            displayBuffer_ = BufferPtr(new uint8_t[byteLength]);
            memset(displayBuffer_.get(), 0, byteLength);

            texture_.create(depthWidth_, depthHeight_);
            sprite_.setTexture(texture_);
            sprite_.setPosition(0, 0);
        }
    }

    void check_fps()
    {
        double fpsFactor = 0.02;

        std::clock_t newTimepoint= std::clock();
        long double frameDuration = (newTimepoint - lastTimepoint_) / static_cast<long double>(CLOCKS_PER_SEC);

        frameDuration_ = frameDuration * fpsFactor + frameDuration_ * (1 - fpsFactor);
        lastTimepoint_ = newTimepoint;
        double fps = 1.0 / frameDuration_;

        printf("FPS: %3.1f (%3.4Lf ms)\n", fps, frameDuration_ * 1000);
    }

    void processDepth(astra::frame& frame)
    {
        astra::pointframe pointFrame = frame.get<astra::pointframe>();

        int width = pointFrame.resolutionX();
        int height = pointFrame.resolutionY();

        init_texture(width, height);

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

    void updateHandTrace(int trackingId, const astra::vector2i& position)
    {
        auto it = pointMap_.find(trackingId);
        if (it == pointMap_.end())
        {
            PointList list;
            for (int i = 0; i < maxTraceLength_; i++)
            {
                list.push_back(position);
            }
            pointMap_.insert(std::make_pair(trackingId, list));
        }
        else
        {
            PointList& list = it->second;
            while (list.size() < maxTraceLength_)
            {
                list.push_back(position);
            }
        }
    }

    void shortenHandTraces()
    {
        auto it = pointMap_.begin();

        while (it != pointMap_.end())
        {
            PointList& list = it->second;
            if (list.size() > 1)
            {
                list.pop_front();
                ++it;
            }
            else
            {
                it = pointMap_.erase(it);
            }
        }
    }

    void processHandFrame(astra::frame& frame)
    {
        astra::handframe handFrame = frame.get<astra::handframe>();

        handPoints_ = handFrame.handpoints();

        shortenHandTraces();
        for (auto handPoint : handPoints_)
        {
            if (handPoint.status() == HAND_STATUS_TRACKING)
            {
                updateHandTrace(handPoint.trackingId(), handPoint.depthPosition());
            }
        }
    }

    virtual void on_frame_ready(astra::stream_reader& reader,
                                astra::frame& frame) override
    {
        processDepth(frame);
        processHandFrame(frame);

        check_fps();
    }

    void drawCircle(sf::RenderWindow& window, float radius, float x, float y, sf::Color color)
    {
        sf::CircleShape shape(radius);

        shape.setFillColor(color);

        shape.setOrigin(radius, radius);
        shape.setPosition(x, y);
        window.draw(shape);
    }

    void drawShadowText(sf::RenderWindow& window, sf::Text& text, sf::Color color, int x, int y)
    {
        text.setColor(sf::Color::Black);
        text.setPosition(x + 5, y + 5);
        window.draw(text);

        text.setColor(color);
        text.setPosition(x, y);
        window.draw(text);
    }

    void drawHandLabel(sf::RenderWindow& window, float radius, float x, float y, astra::handpoint& handPoint)
    {
        int32_t trackingId = handPoint.trackingId();
        std::stringstream str;
        str << trackingId;
        if (handPoint.status() == HAND_STATUS_LOST)
        {
            str << " Lost";
        }
        sf::Text label(str.str(), font_);
        int characterSize = 60;
        label.setCharacterSize(characterSize);

        auto bounds = label.getLocalBounds();
        label.setOrigin(bounds.left + bounds.width / 2.0, characterSize);
        drawShadowText(window, label, sf::Color::White, x, y - radius - 10);
    }

    void drawHandPosition(sf::RenderWindow& window, float radius, float x, float y, astra::handpoint& handPoint)
    {
        auto worldPosition = handPoint.worldPosition();
        std::stringstream str;
        str << std::fixed << std::setprecision(0);
        str << worldPosition.x << "," << worldPosition.y << "," << worldPosition.z;
        sf::Text label(str.str(), font_);
        int characterSize = 60;
        label.setCharacterSize(characterSize);

        auto bounds = label.getLocalBounds();
        label.setOrigin(bounds.left + bounds.width / 2.0, 0);
        drawShadowText(window, label, sf::Color::White, x, y + radius + 10);
    }

    void drawHandTrace(sf::RenderWindow& window, const PointList& pointList, const sf::Color& color, const float depthScale)
    {
        if (pointList.size() < 2)
        {
            return;
        }

        float thickness = 4;
        auto it = pointList.begin();

        astra::vector2i lastPoint = *it;
        while (it != pointList.end())
        {
            astra::vector2i currentPoint = *it;
            ++it;

            sf::Vector2f p1((lastPoint.x + 0.5) * depthScale,
                            (lastPoint.y + 0.5) * depthScale);
            sf::Vector2f p2((currentPoint.x + 0.5) * depthScale,
                            (currentPoint.y + 0.5) * depthScale);
            lastPoint = currentPoint;
            sfLine line(p1, p2, color, thickness);
            window.draw(line);
        }
    }

    void drawhandpoints(sf::RenderWindow& window, float depthScale)
    {
        float radius = 16;
        sf::Color candidateColor(255, 255, 0);
        sf::Color lostColor(255, 0, 0);
        sf::Color trackingColor(0, 139, 69);

        for (auto handPoint : handPoints_)
        {
            sf::Color color = trackingColor;
            if (handPoint.status() == HAND_STATUS_LOST)
            {
                color = lostColor;
            }
            else if (handPoint.status() == HAND_STATUS_CANDIDATE)
            {
                color = candidateColor;
            }

            const astra::vector2i& p = handPoint.depthPosition();

            float circleX = (p.x + 0.5) * depthScale;
            float circleY = (p.y + 0.5) * depthScale;

            drawCircle(window, radius, circleX, circleY, color);

            drawHandLabel(window, radius, circleX, circleY, handPoint);
            if (handPoint.status() == HAND_STATUS_TRACKING)
            {
                drawHandPosition(window, radius, circleX, circleY, handPoint);
            }
        }

        sf::Color lineColor(0, 0, 255);
        for (auto it : pointMap_)
        {
            PointList& list = it.second;
            drawHandTrace(window, list, lineColor, depthScale);
        }
    }

    void drawTo(sf::RenderWindow& window)
    {
        if (displayBuffer_ != nullptr)
        {
            float depthScale = window.getView().getSize().x / depthWidth_;

            sprite_.setScale(depthScale, depthScale);

            window.draw(sprite_);

            drawhandpoints(window, depthScale);
        }
    }

private:
    samples::common::lit_depth_visualizer visualizer_;

    long double frameDuration_{ 0 };
    std::clock_t lastTimepoint_ { 0 };
    sf::Texture texture_;
    sf::Sprite sprite_;
    sf::Font font_;

    using BufferPtr = std::unique_ptr < uint8_t[] >;
    BufferPtr displayBuffer_{ nullptr };

    std::vector<astra::handpoint> handPoints_;

    PointMap pointMap_;

    int depthWidth_ { 0 };
    int depthHeight_ { 0 };
    int maxTraceLength_{ 15 };
};

int main(int argc, char** argv)
{
    astra::initialize();

    set_key_handler();

    sf::RenderWindow window(sf::VideoMode(1280, 960), "Hand Viewer");

    astra::streamset streamset;
    astra::stream_reader reader = streamset.create_reader();

    reader.stream<astra::pointstream>().start();
    reader.stream<astra::handstream>().start();

    HandFrameListener listener;

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
                    if (event.key.code == sf::Keyboard::Escape ||
                        (event.key.code == sf::Keyboard::C && event.key.control))
                    {
                        window.close();
                    }
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
