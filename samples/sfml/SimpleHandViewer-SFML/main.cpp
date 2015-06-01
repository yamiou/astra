#include <SFML/Graphics.hpp>
#include <Sensekit/SenseKit.h>
#include <SensekitUL/SenseKitUL.h>
#include "../../common/LitDepthVisualizer.h"
#include <sstream>
#include <iomanip>
#include <deque>
#include <unordered_map>

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

class HandFrameListener : public sensekit::FrameReadyListener
{
public:
    using PointList = std::deque < sensekit::Vector2i >;
    using PointMap = std::unordered_map < int, PointList >;

    HandFrameListener()
    {
        m_font.loadFromFile("Inconsolata.otf");
    }

    HandFrameListener(const HandFrameListener&) = delete;
    HandFrameListener& operator=(const HandFrameListener&) = delete;

    void init_texture(int width, int height)
    {
        if (m_displayBuffer == nullptr || width != m_depthWidth || height != m_depthHeight)
        {
            m_depthWidth = width;
            m_depthHeight = height;
            int byteLength = m_depthWidth * m_depthHeight * 4;

            m_displayBuffer = BufferPtr(new uint8_t[byteLength]);
            memset(m_displayBuffer.get(), 0, byteLength);

            m_texture.create(m_depthWidth, m_depthHeight);
            m_sprite.setTexture(m_texture);
            m_sprite.setPosition(0, 0);
        }
    }

    void check_fps()
    {
        double fpsFactor = 0.02;

        std::clock_t newTimepoint= std::clock();
        long double frameDuration = (newTimepoint - m_lastTimepoint) / static_cast<long double>(CLOCKS_PER_SEC);

        m_frameDuration = frameDuration * fpsFactor + m_frameDuration * (1 - fpsFactor);
        m_lastTimepoint = newTimepoint;
        double fps = 1.0 / m_frameDuration;

        printf("FPS: %3.1f (%3.4Lf ms)\n", fps, m_frameDuration * 1000);
    }

    void processDepth(sensekit::Frame& frame)
    {
        sensekit::PointFrame pointFrame = frame.get<sensekit::PointFrame>();

        int width = pointFrame.resolutionX();
        int height = pointFrame.resolutionY();

        init_texture(width, height);

        m_visualizer.update(pointFrame);
        sensekit_rgb_pixel_t* vizBuffer = m_visualizer.get_output();

        for (int i = 0; i < width * height; i++)
        {
            int rgbaOffset = i * 4;
            m_displayBuffer[rgbaOffset] = vizBuffer[i].r;
            m_displayBuffer[rgbaOffset + 1] = vizBuffer[i].b;
            m_displayBuffer[rgbaOffset + 2] = vizBuffer[i].g;
            m_displayBuffer[rgbaOffset + 3] = 255;
        }

        m_texture.update(m_displayBuffer.get());
    }

    void updateHandTrace(int trackingId, const sensekit::Vector2i& position)
    {
        auto it = m_pointMap.find(trackingId);
        if (it == m_pointMap.end())
        {
            PointList list;
            for (int i = 0; i < m_maxTraceLength; i++)
            {
                list.push_back(position);
            }
            m_pointMap.insert(std::make_pair(trackingId, list));
        }
        else
        {
            PointList& list = it->second;
            while (list.size() < m_maxTraceLength)
            {
                list.push_back(position);
            }
        }
    }

    void shortenHandTraces()
    {
        auto it = m_pointMap.begin();

        while (it != m_pointMap.end())
        {
            PointList& list = it->second;
            if (list.size() > 1)
            {
                list.pop_front();
                ++it;
            }
            else
            {
                it = m_pointMap.erase(it);
            }
        }
    }

    void processHandFrame(sensekit::Frame& frame)
    {
        sensekit::HandFrame handFrame = frame.get<sensekit::HandFrame>();

        m_handPoints = handFrame.handpoints();

        shortenHandTraces();
        for (auto handPoint : m_handPoints)
        {
            if (handPoint.status() == HAND_STATUS_TRACKING)
            {
                updateHandTrace(handPoint.trackingId(), handPoint.depthPosition());
            }
        }
    }

    virtual void on_frame_ready(sensekit::StreamReader& reader,
                                sensekit::Frame& frame) override
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

    void drawHandLabel(sf::RenderWindow& window, float radius, float x, float y, sensekit::HandPoint& handPoint)
    {
        int32_t trackingId = handPoint.trackingId();
        std::stringstream str;
        str << trackingId;
        if (handPoint.status() == HAND_STATUS_LOST)
        {
            str << " Lost";
        }
        sf::Text label(str.str(), m_font);
        int characterSize = 60;
        label.setCharacterSize(characterSize);

        auto bounds = label.getLocalBounds();
        label.setOrigin(bounds.left + bounds.width / 2.0, characterSize);
        drawShadowText(window, label, sf::Color::White, x, y - radius - 10);
    }

    void drawHandPosition(sf::RenderWindow& window, float radius, float x, float y, sensekit::HandPoint& handPoint)
    {
        auto worldPosition = handPoint.worldPosition();
        std::stringstream str;
        str << std::fixed << std::setprecision(0);
        str << worldPosition.x << "," << worldPosition.y << "," << worldPosition.z;
        sf::Text label(str.str(), m_font);
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

        sensekit::Vector2i lastPoint = *it;
        while (it != pointList.end())
        {
            sensekit::Vector2i currentPoint = *it;
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

    void drawHandPoints(sf::RenderWindow& window, float depthScale)
    {
        float radius = 16;
        sf::Color candidateColor(255, 255, 0);
        sf::Color lostColor(255, 0, 0);
        sf::Color trackingColor(128, 138, 0);

        for (auto handPoint : m_handPoints)
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

            const sensekit::Vector2i& p = handPoint.depthPosition();

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
        for (auto it : m_pointMap)
        {
            PointList& list = it.second;
            drawHandTrace(window, list, lineColor, depthScale);
        }
    }

    void drawTo(sf::RenderWindow& window)
    {
        if (m_displayBuffer != nullptr)
        {
            float depthScale = window.getView().getSize().x / m_depthWidth;

            m_sprite.setScale(depthScale, depthScale);

            window.draw(m_sprite);

            drawHandPoints(window, depthScale);
        }
    }

private:
    samples::common::LitDepthVisualizer m_visualizer;

    long double m_frameDuration{ 0 };
    std::clock_t m_lastTimepoint { 0 };
    sf::Texture m_texture;
    sf::Sprite m_sprite;
    sf::Font m_font;

    using BufferPtr = std::unique_ptr < uint8_t[] >;
    BufferPtr m_displayBuffer{ nullptr };

    std::vector<sensekit::HandPoint> m_handPoints;

    PointMap m_pointMap;

    int m_depthWidth { 0 };
    int m_depthHeight { 0 };
    int m_maxTraceLength{ 15 };
};

int main(int argc, char** argv)
{
    sensekit::SenseKit::initialize();

    sf::RenderWindow window(sf::VideoMode(1280, 960), "Hand Viewer");

    sensekit::Sensor sensor;
    sensekit::StreamReader reader = sensor.create_reader();

    reader.stream<sensekit::PointStream>().start();
    reader.stream<sensekit::HandStream>().start();

    HandFrameListener listener;

    reader.addListener(listener);

    while (window.isOpen())
    {
        sensekit_temp_update();

        sf::Event event;
        while (window.pollEvent(event))
        {
            if (event.type == sf::Event::Closed)
                window.close();
            if ((event.type == sf::Event::KeyPressed) && (event.key.code == sf::Keyboard::Escape))
                window.close();
        }

        // clear the window with black color
        window.clear(sf::Color::Black);

        listener.drawTo(window);
        window.display();
    }

    sensekit::SenseKit::terminate();

    return 0;
}
