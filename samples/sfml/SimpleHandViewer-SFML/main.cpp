#include <SFML/Graphics.hpp>
#include <Sensekit/SenseKit.h>
#include <SensekitUL/SenseKitUL.h>
#include <sstream>
#include <iomanip>

class HandFrameListener : public sensekit::FrameReadyListener
{
public:
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
        sensekit::DepthFrame depthFrame = frame.get<sensekit::DepthFrame>();

        int width = depthFrame.resolutionX();
        int height = depthFrame.resolutionY();

        init_texture(width, height);

        const int16_t* depthPtr = depthFrame.data();
        for(int y = 0; y < height; y++)
        {
            for(int x = 0; x < width; x++)
            {
                int index = (x + y * width);
                int index4 = index * 4;

                int16_t depth = depthPtr[index];
                float normDepth = std::min(1.0f,std::max(0.0f,(depth - 400.0f) / 5600.0f));
                uint8_t value = 255*(1-normDepth);
                if (depth == 0)
                {
                    value = 0;
                }

                m_displayBuffer[index4] = value;
                m_displayBuffer[index4 + 1] = value;
                m_displayBuffer[index4 + 2] = value;
                m_displayBuffer[index4 + 3] = 255;
            }
        }

        m_texture.update(m_displayBuffer.get());
    }

    void processHandFrame(sensekit::Frame& frame)
    {
        sensekit::HandFrame handFrame = frame.get<sensekit::HandFrame>();

        m_handPoints = handFrame.handpoints();
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

    void drawHandPoints(sf::RenderWindow& window, float depthScale)
    {
        float radius = 16;
        auto size = window.getSize();
        sf::Color candidateColor(100, 250, 50);
        sf::Color lostColor(200, 50, 50);
        sf::Color trackingColor(10, 10, 200);

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
    long double m_frameDuration{ 0 };
    std::clock_t m_lastTimepoint { 0 };
    sf::Texture m_texture;
    sf::Sprite m_sprite;
    sf::Font m_font;

    using BufferPtr = std::unique_ptr < uint8_t[] >;
    BufferPtr m_displayBuffer{ nullptr };

    std::vector<sensekit::HandPoint> m_handPoints;

    int m_depthWidth { 0 };
    int m_depthHeight { 0 };
};

int main(int argc, char** argv)
{
    sensekit::SenseKit::initialize();

    sf::RenderWindow window(sf::VideoMode(1280, 960), "Hand Viewer");

    sensekit::Sensor sensor;
    sensekit::StreamReader reader = sensor.create_reader();

    HandFrameListener listener;

    reader.stream<sensekit::DepthStream>().start();
    reader.stream<sensekit::HandStream>().start();
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
