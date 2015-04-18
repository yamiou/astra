#include <SFML/Graphics.hpp>
#include <Sensekit/SenseKit.h>
#include <SensekitUL/SenseKitUL.h>

class HandDebugFrameListener : public sensekit::FrameReadyListener
{
public:
    void init_texture(int width, int height)
    {
        if (m_displayBuffer == nullptr || width != m_displayWidth || height != m_displayHeight)
        {
            m_displayWidth = width;
            m_displayHeight = height;
            int byteLength = m_displayWidth * m_displayHeight * 4;

            m_displayBuffer = BufferPtr(new uint8_t[byteLength]);
            memset(m_displayBuffer.get(), 0, byteLength);

            m_texture.create(m_displayWidth, m_displayHeight);
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

        printf("FPS: %3.1f (%3.4f ms)\n", fps, m_frameDuration * 1000);
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
                int16_t depth = depthPtr[index];
                uint8_t value = depth % 255;
                m_displayBuffer[index * 4] = value;
                m_displayBuffer[index * 4 + 1] = value;
                m_displayBuffer[index * 4 + 2] = value;
                m_displayBuffer[index * 4 + 3] = 255;
            }
        }

        m_texture.update(m_displayBuffer.get());
    }

    void processHands(sensekit::Frame& frame)
    {
        sensekit::HandFrame handFrame = frame.get<sensekit::HandFrame>();

        m_handPoints = handFrame.handpoints();
    }

    void processHandsDebug(sensekit::Frame& frame)
    {
        sensekit::HandDebugFrame handFrame = frame.get<sensekit::HandDebugFrame>();

        int width = handFrame.resolutionX();
        int height = handFrame.resolutionY();

        init_texture(width, height);

        const sensekit::RGBPixel* imagePtr = handFrame.data();
        for (int y = 0; y < height; y++)
        {
            for (int x = 0; x < width; x++)
            {
                int index = (x + y * width);
                int index4 = index * 4;
                sensekit::RGBPixel rgb = imagePtr[index];
                
                m_displayBuffer[index4] = rgb.r;
                m_displayBuffer[index4 + 1] = rgb.g;
                m_displayBuffer[index4 + 2] = rgb.b;
                m_displayBuffer[index4 + 3] = 255;
            }
        }

        m_texture.update(m_displayBuffer.get());
    }

    virtual void on_frame_ready(sensekit::StreamReader& reader,
                                sensekit::Frame& frame) override
        {
            //processDepth(frame);
            processHands(frame);
            processHandsDebug(frame);

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

    void drawHands(sf::RenderWindow& window)
    {
        float radius = 25;
        auto size = window.getSize();
        sf::Color trackingColor(100, 250, 50);
        sf::Color lostColor(200, 50, 50);

        for (auto handPoint : m_handPoints)
        {
            sf::Color& color = trackingColor;
            if (handPoint.status() == HAND_STATUS_LOST)
            {
                color = lostColor;
            }

            const sensekit::Vector2i& p = handPoint.depthPosition();

            drawCircle(window, radius, p.x * m_scale, p.y * m_scale, color);
        }
    }
    void drawTo(sf::RenderWindow& window)
    {
        if (m_displayBuffer != nullptr)
        {
            m_scale = window.getSize().x / m_displayWidth;

            m_sprite.setScale(m_scale, m_scale);

            window.draw(m_sprite);

            drawHands(window);
        }
    }

private:
    float m_scale{ 1 };
    long double m_frameDuration{ 0 };
    std::clock_t m_lastTimepoint { 0 };
    sf::Texture m_texture;
    sf::Sprite m_sprite;

    using BufferPtr = std::unique_ptr < uint8_t[] >;
    BufferPtr m_displayBuffer{ nullptr };
    BufferPtr m_debugBuffer{ nullptr };

    std::vector<sensekit::HandPoint> m_handPoints;

    int m_displayWidth { 0 };
    int m_displayHeight { 0 };
};

int main(int argc, char** argv)
{
    sensekit::SenseKit::initialize();

    sf::RenderWindow window(sf::VideoMode(1280, 960), "Hand Debug Viewer");

    sensekit::Sensor sensor;
    sensekit::StreamReader reader = sensor.create_reader();

    HandDebugFrameListener listener;

    reader.stream<sensekit::DepthStream>().start();
    reader.stream<sensekit::HandStream>().start();
    reader.stream<sensekit::HandDebugStream>().start();
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
