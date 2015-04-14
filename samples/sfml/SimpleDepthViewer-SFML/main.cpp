#include <SFML/Graphics.hpp>
#include <Sensekit/SenseKit.h>
#include <SensekitUL/SenseKitUL.h>

class SampleFrameListener : public sensekit::FrameReadyListener
{
public:
    void init_texture(int width, int height)
    {
        if (m_depthVizBuffer == nullptr || width != m_width || height != m_height)
        {
            m_width = width;
            m_height = height;
            int byteLength = m_width * m_height * 4;

            m_depthVizBuffer = DepthPtr( new uint8_t[byteLength] );
            memset(m_depthVizBuffer.get(), 0, byteLength);

            m_texture.create(m_width, m_height);
            m_sprite.setTexture(m_texture);
            m_sprite.setPosition(0, 0);
            m_sprite.setScale(2, 2);
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

    virtual void on_frame_ready(sensekit::StreamReader& reader,
                                sensekit::Frame& frame) override
        {
            sensekit::DepthFrame depthFrame = frame.get<sensekit::DepthFrame>();

            int width = depthFrame.get_resolutionX();
            int height = depthFrame.get_resolutionY();

            init_texture(width, height);

            const int16_t* depthPtr = depthFrame.data();
            for(int y = 0; y < height; y++)
            {
                for(int x = 0; x < width; x++)
                {
                    int index = (x + y * width);
                    int16_t depth = depthPtr[index];
                    uint8_t value = depth % 255;
                    m_depthVizBuffer[index * 4] = value;
                    m_depthVizBuffer[index * 4 + 1] = value;
                    m_depthVizBuffer[index * 4 + 2] = value;
                    m_depthVizBuffer[index * 4 + 3] = 255;
                }
            }

            m_texture.update(m_depthVizBuffer.get());
            check_fps();
        }

    void drawTo(sf::RenderWindow& window)
        {
            window.draw(m_sprite);
        }

private:
    long double m_frameDuration{ 0 };
    std::clock_t m_lastTimepoint { 0 };
    sf::Texture m_texture;
    sf::Sprite m_sprite;
    
    using DepthPtr = std::unique_ptr < uint8_t[] > ;
    DepthPtr m_depthVizBuffer { nullptr };
    int m_width { 0 };
    int m_height { 0 };
};

int main(int argc, char** argv)
{
    sensekit::SenseKit::initialize();

    sf::RenderWindow window(sf::VideoMode(1280, 960), "Depth Viewer");

    sensekit::Sensor sensor;
    sensekit::StreamReader reader = sensor.create_reader();

    SampleFrameListener listener;

    reader.stream<sensekit::DepthStream>().start();

    reader.addListener(listener);

    while (window.isOpen())
    {
        sensekit_temp_update();

        sf::Event event;
        while (window.pollEvent(event))
        {
            if (event.type == sf::Event::Closed)
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
