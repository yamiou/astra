#include <SFML/Graphics.hpp>
#include <Sensekit/SenseKit.h>
#include <SensekitUL/SenseKitUL.h>
#include "../../common/LitDepthVisualizer.h"

class DepthFrameListener : public sensekit::FrameReadyListener
{
public:
    DepthFrameListener(sensekit::DepthStream& depthStream)
        : m_visualizerPtr(new samples::common::LitDepthVisualizer(depthStream))
    {
        //    m_visualizerPtr->set_light_color(sensekit_rgb_pixel_t{255,0,0});
    }

    void init_texture(int width, int height)
    {
        if (m_displayBuffer == nullptr || width != m_displayWidth || height != m_displayHeight)
        {
            m_displayWidth = width;
            m_displayHeight = height;

            // texture is RGBA
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

        printf("FPS: %3.1f (%3.4Lf ms)\n", fps, m_frameDuration * 1000);
    }

    virtual void on_frame_ready(sensekit::StreamReader& reader,
                                sensekit::Frame& frame) override
    {
        sensekit::DepthFrame depthFrame = frame.get<sensekit::DepthFrame>();

        int width = depthFrame.resolutionX();
        int height = depthFrame.resolutionY();

        init_texture(width, height);
        m_visualizerPtr->update(depthFrame);
        sensekit_rgb_pixel_t* vizBuffer = m_visualizerPtr->get_output();
        for(int i = 0; i < width * height; i++)
        {
            int rgbaOffset = i *4;
            m_displayBuffer[rgbaOffset] = vizBuffer[i].r;
            m_displayBuffer[rgbaOffset + 1] = vizBuffer[i].b;
            m_displayBuffer[rgbaOffset + 2] = vizBuffer[i].g;
            m_displayBuffer[rgbaOffset + 3] = 255;
        }
        m_texture.update(m_displayBuffer.get());
        check_fps();
    }

    void drawTo(sf::RenderWindow& window)
    {
        if (m_displayBuffer != nullptr)
        {
            m_scale = window.getView().getSize().x / m_displayWidth;

            m_sprite.setScale(m_scale, m_scale);

            window.draw(m_sprite);
        }
    }

private:
    float m_scale{ 1 };

    using VizPtr = std::unique_ptr<samples::common::LitDepthVisualizer>;
    VizPtr m_visualizerPtr;

    long double m_frameDuration{ 0 };
    std::clock_t m_lastTimepoint { 0 };
    sf::Texture m_texture;
    sf::Sprite m_sprite;

    using BufferPtr = std::unique_ptr < uint8_t[] > ;
    BufferPtr m_displayBuffer { nullptr };
    int m_displayWidth { 0 };
    int m_displayHeight { 0 };
};

int main(int argc, char** argv)
{
    sensekit::SenseKit::initialize();

    sf::RenderWindow window(sf::VideoMode(1280, 960), "Depth Viewer");

    sensekit::Sensor sensor;
    sensekit::StreamReader reader = sensor.create_reader();

    auto ds = reader.stream<sensekit::DepthStream>();
    ds.start();

    DepthFrameListener listener(ds);

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
