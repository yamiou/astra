#include <SFML/Graphics.hpp>
#include <Astra/Astra.h>
#include <AstraUL/AstraUL.h>

#include <chrono>
#include <iostream>
#include <iomanip>
#include <cstring>

class ColorFrameListener : public astra::FrameReadyListener
{
public:
    ColorFrameListener()
    {
        m_lastTimepoint = clock_type::now();
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
            std::memset(m_displayBuffer.get(), 0, byteLength);

            m_texture.create(m_displayWidth, m_displayHeight);
            m_sprite.setTexture(m_texture);
            m_sprite.setPosition(0, 0);
        }
    }

    void check_fps()
    {
        const double frameWeight = 0.2;

        auto newTimepoint = clock_type::now();
        auto frameDuration = std::chrono::duration_cast<duration_type>(newTimepoint - m_lastTimepoint);

        m_frameDuration = frameDuration * frameWeight + m_frameDuration * (1 - frameWeight);
        m_lastTimepoint = newTimepoint;

        double fps = 1.0 / m_frameDuration.count();

        auto precision = std::cout.precision();
        std::cout << std::fixed
            << std::setprecision(1)
            << fps << " fps ("
            << std::setprecision(2)
            << frameDuration.count() * 1000 << " ms)"
            << std::setprecision(precision)
            << std::endl;
    }

    virtual void on_frame_ready(astra::StreamReader& reader,
        astra::Frame& frame) override
    {
        astra::ColorFrame colorFrame = frame.get<astra::ColorFrame>();

        int width = colorFrame.resolutionX();
        int height = colorFrame.resolutionY();

        init_texture(width, height);

        const astra_rgb_pixel_t* colorData = colorFrame.data();

        for (int i = 0; i < width * height; i++)
        {
            int rgbaOffset = i * 4;
            m_displayBuffer[rgbaOffset] = colorData[i].r;
            m_displayBuffer[rgbaOffset + 1] = colorData[i].g;
            m_displayBuffer[rgbaOffset + 2] = colorData[i].b;
            m_displayBuffer[rgbaOffset + 3] = 255;
        }
        m_texture.update(m_displayBuffer.get());
        check_fps();
    }

    void drawTo(sf::RenderWindow& window)
    {
        if (m_displayBuffer != nullptr)
        {
            float imageScale = window.getView().getSize().x / m_displayWidth;

            m_sprite.setScale(imageScale, imageScale);

            window.draw(m_sprite);
        }
    }

private:
    using duration_type = std::chrono::duration<double>;
    duration_type m_frameDuration{ 0.0 };

    using clock_type = std::chrono::system_clock;
    std::chrono::time_point<clock_type> m_lastTimepoint;
    sf::Texture m_texture;
    sf::Sprite m_sprite;

    using BufferPtr = std::unique_ptr < uint8_t[] >;
    BufferPtr m_displayBuffer{ nullptr };
    int m_displayWidth{ 0 };
    int m_displayHeight{ 0 };
};



int main(int argc, char** argv)
{
    astra::Astra::initialize();

    sf::RenderWindow window(sf::VideoMode(1280, 960), "Color Viewer");

    astra::StreamSet streamset;
    astra::StreamReader reader = streamset.create_reader();

    reader.stream<astra::ColorStream>().start();

    ColorFrameListener listener;
    reader.addListener(listener);

    while (window.isOpen())
    {
        astra_temp_update();

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

    astra::Astra::terminate();
    return 0;
}
