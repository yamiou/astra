#include <SFML/Graphics.hpp>
#include <Astra/Astra.h>
#include <AstraUL/AstraUL.h>
#include "../../common/LitDepthVisualizer.h"
#include <chrono>
#include <iostream>
#include <iomanip>

class MultiFrameListener : public astra::FrameReadyListener
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
        m_lastTimepoint = clock_type::now();
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

    void update_depth(astra::Frame& frame)
    {
        astra::PointFrame pointFrame = frame.get<astra::PointFrame>();

        if (!pointFrame.is_valid())
        {
            clear_view(m_depthView);
            return;
        }

        int depthWidth = pointFrame.resolutionX();
        int depthHeight = pointFrame.resolutionY();

        init_texture(depthWidth, depthHeight, m_depthView);

        m_visualizer.update(pointFrame);

        astra_rgb_pixel_t* vizBuffer = m_visualizer.get_output();
        uint8_t* buffer = m_depthView.buffer.get();
        for (int i = 0; i < depthWidth * depthHeight; i++)
        {
            int rgbaOffset = i * 4;
            buffer[rgbaOffset] = vizBuffer[i].r;
            buffer[rgbaOffset + 1] = vizBuffer[i].g;
            buffer[rgbaOffset + 2] = vizBuffer[i].b;
            buffer[rgbaOffset + 3] = 255;
        }

        m_depthView.texture.update(m_depthView.buffer.get());
    }

    void update_color(astra::Frame& frame)
    {
        astra::ColorFrame colorFrame = frame.get<astra::ColorFrame>();

        if (!colorFrame.is_valid())
        {
            clear_view(m_colorView);
            return;
        }

        int colorWidth = colorFrame.resolutionX();
        int colorHeight = colorFrame.resolutionY();

        init_texture(colorWidth, colorHeight, m_colorView);

        const astra::RGBPixel* color = colorFrame.data();
        uint8_t* buffer = m_colorView.buffer.get();
        for(int i = 0; i < colorWidth * colorHeight; i++)
        {
            int rgbaOffset = i * 4;
            buffer[rgbaOffset] = color[i].r;
            buffer[rgbaOffset + 1] = color[i].g;
            buffer[rgbaOffset + 2] = color[i].b;
            buffer[rgbaOffset + 3] = 255;
        }

        m_colorView.texture.update(m_colorView.buffer.get());
    }

    void update_ir(astra::Frame& frame)
    {
        astra::InfraredFrame irFrame = frame.get<astra::InfraredFrame>();

        if (!irFrame.is_valid())
        {
            clear_view(m_colorView);
            return;
        }

        int irWidth = irFrame.resolutionX();
        int irHeight = irFrame.resolutionY();

        init_texture(irWidth, irHeight, m_colorView);

        const astra::RGBPixel* irRGB = irFrame.data();
        uint8_t* buffer = m_colorView.buffer.get();
        for (int i = 0; i < irWidth * irHeight; i++)
        {
            int rgbaOffset = i * 4;
            buffer[rgbaOffset] = irRGB[i].r;
            buffer[rgbaOffset + 1] = irRGB[i].g;
            buffer[rgbaOffset + 2] = irRGB[i].b;
            buffer[rgbaOffset + 3] = 255;
        }

        m_colorView.texture.update(m_colorView.buffer.get());
    }

    virtual void on_frame_ready(astra::StreamReader& reader,
                                astra::Frame& frame) override
    {
        update_depth(frame);

        update_color(frame);

        update_ir(frame);

        check_fps();
    }

    void drawTo(sf::RenderWindow& window)
    {
        int viewSize = (int)(window.getView().getSize().x / 2.0f);

        if (m_depthView.buffer != nullptr)
        {
            float depthScale = viewSize / (float)m_depthView.width;
            float height = m_depthView.height * depthScale;
            int horzCenter = window.getView().getCenter().y - height / 2.0f;
            m_depthView.sprite.setScale(depthScale, depthScale);
            m_depthView.sprite.setPosition(0, horzCenter);
            window.draw(m_depthView.sprite);
        }

        if (m_colorView.buffer != nullptr)
        {
            float colorScale = viewSize / (float)m_colorView.width;
            float height = m_colorView.height * colorScale;
            int horzCenter = window.getView().getCenter().y - height / 2.0f;
            m_colorView.sprite.setScale(colorScale, colorScale);
            m_colorView.sprite.setPosition(viewSize, horzCenter);
            window.draw(m_colorView.sprite);
        }
    }

private:
    samples::common::LitDepthVisualizer m_visualizer;

    using duration_type = std::chrono::duration<double>;
    duration_type m_frameDuration{0.0};

    using clock_type = std::chrono::system_clock;
    std::chrono::time_point<clock_type> m_lastTimepoint;

    stream_view m_depthView;
    stream_view m_colorView;
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

astra::InfraredStream configure_ir(astra::StreamReader& reader)
{
    auto irStream = reader.stream<astra::InfraredStream>();

    astra::ImageStreamMode irMode;

    irMode.set_width(640);
    irMode.set_height(480);
    irMode.set_pixelFormat(astra_pixel_formats::ASTRA_PIXEL_FORMAT_RGB888);
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
    astra::Astra::initialize();

    sf::VideoMode mode = sf::VideoMode::getFullscreenModes()[0];
    sf::RenderWindow window(mode, "Stream Viewer", sf::Style::Fullscreen);

    astra::StreamSet streamset;
    astra::StreamReader reader = streamset.create_reader();

    reader.stream<astra::PointStream>().start();
    
    auto depthStream = configure_depth(reader);
    depthStream.start();

    auto colorStream = configure_color(reader);
    colorStream.start();

    auto irStream = configure_ir(reader);
    //irStream.start();

    MultiFrameListener listener;

    reader.addListener(listener);

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
                case sf::Keyboard::I:
                    colorStream.stop();
                    irStream.start();
                    break;
                case sf::Keyboard::C:
                    irStream.stop();
                    colorStream.start();
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
    }

    astra::Astra::terminate();
    return 0;
}
