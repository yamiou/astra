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
            memset(view.buffer.get(), 0, byteLength);

            view.texture.create(view.width, view.height);
            view.sprite = sf::Sprite();
            view.sprite.setTexture(view.texture);
            view.sprite.setPosition(0, 0);
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
        astra::PointFrame pointFrame = frame.get<astra::PointFrame>();
        astra::InfraredFrame infraredFrame = frame.get<astra::InfraredFrame>();
        astra::ColorFrame colorFrame = frame.get<astra::ColorFrame>();

        if (pointFrame.is_valid())
        {
            int depthWidth = pointFrame.resolutionX();
            int depthHeight = pointFrame.resolutionY();

            init_texture(depthWidth, depthHeight, m_depthView);

            m_visualizer.update(pointFrame);

            astra_rgb_pixel_t* vizBuffer = m_visualizer.get_output();
            for(int i = 0; i < depthWidth * depthHeight; i++)
            {
                int rgbaOffset = i * 4;
                m_depthView.buffer[rgbaOffset] = vizBuffer[i].r;
                m_depthView.buffer[rgbaOffset + 1] = vizBuffer[i].g;
                m_depthView.buffer[rgbaOffset + 2] = vizBuffer[i].b;
                m_depthView.buffer[rgbaOffset + 3] = 255;
            }

            m_depthView.texture.update(m_depthView.buffer.get());
        }

        if (infraredFrame.is_valid())
        {
            int colorWidth = infraredFrame.resolutionX();
            int colorHeight = infraredFrame.resolutionY();

            init_texture(colorWidth, colorHeight, m_colorView);

            const astra::RGBPixel* color = infraredFrame.data();
            for(int i = 0; i < colorWidth * colorHeight; i++)
            {
                int rgbaOffset = i * 4;
                m_colorView.buffer[rgbaOffset] = color[i].r;
                m_colorView.buffer[rgbaOffset + 1] = color[i].g;
                m_colorView.buffer[rgbaOffset + 2] = color[i].b;
                m_colorView.buffer[rgbaOffset + 3] = 255;
            }
            m_colorView.texture.update(m_colorView.buffer.get());
        }
        else if (colorFrame.is_valid())
        {
            int colorWidth = colorFrame.resolutionX();
            int colorHeight = colorFrame.resolutionY();

            init_texture(colorWidth, colorHeight, m_colorView);

            if (m_colorView.buffer != nullptr)
            {
                std::memset(m_colorView.buffer.get(), 128, colorWidth * colorHeight * 4);
            }

            const astra::RGBPixel* color = colorFrame.data();
            for(int i = 0; i < colorWidth * colorHeight; i++)
            {
                int rgbaOffset = i * 4;

                m_colorView.buffer[rgbaOffset] = color[i].r;
                m_colorView.buffer[rgbaOffset + 1] = color[i].g;
                m_colorView.buffer[rgbaOffset + 2] = color[i].b;
                m_colorView.buffer[rgbaOffset + 3] = 255;
            }

            m_colorView.texture.update(m_colorView.buffer.get());
        }

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

int main(int argc, char** argv)
{
    astra::Astra::initialize();

    sf::VideoMode mode = sf::VideoMode::getFullscreenModes()[0];
    sf::RenderWindow window(mode, "Stream Viewer", sf::Style::Fullscreen);

    astra::Sensor sensor("device/mock_sensor0");

    astra::StreamReader infraredReader = sensor.create_reader();
    astra::StreamReader depthReader = sensor.create_reader();
    astra::StreamReader colorReader = sensor.create_reader();

    depthReader.stream<astra::DepthStream>().start();
    depthReader.stream<astra::PointStream>().start();

    astra::ImageStreamMode depthMode;

    depthMode.set_width(640);
    depthMode.set_height(480);
    depthMode.set_pixelFormat(astra_pixel_formats::ASTRA_PIXEL_FORMAT_DEPTH_MM);
    depthMode.set_fps(30);

    depthReader.stream<astra::DepthStream>().set_mode(depthMode);

    astra::ImageStreamMode irMode1;

    irMode1.set_width(320);
    irMode1.set_height(240);
    irMode1.set_pixelFormat(astra_pixel_formats::ASTRA_PIXEL_FORMAT_RGB888);
    irMode1.set_fps(30);

    astra::ImageStreamMode irMode2;

    irMode2.set_width(640);
    irMode2.set_height(480);
    irMode2.set_pixelFormat(astra_pixel_formats::ASTRA_PIXEL_FORMAT_RGB888);
    irMode2.set_fps(30);

    //infraredReader.stream<astra::InfraredStream>().set_mode(irMode2);
    infraredReader.stream<astra::InfraredStream>().start();

    astra::ImageStreamMode colorMode1;

    colorMode1.set_width(640);
    colorMode1.set_height(480);
    colorMode1.set_pixelFormat(astra_pixel_formats::ASTRA_PIXEL_FORMAT_RGB888);
    colorMode1.set_fps(30);

    astra::ImageStreamMode colorMode2;

    colorMode2.set_width(640);
    colorMode2.set_height(480);
    colorMode2.set_pixelFormat(astra_pixel_formats::ASTRA_PIXEL_FORMAT_RGB888);
    colorMode2.set_fps(60);

    MultiFrameListener listener;

    infraredReader.addListener(listener);
    depthReader.addListener(listener);
    colorReader.addListener(listener);

    bool infrared = true;
    bool hiRes = false;

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
            if ((event.type == sf::Event::KeyPressed) && (event.key.code == sf::Keyboard::C))
            {
                if (infrared)
                {
                    infraredReader.stream<astra::InfraredStream>().stop();
                    colorReader.stream<astra::ColorStream>().start();
                }
                else
                {
                    colorReader.stream<astra::ColorStream>().stop();
                    infraredReader.stream<astra::InfraredStream>().start();
                }

                infrared = !infrared;
            }
            if ((event.type == sf::Event::KeyPressed) && (event.key.code == sf::Keyboard::M))
            {
                if (infrared)
                {
                    if (hiRes)
                    {
                        infraredReader.stream<astra::InfraredStream>().set_mode(irMode1);
                    }
                    else
                    {
                        infraredReader.stream<astra::InfraredStream>().set_mode(irMode2);
                    }
                }
                else
                {
                    if (hiRes)
                    {
                        colorReader.stream<astra::ColorStream>().set_mode(colorMode1);
                    }
                    else
                    {
                        colorReader.stream<astra::ColorStream>().set_mode(colorMode2);
                    }
                }
                hiRes = !hiRes;
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
