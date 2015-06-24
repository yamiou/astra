#include <SensekitUL/streams/Depth.h>
#include <SenseKit/Plugins/plugin_capi.h>
#include <SenseKit/sensekit_capi.h>
#include <Sensekit/SenseKit.h>
#include <SenseKit/sensekit_types.h>

#include <memory>
#include <chrono>
#include <iostream>
#include <iomanip>

#include <SFML/Graphics.hpp>

#include "LitDepthVisualizer.h"
#include <common/serialization/FrameStreamWriter.h>
#include <common/serialization/FrameOutputStream.h>

using namespace sensekit::serialization;

class PointFrameListener : public sensekit::FrameReadyListener
{
public:
    PointFrameListener(sensekit::PointStream& pointStream)
    {
        m_lastTimepoint = clock_type::now();
    }

    ~PointFrameListener()
    {
        
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

    virtual void on_frame_ready(sensekit::StreamReader& reader,
                                sensekit::Frame& frame) override
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

        if (m_shouldCheckFps)
        {
            check_fps();
        }
    }

    void set_shouldcheckfps(bool shouldCheckFps)
    {
        m_shouldCheckFps = shouldCheckFps;
    }

    void drawTo(sf::RenderWindow& window)
    {
        if (m_displayBuffer != nullptr)
        {
            float depthScale = window.getView().getSize().x / m_displayWidth;

            m_sprite.setScale(depthScale, depthScale);

            window.draw(m_sprite);
        }
    }

private:
    samples::common::LitDepthVisualizer m_visualizer;

    using duration_type = std::chrono::duration<double>;
    duration_type m_frameDuration;

    using clock_type = std::chrono::system_clock;
    std::chrono::time_point<clock_type> m_lastTimepoint;
    sf::Texture m_texture;
    sf::Sprite m_sprite;

    using BufferPtr = std::unique_ptr<uint8_t[]>;
    BufferPtr m_displayBuffer{ nullptr };
    int m_displayWidth{0};
    int m_displayHeight{0};

    bool m_shouldCheckFps{false};
};


class DepthFrameListener : public sensekit::FrameReadyListener
{
public:
    DepthFrameListener(sensekit::DepthStream& depthStream, FrameStreamWriter& serializer)
        : m_frameStreamWriter(serializer)
    {
        
    }

    ~DepthFrameListener()
    {
        
    }

    virtual void on_frame_ready(sensekit::StreamReader& reader,
                                sensekit::Frame& frame) override
    {
        sensekit::DepthFrame depthFrame = frame.get<sensekit::DepthFrame>();

        m_frameStreamWriter.write(depthFrame);
    }

private:

    FrameStreamWriter& m_frameStreamWriter;
};

int main(int argc, char** argv)
{
    bool isRecording = false;
    bool isPlaying = false;
    bool shouldBlockCommands = false;

    FILE* outputFile = fopen("test.df", "wb");;

    std::unique_ptr<FrameOutputStream> outputStream = 
        std::unique_ptr<FrameOutputStream>(open_frame_output_stream(outputFile));
    FrameStreamWriter streamWriter(*outputStream.get());

    sensekit::SenseKit::initialize();

    sf::RenderWindow window(sf::VideoMode(1280, 960), "Stream Recorder");

    sensekit::Sensor sensor;
    sensekit::StreamReader sensorReader = sensor.create_reader();

    sensekit::Sensor streamPlayer("stream_player");
    sensekit::StreamReader streamPlayerReader = streamPlayer.create_reader();

    auto sensorDs = sensorReader.stream<sensekit::DepthStream>();
    auto sensorPs = sensorReader.stream<sensekit::PointStream>();
    auto streamPlayerPs = streamPlayerReader.stream<sensekit::PointStream>();

    sensorDs.start();
    sensorPs.start();

    DepthFrameListener sensorDsListener(sensorDs, streamWriter);
    PointFrameListener sensorPsListener(sensorPs);
    PointFrameListener streamPlayerPsListener(streamPlayerPs);

    sensorReader.addListener(sensorDsListener);
    sensorReader.addListener(sensorPsListener);
    streamPlayerReader.addListener(streamPlayerPsListener);

    while (window.isOpen())
    {
        sensekit_temp_update();

        sf::Event event;
        while (window.pollEvent(event))
        {
            if (event.type == sf::Event::Closed)
            {
                window.close();
            }
            if ((event.type == sf::Event::KeyPressed) && (event.key.code == sf::Keyboard::Escape))
            {
                window.close();
            }

            if (!shouldBlockCommands &&
                !isPlaying &&
                !isRecording &&
                (event.type == sf::Event::KeyPressed) &&
                (event.key.code == sf::Keyboard::R))
            {
                sensorPsListener.set_shouldcheckfps(true);
                streamWriter.begin_write();
                isRecording = true;
                shouldBlockCommands = true;
            }

            if (!shouldBlockCommands &&
                !isPlaying &&
                isRecording &&
                (event.type == sf::Event::KeyPressed) &&
                (event.key.code == sf::Keyboard::R))
            {
                sensorPsListener.set_shouldcheckfps(false);
                streamWriter.end_write();
                isRecording = false;
                shouldBlockCommands = true;
            }

            if (!shouldBlockCommands &&
                !isRecording && !isPlaying &&
                (event.type == sf::Event::KeyPressed) &&
                (event.key.code == sf::Keyboard::P))
            {
                streamPlayerPsListener.set_shouldcheckfps(true);
                isPlaying = true;
                shouldBlockCommands = true;
            }
            if (!shouldBlockCommands &&
                !isRecording &&
                isPlaying &&
                (event.type == sf::Event::KeyPressed) &&
                (event.key.code == sf::Keyboard::S))
            {
                streamPlayerPsListener.set_shouldcheckfps(false);
                isPlaying = false;
                shouldBlockCommands = true;
            }

            if (shouldBlockCommands)
            {
                shouldBlockCommands = false;
            }
        }

        window.clear(sf::Color::Black);

        if (isPlaying)
        {
            streamPlayerPsListener.drawTo(window);
        }
        else if (isRecording)
        {
            sensorPsListener.drawTo(window);
        }

        window.display();
    }

    sensekit::SenseKit::terminate();

    outputStream.reset();
    fclose(outputFile);

    return 0;
}