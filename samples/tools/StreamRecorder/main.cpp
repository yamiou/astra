#include <AstraUL/streams/Depth.h>
#include <Astra/Plugins/plugin_capi.h>
#include <Astra/astra_capi.h>
#include <Astra/Astra.h>
#include <Astra/astra_types.h>

#include <memory>
#include <chrono>
#include <iostream>
#include <iomanip>

#include <SFML/Graphics.hpp>

#include "LitDepthVisualizer.h"
#include <common/serialization/FrameStreamWriter.h>
#include <common/serialization/FrameOutputStream.h>

using namespace astra;

class Recorder
{
public:
    explicit Recorder(const char* filename) {
        m_outputFile = fopen(filename, "wb");
        m_frameOutputStream = serialization::open_frame_output_stream(m_outputFile);
        m_frameStreamWriter = FrameStreamWriterPtr(new serialization::FrameStreamWriter(*m_frameOutputStream));
    }

    ~Recorder() {
        serialization::close_frame_output_stream(m_frameOutputStream);
        m_frameStreamWriter = nullptr;
        fclose(m_outputFile);
        printf("closed file\n");
    }

    void add_frame(DepthFrame& depthFrame) {
        if (!depthFrame.is_valid()) {
            return;
        }
        bool result = m_frameStreamWriter->write(depthFrame);
        printf("Saving frame: %d: %d\n", m_frameCount, result);
        ++m_frameCount;
    }

private:
    FILE* m_outputFile;
    serialization::FrameOutputStream* m_frameOutputStream;
    int m_frameCount { 0 };

    using FrameStreamWriterPtr = std::unique_ptr<serialization::FrameStreamWriter>;
    FrameStreamWriterPtr m_frameStreamWriter;
};

class Viewer : public FrameReadyListener
{
public:
    Viewer(StreamSet& streamset) :
        m_reader(streamset.create_reader())
    {
        m_lastTimepoint = clock_type::now();

        m_reader.stream<DepthStream>().start();
        m_reader.stream<PointStream>().start();

        m_reader.addListener(*this);
    }

    void draw_to(sf::RenderWindow& window)
    {
        if (m_displayBuffer != nullptr)
        {
            float depthScale = window.getView().getSize().x / m_displayWidth;

            m_sprite.setScale(depthScale, depthScale);

            window.draw(m_sprite);
        }
    }

    void start_recording()
    {
        m_recorder = RecorderPtr(new Recorder("test.df"));
    }

    void stop_recording()
    {
        m_recorder = nullptr;
    }

private:
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

    void visualize_frame(PointFrame& pointFrame)
    {
        if (!pointFrame.is_valid()) {
            return;
        }
        int width = pointFrame.resolutionX();
        int height = pointFrame.resolutionY();

        init_texture(width, height);

        m_visualizer.update(pointFrame);

        astra_rgb_pixel_t* vizBuffer = m_visualizer.get_output();
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

    virtual void on_frame_ready(StreamReader& reader,
                                Frame& frame) override
    {
        PointFrame pointFrame = frame.get<PointFrame>();
        DepthFrame depthFrame = frame.get<DepthFrame>();

        if (m_recorder != nullptr) {
            m_recorder->add_frame(depthFrame);
        }

        visualize_frame(pointFrame);

        //check_fps();
    }

    samples::common::LitDepthVisualizer m_visualizer;

    using duration_type = std::chrono::duration<double>;
    duration_type m_frameDuration{0.0};

    using clock_type = std::chrono::system_clock;
    std::chrono::time_point<clock_type> m_lastTimepoint;
    sf::Texture m_texture;
    sf::Sprite m_sprite;

    using BufferPtr = std::unique_ptr<uint8_t[]>;
    BufferPtr m_displayBuffer{ nullptr };
    int m_displayWidth{0};
    int m_displayHeight{0};

    StreamReader m_reader;
    using RecorderPtr = std::unique_ptr<Recorder>;
    RecorderPtr m_recorder;
};

void handle_key(sf::Keyboard::Key key, sf::RenderWindow& window, Viewer& viewer)
{
    if (key == sf::Keyboard::Escape)
    {
        window.close();
    }
    else if (key == sf::Keyboard::S)
    {
        viewer.stop_recording();
    }
    if (key == sf::Keyboard::R)
    {
        viewer.start_recording();
    }
}

int main(int argc, char** argv)
{
    Astra::initialize();

    sf::RenderWindow window(sf::VideoMode(1280, 960), "Stream Recorder");

    StreamSet streamset;

/*
    StreamSet streamPlayer("stream_player");
    StreamReader streamPlayerReader = streamPlayer.create_reader();

    auto streamPlayerPs = streamPlayerReader.stream<PointStream>();
*/
    Viewer viewer(streamset);

    while (window.isOpen())
    {
        astra_temp_update();

        sf::Event event;

        window.clear(sf::Color::Black);

        while (window.pollEvent(event))
        {
            switch (event.type)
            {
            case sf::Event::Closed:
                window.close();
                break;
            case sf::Event::KeyPressed:
            {
                sf::Keyboard::Key key = event.key.code;
                handle_key(key, window, viewer);
                break;
            }
            default:
                break;
            }
        }

        viewer.draw_to(window);

        window.display();
    }

    Astra::terminate();

    return 0;
}
