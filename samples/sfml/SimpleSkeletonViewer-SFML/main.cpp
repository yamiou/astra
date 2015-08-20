#include <SFML/Graphics.hpp>
#include <Astra/Astra.h>
#include <AstraUL/AstraUL.h>
#include <iostream>

class SkeletonFrameListener : public astra::FrameReadyListener
{
public:
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

    void processDepth(astra::Frame& frame)
    {
        astra::DepthFrame depthFrame = frame.get<astra::DepthFrame>();

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
                uint8_t value = depth % 255;

                m_displayBuffer[index4] = value;
                m_displayBuffer[index4 + 1] = value;
                m_displayBuffer[index4 + 2] = value;
                m_displayBuffer[index4 + 3] = 255;
            }
        }

        m_texture.update(m_displayBuffer.get());
    }

    void processSkeletons(astra::Frame& frame)
    {
        astra::SkeletonFrame skeletonFrame = frame.get<astra::SkeletonFrame>();

        m_skeletons = skeletonFrame.skeletons();
        m_jointPositions.clear();

        for (auto skeleton : m_skeletons)
        {
            for(auto joint : skeleton.joints())
            {
                auto depthPosition =
                    m_mapper->convert_world_to_depth(joint.position());

                m_jointPositions.push_back(depthPosition);
            }
        }
    }

    virtual void on_frame_ready(astra::StreamReader& reader,
                                astra::Frame& frame) override
    {
        if (m_mapper == nullptr)
        {
            auto& mapper = reader.stream<astra::DepthStream>().coordinateMapper();
            m_mapper = std::make_unique<astra::CoordinateMapper>(mapper);
        }

        processDepth(frame);
        processSkeletons(frame);

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

    void drawSkeletons(sf::RenderWindow& window, float depthScale)
    {
        float radius = 16;
        sf::Color trackingColor(10, 10, 200);

        for (auto position : m_jointPositions)
        {
            drawCircle(window,
                       radius,
                       position.x * depthScale,
                       position.y * depthScale,
                       trackingColor);
        }
    }

    void drawTo(sf::RenderWindow& window)
    {
        if (m_displayBuffer != nullptr)
        {
            float depthScale = window.getView().getSize().x / m_depthWidth;

            m_sprite.setScale(depthScale, depthScale);

            window.draw(m_sprite);

            drawSkeletons(window, depthScale);
        }
    }

private:
    long double m_frameDuration{ 0 };
    std::clock_t m_lastTimepoint { 0 };
    sf::Texture m_texture;
    sf::Sprite m_sprite;

    using BufferPtr = std::unique_ptr < uint8_t[] >;
    BufferPtr m_displayBuffer{ nullptr };

    std::unique_ptr<astra::CoordinateMapper> m_mapper;
    std::vector<astra::Skeleton> m_skeletons;
    std::vector<astra::Vector3f> m_jointPositions;

    int m_depthWidth{0};
    int m_depthHeight{0};
};

int main(int argc, char** argv)
{
    astra::Astra::initialize();

    sf::RenderWindow window(sf::VideoMode(1280, 960), "Skeleton Viewer");

    astra::Sensor sensor;
    astra::StreamReader reader = sensor.create_reader();

    SkeletonFrameListener listener;

    reader.stream<astra::DepthStream>().start();
    reader.stream<astra::SkeletonStream>().start();
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
