#include <SFML/Graphics.hpp>
#include <SenseKit.h>
#include <SenseKitUL.h>

class SampleFrameListener : public sensekit::FrameReadyListener
{
public:
    SampleFrameListener()
        {
            buffer = new uint8_t[320*240*4];
            memset(buffer, 0, 320*240*4);

            m_texture.create(320, 240);
            m_sprite.setTexture(m_texture);
        }

    virtual void on_frame_ready(sensekit::StreamReader& reader,
                                sensekit::Frame& frame) override
        {
            sensekit::DepthFrame depthFrame = frame.get<sensekit::DepthFrame>();

            int width = depthFrame.get_resolutionX();
            int height = depthFrame.get_resolutionY();
            const int16_t* depthPtr = depthFrame.data();
            for(int y = 0; y < height; y++)
            {
                for(int x = 0; x < width; x++)
                {
                    int index = (y * width + x);
                    int16_t depth = depthPtr[index];
                    buffer[index * 4] = depth % 255;
                    buffer[index * 4 + 1] = depth % 255;
                    buffer[index * 4 + 2] = depth % 255;
                    buffer[index * 4 + 3] = 255;
                }
            }
            m_texture.update(buffer);
        }

    void draw(sf::RenderWindow& window)
        {
            window.draw(m_sprite);
        }

private:
    sf::Texture m_texture;
    sf::Sprite m_sprite;
    uint8_t* buffer;

};

int main(int argc, char** argv)
{
    // create the window
    sf::RenderWindow window(sf::VideoMode(320, 240), "My window");

    sensekit::Sensor sensor;
    sensekit::StreamReader reader = sensor.create_reader();

    SampleFrameListener listener;

    reader.stream<sensekit::DepthStream>().start();
    reader.stream<sensekit::ColorStream>().start();

    reader.addListener(listener);

    // run the program as long as the window is open
    while (window.isOpen())
    {
        sensekit_temp_update();

        // check all the window's events that were triggered since the last iteration of the loop
        sf::Event event;
        while (window.pollEvent(event))
        {
            // "close requested" event: we close the window
            if (event.type == sf::Event::Closed)
                window.close();
        }

        // clear the window with black color
        window.clear(sf::Color::Black);

        // draw everything here...
        // window.draw(...);

        // end the current frame
        listener.draw(window);
        window.display();
    }

    return 0;
}
