.. |sdkname| replace:: Astra

***********************
4.2 Simple Depth Reader
***********************
*Time Required: ~10 minutes*

Thirsting for more knowledge after finishing the Hello World Tutorial? Now that you've mastered some of the basic concepts of |sdkname|, let's read the depth stream from our Astra using another |sdkname| feature.

By the end of this tutorial you should be familiar with:

- The purpose of the ``FrameReadyListener`` class
- How to define a ``FrameReadyListener``
- Using a ``FrameReadyListener`` to process a depth stream

Before We Begin
===============
#. Download and decompress the latest |sdkname| SDK, if you haven't already.
#. Using your favorite IDE, set up a new console application project and create a new source file called "main.cpp".
#. Copy the following into your main.cpp file:

.. code-block:: c++
   :linenos:

   #include <Astra\Astra.h>
   #include <AstraUL\AstraUL.h>

   #include <cstdio>
   #include <iostream>

   int main(int argc, char** arvg)
   {
      astra::Astra::initialize();

      astra::Sensor sensor;
      astra::StreamReader reader = sensor.create_reader();

      reader.stream<astra::DepthStream>().start();

      // Your code will go here

      astra::Astra::terminate();

      return 0;
   }
- Line 9 - Initializes |sdkname|
- Line 11 - Constructs a ``Sensor``
- Line 12 - Creates a ``StreamReader``
- Line 14 - Starts a depth stream
- Line 18 - Terminates |sdkname|

Listening to Streams
====================
In the Hello World tutorial, we processed a stream of frames by looping over a call to our ``StreamReader``'s ``get_latest_frame`` function. This solution works perfectly fine in a simple case such as our Hello World application. But, what if we wanted to register for a number of streams and work with them? Or, what if we were working with more than one ``Sensor``, or possibly more than one ``StreamReader`` per ``Sensor``? In all of those cases, the code within the loop could quickly become complex, cluttered and cumbersome.

To alleviate these issues, |sdkname| provides us with a framework to define and create ``FrameReadyListener`` s. A ``FrameReadyListener`` has one function called ``on_frame_ready`` that (you guessed it!) is called when a new frame of a specific type is ready for processing. So, instead of looping over our ``StreamReader``'s ``get_latest_frame`` function, our listener will have the latest frame automatically delivered to it as soon as the frame is ready. Neato!

In order to use a ``FrameReadyListener`` with our example...

1. We need to define a listener class that implements ``FrameReadyListener``. This class will give us access to the actual frames that are coming from the Astra sensor. We'll get those frames in the ``on_frame_ready`` function. Copy the following code below your ``#include`` directives and above your ``main`` function:

.. code-block:: c++
   :linenos:
   :lineno-start: 7

   class DepthFrameListener : public astra::FrameReadyListener
   {
   public:
      DepthFrameListener(int maxFramesToProcess) :
          m_maxFramesToProcess(maxFramesToProcess)
      {

      }

      bool is_finished()
      {
          return m_isFinished;
      }

   private:
      virtual void on_frame_ready(astra::StreamReader& reader,
                                  astra::Frame& frame) override
      {
          astra::DepthFrame depthFrame = frame.get<astra::DepthFrame>();

          if (depthFrame.is_valid())
          {
              print_depth_frame(depthFrame);
              ++m_framesProcessed;
          }

          if (m_framesProcessed >= m_maxFramesToProcess)
          {
              m_isFinished = true;
          }
      }

      void print_depth_frame(astra::DepthFrame& depthFrame)
      {
          int frameIndex = depthFrame.frameIndex();
          short middleValue = get_middle_value(depthFrame);

         std::printf("Depth frameIndex: %d value: %d \n", frameIndex, middleValue);
      }

      int16_t get_middle_value(astra::DepthFrame& depthFrame)
      {
          int width = depthFrame.resolutionX();
          int height = depthFrame.resolutionY();

          size_t middleIndex = ((width * (height / 2.0f)) + (width / 2.0f));

          const int16_t* frameData = depthFrame.data();
          int16_t middleValue = frameData[middleIndex];

          return middleValue;
      }

      bool m_isFinished{false};
      int m_framesProcessed{0};
      int m_maxFramesToProcess{0};
   };

   int main(int argc, char** argv)
   {
- Line 10 - Constructor parameter specifies the total number of frames we're going to process before exiting our loop
- Line 16 - ``is_finished`` will be used in a later step to check whether we've looped the maximum number of times or not
- Line 25 - Gets the depth frame data from our frame
- Line 27 - Check to verify that we received a valid frame
- Line 29 - Prints depth frame information to the console
- Line 52 - Calculates the index of the middle pixel in our depth frame's data
- Line 55 - Gets the value of the middle depth frame pixel

.. note::

   The only required function is the ``on_frame_ready`` function. The other functions in this class support what we do within that function.

2. With the ``DepthFrameListener`` defined, let's construct our listener in the ``main`` function and add it to the ``StreamReader`` that we created in a previous step.

.. code-block:: c++
   :linenos:
   :lineno-start: 65
   :emphasize-lines: 10,11,13,17

   int main(int argc, char** arvg)
   {
      astra::Astra::initialize();

      astra::Sensor sensor;
      astra::StreamReader reader = sensor.create_reader();

      reader.stream<astra::DepthStream>().start();

      int maxFramesToProcess = 100;
      DepthFrameListener listener(maxFramesToProcess);

      reader.addListener(listener);

      // More of your code will go here

      reader.removeListener(listener);

      astra::Astra::terminate();

      return 0;
   }
- Line 75 - Constructs a ``DepthFrameListener`` that will loop 100 times
- Line 77 - Adds the listener to our reader
- Line 81 - Removes the listener from our reader

Updating the Listeners
======================

We've got |sdkname| and the ``Sensor`` running, and we're listening to depth frames as they stream in through the ``Sensor``'s ``StreamReader``. We don't know when frames are going to arrive from our Astra, so we need to continuously update those listeners by calling ``astra_temp_update`` in a loop.

.. code-block:: c++
   :linenos:
   :lineno-start: 65
   :emphasize-lines: 15-18

   int main(int argc, char** arvg)
   {
      astra::Astra::initialize();

      astra::Sensor sensor;
      astra::StreamReader reader = sensor.create_reader();

      reader.stream<astra::DepthStream>().start();

      int maxFramesToProcess = 100;
      DepthFrameListener listener(maxFramesToProcess);

      reader.addListener(listener);

      do
      {
         astra_temp_update();
      } while (!listener.is_finished());

      reader.removeListener(listener);

      astra::Astra::terminate();

      return 0;
   }
- Line 79-82 - The |sdkname| update loop.

Let's compile and run our solution. After you've watched some depth frame information print to the console, revel in the knowledge that you've mastered the listener along with other core |sdkname| functionality. Now, go forth, let your imagination run wild and use |sdkname| to do all sorts of innovative things!