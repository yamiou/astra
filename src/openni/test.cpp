#include <OpenNI.h>

#include <iostream>
using std::cout;
using std::endl;

int main(int argc, char** argv)
{
    openni::Device device;
    openni::VideoStream depthStream;
    openni::VideoStream colorStream;

    openni::Status rc = openni::STATUS_OK;

        const char* deviceURI = openni::ANY_DEVICE;

        cout << "Initializing openni" << endl;
        rc = openni::OpenNI::initialize();
        cout << "Opening device" << endl;
        rc = device.open(deviceURI);

        if (rc != openni::STATUS_OK)
        {
            cout << "Failed to open" << endl;
                openni::OpenNI::shutdown();
        }
        cout << "opening depth stream" << endl;
        rc = depthStream.create(device, openni::SENSOR_DEPTH);
        if (rc == openni::STATUS_OK)
        {
            cout << "starting depth stream." << endl;
                rc = depthStream.start();
                if (rc != openni::STATUS_OK)
                {
                    cout << "failed to start depth stream" << endl;
                        depthStream.destroy();
                }
        }
        else
        {
            cout << "Failed to open depth stream" << endl;
        }

        cout << "opening color stream" << endl;
        rc = colorStream.create(device, openni::SENSOR_COLOR);
        if (rc == openni::STATUS_OK)
        {
            cout << "starting color stream" << endl;
                rc = colorStream.start();
                if (rc != openni::STATUS_OK)
                {
                    cout << "failed to start color stream" << endl;
                        colorStream.destroy();
                }
        }
        else
        {
            cout << "failed to start color stream" << endl;
        }

        if (!depthStream.isValid() || !colorStream.isValid())
        {
            cout << "shutting down openni because of failure" << endl;
                openni::OpenNI::shutdown();

        }

        const openni::DeviceInfo& info = device.getDeviceInfo();

        cout << "stoping depth stream" << endl;
        depthStream.stop();
        cout << "destroying depth stream" << endl;
        depthStream.destroy();
        cout << "stopping color stream" << endl;
        colorStream.stop();
        cout << "destroying color stream" << endl;
        colorStream.destroy();
        cout << "closing device" << endl;
        device.close();
        cout << "shutting down openni" << endl;
        openni::OpenNI::shutdown();
}
