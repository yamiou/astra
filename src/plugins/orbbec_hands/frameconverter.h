#ifndef FRAMECONVERTER_H
#define FRAMECONVERTER_H

#include <opencv2/opencv.hpp>
#include <SenseKitUL.h>

class FrameConverter
{
public:
    static inline void depthFrameToMat(sensekit_depthframe_t* depthFrameSrc, cv::Mat matTarget)
    {
        int width = depthFrameSrc->width;
        int height = depthFrameSrc->height;
        
        //ensure initialized
        matTarget.create(height, width, CV_32FC1);

        const int16_t* depthData = depthFrameSrc->data;

        for (int y = 0; y < height; ++y)
        {
            float* row = matTarget.ptr<float>(y);
            for (int x = 0; x < width; ++x)
            {
                row[x] = static_cast<float>(*depthData);
                ++depthData;
            }
        }
    }

};

#endif // FRAMECONVERTER_H