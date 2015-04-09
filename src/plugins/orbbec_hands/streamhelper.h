#ifndef STREAMHELPER_H
#define STREAMHELPER_H

#include <opencv2/opencv.hpp>
#include "coordinateconverter.h"

struct TrackingData;

class StreamHelper
{
public:
    StreamHelper(sensekit_streamset_t setHandle, sensekit_stream_desc_t desc);
    virtual ~StreamHelper();


private:
    sensekit_stream_t m_streamHandle { nullptr };


};

#endif // STREAMHELPER_H