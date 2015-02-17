#ifndef OPENNIADAPTER_H
#define OPENNIADAPTER_H

#include "../SenseKit-private.h"
#include <OpenNI.h>

class OpenNIAdapter
{
public:
	OpenNIAdapter();
	~OpenNIAdapter();

	sensekit_status_t Initialize();
	
private:
	openni::Device m_device;
	openni::VideoStream m_colorStream;
	openni::VideoStream m_depthStream;
};

#endif // OPENNIADAPTER_H

