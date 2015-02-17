#include "OpenNIAdapter.hpp"
#include "../SenseKit-adapter-private.h"

OpenNIAdapter::OpenNIAdapter() { }
OpenNIAdapter::~OpenNIAdapter() { }

sensekit_status_t OpenNIAdapter::Initialize()
{
	openni::Status rc = openni::STATUS_OK;
	
	const char* deviceURI = openni::ANY_DEVICE;
	
	rc = openni::OpenNI::initialize();
	rc = m_device.open(deviceURI);

	if (rc != openni::STATUS_OK)
	{
		openni::OpenNI::shutdown();
		return SENSEKIT_STATUS_DEVICE_ERROR;
	}

	rc = m_depthStream.create(m_device, openni::SENSOR_DEPTH);
	if (rc == openni::STATUS_OK)
	{
		rc = m_depthStream.start();
		if (rc != openni::STATUS_OK)
		{
			m_depthStream.destroy();
		}
	}
	else
	{
		return SENSEKIT_STATUS_DEVICE_ERROR;
	}

	rc = m_colorStream.create(m_device, openni::SENSOR_COLOR);
	if (rc == openni::STATUS_OK)
	{
		rc = m_colorStream.start();
		if (rc != openni::STATUS_OK)
		{
			m_colorStream.destroy();
		}
	}
	else
	{
		return SENSEKIT_STATUS_DEVICE_ERROR;
	}

	if (!m_depthStream.isValid() || !m_colorStream.isValid())
	{
		openni::OpenNI::shutdown();
		return SENSEKIT_STATUS_DEVICE_ERROR;
	}

	return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_BEGIN_DECLS

sensor_handle_t* _create_openni_adapter()
{
	OpenNIAdapter* adapter = new OpenNIAdapter();
	adapter->Initialize();

	return reinterpret_cast<sensor_handle_t*>(adapter);
}

void _destroy_openni_adapter(sensor_handle_t* handle)
{
	if (handle == nullptr)
	{
		return;
	}

	OpenNIAdapter* adapter = reinterpret_cast<OpenNIAdapter*>(handle);
	delete adapter;
}

SENSEKIT_END_DECLS