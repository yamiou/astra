#ifndef ONISTREAMSOURCE_H
#define ONISTREAMSOURCE_H

#include <OpenNI.h>
#include <string>
#include "../StreamSource.h"

namespace sensekit { namespace openni {

        class OniStreamSource : public sensekit::StreamSource
        {
        public:
            OniStreamSource(std::string shortName,
                            ::openni::SensorInfo* sensorInfo,
                            ::openni::SensorType sensorType,
                            ::openni::DeviceInfo* deviceInfo,
                            ::openni::Device* device) :
                StreamSource(shortName),
                m_oniSensorInfo(sensorInfo),
                m_oniSensorType(sensorType),
                m_oniDeviceInfo(deviceInfo),
                m_oniDevice(device) {}

            virtual ~OniStreamSource() {}

            virtual Stream* create_stream() override;
            virtual void destroy_stream(Stream* stream) override;

        private:
            ::openni::SensorInfo* m_oniSensorInfo;
            ::openni::SensorType m_oniSensorType;
            ::openni::DeviceInfo* m_oniDeviceInfo;
            ::openni::Device* m_oniDevice;
        };
    }}

#endif /* ONISTREAMSOURCE_H */
