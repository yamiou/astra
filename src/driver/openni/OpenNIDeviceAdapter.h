#ifndef OPENNIDEVICEADAPTER_H
#define OPENNIDEVICEADAPTER_H

#include "../DeviceAdapter.h"
#include <OpenNI.h>

namespace sensekit { namespace openni {

        class OpenNIDeviceAdapter : public sensekit::driver::DeviceAdapter
        {
        public:

            OpenNIDeviceAdapter(const ::openni::DeviceInfo* deviceInfo)
                : m_pOniDeviceInfo(deviceInfo),
                  m_pDescription(nullptr) { }

            virtual ~OpenNIDeviceAdapter();

            virtual void initialize_impl() override;
            virtual void terminate_impl() override;

            virtual void open() override;
            virtual void close() override;

            virtual bool is_open() { return m_oniDevice.isValid(); }

            virtual driver::device_handle_t get_handle() override { return driver::device_handle_t(&m_oniDevice); }
            virtual const sensekit_device_desc_t& get_description();

        private:

            sensekit_device_desc_t* m_pDescription;
            const ::openni::DeviceInfo* m_pOniDeviceInfo;
            ::openni::Device m_oniDevice;
        };
}}

#endif /* OPENNIDEVICEADAPTER_H */
