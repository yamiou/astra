#include "OpenNIDeviceAdapter.h"

namespace sensekit { namespace openni {

        sensekit_device_desc_t* create_device_desc(const ::openni::DeviceInfo& info)
        {
            sensekit_device_desc_t* desc = new sensekit_device_desc_t;

            strncpy(desc->uri, info.getUri(), MAX_STRING_FIELD_LENGTH);
            strncpy(desc->name, info.getName(), MAX_STRING_FIELD_LENGTH);
            strncpy(desc->vendor, info.getVendor(), MAX_STRING_FIELD_LENGTH);
            desc->usbVendorId = info.getUsbVendorId();
            desc->usbProductId = info.getUsbProductId();

            return desc;
        }

        OpenNIDeviceAdapter::~OpenNIDeviceAdapter()
        {
            if (m_pDescription)
            {
                delete m_pDescription;
            }
        }

        void OpenNIDeviceAdapter::initialize_impl()
        {

        }

        void OpenNIDeviceAdapter::terminate_impl()
        {
            // Kill open streams
            // Close device
            // Destroy device
        }

        void OpenNIDeviceAdapter::open()
        {
            raiseOpening();

            ::openni::Status rc = m_oniDevice.open(m_pOniDeviceInfo->getUri());

            // Check for failures, throw exceptions?
            if (rc == ::openni::STATUS_OK)
            {

            }

            raiseOpen();
        }

        void OpenNIDeviceAdapter::close()
        {
            if (!is_open()) return;

            raiseClosing();

            m_oniDevice.close();

            raiseClosed();
        }

        const sensekit_device_desc_t& OpenNIDeviceAdapter::get_description()
        {
            if (!m_pDescription)
            {
                m_pDescription = create_device_desc(*m_pOniDeviceInfo);
            }

            return *m_pDescription;
        }
}}
