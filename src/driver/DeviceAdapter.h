#ifndef DEVICEADAPTER_H
#define DEVICEADAPTER_H

#include <SenseKit.h>

namespace sensekit { namespace driver {

        using device_handle_t = void*;
        using DeviceCallback = void (*)();

        class DeviceAdapter
        {
        public:
            DeviceAdapter()
                : m_initialized(false) {}

            virtual ~DeviceAdapter() {}

            void initialize(DeviceCallback openingCallback,
                            DeviceCallback openCallback,
                            DeviceCallback closingCallback,
                            DeviceCallback closedCallback)
            {
                if (m_initialized) return;

                m_deviceOpening = openingCallback;
                m_deviceOpen = openCallback;
                m_deviceClosing = closingCallback;
                m_deviceClosed = closedCallback;

                initialize_impl();

                m_initialized = true;
            }

            void terminate()
            {
                if (!m_initialized) return;

                terminate_impl();
                m_initialized = false;
            }

            virtual void initialize_impl() { };
            virtual void terminate_impl() { };

            virtual void open() = 0;
            virtual void close() = 0;

            virtual bool is_open() = 0;

            virtual const sensekit_device_desc_t& get_description() = 0;
            virtual device_handle_t get_handle() = 0;

            const bool& is_initialized() { return m_initialized; }

        protected:

            inline void raiseOpening() { if (m_deviceOpening) m_deviceOpening(); }
            inline void raiseOpen() { if (m_deviceOpen) m_deviceOpen(); }
            inline void raiseClosing() { if (m_deviceClosing) m_deviceClosing(); }
            inline void raiseClosed() { if (m_deviceClosed) m_deviceClosed(); }

        private:

            bool m_initialized;

            DeviceCallback m_deviceOpening;
            DeviceCallback m_deviceOpen;
            DeviceCallback m_deviceClosing;
            DeviceCallback m_deviceClosed;
        };
}}

#endif /* DEVICEADAPTER_H */
