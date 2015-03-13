#ifndef ONISTREAMSERVER_H
#define ONISTREAMSERVER_H

#include "../StreamServer.h"
#include <OpenNI.h>

namespace sensekit { namespace openni {

        class OniStreamServer : public StreamServer
        {
        public:
            OniStreamServer()
                : m_oniListener(*this) {}

            virtual ~OniStreamServer() {}

        protected:
            virtual void on_initialize() override;
            virtual void on_terminate() override;

        private:
            class OniEventsListener :
                public ::openni::OpenNI::DeviceConnectedListener,
                public ::openni::OpenNI::DeviceDisconnectedListener,
                public ::openni::OpenNI::DeviceStateChangedListener
            {
            public:
                virtual void onDeviceConnected(const ::openni::DeviceInfo* info) override
                    {
                        m_streamServer.on_device_connected(info);
                    }

                virtual void onDeviceDisconnected(const ::openni::DeviceInfo* info) override
                    {
                        m_streamServer.on_device_disconnected(info);
                    }

                virtual void onDeviceStateChanged(const ::openni::DeviceInfo* info,
                                                  ::openni::DeviceState state) override
                    {
                        m_streamServer.on_device_changed(info, state);
                    }

            private:
                OniEventsListener(OniStreamServer& server)
                    : m_streamServer(server) { }

                OniStreamServer& m_streamServer;

                friend class OniStreamServer;
            };

            OniEventsListener m_oniListener;

            void on_device_connected(const ::openni::DeviceInfo* info);
            void on_device_disconnected(const ::openni::DeviceInfo* info);
            void on_device_changed(const ::openni::DeviceInfo* info,
                                   ::openni::DeviceState state);

            friend class OniEventsListener;
        };
    }}


#endif /* ONISTREAMSERVER_H */
