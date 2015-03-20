#ifndef STREAMCONNECTION_H
#define STREAMCONNECTION_H

namespace sensekit {

    using StreamHandle = unsigned;
    class StreamBin;

    class StreamConnection
    {
    public:
       
        void set_bin(StreamBin* new_bin);

        StreamBin* get_bin() { return m_bin; }

        StreamHandle get_handle() { return m_handle; }
    private:
        StreamBin* m_bin;
        StreamHandle m_handle;
    };
}

#endif /* STREAMCONNECTION_H */
