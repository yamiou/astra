#ifndef SENSEKIT_H
#define SENSEKIT_H

#include "sensekit_core.h"
#include <functional>
#include <map>

template<>
struct std::less<sensekit_stream_typepair_t>
{
    bool operator() (const sensekit_stream_typepair_t& lhs,
                     const sensekit_stream_typepair_t& rhs) const
        {
            return lhs.type < rhs.type ||
                              (lhs.type == rhs.type && lhs.subType < rhs.subType);
        }
};

namespace sensekit {

    class StreamReader;

    class Sensor
    {
    public:
        Sensor()
            {
                sensekit_streamset_open("", &m_pStreamSet);
            }

        ~Sensor()
            {
                sensekit_streamset_close(&m_pStreamSet);
            }

        inline StreamReader create_reader();
    private:
        sensekit_streamset_t* m_pStreamSet;

        friend class StreamReader;
    };

    class FrameRef
    {
    public:
        FrameRef() { }

        template<typename T>
        T get()
            {
                return T();
            }

        void release()
            {

            }

    private:
    };

    class StreamReader
    {
    public:
        explicit StreamReader(Sensor& sensor)
            : m_sensor(sensor)
            {
                sensekit_reader_create(sensor.m_pStreamSet, &m_reader);
            }

        template<typename T>
        T stream()
            {
                return stream<T>(DEFAULT_SUBTYPE);
            }

        template<typename T>
        T stream(sensekit_stream_subtype_t subType)
            {
                sensekit_stream_typepair_t typePair;
                typePair.type = T::id;
                typePair.subType = subType;

                sensekit_streamconnection_t* connection;

                auto it = m_streams.find(typePair);
                if (it == m_streams.end())
                {
                    sensekit_stream_get(m_reader,
                                        T::id,
                                        subType,
                                        &connection);

                    m_streams.insert(std::make_pair(typePair, connection));
                }
                else
                {
                    connection = it->second;
                }

                return T(connection);

            }

        FrameRef get_latest_frame(int timeoutMillis = -1)
            {
                sensekit_temp_update();
                return FrameRef();
            }

    private:
        Sensor& m_sensor;
        sensekit_reader_t* m_reader;
        std::map<sensekit_stream_typepair_t,
                 sensekit_streamconnection_t*,
                 std::less<sensekit_stream_typepair_t> > m_streams;
    };

    StreamReader Sensor::create_reader()
    {
        return StreamReader(*this);
    }
}

#endif // SENSEKIT_H
