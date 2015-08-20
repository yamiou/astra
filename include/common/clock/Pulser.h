#ifndef PULSER_H
#define PULSER_H

#include "Stopwatch.h"

namespace astra { namespace clock {

    class Pulser
    {
    public:
        Pulser() :
            m_swatchName("Pulser")
        {
            m_swatch.set_mode(REAL_TIME);
            m_swatch.start(m_swatchName);

            reset();
        }
        ~Pulser()
        {

        }

        void set_period(double period)
        {
            m_period = period;
        }

        double get_period()
        {
            return m_period;
        }

        void start()
        {
            m_swatch.start(m_swatchName);
        }

        void stop()
        {
            m_swatch.stop(m_swatchName);
        }

        void pause()
        {
            m_swatch.pause(m_swatchName);
        }

        bool is_pulse()
        {
            bool isPulse = m_swatch.get_time_so_far(m_swatchName) >= m_period;

            if (isPulse)
            {
                reset();
            }

            return isPulse;
        }
        void reset()
        {
            m_swatch.reset(m_swatchName);
        }

    private:
        Stopwatch m_swatch;
        std::string m_swatchName;

        double m_period{ 0 };
    };

}}

#endif /* PULSER_H */