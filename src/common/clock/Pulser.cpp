#include <common/clock/Pulser.h>

namespace astra { namespace clock {

    Pulser::Pulser() :
        m_swatchName("Pulser")
    {
        m_swatch.set_mode(REAL_TIME);
        m_swatch.start(m_swatchName);

        reset();
    }

    Pulser::~Pulser()
    {

    }

    void Pulser::set_period(double period)
    {
        m_period = period;
    }

    double Pulser::get_period()
    {
        return m_period;
    }

    void Pulser::start()
    {
        m_swatch.start(m_swatchName);
    }

    void Pulser::stop()
    {
        m_swatch.stop(m_swatchName);
    }

    void Pulser::pause()
    {
        m_swatch.pause(m_swatchName);
    }

    bool Pulser::is_pulse()
    {
        bool isPulse = m_swatch.get_time_so_far(m_swatchName) >= m_period;

        if (isPulse)
        {
            reset();
        }

        return isPulse;
    }

    void Pulser::reset()
    {
        m_swatch.reset(m_swatchName);
    }
}}
