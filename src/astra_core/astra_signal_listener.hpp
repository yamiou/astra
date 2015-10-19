#ifndef ASTRA_SIGNAL_LISTENER_H
#define ASTRA_SIGNAL_LISTENER_H

#include "astra_signal.hpp"
#include <cstddef>

namespace astra {

    template<typename TSignal>
    class signal_listener
    {
    public:
        using callback_id = std::size_t;
        using callback_type = typename TSignal::callback_type;

        signal_listener(TSignal signal, callback_type callback)
            : signal_(signal),
              callback_(callback)
        {
            signal += callback_;
        }

        ~signal_listener()
        {
            signal_ -= id_;
        }

    private:
        callback_id id_;
        std::function<callback_type> callback_;
        TSignal signal_;
    };

}

#endif /* ASTRA_SIGNAL_LISTENER_H */
