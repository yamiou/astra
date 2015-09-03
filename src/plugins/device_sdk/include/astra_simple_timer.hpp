#ifndef ASTRA_SIMPLE_TIMER_H
#define ASTRA_SIMPLE_TIMER_H

#include <chrono>
#include <thread>

namespace astra { namespace utility {

    template<typename TClock>
    class simple_timer
    {
    public:
        simple_timer();

        void start();
        void stop();

        double interval() const;
        void set_interval(double interval);

        bool is_triggered() const;

        void reset();

    private:
        void thread_func();

        using duration_type = std::chrono::duration<double>;
        using clock_type = TClock;
        using timepoint_type = std::chrono::time_point<clock_type>;

        bool running_{false};
        bool isTriggered_{false};

        std::mutex mutex_;
        std::thread thread_;

        duration_type interval_;
        timepoint_type previousTimepoint_;
    };
}}

namespace astra { namespace utility {

    template<typename TClock>
    simple_timer<TClock>::simple_timer()
        : interval_(2.0)
    {
    }

    template<typename TClock>
    void simple_timer<TClock>::start()
    {
        {
            std::lock_guard<std::mutex> lock(mutex_);

            if (running_)
                return;
        }

        running_ = true;
        previousTimepoint_ = clock_type::now();

        thread_ = std::thread([this]()
                              {
                                  thread_func();
                              });
    }

    template<typename TClock>
    void simple_timer<TClock>::stop()
    {
        {
            std::lock_guard<std::mutex> lock(mutex_);

            if (!running_)
                return;

            running_ = false;
        }

        thread_.join();
    }

    template<typename TClock>
    double simple_timer<TClock>::interval() const
    {
        return interval_.count();
    }

    template<typename TClock>
    void simple_timer<TClock>::set_interval(double interval)
    {
        interval_ = duration_type(interval);

        {
            std::lock_guard<std::mutex> lock(mutex_);

            if (running_)
            {
                previousTimepoint_ = clock_type::now();
            }
        }
    }

    template<typename TClock>
    bool simple_timer<TClock>::is_triggered() const
    {
        return isTriggered_;
    }

    template<typename TClock>
    void simple_timer<TClock>::reset()
    {
        std::lock_guard<std::mutex> lock(mutex_);
        isTriggered_ = false;
    }

    template<typename TClock>
    void simple_timer<TClock>::thread_func()
    {
        while (true)
        {
            {
                std::lock_guard<std::mutex> lock(mutex_);

                if (!running_)
                    return;
            }

            timepoint_type currentTimepoint = clock_type::now();
            duration_type duration = currentTimepoint - previousTimepoint_;

            if (duration >= interval_)
            {
                isTriggered_ = true;
                previousTimepoint_ = currentTimepoint;
            }
        }
    }
}}

#endif /* ASTRA_SIMPLE_TIMER_H */
