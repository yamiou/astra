#ifndef ASTRA_REGISTRY_H
#define ASTRA_REGISTRY_H

#include "astra_logger.hpp"
#include <set>

namespace astra {

    template<typename>
    class tracked_instance;

    class registry
    {
    public:

        template<typename T>
        static T* get(void* obj);

    private:
        struct tag_ {};

        using key = std::pair<const tag_*, const void*>;
        using store = std::set<key>;

        template<typename T>
        static void register_instance(tracked_instance<T>* instance);

        template<typename T>
        static void unregister_instance(tracked_instance<T>* instance);

        static store& get_store();

        template<typename T>
        friend class tracked_instance;
    };

    template<typename T>
    void registry::register_instance(tracked_instance<T>* instance)
    {
        if (!instance)
            return;

        const tag_* tag = &tracked_instance<T>::TAG;
        const void* object = instance;

        get_store().insert(std::make_pair(tag, object));
    }

    template<typename T>
    void registry::unregister_instance(tracked_instance<T>* instance)
    {
        if (!instance)
            return;

        const tag_* tag = &tracked_instance<T>::TAG;
        const void* object = instance;

        auto& store = get_store();
        auto pair = std::make_pair(tag, object);

        store.erase(std::move(pair));
    }

    template<typename T>
    T* registry::get(void* obj)
    {
        if (!obj)
            return nullptr;

        const tag_* tag = &tracked_instance<T>::TAG;
        if (get_store().count(std::make_pair(tag, obj)) == 0)
            return nullptr;

        return static_cast<T*>(reinterpret_cast<tracked_instance<T>*>(obj));
    }

    template<typename T>
    class tracked_instance
    {
    public:
        tracked_instance();
        virtual ~tracked_instance();

        // comment these out until we find a workaround for VS2013
        //tracked_instance(tracked_instance&&) = default;
        //tracked_instance& operator=(tracked_instance&&) = default;

        tracked_instance(const tracked_instance&) = default;
        tracked_instance& operator=(const tracked_instance&) = default;

    private:
        static const registry::tag_ TAG;

        friend class registry;

    };

    template<typename T>
    const registry::tag_ tracked_instance<T>::TAG = registry::tag_();

    template<typename T>
    tracked_instance<T>::tracked_instance()
    {
        registry::register_instance(this);
    }

    template<typename T>
    tracked_instance<T>::~tracked_instance()
    {
        registry::unregister_instance(this);
    }
}

#endif /* ASTRA_REGISTRY_H */
