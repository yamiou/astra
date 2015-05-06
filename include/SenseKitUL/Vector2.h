#ifndef VECTOR2_H
#define VECTOR2_H

#include <math.h>
#include <SenseKitUL/skul_ctypes.h>

namespace sensekit
{
    template<typename TType>
    struct Vector2_;

    template<typename T>
    inline bool operator==(const Vector2_<T>& lhs, const Vector2_<T>& rhs)
    {
        return lhs.x == rhs.x && lhs.y == rhs.y;
    }

    template<typename T>
    inline bool operator!=(const Vector2_<T>& lhs, const Vector2_<T>& rhs)
    {
        return !(lhs == rhs);
    }

    template<typename T>
    inline Vector2_<T> operator+(const Vector2_<T>& lhs, const Vector2_<T>& rhs)
    {
        return Vector2_<T>(lhs.x + rhs.x, lhs.y + rhs.y);
    }

    template<typename T>
    inline Vector2_<T> operator-(const Vector2_<T>& lhs, const Vector2_<T>& rhs)
    {
        return Vector2_<T>(lhs.x - rhs.x, lhs.y - rhs.y);
    }

    template<typename T>
    inline Vector2_<T> operator*(const Vector2_<T>& lhs, const float& rhs)
    {
        return Vector2_<T>(lhs.x * rhs, lhs.y * rhs);
    }

    template<typename T>
    inline Vector2_<T> operator*(const float& lhs, const Vector2_<T>& rhs)
    {
        return rhs * lhs;
    }

    template<typename T>
    inline Vector2_<T> operator/(const Vector2_<T>& lhs, const float& rhs)
    {
        return Vector2_<T>(lhs.x / rhs, lhs.y / rhs);
    }

    using Vector2i = Vector2_<int>;
    using Vector2f = Vector2_<float>;

    template<typename T>
    struct Vector2_
    {
        T x;
        T y;

        Vector2_() :
            x(0), y(0)
        { }

        Vector2_(T x, T y) :
            x(x), y(y)
        { }

        float length() const;
        float length_squared() const;
        float dot(const Vector2_<T>& v) const;

        static const Vector2_<T> zero;

        static Vector2_<T> normalize(Vector2_<T> v);

        bool is_zero() const { return *this == zero; }

        friend bool operator==<>(const Vector2_<T>& lhs, const Vector2_<T>& rhs);
        friend Vector2_<T> operator+<>(const Vector2_<T>& lhs, const Vector2_<T>& rhs);
        friend Vector2_<T> operator-<>(const Vector2_<T>& lhs, const Vector2_<T>& rhs);
        friend Vector2_<T> operator*<>(const Vector2_<T>& lhs, const float& rhs);
        friend Vector2_<T> operator*<>(const float& lhs, const Vector2_& rhs);
        friend Vector2_<T> operator/<>(const Vector2_<T>& lhs, const float& rhs);

        Vector2_<T> operator-();
        Vector2_<T>& operator+=(const Vector2_<T>& rhs);
        Vector2_<T>& operator-=(const Vector2_<T>& rhs);
        Vector2_<T>& operator*=(const float& rhs);
        Vector2_<T>& operator/=(const float& rhs);

    };

    template<typename T>
    const Vector2_<T> Vector2_<T>::zero = Vector2_<T>();

    inline Vector2i cvector_to_vector(const sensekit_vector2i_t& cvector)
    {
        return Vector2i(cvector.x, cvector.y);
    }

    inline sensekit_vector2i_t vector_to_cvector(const Vector2i& vector)
    {
        sensekit_vector2i_t cvector;
        cvector.x = vector.x;
        cvector.y = vector.y;
        return cvector;
    }

    inline Vector2f cvector_to_vector(const sensekit_vector2f_t& cvector)
    {
        return Vector2f(cvector.x, cvector.y);
    }

    inline sensekit_vector2f_t vector_to_cvector(const Vector2f& vector)
    {
        sensekit_vector2f_t cvector;
        cvector.x = vector.x;
        cvector.y = vector.y;
        return cvector;
    }

    template<typename T>
    inline Vector2_<T> Vector2_<T>::normalize(Vector2_<T> v)
    {
        double length = sqrtf(v.x*v.x + v.y*v.y);
        if (length < 1e-9)
        {
            return Vector2_<T>(0, 0);
        }
        else
        {
            return Vector2_<T>(v.x / length, v.y / length);
        }
    }

    template<typename T>
    inline float Vector2_<T>::length() const
    {
        return sqrtf(x * x + y * y);
    }

    template<typename T>
    inline float Vector2_<T>::length_squared() const
    {
        return x * x + y * y;
    }

    template<typename T>
    inline float Vector2_<T>::dot(const Vector2_& v) const
    {
        return x * v.x + y * v.y;
    }

    template<typename T>
    inline Vector2_<T>& Vector2_<T>::operator+=(const Vector2_<T>& rhs)
    {
        this->x = this->x + rhs.x;
        this->y = this->y + rhs.y;
        return *this;
    }

    template<typename T>
    inline Vector2_<T>& Vector2_<T>::operator-=(const Vector2_<T>& rhs)
    {
        this->x = this->x - rhs.x;
        this->y = this->y - rhs.y;
        return *this;
    }

    template<typename T>
    inline Vector2_<T>& Vector2_<T>::operator*=(const float& rhs)
    {
        this->x = this->x * rhs;
        this->y = this->y * rhs;
        return *this;
    }

    template<typename T>
    inline Vector2_<T>& Vector2_<T>::operator/=(const float& rhs)
    {
        this->x = this->x / rhs;
        this->y = this->y / rhs;
        return *this;
    }

    template<typename T>
    inline Vector2_<T> Vector2_<T>::operator-()
    {
        return Vector2_(-this->x, -this->y);
    }


}

#endif // VECTOR2_H