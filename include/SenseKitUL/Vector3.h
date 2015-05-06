#ifndef VECTOR3_H
#define VECTOR3_H

#include <math.h>
#include <SenseKitUL/skul_ctypes.h>

struct cVector3i { };
namespace sensekit
{
    template<typename T>
    struct Vector3_;

    template<typename T>
    inline bool operator==(const Vector3_<T>& lhs, const Vector3_<T>& rhs)
    {
        return lhs.x == rhs.x && lhs.y == rhs.y && lhs.z == rhs.z;
    }

    template<typename T>
    inline bool operator!=(const Vector3_<T>& lhs, const Vector3_<T>& rhs)
    {
        return !(lhs == rhs);
    }

    template<typename T>
    inline Vector3_<T> operator+(const Vector3_<T>& lhs, const Vector3_<T>& rhs)
    {
        return Vector3_<T>(lhs.x + rhs.x, lhs.y + rhs.y, lhs.z + rhs.z);
    }

    template<typename T>
    inline Vector3_<T> operator-(const Vector3_<T>& lhs, const Vector3_<T>& rhs)
    {
        return Vector3_<T>(lhs.x - rhs.x, lhs.y - rhs.y, lhs.z - rhs.z);
    }

    template<typename T>
    inline Vector3_<T> operator*(const Vector3_<T>& lhs, const float& rhs)
    {
        return Vector3_<T>(lhs.x * rhs, lhs.y * rhs, lhs.z * rhs);
    }

    template<typename T>
    inline Vector3_<T> operator*(const float& lhs, const Vector3_<T>& rhs)
    {
        return rhs * lhs;
    }

    template<typename T>
    inline Vector3_<T> operator/(const Vector3_<T>& lhs, const float& rhs)
    {
        return Vector3_<T>(lhs.x / rhs, lhs.y / rhs, lhs.z / rhs);
    }

    using Vector3i = Vector3_<int>;
    using Vector3f = Vector3_<float>;

    template<typename T>
    struct Vector3_
    {
        T x;
        T y;
        T z;

        Vector3_()
            : x(0), y(0), z(0)
        { }

        Vector3_(T x, T y, T z)
            : x(x), y(y), z(z)
        { }

        float length() const;
        float length_squared() const;
        float dot(const Vector3_<T>& v) const;

        Vector3_<T> cross(const Vector3_<T>& v) const;

        static const Vector3_<T> zero;

        static Vector3_<T> normalize(Vector3_<T> v);

        friend bool operator== <>(const Vector3_<T>& lhs, const Vector3_<T>& rhs);
        friend bool operator!= <>(const Vector3_<T>& lhs, const Vector3_<T>& rhs);

        bool is_zero() const;

        Vector3_<T>& operator+=(const Vector3_<T>& rhs);
        Vector3_<T>& operator-=(const Vector3_<T>& rhs);
        Vector3_<T>& operator*=(const float& rhs);
        Vector3_<T>& operator/=(const float& rhs);

        friend Vector3_<T> operator+ <>(const Vector3_<T>& lhs, const Vector3_<T>& rhs);
        friend Vector3_<T> operator- <>(const Vector3_<T>& lhs, const Vector3_<T>& rhs);
        friend Vector3_<T> operator* <>(const Vector3_<T>& lhs, const float& rhs);
        friend Vector3_<T> operator* <>(const float& lhs, const Vector3_<T>& rhs);
        friend Vector3_<T> operator/ <>(const Vector3_<T>& lhs, const float& rhs);

        Vector3_<T> operator-();
    };

    template<typename T>
    const Vector3_<T> Vector3_<T>::zero = Vector3_<T>();

    inline Vector3f cvector_to_vector(const sensekit_vector3f_t& cvector)
    {
        return Vector3f(cvector.x, cvector.y, cvector.z);
    }

    inline sensekit_vector3f_t vector_to_cvector(const Vector3f& vector)
    {
        sensekit_vector3f_t cvector;
        cvector.x = vector.x;
        cvector.y = vector.y;
        cvector.z = vector.z;

        return cvector;
    }

    template<typename T>
    inline float Vector3_<T>::length() const
    {
        return sqrtf(x * x + y * y + z * z);
    }

    template<typename T>
    inline float Vector3_<T>::length_squared() const
    {
        return x * x + y * y + z * z;
    }

    template<typename T>
    inline float Vector3_<T>::dot(const Vector3_<T>& v) const
    {
        return x * v.x + y * v.y + z * v.z;
    }

    template<typename T>
    inline Vector3_<T> Vector3_<T>::cross(const Vector3_<T>& v) const
    {
        return Vector3_(y*v.z - z*v.y, z*v.x - x*v.z, x*v.y - y*v.x);
    }

    template<typename T>
    inline Vector3_<T> Vector3_<T>::normalize(Vector3_<T> v)
    {
        double length = sqrtf(v.x*v.x + v.y*v.y + v.z*v.z);
        if (length < 1e-9)
        {
            return Vector3_<T>(0, 0, 0);
        }
        else
        {
            return Vector3_<T>(v.x / length, v.y / length, v.z / length);
        }
    }

    template<typename T>
    inline bool Vector3_<T>::is_zero() const { return *this == Vector3_<T>::zero; }

    template<typename T>
    inline Vector3_<T>& Vector3_<T>::operator+=(const Vector3_<T>& rhs)
    {
        this->x = this->x + rhs.x;
        this->y = this->y + rhs.y;
        this->z = this->z + rhs.z;
        return *this;
    }

    template<typename T>
    inline Vector3_<T>& Vector3_<T>::operator-=(const Vector3_& rhs)
    {
        this->x = this->x - rhs.x;
        this->y = this->y - rhs.y;
        this->z = this->z - rhs.z;
        return *this;
    }

    template<typename T>
    inline Vector3_<T>& Vector3_<T>::operator*=(const float& rhs)
    {
        this->x = this->x * rhs;
        this->y = this->y * rhs;
        this->z = this->z * rhs;
        return *this;
    }

    template<typename T>
    inline Vector3_<T>& Vector3_<T>::operator/=(const float& rhs)
    {
        this->x = this->x / rhs;
        this->y = this->y / rhs;
        this->z = this->z / rhs;
        return *this;
    }

    template<typename T>
    inline Vector3_<T> Vector3_<T>::operator-()
    {
        return Vector3_<T>(-this->x, -this->y, -this->z);
    }
}

#endif // VECTOR3_H