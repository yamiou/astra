#ifndef VECTOR3_H
#define VECTOR3_H

#include <math.h>
#include <SenseKitUL/cvector.h>

struct cVector3i { };
namespace sensekit
{
    template<typename TType>
    struct Vector3_;

    using Vector3i = Vector3_ < int > ;
    using Vector3f = Vector3_ < float > ;

    template<typename TType>
    struct Vector3_
    {
        TType x;
        TType y;
        TType z;

        Vector3_() :
            x(0), y(0), z(0)
        { }

        Vector3_(TType x, TType y, TType z) :
            x(x), y(y), z(z)
        { }

        float length() const;
        float lengthSquared() const;
        float dot(const Vector3_& v);
        Vector3_ cross(const Vector3_& v);

        friend inline bool operator==(const Vector3_& lhs, const Vector3_& rhs);
        friend inline bool operator!=(const Vector3_& lhs, const Vector3_& rhs);

        Vector3_& operator+=(const Vector3_& rhs);
        Vector3_& operator-=(const Vector3_& rhs);
        Vector3_& operator*=(const float& rhs);
        Vector3_& operator/=(const float& rhs);

        Vector3_ operator-();

        friend Vector3_ operator+(const Vector3_& lhs, const Vector3_& rhs);
        friend Vector3_ operator-(const Vector3_& lhs, const Vector3_& rhs);

        friend Vector3_ operator*(const Vector3_& lhs, const float& rhs);
        friend Vector3_ operator*(const float& lhs, const Vector3_& rhs);
        friend Vector3_ operator/(const Vector3_& lhs, const float& rhs);
    };

    inline Vector3f cvectorToVector(const sensekit_vector3f_t& other)
    {
        return Vector3f(other.x, other.y, other.z);
    }

    inline sensekit_vector3f_t vectorToCvector(const Vector3f& other)
    {
        sensekit_vector3f_t v;
        v.x = other.x;
        v.y = other.y;
        v.z = other.z;
        return v;
    }

    template<typename TType>
    inline float Vector3_<TType>::length() const
    {
        return sqrtf(x * x + y * y + z * z);
    }

    template<typename TType>
    inline float Vector3_<TType>::lengthSquared() const
    {
        return x * x + y * y + z * z;
    }

    template<typename TType>
    inline float Vector3_<TType>::dot(const Vector3_& v)
    {
        return x * v.x + y * v.y + z * v.z;
    }

    template<typename TType>
    inline Vector3_<TType> Vector3_<TType>::cross(const Vector3_& v)
    {
        return Vector3_(y*v.z - z*v.y, z*v.x - x*v.z, x*v.y - y*v.x);
    }

    template<typename TType>
    inline bool operator==(const Vector3_<TType>& lhs, const Vector3_<TType>& rhs)
    {
        return lhs.x == rhs.x && lhs.y == rhs.y && lhs.z == rhs.z;
    }

    template<typename TType>
    inline bool operator!=(const Vector3_<TType>& lhs, const Vector3_<TType>& rhs)
    {
        return !(lhs == rhs);
    }

    template<typename TType>
    inline Vector3_<TType>& Vector3_<TType>::operator+=(const Vector3_<TType>& rhs)
    {
        this->x = this->x + rhs.x;
        this->y = this->y + rhs.y;
        this->z = this->z + rhs.z;
        return *this;
    }

    template<typename TType>
    inline Vector3_<TType>& Vector3_<TType>::operator-=(const Vector3_& rhs)
    {
        this->x = this->x - rhs.x;
        this->y = this->y - rhs.y;
        this->z = this->z - rhs.z;
        return *this;
    }

    template<typename TType>
    inline Vector3_<TType>& Vector3_<TType>::operator*=(const float& rhs)
    {
        this->x = this->x * rhs;
        this->y = this->y * rhs;
        this->z = this->z * rhs;
        return *this;
    }

    template<typename TType>
    inline Vector3_<TType>& Vector3_<TType>::operator/=(const float& rhs)
    {
        this->x = this->x / rhs;
        this->y = this->y / rhs;
        this->z = this->z / rhs;
        return *this;
    }

    template<typename TType>
    inline Vector3_<TType> Vector3_<TType>::operator-()
    {
        return Vector3_<TType>(-this->x, -this->y, -this->z);
    }

    template<typename TType>
    inline Vector3_<TType> operator+(const Vector3_<TType>& lhs, const Vector3_<TType>& rhs)
    {
        return Vector3_<TType>(lhs.x + rhs.x, lhs.y + rhs.y, lhs.z + rhs.z);
    }

    template<typename TType>
    inline Vector3_<TType> operator-(const Vector3_<TType>& lhs, const Vector3_<TType>& rhs)
    {
        return Vector3_<TType>(lhs.x - rhs.x, lhs.y - rhs.y, lhs.z - rhs.z);
    }

    template<typename TType>
    inline Vector3_<TType> operator*(const Vector3_<TType>& lhs, const float& rhs)
    {
        return Vector3_<TType>(lhs.x * rhs, lhs.y * rhs, lhs.z * rhs);
    }

    template<typename TType>
    inline Vector3_<TType> operator*(const float& lhs, const Vector3_<TType>& rhs)
    {
        return rhs * lhs;
    }

    template<typename TType>
    inline Vector3_<TType> operator/(const Vector3_<TType>& lhs, const float& rhs)
    {
        return Vector3_<TType>(lhs.x / rhs, lhs.y / rhs, lhs.z / rhs);
    }
}

#endif // VECTOR3_H