#ifndef POINT_H
#define POINT_H

#include <math.h>

struct Point3D
{
    float x;
    float y;
    float z;

    Point3D() :
        x(0), y(0), z(0)
    { }

    Point3D(float x, float y, float z) :
        x(x), y(y), z(z)
    { }
};

#endif // POINT_H