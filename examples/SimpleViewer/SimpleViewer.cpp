// Undeprecate CRT functions
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE 1
#endif

#include <SenseKit.h>
#include <SenseKitUL.h>
#include "SimpleViewer.h"
#include <memory.h>

#ifdef _WIN32
#include <GL/glut.h>
#else
#include <GLUT/glut.h>
#endif

//from glext.h
#ifndef GL_SGIS_generate_mipmap
#define GL_GENERATE_MIPMAP_SGIS           0x8191
#define GL_GENERATE_MIPMAP_HINT_SGIS      0x8192
#endif

#include "utils.h"
#ifdef _WIN32
//for strncpy
#include <stdexcept>
#endif

#define GL_WIN_SIZE_X   1280
#define GL_WIN_SIZE_Y   1024
#define TEXTURE_SIZE    512

#define DEFAULT_DISPLAY_MODE    DISPLAY_MODE_DEPTH

#define MIN_NUM_CHUNKS(data_size, chunk_size)   ((((data_size)-1) / (chunk_size) + 1))
#define MIN_CHUNKS_SIZE(data_size, chunk_size)  (MIN_NUM_CHUNKS(data_size, chunk_size) * (chunk_size))
#include <algorithm>

SampleViewer* SampleViewer::ms_self = NULL;

void SampleViewer::glutIdle()
{
    glutPostRedisplay();
}
void SampleViewer::glutDisplay()
{
    SampleViewer::ms_self->display();
}
void SampleViewer::glutKeyboard(unsigned char key, int x, int y)
{
    SampleViewer::ms_self->onKey(key, x, y);
}

SampleViewer::SampleViewer(const char* strSampleName) :
m_pTexMap(NULL)
{
    ms_self = this;
    strncpy(m_strSampleName, strSampleName, 255);
}

SampleViewer::~SampleViewer()
{
    sensekit_reader_destroy(&m_reader);
    sensekit_streamset_close(&m_sensor);
    sensekit_terminate();

    delete[] m_pTexMap;

    ms_self = NULL;
}

void SampleViewer::init(int argc, char **argv)
{
    sensekit_initialize();

    sensekit_streamset_open("1d27/0601@20/30", &m_sensor);
    sensekit_reader_create(m_sensor, &m_reader);

    sensekit_depth_get(m_reader, &m_depthStream);

    int depthWidth = 320;
    int depthHeight = 240;
    m_width = depthWidth;
    m_height = depthHeight;

    // Texture map init
    m_nTexMapX = MIN_CHUNKS_SIZE(m_width, TEXTURE_SIZE);
    m_nTexMapY = MIN_CHUNKS_SIZE(m_height, TEXTURE_SIZE);
    m_pTexMap = new RGB888Pixel[m_nTexMapX * m_nTexMapY];

    m_lightVector = Vector3::Normalize(Vector3(.5, -0.2, 1));
    //m_lightVector = Vector3::Normalize(Vector3(0, 0, 1));
    m_lightColor.r = 210;
    m_lightColor.g = 210;
    m_lightColor.b = 210;

    m_ambientColor.r = 30;
    m_ambientColor.g = 30;
    m_ambientColor.b = 30;

    return initOpenGL(argc, argv);

}
void SampleViewer::run()      //Does not return
{
    glutMainLoop();
}

void SampleViewer::calculateNormals(sensekit_depthframe_t& frame)
{
    int width = frame.width;
    int height = frame.height;
    int length = width * height;
    if (m_normalMap == nullptr || m_normalMapLen != length)
    {
        m_normalMap = new Vector3[length];
        m_blurNormalMap = new Vector3[length];
        memset(m_blurNormalMap, 0, sizeof(Vector3)*length);
        m_normalMapLen = length;
    }
    Vector3* normMap = m_normalMap;
    int16_t* depthData = frame.data;

    //top row
    for (int x = 0; x < width - 1; ++x)
    {
        *normMap = Vector3();
        ++normMap;
    }
    for (int y = 1; y < height - 1; ++y)
    {
        //first pixel at start of row
        *normMap = Vector3();
        ++normMap;

        for (int x = 1; x < width - 1; ++x)
        {
            int index = x + y * width;
            int rightIndex = index + 1;
            int leftIndex = index - 1;
            int upIndex = index - width;
            int downIndex = index + width;

            int16_t depth = *(depthData + index);
            int16_t depthLeft = *(depthData + leftIndex);
            int16_t depthRight = *(depthData + rightIndex);
            int16_t depthUp = *(depthData + upIndex);
            int16_t depthDown = *(depthData + downIndex);

            Vector3 normAvg;

            if (depth != 0 && depthRight != 0 && depthDown != 0)
            {
                float worldX1, worldY1, worldZ1;
                float worldX2, worldY2, worldZ2;
                float worldX3, worldY3, worldZ3;
                convert_depth_to_world(x, y, depth, &worldX1, &worldY1, &worldZ1);
                convert_depth_to_world(x + 1, y, depthRight, &worldX2, &worldY2, &worldZ2);
                convert_depth_to_world(x, y + 1, depthDown, &worldX3, &worldY3, &worldZ3);

                Vector3 v1 = Vector3(worldX2 - worldX1, worldY2 - worldY1, worldZ2 - worldZ1);
                Vector3 v2 = Vector3(worldX3 - worldX1, worldY3 - worldY1, worldZ3 - worldZ1);

                Vector3 norm = Vector3::CrossProduct(v2, v1);
                normAvg.x += norm.x;
                normAvg.y += norm.y;
                normAvg.z += norm.z;
            }

            if (depth != 0 && depthRight != 0 && depthUp != 0)
            {
                float worldX1, worldY1, worldZ1;
                float worldX2, worldY2, worldZ2;
                float worldX3, worldY3, worldZ3;
                convert_depth_to_world(x, y, depth, &worldX1, &worldY1, &worldZ1);
                convert_depth_to_world(x, y - 1, depthUp, &worldX2, &worldY2, &worldZ2);
                convert_depth_to_world(x + 1, y, depthRight, &worldX3, &worldY3, &worldZ3);

                Vector3 v1 = Vector3(worldX2 - worldX1, worldY2 - worldY1, worldZ2 - worldZ1);
                Vector3 v2 = Vector3(worldX3 - worldX1, worldY3 - worldY1, worldZ3 - worldZ1);

                Vector3 norm = Vector3::CrossProduct(v2, v1);

                normAvg.x += norm.x;
                normAvg.y += norm.y;
                normAvg.z += norm.z;
            }


            if (depth != 0 && depthLeft != 0 && depthUp != 0)
            {
                float worldX1, worldY1, worldZ1;
                float worldX2, worldY2, worldZ2;
                float worldX3, worldY3, worldZ3;
                convert_depth_to_world(x, y, depth, &worldX1, &worldY1, &worldZ1);
                convert_depth_to_world(x - 1, y, depthLeft, &worldX2, &worldY2, &worldZ2);
                convert_depth_to_world(x, y - 1, depthUp, &worldX3, &worldY3, &worldZ3);

                Vector3 v1 = Vector3(worldX2 - worldX1, worldY2 - worldY1, worldZ2 - worldZ1);
                Vector3 v2 = Vector3(worldX3 - worldX1, worldY3 - worldY1, worldZ3 - worldZ1);

                Vector3 norm = Vector3::CrossProduct(v2, v1);

                normAvg.x += norm.x;
                normAvg.y += norm.y;
                normAvg.z += norm.z;
            }

            if (depth != 0 && depthLeft != 0 && depthDown != 0)
            {
                float worldX1, worldY1, worldZ1;
                float worldX2, worldY2, worldZ2;
                float worldX3, worldY3, worldZ3;
                convert_depth_to_world(x, y, depth, &worldX1, &worldY1, &worldZ1);
                convert_depth_to_world(x, y + 1, depthDown, &worldX2, &worldY2, &worldZ2);
                convert_depth_to_world(x - 1, y, depthLeft, &worldX3, &worldY3, &worldZ3);

                Vector3 v1 = Vector3(worldX2 - worldX1, worldY2 - worldY1, worldZ2 - worldZ1);
                Vector3 v2 = Vector3(worldX3 - worldX1, worldY3 - worldY1, worldZ3 - worldZ1);

                Vector3 norm = Vector3::CrossProduct(v2, v1);
                normAvg.x += norm.x;
                normAvg.y += norm.y;
                normAvg.z += norm.z;
            }

            *normMap = Vector3::Normalize(normAvg);
            /*
            //reference sphere

            const float PI = 3.141592;
            float normX = 2*(x / (float)width)-1;
            float angleX = 0.5 * PI * normX;
            float normY = 2*(y / (float)height)-1;
            float angleY = 0.5 * PI * normY;
            if (sqrt(normX*normX + normY*normY) < 1)
            {
            *normMap = Vector3(sin(angleX)*cos(angleY), sin(angleY)*cos(angleX), cos(angleY)*cos(angleX));
            }
            else
            {
            *normMap = Vector3();
            }
            */
            ++normMap;
        }
        //last pixel at end of row
        *normMap = Vector3();
        ++normMap;
    }
    //bottom row
    for (int x = 0; x < width - 1; ++x)
    {
        *normMap = Vector3();
        ++normMap;
    }

    const int blurRadius = 1;
    //box blur
    for (int y = blurRadius; y < height - blurRadius; y++)
    {
        for (int x = blurRadius; x < width - blurRadius; x++)
        {
            Vector3 normAvg;

            for (int dy = -blurRadius; dy <= blurRadius; dy++)
            {
                for (int dx = -blurRadius; dx <= blurRadius; dx++)
                {
                    int index = x + dx + (y + dy) * width;
                    Vector3 norm = *(m_normalMap + index);

                    normAvg.x += norm.x;
                    normAvg.y += norm.y;
                    normAvg.z += norm.z;
                }
            }
            int centerIndex = x + y*width;
            *(m_blurNormalMap + centerIndex) = Vector3::Normalize(normAvg);
        }
    }
}

void SampleViewer::display()
{
    sensekit_temp_update();
    sensekit_depth_frame_open(m_depthStream, 30, &m_depthFrame);

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(0, GL_WIN_SIZE_X, GL_WIN_SIZE_Y, 0, -1.0, 1.0);

    calculateHistogram(m_pDepthHist, MAX_DEPTH, *m_depthFrame);

    calculateNormals(*m_depthFrame);

    memset(m_pTexMap, 0, m_nTexMapX*m_nTexMapY*sizeof(RGB888Pixel));

    short* pDepthRow = (short*)m_depthFrame->data;
    RGB888Pixel* pTexRow = m_pTexMap;
    int rowSize = m_depthFrame->width;

    Vector3* normMap = m_blurNormalMap;

    for (int y = 0; y < m_depthFrame->height; ++y)
    {
        short* pDepth = pDepthRow;
        RGB888Pixel* pTex = pTexRow;

        for (int x = 0; x < m_depthFrame->width; ++x, ++pDepth, ++normMap, ++pTex)
        {
            short depth = *pDepth;
            if (depth != 0)
            {
                /*
                int nHistValue = m_pDepthHist[*pDepth];
                pTex->r = nHistValue;
                pTex->g = nHistValue;
                pTex->b = 0;
                */
            }

            Vector3 norm = *normMap;
            if (!norm.isEmpty())
            {
                /*    pTex->r = (norm.x * 0.5 + 1) * 255;
                    pTex->g = (norm.y * 0.5 + 1) * 255;
                    pTex->b = (norm.z * 0.5 + 1) * 255;
                    */

                float fadeFactor = 1 - 0.6*std::max(0.0f, std::min(1.0f, ((depth - 400) / 3200.0f)));
                float diffuseFactor = Vector3::DotProduct(norm, m_lightVector);
                RGB888Pixel diffuseColor;

                if (diffuseFactor > 0)
                {
                    //only add diffuse when mesh is facing the light
                    diffuseColor.r = m_lightColor.r * diffuseFactor;
                    diffuseColor.g = m_lightColor.g * diffuseFactor;
                    diffuseColor.b = m_lightColor.b * diffuseFactor;
                }
                else
                {
                    diffuseColor.r = 0;
                    diffuseColor.g = 0;
                    diffuseColor.b = 0;
                }

                pTex->r = std::max(0, std::min(255, (int)(fadeFactor*(m_ambientColor.r + diffuseColor.r))));
                pTex->g = std::max(0, std::min(255, (int)(fadeFactor*(m_ambientColor.g + diffuseColor.g))));
                pTex->b = std::max(0, std::min(255, (int)(fadeFactor*(m_ambientColor.b + diffuseColor.b))));
            }
        }

        pDepthRow += rowSize;
        pTexRow += m_nTexMapX;
    }

    sensekit_depth_frame_close(&m_depthFrame);

    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_TRUE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, m_nTexMapX, m_nTexMapY, 0, GL_RGB, GL_UNSIGNED_BYTE, m_pTexMap);

    // Display the OpenGL texture map
    glColor4f(1, 1, 1, 1);

    glBegin(GL_QUADS);

    int nXRes = m_width;
    int nYRes = m_height;

    // upper left
    glTexCoord2f(0, 0);
    glVertex2f(0, 0);
    // upper right
    glTexCoord2f((float)nXRes / (float)m_nTexMapX, 0);
    glVertex2f(GL_WIN_SIZE_X, 0);
    // bottom right
    glTexCoord2f((float)nXRes / (float)m_nTexMapX, (float)nYRes / (float)m_nTexMapY);
    glVertex2f(GL_WIN_SIZE_X, GL_WIN_SIZE_Y);
    // bottom left
    glTexCoord2f(0, (float)nYRes / (float)m_nTexMapY);
    glVertex2f(0, GL_WIN_SIZE_Y);

    glEnd();

    // Swap the OpenGL display buffers
    glutSwapBuffers();

}

void SampleViewer::onKey(unsigned char key, int /*x*/, int /*y*/)
{
    switch (key)
    {
    case 27:
        //shutdown sensekit
        sensekit_reader_destroy(&m_reader);
        sensekit_streamset_close(&m_sensor);
        sensekit_terminate();
        exit(1);
    }

}

void SampleViewer::initOpenGL(int argc, char **argv)
{
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
    glutInitWindowSize(GL_WIN_SIZE_X, GL_WIN_SIZE_Y);
    glutCreateWindow(m_strSampleName);
    //      glutFullScreen();
    glutSetCursor(GLUT_CURSOR_NONE);

    initOpenGLHooks();

    glDisable(GL_DEPTH_TEST);
    glEnable(GL_TEXTURE_2D);
}

void SampleViewer::initOpenGLHooks()
{
    glutKeyboardFunc(glutKeyboard);
    glutDisplayFunc(glutDisplay);
    glutIdleFunc(glutIdle);
}
