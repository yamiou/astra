// Undeprecate CRT functions
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE 1
#endif

#include "SimpleViewer.h"
#include <memory.h>

#ifdef _WIN32
#include <GL/glut.h>
#else
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
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

#define GL_WIN_SIZE_X   640
#define GL_WIN_SIZE_Y   480
#define TEXTURE_SIZE    1024

#define DEFAULT_DISPLAY_MODE    DISPLAY_MODE_DEPTH

#define MIN_NUM_CHUNKS(data_size, chunk_size)   ((((data_size)-1) / (chunk_size) + 1))
#define MIN_CHUNKS_SIZE(data_size, chunk_size)  (MIN_NUM_CHUNKS(data_size, chunk_size) * (chunk_size))
#include <algorithm>

SampleViewer* SampleViewer::ms_self = nullptr;

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
m_pTexMap(nullptr)
{
    ms_self = this;
    strncpy(m_strSampleName, strSampleName, 255);
}

SampleViewer::~SampleViewer()
{
    astra_reader_destroy(&m_reader);
    astra_streamset_close(&m_sensor);
    astra_terminate();

    delete[] m_pTexMap;

    ms_self = NULL;
}

void SampleViewer::initTextMap(int width, int height)
{
    int desiredW = MIN_CHUNKS_SIZE(width, TEXTURE_SIZE);
    int desiredH = MIN_CHUNKS_SIZE(height, TEXTURE_SIZE);
    if (m_pTexMap == nullptr || desiredW != m_nTexMapX || desiredH != m_nTexMapY)
    {
        m_nTexMapX = desiredW;
        m_nTexMapY = desiredH;
        if (m_pTexMap != nullptr)
        {
            delete m_pTexMap;
        }
        m_pTexMap = new RGB888Pixel[m_nTexMapX * m_nTexMapY];
    }
}

void SampleViewer::init(int argc, char **argv)
{
    astra_initialize();

    astra_streamset_open("1d27/0601@20/30", &m_sensor);
    astra_reader_create(m_sensor, &m_reader);

    astra_reader_get_depthstream(m_reader, &m_depthStream);

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

void SampleViewer::calculateNormals(astra_depthframe_t& frame,
                                    astra_image_metadata_t metadata)
{
    int16_t* depthData;
    size_t depthLength;
    astra_depthframe_get_data_ptr(frame, &depthData, &depthLength);

    int depthWidth = metadata.width;
    int depthHeight = metadata.height;

    int numPixels = depthWidth * depthHeight;
    if (m_normalMap == nullptr || m_normalMapLen != numPixels)
    {
        m_normalMap = new Vector3[numPixels];
        m_blurNormalMap = new Vector3[numPixels];
        memset(m_blurNormalMap, 0, sizeof(Vector3)*numPixels);
        m_normalMapLen = numPixels;
    }
    Vector3* normMap = m_normalMap;

    //top row
    for (int x = 0; x < depthWidth; ++x)
    {
        *normMap = Vector3();
        ++normMap;
    }
    for (int y = 1; y < depthHeight - 1; ++y)
    {
        //first pixel at start of row
        *normMap = Vector3();
        ++normMap;

        for (int x = 1; x < depthWidth - 1; ++x)
        {
            int index = x + y * depthWidth;
            int rightIndex = index + 1;
            int leftIndex = index - 1;
            int upIndex = index - depthWidth;
            int downIndex = index + depthWidth;

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
                astra_convert_depth_to_world(m_depthStream, x, y, depth, &worldX1, &worldY1, &worldZ1);
                astra_convert_depth_to_world(m_depthStream, x + 1, y, depthRight, &worldX2, &worldY2, &worldZ2);
                astra_convert_depth_to_world(m_depthStream, x, y + 1, depthDown, &worldX3, &worldY3, &worldZ3);

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
                astra_convert_depth_to_world(m_depthStream, x, y, depth, &worldX1, &worldY1, &worldZ1);
                astra_convert_depth_to_world(m_depthStream, x, y - 1, depthUp, &worldX2, &worldY2, &worldZ2);
                astra_convert_depth_to_world(m_depthStream, x + 1, y, depthRight, &worldX3, &worldY3, &worldZ3);

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
                astra_convert_depth_to_world(m_depthStream, x, y, depth, &worldX1, &worldY1, &worldZ1);
                astra_convert_depth_to_world(m_depthStream, x - 1, y, depthLeft, &worldX2, &worldY2, &worldZ2);
                astra_convert_depth_to_world(m_depthStream, x, y - 1, depthUp, &worldX3, &worldY3, &worldZ3);

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
                astra_convert_depth_to_world(m_depthStream, x, y, depth, &worldX1, &worldY1, &worldZ1);
                astra_convert_depth_to_world(m_depthStream, x, y + 1, depthDown, &worldX2, &worldY2, &worldZ2);
                astra_convert_depth_to_world(m_depthStream, x - 1, y, depthLeft, &worldX3, &worldY3, &worldZ3);

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
    for (int x = 0; x < depthWidth; ++x)
    {
        *normMap = Vector3();
        ++normMap;
    }

    const int blurRadius = 1;
    //box blur
    for (int y = blurRadius; y < depthHeight - blurRadius; y++)
    {
        for (int x = blurRadius; x < depthWidth - blurRadius; x++)
        {
            Vector3 normAvg;

            for (int dy = -blurRadius; dy <= blurRadius; dy++)
            {
                for (int dx = -blurRadius; dx <= blurRadius; dx++)
                {
                    int index = x + dx + (y + dy) * depthWidth;
                    Vector3 norm = *(m_normalMap + index);

                    normAvg.x += norm.x;
                    normAvg.y += norm.y;
                    normAvg.z += norm.z;
                }
            }
            int centerIndex = x + y*depthWidth;
            *(m_blurNormalMap + centerIndex) = Vector3::Normalize(normAvg);
        }
    }
}

void SampleViewer::showTex(int depthWidth, int depthHeight)
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(0, GL_WIN_SIZE_X, GL_WIN_SIZE_Y, 0, -1.0, 1.0);

    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_TRUE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, m_nTexMapX, m_nTexMapY, 0, GL_RGB, GL_UNSIGNED_BYTE, m_pTexMap);

    // Display the OpenGL texture map

    glColor4f(1, 1, 1, 1);

    glBegin(GL_QUADS);

    int nXRes = depthWidth;
    int nYRes = depthHeight;

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
}

void SampleViewer::updateTex(astra_depthframe_t depthFrame, astra_image_metadata_t metadata)
{
    int depthWidth = metadata.width;
    int depthHeight = metadata.height;

    initTextMap(depthWidth, depthHeight);
    memset(m_pTexMap, 0, m_nTexMapX*m_nTexMapY*sizeof(RGB888Pixel));

    int16_t* pDepthRow;
    size_t depthLength;
    astra_depthframe_get_data_ptr(depthFrame, &pDepthRow, &depthLength);

    RGB888Pixel* pTexRow = m_pTexMap;
    int rowSize = depthWidth;

    Vector3* normMap = m_blurNormalMap;
    bool showNormMap = normMap != nullptr;
    for (int y = 0; y < depthHeight; ++y)
    {
        int16_t* pDepth = pDepthRow;
        RGB888Pixel* pTex = pTexRow;

        for (int x = 0; x < depthWidth; ++x, ++pDepth, ++normMap, ++pTex)
        {
            int16_t depth = *pDepth;
            if (depth != 0)
            {
                //int nHistValue = m_pDepthHist[*pDepth];
                //pTex->r = nHistValue;
                //pTex->g = nHistValue;
                //pTex->b = 0;
            }

            Vector3 norm(1,0,0);
            if (showNormMap)
            {
                norm = *normMap;
            }
            if (!norm.isEmpty())
            {
                //pTex->r = (norm.x * 0.5 + 1) * 255;
                //pTex->g = (norm.y * 0.5 + 1) * 255;
                //pTex->b = (norm.z * 0.5 + 1) * 255;


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
}


void SampleViewer::CalculateHistogram(float* pHistogram, int histogramSize, astra_depthframe_t frame, astra_image_metadata_t metadata)
{
    int16_t* pDepth;
    size_t length;
    astra_depthframe_get_data_ptr(frame, &pDepth, &length);

    // Calculate the accumulative histogram (the yellow display...)
    memset(pHistogram, 0, histogramSize*sizeof(float));

    int depthWidth = metadata.width;
    int depthHeight = metadata.height;

    unsigned int nNumberOfPoints = 0;
    for (int y = 0; y < depthHeight; ++y)
    {
        for (int x = 0; x < depthWidth; ++x, ++pDepth)
        {
            if (*pDepth != 0)
            {
                pHistogram[*pDepth]++;
                nNumberOfPoints++;
            }
        }
    }
    for (int nIndex = 1; nIndex<histogramSize; nIndex++)
    {
        pHistogram[nIndex] += pHistogram[nIndex - 1];
    }
    if (nNumberOfPoints)
    {
        for (int nIndex = 1; nIndex<histogramSize; nIndex++)
        {
            pHistogram[nIndex] = (256 * (1.0f - (pHistogram[nIndex] / nNumberOfPoints)));
        }
    }
}

void SampleViewer::display()
{
    astra_temp_update();
    astra_reader_frame_t frame;
    astra_status_t rc = astra_reader_open_frame(m_reader, 30, &frame);
    if (rc != ASTRA_STATUS_SUCCESS)
    {
        return;
    }

    astra_depthframe_t depthFrame;
    rc = astra_frame_get_depthframe(frame, &depthFrame);

    if (rc != ASTRA_STATUS_SUCCESS)
    {
        return;
    }

    astra_image_metadata_t metadata;
    astra_depthframe_get_metadata(depthFrame, &metadata);

    int depthWidth = metadata.width;
    int depthHeight = metadata.height;

    int16_t* depthData;
    size_t depthLength;
    astra_depthframe_get_data_ptr(depthFrame, &depthData, &depthLength);

    CalculateHistogram(m_pDepthHist, MAX_DEPTH, depthFrame, metadata);

    calculateNormals(depthFrame, metadata);

    updateTex(depthFrame, metadata);

    astra_reader_close_frame(&frame);

    showTex(depthWidth, depthHeight);

    // Swap the OpenGL display buffers
    glutSwapBuffers();
}

void SampleViewer::onKey(unsigned char key, int /*x*/, int /*y*/)
{
    switch (key)
    {
    case 27:
        //shutdown astra
        astra_reader_destroy(&m_reader);
        astra_streamset_close(&m_sensor);
        astra_terminate();
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
