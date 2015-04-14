// Undeprecate CRT functions
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE 1
#endif

#include "SimpleColorViewer.h"
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

SimpleColorViewer* SimpleColorViewer::ms_self = NULL;

void SimpleColorViewer::glutIdle()
{
    glutPostRedisplay();
}
void SimpleColorViewer::glutDisplay()
{
    SimpleColorViewer::ms_self->display();
}
void SimpleColorViewer::glutKeyboard(unsigned char key, int x, int y)
{
    SimpleColorViewer::ms_self->onKey(key, x, y);
}

SimpleColorViewer::SimpleColorViewer(const char* strSampleName) :
m_pTexMap(NULL)
{
    ms_self = this;
    strncpy(m_strSampleName, strSampleName, 255);
}

SimpleColorViewer::~SimpleColorViewer()
{
    sensekit_reader_destroy(&m_reader);
    sensekit_streamset_close(&m_sensor);
    sensekit_terminate();

    delete[] m_pTexMap;

    ms_self = NULL;
}

void SimpleColorViewer::init(int argc, char **argv)
{
    sensekit_initialize();

    sensekit_streamset_open("1d27/0601@20/30", &m_sensor);
    sensekit_reader_create(m_sensor, &m_reader);
    sensekit_color_stream_get(m_reader, &m_colorStream);

    int colorWidth = 320;
    int colorHeight = 240;
    m_width = colorWidth;
    m_height = colorHeight;

    // Texture map init
    m_nTexMapX = MIN_CHUNKS_SIZE(m_width, TEXTURE_SIZE);
    m_nTexMapY = MIN_CHUNKS_SIZE(m_height, TEXTURE_SIZE);
    m_pTexMap = new RGB888Pixel[m_nTexMapX * m_nTexMapY];

    return initOpenGL(argc, argv);

}
void SimpleColorViewer::run()      //Does not return
{
    glutMainLoop();
}

void SimpleColorViewer::display()
{
    sensekit_temp_update();
    sensekit_reader_frame_t frame;
    sensekit_status_t rc = sensekit_reader_open_frame(m_reader, 30, &frame);

    if (rc != SENSEKIT_STATUS_SUCCESS)
    {
        return;
    }
    sensekit_color_frame_get(frame, &m_colorFrame);

    uint8_t* colorData;
    size_t colorLength;

    sensekit_colorframe_get_data_ptr(m_colorFrame, &colorData, &colorLength);

    sensekit_colorframe_metadata_t metadata;
    sensekit_colorframe_get_metadata(m_colorFrame, &metadata);

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(0, GL_WIN_SIZE_X, GL_WIN_SIZE_Y, 0, -1.0, 1.0);

    memset(m_pTexMap, 0, m_nTexMapX*m_nTexMapY*sizeof(RGB888Pixel));

    RGB888Pixel* pColorRow = (RGB888Pixel*)colorData;
    RGB888Pixel* pTexRow = m_pTexMap;
    int rowSize = metadata.width;

    for (int y = 0; y < metadata.height; ++y)
    {
        RGB888Pixel* pColor = pColorRow;
        RGB888Pixel* pTex = pTexRow;

        for (int x = 0; x < metadata.width; ++x, ++pColor, ++pTex)
        {
            RGB888Pixel color = *pColor;
            pTex->r = color.r;
            pTex->g = color.g;
            pTex->b = color.b;
        }

        pColorRow += rowSize;
        pTexRow += m_nTexMapX;
    }

    sensekit_reader_close_frame(&frame);

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

void SimpleColorViewer::onKey(unsigned char key, int /*x*/, int /*y*/)
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

void SimpleColorViewer::initOpenGL(int argc, char **argv)
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

void SimpleColorViewer::initOpenGLHooks()
{
    glutKeyboardFunc(glutKeyboard);
    glutDisplayFunc(glutDisplay);
    glutIdleFunc(glutIdle);
}
