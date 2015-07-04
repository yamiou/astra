#include "SimpleColorViewer.h"

int main(int argc, char** argv)
{
    sensekit::SenseKit::initialize();
    SimpleColorViewer colorViewer("Color Viewer");

    colorViewer.init(argc, argv);
    colorViewer.run();
}
