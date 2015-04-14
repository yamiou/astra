#include "SimpleColorViewer.h"

int main(int argc, char** argv)
{
    SimpleColorViewer colorViewer("Color Viewer");

    colorViewer.init(argc, argv);
    colorViewer.run();
}
