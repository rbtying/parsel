#include <vector>
#include <iostream>
#include <cstdlib>

#include "fft4g.c"
#include "Signal.h"

int main(int argc, char **argv)
{
    if(argc < 2)
    {
        std::cout << "Give a file please\n";
        std::exit(1);
    }
    psl::Signal(std::string(argv[1]));
}