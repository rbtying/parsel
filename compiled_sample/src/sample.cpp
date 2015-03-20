#include <vector>
#include <iostream>
#include <cstdlib>

#include "fft4g.c"
#include "outputs.h"

int main(int argc, char **argv)
{
    if(argc < 2)
    {
        std::cout << "Give a file please\n";
        std::exit(1);
    }
    psl::Signal signal((std::string(argv[1])));
    psl::toWavFile(signal);
}
