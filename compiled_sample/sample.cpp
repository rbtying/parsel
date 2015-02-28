#include <vector>
#include <iostream>
#include <cstdlib>

#include "fft4g.c"
#include "wavfile.h"
#include "Signal.h"

int main(int argc, char **argv)
{
    if(argc < 2)
    {
        std::cout << "Give a file pls\n";
        std::exit(1);
    }

    FILE *input = wavfile_open(argv[1]);
}
