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
    psl::Signal insignal((std::string(argv[1])));
    psl::fill_t wavFill(psl::toWavFile(&insignal, "output.wav", 5));
    
    psl::Signal outsignal(wavFill,
            insignal.sampleRate(), insignal.channels());

    // main loop
    bool B = true;
    while(outsignal.fillBuffer(B))
        B = !B;
}
