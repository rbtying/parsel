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
     
    psl::Signal newSignal = insignal; 
    // shift doesnt work yet 
    // psl::Signal newSignal = insignal.shift(2);
    
    psl::Signal outsignal(psl::toWavFile(&newSignal, "output.wav", 5),
            newSignal.sampleRate(), newSignal.channels());
    

    // main loop
    bool B = true;
    while(outsignal.fillBuffer(B))
        B = !B;
}
