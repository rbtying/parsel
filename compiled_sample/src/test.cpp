#include <vector>
#include <iostream>
#include <cstdlib>
#include <fstream>
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
    
    psl::Signal insignal2 = ((std::string(argv[2])));   
  
    // this doesn't work... why?
    psl::Signal newSignal = insignal.add(&insignal2);

    psl::Signal outsignal1(psl::toWavFile(&newSignal, "output.wav", 5),
            newSignal.sampleRate(), newSignal.channels());
    
    

    // main loop
    bool B = true;
    while(outsignal1.fillBuffer(B))
        B = !B;
}
