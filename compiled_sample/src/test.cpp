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

    psl::Signal outsignal(psl::toWavFile(&insignal, "output.wav", 5),
            insignal.sampleRate(), insignal.channels());

    std::function<int(int, int)> thing;
    std::function<int(int)> thing2;

    thing = [&](int x, int y) { return thing2(x) + thing2(y); };
    thing2 = [&](int x) { return x + 1; };

    std::cout << thing(3, 4) << "\n";


    // main loop
    bool B = true;
    while(outsignal.fillBuffer(B))
        B = !B;
}
