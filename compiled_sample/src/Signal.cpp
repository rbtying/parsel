#include "Signal.h"

#include <sndfile.hh>

using namespace psl;


Signal::Signal(std::string filename) :
    Signal(fillFromFile(filename))
{ }

Signal::Signal(fill_t fill) :
    fill_(fill)
{ }

void Signal::fillBuffer()
{
    fill_(buffer_);
}

fill_t Signal::fillFromFile(std::string filename)
{
    SndfileHandle file(filename);

    std::vector<short> fileData(file.channels() * file.frames());
    file.read(fileData.data(), fileData.size());

    int pos = 0;
    return [fileData, pos](buffer_t& buffer) {
        // fill fileData from buffer
    };
}
