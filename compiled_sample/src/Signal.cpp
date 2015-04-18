#include "Signal.h"
#include "outputs.h"
#include <sndfile.hh>
#include <memory>
#include <iostream>

// TODO: do this better
#define BUFFER_SECS 0.01

using namespace psl;

// so jank but i had to put it on the heap to pass the reference
Signal::Signal(std::string filepath) :
    Signal(*std::auto_ptr<SndfileHandle>(new SndfileHandle(filepath)))
{ }

Signal::Signal(SndfileHandle& file) :
    Signal(psl::fillFromFile(file), file.samplerate(), file.channels())
{ }

Signal::Signal(fill_t fill, int sampleRate, int channels) :
    buffer_(sampleRate * BUFFER_SECS, sample_t(channels, 0)),
    fill_(fill),
    sampleRate_(sampleRate),
    cacheB_(),
    firstRun_(true),
    more_(true),
    channels_(channels)
{ }

bool Signal::fillBuffer(bool B)
{
    if(B != cacheB_ || firstRun_)
    {
        more_ = fill_(&buffer_, B);
        cacheB_ = B;
        firstRun_ = false;
    }
    return more_;
}

int Signal::sampleRate() const
{
    return sampleRate_;
}

int Signal::channels() const
{
    return channels_;
}


Signal Signal::add(Signal* s) 
{
    op_ta f = [](std::complex<double> l, std::complex<double> r) {return l+r;};

    return Signal(fillFromOperator(f, this, s), sampleRate_, channels_);
}

Signal Signal::add(double s)
{
    op_tb f = [s](std::complex<double> l) {return l + std::complex<double>(s);};
     
    return Signal(fillFromOperator(f, this), sampleRate_, channels_);

}

Signal Signal::sub(Signal* s) 
{
    op_ta f = [](std::complex<double> l, std::complex<double> r) {return l-r;};

    return Signal(fillFromOperator(f, this, s), sampleRate_, channels_);
}

Signal Signal::sub(double s)
{
    op_tb f = [s](std::complex<double> l) {return l - std::complex<double>(s);};
     
    return Signal(fillFromOperator(f, this), sampleRate_, channels_);

}

Signal Signal::mul(Signal* s) 
{
    op_ta f = [](std::complex<double> l, std::complex<double> r) {return l*r;};
    
    return Signal(fillFromOperator(f, this, s), sampleRate_, channels_);
    
}

Signal Signal::mul(double s)
{
    op_tb f = [s](std::complex<double> l) {return l * std::complex<double>(s);};
     
    return Signal(fillFromOperator(f, this), sampleRate_, channels_);

}

Signal Signal::div(Signal* s) 
{
    op_ta f = [](std::complex<double> l, std::complex<double> r) {return l/r;};

    return Signal(fillFromOperator(f, this, s), sampleRate_ , channels_);

}

Signal Signal::div(double s)
{
    op_tb f = [s](std::complex<double> l) {return l / std::complex<double>(s);};
     
    return Signal(fillFromOperator(f, this), sampleRate_, channels_);

}

Signal Signal::shift(utime_t delay)
{
    return Signal(fillFromPhaseShift(delay, this), sampleRate_, channels_);
}
