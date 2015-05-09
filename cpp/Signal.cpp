#include "Signal.h"
#include "fillers.h"
#include "utils.h"

#include <sndfile.hh>
#include <iostream>

// TODO: do this better
#define BUFFER_SECS 0.01

using namespace psl;

Signal::Signal(std::string filepath) :
    Signal(*std::shared_ptr<SndfileHandle>(new SndfileHandle(filepath)))
{ }

Signal::Signal(SndfileHandle& file) :
    Signal(psl::fillFromFile(file), file.samplerate(), file.channels())
{ }

Signal::Signal(fill_t fill, int sampleRate, int channels) :
    buffer_(new buffer_t(sampleRate * BUFFER_SECS, sample_t(channels, 0))),
    fill_(fill),
    cacheB_(new bool(false)),
    firstRun_(new bool(true)),
    more_(new bool(true)),
    sampleRate_(sampleRate),
    channels_(channels)
{ }

Signal::Signal(const Signal& other) :
    buffer_(other.buffer_),
    fill_(other.fill_),
    cacheB_(other.cacheB_),
    firstRun_(other.firstRun_),
    more_(other.more_),
    sampleRate_(other.sampleRate_),
    channels_(other.channels_)
{ }

Signal& Signal::operator=(const Signal& other)
{
    buffer_ = other.buffer_;
    fill_ = other.fill_;
    cacheB_ = other.cacheB_;
    firstRun_ = other.firstRun_;
    more_ = other.more_;
    sampleRate_ = other.sampleRate_;
    channels_ = other.channels_;
}

bool Signal::fillBuffer(bool B)
{
    if(B != *cacheB_ || *firstRun_)
    {
        *more_ = fill_(&(*buffer_), B);
        *cacheB_ = B;
        *firstRun_ = false;
    }
    return *more_;
}

int Signal::sampleRate() const
{
    return sampleRate_;
}

int Signal::channels() const
{
    return channels_;
}

// TODO: lazy attribute-getting
Signal psl::operator+(Chunk<Signal> s1, Chunk<Signal> s2)
{
    binop_t f = [](std::complex<double> a, std::complex<double> b)
            { return a + b; };
    return Signal(fillFromOperator(f, s1, s2), s1().sampleRate(),
            s1().channels());
}

Signal psl::operator-(Chunk<Signal> s1, Chunk<Signal> s2)
{
    binop_t f = [](std::complex<double> a, std::complex<double> b)
            { return a - b; };
    return Signal(fillFromOperator(f, s1, s2), s1().sampleRate(),
            s1().channels());
}

Signal psl::operator*(Chunk<Signal> s1, Chunk<Signal> s2)
{
    binop_t f = [](std::complex<double> a, std::complex<double> b)
            { return a * b; };
    return Signal(fillFromOperator(f, s1, s2), s1().sampleRate(),
            s1().channels());
}

Signal psl::operator/(Chunk<Signal> s1, Chunk<Signal> s2)
{
    binop_t f = [](std::complex<double> a, std::complex<double> b)
            { return a / b; };
    return Signal(fillFromOperator(f, s1, s2), s1().sampleRate(),
            s1().channels());
}


Signal Signal::add(double s)
{
    unop_t f = [s](std::complex<double> l) {return l + std::complex<double>(s);};

    return Signal(fillFromOperator(f, chunk()), sampleRate_, channels_);

}

Signal Signal::sub(double s)
{
    unop_t f = [s](std::complex<double> l) {return l - std::complex<double>(s);};

    return Signal(fillFromOperator(f, chunk()), sampleRate_, channels_);

}

Signal Signal::mul(double s)
{
    unop_t f = [s](std::complex<double> l)
    {
    	return l * std::complex<double>(s);
    };

    return Signal(fillFromOperator(f, chunk()), sampleRate_, channels_);

}

Signal Signal::div(double s)
{
    unop_t f = [s](std::complex<double> l) {return l / std::complex<double>(s);};

    return Signal(fillFromOperator(f, chunk()), sampleRate_, channels_);

}

Signal Signal::shift(utime_t delay)
{
    return Signal(fillFromPhaseShift(delay, chunk()), sampleRate_, channels_);
}

Chunk<Signal> Signal::chunk()
{
    return toChunk([this]() { return *this; } );
}
