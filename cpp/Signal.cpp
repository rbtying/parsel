#include "Signal.h"
#include "fillers.h"
#include "utils.h"

#include <sndfile.hh>
#include <iostream>

// TODO: do this better

using namespace psl;

Signal::Signal(std::string filepath) :
    Signal(*std::shared_ptr<SndfileHandle>(new SndfileHandle(filepath)))
{ }

Signal::Signal(SndfileHandle& file) :
    Signal(std::bind(psl::fillFromFile, file), file.samplerate(), file.channels())
{ }

Signal::Signal(std::function<fill_t()> fill_generator, int sampleRate, int channels) :
    buffer_(new buffer_t(4096, sample_t(channels, 0))),
    fill_gen_(fill_generator),
    fill_(fill_generator()),
    cacheB_(new bool(false)),
    firstRun_(new bool(true)),
    more_(new bool(true)),
    sampleRate_(sampleRate),
    channels_(channels)
{ }

Signal::Signal(const Signal& other) :
    buffer_(other.buffer_),
    fill_gen_(other.fill_gen_),
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
    fill_gen_ = other.fill_gen_;
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

    std::function<fill_t()> fn = [=]()->fill_t { return fillFromOperator(f, s1, s2); };
    return Signal(fn, s1().sampleRate(), s1().channels());
}

Signal psl::operator-(Chunk<Signal> s1, Chunk<Signal> s2)
{
    binop_t f = [](std::complex<double> a, std::complex<double> b)
            { return a - b; };
    std::function<fill_t()> fn = [=]()->fill_t { return fillFromOperator(f, s1, s2); };
    return Signal(fn, s1().sampleRate(), s1().channels());
}

Signal psl::operator*(Chunk<Signal> s1, Chunk<Signal> s2)
{
    binop_t f = [](std::complex<double> a, std::complex<double> b)
            { return a * b; };

    std::function<fill_t()> fn = [=]()->fill_t { return fillFromOperator(f, s1, s2); };

    return Signal(fn, s1().sampleRate(), s1().channels());
}

Signal psl::operator/(Chunk<Signal> s1, Chunk<Signal> s2)
{
    binop_t f = [](std::complex<double> a, std::complex<double> b)
            { return a / b; };

    std::function<fill_t()> fn = [=]()->fill_t { return fillFromOperator(f, s1, s2); };

    return Signal(fn, s1().sampleRate(), s1().channels());
}


Signal Signal::add(double s)
{
    unop_t f = [s](std::complex<double> l) {return l + std::complex<double>(s);};

    std::function<fill_t()> fn = [=]()->fill_t{ return fillFromOperator(f, chunk()); };

    return Signal(fn, sampleRate_, channels_);

}

Signal Signal::sub(double s)
{
    unop_t f = [s](std::complex<double> l) {return l - std::complex<double>(s);};

    std::function<fill_t()> fn = [=]()->fill_t{ return fillFromOperator(f, chunk()); };

    return Signal(fn, sampleRate_, channels_);

}

Signal Signal::mul(double s)
{
    unop_t f = [s](std::complex<double> l) { return l * std::complex<double>(s); };

    std::function<fill_t()> fn = [=]()->fill_t{ return fillFromOperator(f, chunk()); };

    return Signal(fn, sampleRate_, channels_);

}

Signal Signal::div(double s)
{
    unop_t f = [s](std::complex<double> l) {return l / std::complex<double>(s);};

    std::function<fill_t()> fn = [=]()->fill_t{ return fillFromOperator(f, chunk()); };

    return Signal(fn, sampleRate_, channels_);

}

Signal Signal::shift(utime_t delay)
{
    return Signal(std::bind(fillFromPhaseShift, delay, chunk()), sampleRate_, channels_);
}

Chunk<Signal> Signal::chunk()
{
    return toChunk([this]() { return *this; } );
}
