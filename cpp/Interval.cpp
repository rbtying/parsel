#include <cassert>
#include "Interval.h"

namespace psl
{
    Interval::Interval() :
        buffer_(0),
        startTime_(0),
        length_(0),
        sampleRate_(0),
        channels_(0),
        f_(0)
    { }

    Interval::Interval(Chunk<Signal> sig, utime_t duration) :
        f_(sig().fill_gen_()),
        startTime_(0),
        length_(ceil_power_2(duration * sig().sampleRate() / 10e6)),
        sampleRate_(sig().sampleRate()),
        channels_(sig().channels()),
        B_(false)
    {
        std::cout << "initialized interval" << std::endl;
        startTime_ = -length_;
        advance();
    }

    Interval& Interval::operator=(const Interval& other)
    {
        if (this != &other) {
            buffer_.clear();
            buffer_.assign(other.buffer_.begin(), other.buffer_.end());
            this->startTime_ = other.startTime_;
            this->length_ = other.length_;
            this->sampleRate_ = other.sampleRate_;
            this->channels_ = other.channels_;
            this->B_ = other.B_;
        }
    }

    Interval::Interval(const Interval& other)
    {
        buffer_.clear();
        buffer_.assign(other.buffer_.begin(), other.buffer_.end());
        this->startTime_ = other.startTime_;
        this->length_ = other.length_;
        this->sampleRate_ = other.sampleRate_;
        this->channels_ = other.channels_;
        this->B_ = other.B_;
    }

    bool Interval::advance()
    {
        B_ = !B_;
        buffer_.clear();
        buffer_.resize(length_);
        bool succ = f_(&buffer_, B_);
        if (!succ) {
            return false;
        }
        startTime_ += duration();
        return true;
    }
}
