#include <cassert>
#include "Interval.h"

namespace psl
{
    Interval::Interval() : startTime_(0), endTime_(0), sampleRate_(0), channels_(0) {}

    Interval::Interval(const Signal& sig, utime_t startTime, utime_t endTime) :
        startTime_(startTime), endTime_(endTime),
        sampleRate_(sig.sampleRate()), channels_(sig.channels())
    {
        assert(endTime >= startTime);

        long start_offset = startTime_ * sampleRate_;
        long end_offset = endTime_ * sampleRate_;

        // fail hard if we can't construct a valid interval
        assert(end_offset < sig.buffer_->size());

        buffer_.assign(sig.buffer_->begin() + start_offset, sig.buffer_->begin() + end_offset);
    }

    Interval& Interval::operator=(const Interval& other)
    {
        if (this != &other) {
            buffer_.assign(other.buffer_.begin(), other.buffer_.end());
            this->startTime_ = other.startTime_;
            this->endTime_ = other.endTime_;
            this->sampleRate_ = other.sampleRate_;
            this->channels_ = other.channels_;
        }
    }
}
