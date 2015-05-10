#pragma once

#include <vector>
#include <complex>
#include <functional>
#include "Chunk.h"
#include "Signal.h"

namespace psl
{
    /*!
     * Represents microseconds
     */
    typedef unsigned long utime_t;

    class Interval
    {
        public:
            /*!
             * Constructs an interval from a signal and a pair of times
             *
             * startTime and endTime are treated as times from the beginning
             * of the signal buffer
             *
             * \param sig signal to grab data from
             * \param startTime the start time of the interval
             * \param endTime the end time of the interval
             */
            Interval(Chunk<Signal> sig, utime_t duration);
            Interval(const Interval& other);
            Interval& operator=(const Interval& other);
            Interval();

            utime_t duration() const { return length_ * 10e6 / sampleRate_; }
            utime_t start() const { return startTime_; }
            utime_t end() const { return startTime_ + duration(); }
            int sampleRate() const { return sampleRate_; }
            int channels() const { return channels_; }

            bool advance();

            const buffer_t& buffer() const { return buffer_; }
            buffer_t buffer_;

        private:
            utime_t startTime_;
            utime_t length_;
            int sampleRate_;
            int channels_;
            fill_t f_;
            bool B_;

            long ceil_power_2(long val)
            {
                long power = 2;
                while (val >>= 1) power <<= 1;
                return power;
            }
    };
}
