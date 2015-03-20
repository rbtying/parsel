#pragma once

#include <functional>

/* For signal-dependent functions that do not return a signal
 * e.g. a function that returns an interval or a list of signals
 */

namespace psl
{
    template<class R, class... Args>
    class SigFunc
    {
    public:
        SigFunc(std::function<R(Args..., bool)> function);
        R run(Args... args, bool B);

    private:
        std::function<R(Args..., bool)> function_;
    };
}
