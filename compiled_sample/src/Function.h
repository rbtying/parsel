#pragma once

#include <functional>
#include <vector>

/* For signal-independent functions
 */

// TODO: Cache is bad! Same call can have different values. Signal should cache things itself.

namespace psl
{
    template<class R, class... Args>
    class Function
    {
    public:
        Function(std::function<R(Args...)> function, int numCalls);
        const R& run(Args... args, int callIndex);
    private:
        std::function<R(Args...)> function_;
        std::vector<R> cache_;
        std::vector<bool> isComputed_;
    };
}
