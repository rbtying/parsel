#include "Function.h"

#include <vector>

using namespace psl;

template<class R, class... Args>
Function<R, Args...>::
Function(std::function<R(Args...)> function, int numCalls) :
    function_(function),
    cache_(numCalls),
    isComputed_(numCalls)
{ }

template<class R, class... Args>
const R& Function<R, Args...>::
run(Args... args, int callIndex)
{
    if(!isComputed_.at(callIndex))
    {
        cache_[callIndex] = function_(args...);
        isComputed_[callIndex] = true;
    }

    return cache_[callIndex];
}
