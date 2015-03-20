#include "SigFunc.h"

using namespace psl;

template<class R, class... Args>
SigFunc<R, Args...>::
SigFunc(std::function<R(Args..., bool)> function) :
    function_(function)
{ }

template<class R, class... Args>
R SigFunc<R, Args...>::
run(Args... args, bool B)
{
    return function_(args..., B);
}
