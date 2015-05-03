#include "Chunk.h"

using namespace psl;

template<class T>
Chunk<T>::Chunk() :
    f_()
{ }

template<class T>
Chunk<T>::Chunk(std::function<T()> f) :
    f_(f)
{ }

template<class T>
T& Chunk<T>::operator()()
{
    return f_();
}

template<class T>
Chunk<T>& Chunk<T>::operator=(std::function<T()> f)
{
    f_ = f;
}

template<class T>
Chunk<T>::operator T()
{
    return f_();
}
