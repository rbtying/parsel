#include <iostream>
#include "outputs.h"
int main(int argc, char **argv) {
std::function<int()> a([]() { return fakeexpr; });
std::function<std::tuple<std::function<signal()>, std::function<signal()>>()> out([]() { return fakeexpr; });
std::tuple<signal, signal> out_;bool B=true;
while(out.get(0).fillBuffer(B)&&out.get(1).fillBuffer(B)) B = !B;
}